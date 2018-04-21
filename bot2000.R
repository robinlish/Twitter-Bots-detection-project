#please install the following packages using install.packages("") if you don't have them installed
#e.g:
##install.packages(c("ldatuning",tidytext","topicmodels","knitr","dplyr","ggplot2","stringr","tidyverse","quanteda"))
library(tidytext)
library(topicmodels)
library(knitr)
library(dplyr)
library(wordcloud2)
library(ggplot2)
library(stringr)
library(tidyverse)

#data preparation:

##load bots data into r:
#bot2000 <- read.csv("all bots.csv")
#bot2000 <- tbl_df(t(bot2000))
#bot2000 <- bot2000[-c(1,2),]
#save(bot2000,file="bot2000.Rdata")
load("bot2000.Rdata")

bot2000.tidy <- gather(bot2000, key="bot", value="tweet", str_c("V",1:ncol(bot2000)))
bot2000.tidy$bot <- as.numeric(str_replace(bot2000.tidy$bot,"V",""))

##class(bot2000)
##typeof(bot2000$bot)

#1. freq of url
bot2000_td <- bot2000.tidy %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words)

bot2000_wordfreq <- bot2000_td %>% group_by(bot) %>% 
  count(word, sort=T)

bot2000_wordfreq1 <- arrange(bot2000_wordfreq,bot)

bot2000_urlfreq <- subset(bot2000_wordfreq1, word=="https")

#save(bot2000_urlfreq,file="bot2000_urlfreq.Rdata")

mean(bot2000_urlfreq$n)
sd(bot2000_urlfreq$n)

#2. freq of RT:

bot2000_1 <- str_count(bot2000.tidy$tweet, "RT")
bot2000_rt <- cbind(bot2000.tidy,bot2000_1)
bot2000_rt$rt <- ifelse(bot2000_rt$bot2000_1>=1,1,0)

bot2000_rt <- bot2000_rt %>% group_by(bot) %>% summarise(freq_of_RT=sum(rt))

mean(bot2000_rt$freq_of_RT)
sd(bot2000_rt$freq_of_RT)

#save(bot2000_rt, file="bot2000_RTfreq.Rdata")

#3. LDA topic modeling of 2000 bots:

##as shown above, rt and http are the most frequent words, 
##which can be treated as stop words.
custom_stop_words <- bind_rows(data_frame(word = c("rt","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

##so we redo the step of excluding the stop words
bot2000_td2 <- bot2000_wordfreq1 %>% anti_join(custom_stop_words)
head(bot2000_td2)

#make it into a matrix form for LDA modeling
names(bot2000_td2) <- c("document","term","count")
dimnames(bot2000_td2)[2]

bot2000_td3 <- bot2000_td2 %>% cast_dtm(document, term, count)
bot2000_td3

##LDA modeling, calculate the proper numbers of topics: k value:
library(ldatuning)
result <- FindTopicsNumber(
  bot2000_td3[1],
  topics = seq(from = 10, to = 300, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)

FindTopicsNumber_plot(result)

lda_bot2000 <- LDA(bot2000_td3, k = 20, method='Gibbs',control = list(seed = 1234))

##by document*topic
topics_bot2000 <- tidy(lda_bot2000,matrix="gamma")

gamma_bot2000 <- as.data.frame(lda_bot2000@gamma) #Create a data frame with topics as cols, documents as rows and cell values as posterior topic distribution for each document
names(gamma_bot2000) <- c(1:20) #Define the header of the data frame (i.e., gammaDF)
head(gamma_bot2000)  #Take a look at the output 
write.csv(gamma_bot2000, "gamma_bot2000.csv") #Export the results as a csv file 

#Find the top-ranked topic(s) for each document 
toptopics_bot2000 <- as.data.frame(cbind(document = row.names(gamma_bot2000), topic = apply(gamma_bot2000,1,function(x) names(gammaDF)[which(x==max(x))])))

head(toptopics_bot2000) #Take a look at the output 

toptopics <- data.frame(lapply(toptopics, as.character), stringsAsFactors=FALSE) #Since one document may contain more than one topic, we need to reformat the data frame. 
write.csv(toptopics, "toptopics.csv")

#by topic*word
bot2000_words <- tidy(lda_bot2000, matrix="beta")
bot2000_words


