#please install the following packages using install.packages("") if you don't have them installed
#e.g:
#install.packages("tidytext")
library(tidytext)
library(topicmodels)
library(knitr)
library(dplyr)
library(wordcloud2)
library(ggplot2)
library(stringr)
library(tidyverse)

#data preparation:

##load bots data into r
##save(a,file="bot520.Rdata")
load("bot520.Rdata")
b <- tbl_df(t(a))
b <- b[-c(1,2),]

bot520 <- gather(b, key="bot", value="tweet", str_c("V",1:520))
bot520$bot <- as.numeric(str_replace(bot520$bot,"V",""))

##class(bot520)
##typeof(bot520$bot)

#1. freq of url
bot520_td <- bot520 %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words)

bot520_wordfreq <- bot520_td %>% group_by(bot) %>% 
  count(word, sort=T)

bot520_wordfreq1 <- arrange(bot520_wordfreq,bot)

bot520_urlfreq <- subset(bot520_wordfreq1, word=="https")

#save(bot520_urlfreq,file="bot520_urlfreq.Rdata")

mean(bot520_urlfreq$n)
sd(bot520_urlfreq$n)

#2. freq of RT:

bot520_1 <- str_count(bot520$tweet, "RT")
bot520_rt <- cbind(bot520,bot520_1)
bot520_rt$rt <- ifelse(bot520_rt$bot520_1>=1,1,0)

bot520_rt <- bot520_rt %>% group_by(bot) %>% summarise(freq_of_RT=sum(rt))

mean(bot520_rt$freq_of_RT)
sd(bot520_rt$freq_of_RT)

#save(bot520_rt, file="bot520_RTfreq.Rdata")

#3. LDA topic modeling of 520 bots:

##as shown above, rt and http are the most frequent words, 
##which can be treated as stop words.
custom_stop_words <- bind_rows(data_frame(word = c("rt","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

##so we redo the step of excluding the stop words
bot520_td2 <- bot520_wordfreq1 %>% anti_join(custom_stop_words)
head(bot520_td2)

#make it into a matrix form for LDA modeling
names(bot520_td2) <- c("document","term","count")

bot520_td3 <- bot520_td2 %>% cast_dfm(document, term, count)
bot520_td3

##LDA modeling, calculate the proper numbers of topics: k value:
library(ldatuning)
result <- FindTopicsNumber(
  bot520_td3,
  topics = seq(from = 2, to = 30, by = 1),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)


lda_bot520 <- LDA(bot520_td3, k = 20, method='Gibbs',control = list(seed = 1234))

##by document*topic
topics_bot520 <- tidy(lda_bot520,matrix="gamma")

gamma_bot520 <- as.data.frame(lda_bot520@gamma) #Create a data frame with topics as cols, documents as rows and cell values as posterior topic distribution for each document
names(gamma_bot520) <- c(1:20) #Define the header of the data frame (i.e., gammaDF)
head(gamma_bot520)  #Take a look at the output 
write.csv(gamma_bot520, "gamma_bot520.csv") #Export the results as a csv file 

#Find the top-ranked topic(s) for each document 
toptopics_bot520 <- as.data.frame(cbind(document = row.names(gamma_bot520), topic = apply(gamma_bot520,1,function(x) names(gammaDF)[which(x==max(x))])))

head(toptopics_bot520) #Take a look at the output 

toptopics <- data.frame(lapply(toptopics, as.character), stringsAsFactors=FALSE) #Since one document may contain more than one topic, we need to reformat the data frame. 
write.csv(toptopics, "toptopics.csv")

#by topic*word
bot520_words <- tidy(lda_bot520, matrix="beta")
bot520_words







