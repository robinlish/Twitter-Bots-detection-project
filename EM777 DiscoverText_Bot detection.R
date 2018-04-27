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


#I. Bot data:
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

#bot2000.tidy1 <- gsub("[^\x20-\x7e]"," ", bot2000.tidy$tweet) #Remove special characters, e.g., emoji
bot2000.tidy1 <- gsub("(http)[^[:blank:]]*|[[:digit:]]",",", bot2000.tidy$tweet) #Remove URLs and digits
typeof(bot2000.tidy1)

bot2000.tidy2 <- tbl_df(cbind(bot2000.tidy$bot, bot2000.tidy1))
names(bot2000.tidy2) <- c("bot","tweet")

bot2000_tidy3 <- bot2000.tidy2 %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words) %>%  group_by(bot) %>% 
  count(word, sort=T) %>% arrange(bot)

##as shown above, rt and http are the most frequent words, 
##which can be treated as stop words.
custom_stop_words <- bind_rows(data_frame(word = c("rt","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

##so we redo the step of excluding the stop words
bot2000_td2 <- bot2000_tidy3 %>% anti_join(custom_stop_words)
head(bot2000_td2)

#make it into a matrix form for LDA modeling
names(bot2000_td2) <- c("document","term","count")
dimnames(bot2000_td2)[2]

bot2000_td3 <- bot2000_td2 %>% cast_dtm(document, term, count)
bot2000_td3

##LDA modeling, a optional but time-consuming way to calculate the proper numbers of topics: k value:
##https://cran.r-project.org/web/packages/ldatuning/vignettes/topics.html
#library(ldatuning)
#result <- FindTopicsNumber(
  bot2000_td3[1],
  topics = seq(from = 10, to = 300, by = 10),
  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  mc.cores = 2L,
  verbose = TRUE)

#FindTopicsNumber_plot(result)

lda_bot2000 <- LDA(bot2000_td3, k = 30, method='Gibbs',control = list(seed = 1234))
#save(lda_bot2000, file="lda_bot2000.Rdata")
load("lda_bot2000.Rdata")

##by document*topic
topics_bot2000 <- tidy(lda_bot2000,matrix="gamma")
topics_bot2000$document <- as.numeric(topics_bot2000$document)
topics_bot2000 <- topics_bot2000 %>% arrange(document)
#save(topics_bot2000, file="topics_bot2000.Rdata")
load("topics_bot2000.Rdata")

top.gamma_bot2000 <- topics_bot2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma_bot2000,file="top.gamma_bot2000.Rdata")
load("top.gamma_bot2000.Rdata")

#some documents have two top topics:
View(topics_bot2000 %>% filter(document==1986) %>% filter(gamma==max(gamma)))
mean(top.gamma_bot2000$topgamma)

topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}

serVis(topicmodels2LDAvis(lda_bot2000), out.dir = "bot_vis" ,open.browser = T)

library(jsonlite)
fromJSON(lda_bot2000) -> z

#II. Non-bot data:
#data preparation:

##load bots data into r:
#real2000 <- read.csv("non-bot account_2027.csv")
#real2000 <- tbl_df(t(real2000))
#real2000 <- real2000[-c(1,2),]
#save(real2000,file="real2000.Rdata")
load("real2000.Rdata")

real2000.tidy <- gather(real2000, key="bot", value="tweet", str_c("V",1:ncol(real2000)))
real2000.tidy$bot <- as.numeric(str_replace(real2000.tidy$bot,"V",""))

##class(real2000)
##typeof(real2000$bot)

#1. freq of url
real2000_td <- real2000.tidy %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words)

real2000_wordfreq <- real2000_td %>% group_by(bot) %>% 
  count(word, sort=T)

real2000_wordfreq1 <- arrange(real2000_wordfreq,bot)

real2000_urlfreq <- subset(real2000_wordfreq1, word=="https")

#save(real2000_urlfreq,file="real2000_urlfreq.Rdata")

mean(real2000_urlfreq$n)
sd(real2000_urlfreq$n)

#2. freq of RT:

real2000_1 <- str_count(real2000.tidy$tweet, "RT")
real2000_rt <- cbind(real2000.tidy,real2000_1)
real2000_rt$rt <- ifelse(real2000_rt$real2000_1>=1,1,0)

real2000_rt <- real2000_rt %>% group_by(bot) %>% summarise(freq_of_RT=sum(rt))

mean(real2000_rt$freq_of_RT)
sd(real2000_rt$freq_of_RT)

#save(real2000_rt, file="real2000_RTfreq.Rdata")

#3. LDA topic modeling of 2000 bots:

#real2000.tidy1 <- gsub("[^\x20-\x7e]"," ", real2000.tidy$tweet) #Remove special characters, e.g., emoji
real2000.tidy1 <- gsub("(http)[^[:blank:]]*|[[:digit:]]",",", real2000.tidy$tweet) #Remove URLs and digits
typeof(real2000.tidy1)

real2000.tidy2 <- tbl_df(cbind(real2000.tidy$bot, real2000.tidy1))
names(real2000.tidy2) <- c("bot","tweet")

real2000.tidy2 <- real2000.tidy2[which(!grepl("[^\x01-\x7F]+", real2000.tidy2$tweet)),]

real2000_tidy3 <- real2000.tidy2 %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words) %>%  group_by(bot) %>% 
  count(word, sort=T) %>% arrange(bot)

##as shown above, rt and http are the most frequent words, 
##which can be treated as stop words.
custom_stop_words <- bind_rows(data_frame(word = c("rt","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

##so we redo the step of excluding the stop words
real2000_td2 <- real2000_tidy3 %>% anti_join(custom_stop_words)
head(real2000_td2)

#make it into a matrix form for LDA modeling
names(real2000_td2) <- c("document","term","count")
dimnames(real2000_td2)[2]

real2000_td3 <- real2000_td2 %>% cast_dtm(document, term, count)
real2000_td3

lda_real2000 <- LDA(real2000_td3, k = 30, method='Gibbs',control = list(seed = 1234))
#save(lda_real2000, file="lda_real2000.Rdata")
load("lda_real2000.Rdata")

##by document*topic
topics_real2000 <- tidy(lda_real2000,matrix="gamma")
topics_real2000$document <- as.numeric(topics_real2000$document)
topics_real2000 <- topics_real2000 %>% arrange(document)
#save(topics_real2000, file="topics_real2000.Rdata")
load("topics_real2000.Rdata")

top.gamma_real2000 <- topics_real2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma_real2000,file="top.gamma_real2000.Rdata")
load("top.gamma_real2000.Rdata")

#some documents have two top topics:
#View(topics_real2000 %>% filter(document==1986) %>% filter(gamma==max(gamma)))

mean(top.gamma_real2000$topgamma)

#III. Comparison between bot and non-bot data:

##1. retweet:
bot2000_rt$bot <- "bot"
real2000_rt$bot <- "nonbot"

RTfreq <- rbind(bot2000_rt,real2000_rt)
names(RTfreq) <- c("Type","Number")

ggplot(RTfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)
ggplot(RTfreq, aes(Number,fill=Type))+geom_histogram(alpha=0.7)
ggplot(RTfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_rt$freq_of_RT,real2000_rt$freq_of_RT)
#2. url:
bot2000_urlfreq$bot <- "bot"
real2000_urlfreq$bot <- "nonbot"

URLfreq <- rbind(bot2000_urlfreq[,c(1,3)],real2000_urlfreq[,c(1,3)])
names(URLfreq) <- c("Type","Number")

ggplot(URLfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)
ggplot(URLfreq, aes(Number,fill=Type))+geom_histogram(alpha=0.7)
ggplot(URLfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_urlfreq$n,real2000_urlfreq$n)

#3. lda gamma:
top.gamma_bot2000$document <- "bot"
top.gamma_real2000$document <- "nonbot"

Top.gamma <- rbind(top.gamma_bot2000,top.gamma_real2000)
names(Top.gamma) <- c("Type","Gamma")

ggplot(Top.gamma, aes(Gamma,fill=Type))+geom_density(alpha=0.7)
ggplot(Top.gamma, aes(Gamma,fill=Type))+geom_histogram(alpha=0.7)
ggplot(Top.gamma, aes(y=Gamma,x=Type, fill=Type))+geom_boxplot()

t.test(top.gamma_bot2000$topgamma,top.gamma_real2000$topgamma)







