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
library(LDAvis)

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
class(bot2000.tidy$bot)

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

bot2000.tidy2$bot <- as.numeric(bot2000.tidy2$bot)

bot2000.tidy2 <- bot2000.tidy2[which(!grepl("[^\x01-\x7F]+", bot2000.tidy2$tweet)),]

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
#  bot2000_td3[1],
#  topics = seq(from = 10, to = 300, by = 10),
#  metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#  method = "Gibbs",
#  control = list(seed = 77),
#  mc.cores = 2L,
#  verbose = TRUE)

#FindTopicsNumber_plot(result)

#k = 50:
#lda50_bot2000 <- LDA(bot2000_td3, k = 50, method='Gibbs',control = list(seed = 1234))
#save(lda50_bot2000, file="lda50_bot2000.Rdata")
load("lda50_bot2000.Rdata")

##by document*topic
topics50_bot2000 <- tidy(lda50_bot2000,matrix="gamma")
topics50_bot2000$document <- as.numeric(topics50_bot2000$document)
topics50_bot2000 <- topics50_bot2000 %>% arrange(document)
#save(topics50_bot2000, file="topics_bot2000.Rdata")
#load("topics_bot2000.Rdata")

#top.gamma50_bot2000 <- topics50_bot2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma50_bot2000,file="top.gamma50_bot2000.Rdata")
load("top.gamma50_bot2000.Rdata")

#some documents have two top topics:
#View(topics_bot2000 %>% filter(document==1986) %>% filter(gamma==max(gamma)))

gammaDF <- as.data.frame(lda50_bot2000@gamma) #Create a data frame with topics as cols, documents as rows and cell values as posterior topic distribution for each document
names(gammaDF) <- c(1:50) #Define the header of the data frame (i.e., gammaDF)
head(gammaDF)  #Take a look at the output 
sum(gammaDF[1,])
#Find the top-ranked topic(s) for each document 
toptopics <- as.data.frame(cbind(document = row.names(gammaDF), topic = apply(gammaDF,1,function(x) names(gammaDF)[which(x==max(x))])))

head(toptopics) #Take a look at the output 

toptopics <- data.frame(lapply(toptopics, as.character), stringsAsFactors=FALSE) #Since one document may contain more than one topic, we need to reformat the data frame. 

mean(top.gamma50_bot2000$topgamma)

# k =30:

#lda_bot2000 <- LDA(bot2000_td3, k = 30, method='Gibbs',control = list(seed = 1234))
#save(lda_bot2000, file="lda_bot2000.Rdata")
load("lda_bot2000.Rdata")

##by document*topic
topics_bot2000 <- tidy(lda_bot2000,matrix="gamma")
topics_bot2000$document <- as.numeric(topics_bot2000$document)
topics_bot2000 <- topics_bot2000 %>% arrange(document)
#save(topics_bot2000, file="topics_bot2000.Rdata")
#load("topics_bot2000.Rdata")

top.gamma_bot2000 <- topics_bot2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma_bot2000,file="top.gamma_bot2000.Rdata")
load("top.gamma_bot2000.Rdata")

#some documents have two top topics:
#View(topics_bot2000 %>% filter(document==1986) %>% filter(gamma==max(gamma)))

mean(top.gamma_bot2000$topgamma)


#4. Visualize LDA:

#k = 50:
post_bot2000 <- topicmodels::posterior(lda50_bot2000)
mat_bot2000 <- lda50_bot2000@wordassignments

ldavis50_bot2000 <- list(phi_bot = post_bot2000[["terms"]], 
                       theta_bot = post_bot2000[["topics"]], 
                       doc.length_bot = slam::row_sums(mat_bot2000, na.rm = T), 
                       vocab_bot = colnames(post_bot2000[["terms"]]), 
                       term.frequency_bot = slam::col_sums(mat_bot2000, na.rm = T))

#save(ldavis_bot2000, file="ldavis_bot2000.Rdata")


#5. expressive marks:

##exclamation mark !!!!!!!!
bot2000_exclamation <- str_count(bot2000.tidy$tweet, "\\!")
bot2000_exclamation <- cbind(bot2000.tidy,bot2000_exclamation)
bot2000_exclamation <- bot2000_exclamation %>% group_by(bot) %>% summarise(freq_of_exclamation=sum(bot2000_exclamation))

##question mark ?????????
bot2000_question <- str_count(bot2000.tidy$tweet, "\\?")
bot2000_question <- cbind(bot2000.tidy,bot2000_question)
bot2000_question <- bot2000_question %>% group_by(bot) %>% summarise(freq_of_question=sum(bot2000_question))


#6. distance:
library(jsonlite)
ldadistance50_bot2000 <- createJSON(phi=ldavis50_bot2000$phi_bot,
                                     theta=ldavis50_bot2000$theta_bot,
                                     doc.length = ldavis50_bot2000$doc.length_bot,
                                     vocab = ldavis50_bot2000$vocab_bot,
                                     term.frequency = ldavis50_bot2000$term.frequency_bot)
save(ldadistance50_bot2000, file = "ldadistance50_bot2000.Rdata")

fromJSON(ldadistance50_bot2000) -> distance50_bot
cbind(distance50_bot$mdsDat$x, distance50_bot$mdsDat$y) -> q_distance50_bot
rownames(q_distance50_bot) <- distance50_bot$mdsDat$topics
as.matrix(dist(q_distance50_bot)) -> r_distance50_bot
r_distance50_bot[lower.tri(r_distance50_bot)] -> p_distance50_bot
View(p_distance50_bot)

#------------------------------------------------------------------------------#


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

#3. LDA topic modeling of 2000 nonbots:

#real2000.tidy1 <- gsub("[^\x20-\x7e]"," ", real2000.tidy$tweet) #Remove special characters, e.g., emoji
real2000.tidy1 <- gsub("(http)[^[:blank:]]*|[[:digit:]]",",", real2000.tidy$tweet) #Remove URLs and digits
typeof(real2000.tidy1)

real2000.tidy2 <- tbl_df(cbind(real2000.tidy$bot, real2000.tidy1))
names(real2000.tidy2) <- c("bot","tweet")

real2000.tidy2$bot <- as.numeric(real2000.tidy2$bot)

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

#k = 50:

#k = 50:
lda50_real2000 <- LDA(real2000_td3, k = 50, method='Gibbs',control = list(seed = 1234))
#save(lda50_real2000, file="lda50_real2000.Rdata")
load("lda50_real2000.Rdata")

##by document*topic
topics50_real2000 <- tidy(lda50_real2000,matrix="gamma")
topics50_real2000$document <- as.numeric(topics50_real2000$document)
topics50_real2000 <- topics50_real2000 %>% arrange(document)
#save(topics50_real2000, file="topics_real2000.Rdata")
#load("topics_real2000.Rdata")

top.gamma50_real2000 <- topics50_real2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma50_real2000,file="top.gamma50_real2000.Rdata")
load("top.gamma50_real2000.Rdata")

mean(top.gamma50_real2000$topgamma)




#k = 30:

lda_real2000 <- LDA(real2000_td3, k = 30, method='Gibbs',control = list(seed = 1234))
#save(lda_real2000, file="lda_real2000.Rdata")
load("lda_real2000.Rdata")

##by document*topic
topics_real2000 <- tidy(lda_real2000,matrix="gamma")
topics_real2000$document <- as.numeric(topics_real2000$document)
topics_real2000 <- topics_real2000 %>% arrange(document)
#save(topics_real2000, file="topics_real2000.Rdata")
#load("topics_real2000.Rdata")

top.gamma_real2000 <- topics_real2000 %>% group_by(document)%>% summarise(topgamma=max(gamma))
#save(top.gamma_real2000,file="top.gamma_real2000.Rdata")
load("top.gamma_real2000.Rdata")

#some documents have two top topics:
#View(topics_real2000 %>% filter(document==1986) %>% filter(gamma==max(gamma)))

mean(top.gamma_real2000$topgamma)

#4. Visualize LDA:
# k = 50:

post_real2000 <- topicmodels::posterior(lda50_real2000)
mat_real2000 <- lda50_real2000@wordassignments

ldavis50_real2000 <- list(phi_real = post_real2000[["terms"]], 
                        theta_real = post_real2000[["topics"]], 
                        doc.length_real = slam::row_sums(mat_real2000, na.rm = T), 
                        vocab_real = colnames(post_real2000[["terms"]]), 
                        term.frequency_real = slam::col_sums(mat_real2000, na.rm = T))
#save(ldavis_real2000, file="ldavis_real2000.Rdata")


#5. expressive marks:

##exclamation mark !!!!!!!!
real2000_exclamation <- str_count(real2000.tidy$tweet, "\\!")
real2000_exclamation <- cbind(real2000.tidy,real2000_exclamation)
real2000_exclamation <- real2000_exclamation %>% group_by(bot) %>% summarise(freq_of_exclamation=sum(real2000_exclamation))

##question mark ?????????
real2000_question <- str_count(real2000.tidy$tweet, "\\?")
real2000_question <- cbind(real2000.tidy,real2000_question)
real2000_question <- real2000_question %>% group_by(bot) %>% summarise(freq_of_question=sum(real2000_question))

#6. distance between topic:

ldadistance50_real2000 <- createJSON(phi=ldavis50_real2000$phi_real,
                                     theta=ldavis50_real2000$theta_real,
                                     doc.length = ldavis50_real2000$doc.length_real,
                                     vocab = ldavis50_real2000$vocab_real,
                                     term.frequency = ldavis50_real2000$term.frequency_real)
save(ldadistance50_real2000, file = "ldadistance50_real2000.Rdata")

fromJSON(ldadistance50_real2000) -> distance50_real
cbind(distance50_real$mdsDat$x, distance50_real$mdsDat$y) -> q_distance50_real
rownames(q_distance50_real) <- distance50_real$mdsDat$topics
as.matrix(dist(q_distance50_real)) -> View(r_distance50_real)
r_distance50_real[lower.tri(r_distance50_real)] -> p_distance50_real
mean(p_distance50_real)

#-----------------------------------------------------#


#III. Comparison between bot and non-bot data:

##1. retweet:
bot2000_rt$bot <- "bot"
real2000_rt$bot <- "nonbot"

RTfreq <- rbind(bot2000_rt,real2000_rt)
names(RTfreq) <- c("Type","Number")
#save(RTfreq, file="RTfreq.Rdata")

ggplot(RTfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)
ggplot(RTfreq, aes(Number,fill=Type))+geom_histogram(alpha=0.7)
ggplot(RTfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_rt$freq_of_RT,real2000_rt$freq_of_RT)
#2. url:
bot2000_urlfreq$bot <- "bot"
real2000_urlfreq$bot <- "nonbot"

URLfreq <- rbind(bot2000_urlfreq[,c(1,3)],real2000_urlfreq[,c(1,3)])
names(URLfreq) <- c("Type","Number")
#save(URLfreq, file="URLfreq.Rdata")

ggplot(URLfreq, aes(Number,fill=Type))+geom_density(alpha=0.7)
ggplot(URLfreq, aes(Number,fill=Type))+geom_histogram(alpha=0.7)
ggplot(URLfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_urlfreq$n,real2000_urlfreq$n)

#3. expressive mark:
##exclamation mark: 
bot2000_exclamation$bot <- "bot"
real2000_exclamation$bot <- "nonbot"

EXCLfreq <- rbind(bot2000_exclamation,real2000_exclamation)
names(EXCLfreq) <- c("Type","Number")

save(EXCLfreq, file="EXCLfreq.Rdata")

ggplot(EXCLfreq, aes(Number,fill=Type))+geom_density(alpha=0.5)
ggplot(EXCLfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_exclamation$freq_of_exclamation, real2000_exclamation$freq_of_exclamation)

##question mark:
bot2000_question$bot <- "bot"
real2000_question$bot <- "nonbot"

QUESfreq <- rbind(bot2000_question,real2000_question)
names(QUESfreq) <- c("Type","Number")

save(QUESfreq, file="QUESfreq.Rdata")

ggplot(QUESfreq, aes(Number,fill=Type))+geom_density(alpha=0.5)
ggplot(QUESfreq, aes(y=Number,x=Type, fill=Type))+geom_boxplot()

t.test(bot2000_question$freq_of_question, real2000_question$freq_of_question)


#4. lda gamma:
top.gamma50_bot2000$document <- "bot"
top.gamma50_real2000$document <- "nonbot"

Top.gamma50 <- rbind(top.gamma50_bot2000,top.gamma50_real2000)
names(Top.gamma50) <- c("Type","Gamma")

save(Top.gamma50, file="Top.gamma50.Rdata")

ggplot(Top.gamma, aes(Gamma,fill=Type))+geom_density(alpha=0.7)
ggplot(Top.gamma, aes(Gamma,fill=Type))+geom_histogram(alpha=0.7)
ggplot(Top.gamma, aes(y=Gamma,x=Type, fill=Type))+geom_boxplot()

t.test(top.gamma50_bot2000$topgamma,top.gamma50_real2000$topgamma)

#5. distance:

distance_bot <- data.frame(Type="bot", Distance=p_distance50_bot)
distance_real <- data.frame(Type="nonbot", Distance=p_distance50_real)

distance <- rbind(distance_bot,distance_real)

save(distance, file="distance.Rdata")



