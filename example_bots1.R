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
#load bots data into r
a <- read.csv("1-500.csv")
b <- tbl_df(t(a))
b <- b[-c(1,2),]

bot520 <- gather(b, key="bot", value="tweet", str_c("V",1:520))
bot520$bot <- as.numeric(str_replace(bot520$bot,"V",""))

##class(bot520)
##typeof(bot520$bot)

bot520_td <- bot520 %>% unnest_tokens(word, tweet) %>% 
  anti_join(stop_words)

bot520_wordfreq <- bot520_td %>% group_by(bot) %>% 
  count(word, sort=T)

bot520_wordfreq1 <- arrange(bot520_wordfreq,bot)

bot520_urlfreq <- subset(bot520_wordfreq1, word=="https")

save(bot520_urlfreq,file="bot520_urlfreq.Rdata")

#tidy bot1 so that it is one token per row
bot1 <- as.character(b$V1)
bot1_td <- tbl_df(bot1) %>% unnest_tokens(word, value)
dim(bot1_td)

#tidy bot1 so that it is one token per row, and also exclude stop words
#data("stop_words")
bot1_td1 <- tbl_df(bot1) %>% unnest_tokens(word, value) %>% anti_join(stop_words)
dim(bot1_td1)

#count the frequency of words
bot1_td2 <- bot1_td1 %>% count(word,sort=T)

##as shown above, rt and http are the most frequent words, 
##which can be treated as stop words.
custom_stop_words <- bind_rows(data_frame(word = c("rt","https","t.co"), 
                                          lexicon = c("custom")), 
                               stop_words)

custom_stop_words

##so we redo the step of excluding the stop words
bot1_td2 <- bot1_td2 %>% anti_join(custom_stop_words)
head(bot1_td2)

#make it into a matrix form for LDA modeling
bot1_td3 <- cbind(document=1, bot1_td2)
colnames(bot1_td3) <- list("document","term","count")
head(bot1_td3)

bot1_td4 <- bot1_td3 %>% cast_dfm(document, term, count)
bot1_td4

##wordcloud of bot1
wordcloud2(bot1_td2)

##LDA modeling, try more numbers on k from 2 to 5 (2 might be the best)
ap_bot1 <- LDA(bot1_td4, k = 2, control = list(seed = 1234))

##by document*topic
bot_1 <- tidy(ap_bot1,matrix="gamma")
bot_1

#by topic*word
bot_1.1 <- tidy(ap_bot1, matrix="beta")
bot_1.1

ap_top_terms <- bot_1.1 %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

## it turns out renee zawawi is a opo singer?


##the frequence of https:

