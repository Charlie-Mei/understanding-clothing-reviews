library(tidyverse)
library(stringr)
library(tidytext)
library(dplyr)
library(lexicon)
library(tm)
library(stringr)
library(ggplot2)
library(ggthemes)


mydata <- read_csv('data/project_cleandata.csv')
head(mydata)
str(mydata)

mydata %>%
  group_by(consumerID) %>%
  unnest_tokens(output = word, input = comments) %>%
  inner_join(get_sentiments('bing')) %>%
  group_by(sentiment)


afinn = get_sentiments('afinn')

# Average rating provided by each consumer
reviewScore <- mydata %>%
  select(consumerID, comments) %>%
  group_by(consumerID) %>%
  unnest_tokens(output = word, input = comments) %>%
  inner_join(afinn) %>%
  summarize(consumerSentiment = mean(value))

# Average rating provided for each clothing product
reviewScore1 <- mydata %>%
  select(clothingID, comments) %>%
  group_by(clothingID) %>%
  unnest_tokens(output = word, input = comments) %>%
  inner_join(afinn) %>%
  summarize(clothingSentiment = mean(value))

# Distribution of sentiment for individuals
reviewScore %>%
  ggplot(aes(x = consumerSentiment, fill = consumerSentiment > 0)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks=seq(-5,5,1)) +
  scale_fill_manual(values=c('tomato','seagreen')) +
  guides(fill=F)+
  theme_wsj()

# Distribution of sentiment for clothing products
reviewScore1 %>%
  ggplot(aes(x = clothingSentiment, fill = clothingSentiment > 0)) +
  geom_histogram(binwidth = 0.1) +
  scale_x_continuous(breaks=seq(-5,5,1)) +
  scale_fill_manual(values=c('tomato','seagreen')) +
  guides(fill=F)+
  theme_wsj()

# str(mydata$recommend)
# recommend <- mydata1[recommend==1,]
# 
# recommendReview <- mydata1%>%
#   filter(recommend==1)
# notrecommendReview <-mydata1%>%
#   filter(recommend==0)
# summary(recommendReview$reviewSentiment)
# summary(notrecommendReview$reviewSentiment)
# 
# recommendReview%>%
#   ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
#   geom_histogram(binwidth = 0.1)+
#   scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
#   guides(fill=F)+
#   theme_wsj()
# 
# notrecommendReview%>%
#   ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
#   geom_histogram(binwidth = 0.1)+
#   scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
#   guides(fill=F)+
#   theme_wsj()

added_data <- left_join(mydata, reviewScore, by = "consumerID")
added_data <- left_join(added_data, reviewScore1, by = "clothingID")

write_csv(added_data, 'mydata1.csv')
