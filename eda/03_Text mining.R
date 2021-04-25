library(stringr)
library(tidytext)
library(dplyr)
library(tm)
library(wordcloud)

clothingData <- read.csv('data/project_cleandata.csv', stringsAsFactors = F)
summary(clothingData)
glimpse(clothingData)


#Text-mining for "comments" colomn: convert to lower-case, remove punctuation, remove stopwrods, strip white space
corpus = Corpus(VectorSource(clothingData$comments))
#convert to lower-case
corpus = tm_map(corpus,FUN = content_transformer(tolower))
#remove punctuation
corpus = tm_map(corpus,FUN = removePunctuation)
#remove stopwords
corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
#strip whitespace
corpus = tm_map(corpus,FUN = stripWhitespace)
#check
corpus[[50]][1]

#Create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(clothingData$comments))),lowfreq = 0)
dict_corpus = Corpus(VectorSource(dict))
#Stem document
corpus = tm_map(corpus,FUN = stemDocument)
corpus[[50]][1]

#Create a document term matrix (tokenize)
dtm = DocumentTermMatrix(corpus); dtm
inspect(dtm[50,])

#Remove Sparse Terms
xdtm = removeSparseTerms(dtm,sparse = 0.95); xdtm

#Complete Stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),
                                dictionary = dict_corpus,
                                type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

#Browse tokens
words <- sort(colSums(xdtm),decreasing = T)

# Wordcloud of most popular words
word_freq <- cbind(word = names(words), freq = words) %>% as_tibble()
word_freq$freq <- as.numeric(word_freq$freq)
word_freq
wordcloud(word_freq$word, word_freq$freq, max.words = 100, random.color = T)



mydata2 <- cbind(clothingData, xdtm)
names(mydata2)
write_csv(mydata2, "final_data.csv")

# #Add rating and recommend back to dataframe of features
# C = cbind(rating = clothingData$rating, xdtm, clothingData$divison,clothingData$department,clothingData$recommend)
# D= cbind(rating = clothingData$rating,recommend = clothingData$recommend, xdtm)
# glimpse(C)
# sort(colSums(D[D$recommend==1,-D$recommend]), decreasing = T)
# sort(colSums(D[D$rating==5,-D$rating]), decreasing = T)
# #Predictive Models (using TF features)
# #Split data
# set.seed(1031)
# split = sample(1:nrow(C),size = 0.7*nrow(C))
# train = C[split,]
# test = C[-split,]
# nrow(test)
# #CART model
# library(rpart); library(rpart.plot)
# tree = rpart(clothingData$recommend~.,train)
# rpart.plot(tree)
# 
# #predictions
# pred_tree_train = predict(tree,newdata=train)
# rmse_tree_train = sqrt(mean((pred_tree_train - train$rating)^2)); rmse_tree_train
# 
# pred_tree = predict(tree,newdata=test)
# rmse_tree = sqrt(mean((pred_tree - test$rating)^2)); rmse_tree
# 
# #Regression
# reg = lm(rating~.,train)
# summary(reg)
# #predictions
# pred_reg = predict(reg, newdata=test)
# rmse_reg = sqrt(mean(pred_reg-test$review_rating,na.rm=TRUE)^2); rmse_reg
# 
# mean(pred_reg - test$review_rating, na.rm = T)


#What is the overall sentiment for recommended clothing items?
#What is the overall sentiment for clothing items that are not recommended?
#Are there differences in sentiment for different departments, product divisions and/or product classes?
#What is the relationship between specific words and the rating for clothing items?
#What is the relationship between text sentiment and the rating for clothing items