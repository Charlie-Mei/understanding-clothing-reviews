library(tidytext)
library(tidyverse)
library(tm)

mydata = read_csv("data/Womens Clothing E-Commerce Reviews.csv")

### 1. Rename Variables
names(mydata) = c("consumerID", "clothingID", "age", 
                  "title", "review", "rating", "recommend", 
                  "positive", "division", "department", "class")
str(mydata)

# Check all customer IDs are unique
nrow(mydata) == length(unique(mydata$consumerID))

### 2. Checking missing values
sum(is.na(mydata$consumerID))    # no missing value
sum(is.na(mydata$clothingID))    # no missing value
sum(is.na(mydata$age))           # no missing value 
sum(is.na(mydata$title))         # 3810 missing values 
sum(is.na(mydata$review))        # 845 missing values 
sum(is.na(mydata$rating))        # no missing value
sum(is.na(mydata$recommend))     # no missing value
sum(is.na(mydata$positive))      # no missing value
sum(is.na(mydata$divison))       # 14 missing values - leave for now
sum(is.na(mydata$department))    # 14 missing values - leave for now
sum(is.na(mydata$class))         # 14 missing values - leave for now


### 3. Creating a combined text variable

#change "NA" values in Title and Review to blank space to prepare for combining

mydata$title[which(is.na(mydata$title))] = ""
mydata$review[which(is.na(mydata$review))] = ""

# Combinging "title" and "review" into one column 
mydata$comments =  paste(mydata$title, mydata$review, sep = " ")
str(mydata)

# Check if there are still missing text comments
sum(mydata$comments == "  ")
# None!

### 4. Do some basic text cleaning

# Before
mydata$comments[5]

# Convert to lower-case, remove punctuation, remove stopwrods, strip white space
mydata$comments <- str_to_lower(mydata$comments)
mydata$comments <- gsub(pattern = "[[:punct:]]", replacement = "", mydata$comments)

stopwords <- get_stopwords(language = "english")$word
mydata$comments <- removeWords(mydata$comments, words = stopwords)
mydata$comments <- stripWhitespace(mydata$comments)

# after
mydata$comments[5]
mydata$comments[which(mydata$comments == " ")] <- NA


# corpus = Corpus(VectorSource(mydata$comments))
# #convert to lower-case
# corpus = tm_map(corpus,FUN = content_transformer(tolower))
# #remove punctuation
# corpus = tm_map(corpus,FUN = removePunctuation)
# #remove stopwords
# corpus = tm_map(corpus,FUN = removeWords,c(stopwords('english')))
# #strip whitespace
# corpus = tm_map(corpus,FUN = stripWhitespace)


### 5. Checking missing values in division, department and class

summary(factor(mydata$division))

mydata[is.na(mydata$division),]    
# It was the same fourteen rows that are missing values from "divison", "department" and "class" - leave for now
# Intimates is spelt wrong though

#Correcting the spelling error in divison
mydata$division[which(mydata$division == "Initmates")] = "Intimates"
summary(factor(mydata$division))


### SAVE THE CLEANED DATA
write_csv(mydata, "data/project_cleandata.csv")
