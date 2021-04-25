library(tidyverse)
library(broom)
library(tidytext)
library(ggthemes)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(vcd)

data <- read_csv("final_data.csv")

data <- data %>% select(-consumerID, -title, -review, -positive, -review_1)

data <- na.omit(data)


########## SENTIMENT ANALYSIS ##########
# What is the overall sentiment for recommended clothing items?
# What is the overall sentiment for clothing items that are not recommended?
# Are there differences in sentiment for different departments, product divisions and/or product classes?


recomComp <- data %>%
  mutate(recommend2 = ifelse(recommend == 1, "Recommended", "Not Recommended")) %>%
  select(recommend2, comments) %>%
  unnest_tokens(input = comments, output = word) %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(recommend2, sentiment) %>%
  summarise(freq = n()) %>%
  mutate(prop = freq / sum(freq))
recomComp


recomComp %>%
  ggplot(aes(x = factor(recommend2), y = freq, fill = sentiment)) + 
  scale_fill_manual(values = c("tomato", "seagreen"), "") +
  geom_col(position = "fill") + theme_wsj() + coord_flip() +
  theme(legend.position = "bottom")
  

# Differences by division
data %>%
  group_by(division) %>%
  summarise(clothing = mean(clothingSentiment), consumer = mean(consumerSentiment)) %>%
  gather(sentiment, score, -division) %>%
  ggplot(aes(x = reorder(division, -score), y = score)) + 
  geom_col(fill = "tomato") + 
  geom_text(aes(label = round(score, 3), y = score + 0.05), vjust = 0) +
  ggtitle("Division") +
  theme_wsj() + facet_wrap(~sentiment)

# Differences by product department
data %>%
  group_by(department) %>%
  summarise(clothing = mean(clothingSentiment), consumer = mean(consumerSentiment)) %>%
  gather(sentiment, score, -department) %>%
  ggplot(aes(x = reorder(department, -score), y = score)) + 
  geom_col(fill = "tomato") + 
  geom_text(aes(label = round(score, 3), y = score + 0.05), vjust = 0) +
  ggtitle("Department") +
  theme_wsj() + facet_wrap(~sentiment)

# Differences by product/class
data %>%
  group_by(class) %>%
  summarise(clothing = mean(clothingSentiment), consumer = mean(consumerSentiment)) %>%
  gather(sentiment, score, -class) %>%
  filter(sentiment == "clothing") %>%
  ggplot(aes(x = reorder(class, -score), y = score)) + 
  geom_col(fill = "tomato") + 
  geom_text(aes(label = round(score, 3), y = score + 0.05), vjust = 0) +
  ggtitle("By clothing ratings") +
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 90))

data %>%
  group_by(class) %>%
  summarise(clothing = mean(clothingSentiment), consumer = mean(consumerSentiment)) %>%
  gather(sentiment, score, -class) %>%
  filter(sentiment == "consumer") %>%
  ggplot(aes(x = reorder(class, -score), y = score)) + 
  geom_col(fill = "tomato") + 
  geom_text(aes(label = round(score, 3), y = score + 0.05), vjust = 0) +
  ggtitle("By consumer ratings") +
  theme_wsj() +
  theme(axis.text.x = element_text(angle = 90))


### Sentiemnt by age

data$age2 <- cut(data$age, 
                 breaks = c(18, 35, 50, 100), 
                 labels = c("Young adults", "Middle-aged", "Elderly")); data$age2

data %>%
  group_by(age2) %>%
  summarise(consumer = mean(consumerSentiment)) %>%
  filter(!is.na(age2)) %>%
  gather(sentiment, score, -age2) %>%
  ggplot(aes(x = age2, y = score)) +
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(score, 3), y = score + 0.05), vjust = 0) +
  ggtitle("Average sentiment") +
  theme_wsj()


########## MODELING ##########

# What is the relationship between specific words and the rating for clothing items?
# What is the relationship between text sentiment and the rating for clothing items?

mdata <- data %>% select(-comments, -recommend, -age2)
mdata$division <- as.factor(mdata$division)
mdata$department <- as.factor(mdata$department)
mdata$class <- as.factor(mdata$class)

set.seed(1031)
split <- sample(1:nrow(mdata), size = 0.8 * nrow(mdata))
train <- mdata[split, ]
test <- mdata[-split, ]

nrow(train) + nrow(test) == nrow(data)

# Tree model
tree <- rpart(rating ~., train)
rpart.plot(tree)

##### RF MODEL 1: PRODUCT RATINGS

# Random forest model
rf <- randomForest(factor(rating) ~., train, num.trees = 1000, mtry = 10)

# Evaluation of the model
pred_test <- predict(rf, test, type = "response")

# Variable importance
varImpPlot(rf)

varimp <- cbind(var = row.names(rf$importance), rf$importance) %>% as_tibble(); varimp
names(varimp) <- c("Variable", "varImp")
varimp$varImp <- as.numeric(varimp$varImp)
varimp <- varimp %>% arrange(-varImp) %>% slice(1:10)

ggplot(varimp, aes(reorder(Variable, -varImp), varImp)) + 
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(varImp, 2), y = varImp + 100), vjust = 0) +
  coord_flip() +
  theme_wsj()

# Evaluate
conf_matrix <- table(test$rating, pred_test); conf_matrix
prop_matrix <- prop.table(conf_matrix, 1); prop_matrix
accuracy <- (conf_matrix[1, 1] + conf_matrix[2, 2] + conf_matrix[3, 3] + conf_matrix[4, 4] + conf_matrix[5, 5])/ sum(conf_matrix); accuracy




##### RF MODEL 2: RECOMMENDATIONS

mdata2 <- data %>% select(-comments, -rating, -age2)
mdata2$division <- as.factor(mdata2$division)
mdata2$department <- as.factor(mdata2$department)
mdata2$class <- as.factor(mdata2$class)

set.seed(1031)
split <- sample(1:nrow(mdata2), size = 0.8 * nrow(mdata2))
train2 <- mdata2[split, ]
test2 <- mdata2[-split, ]

nrow(train2) + nrow(test2) == nrow(mdata2)


# Random forest model
rf2 <- randomForest(factor(recommend) ~., train2, num.trees = 1000, mtry = 10)

# Evaluation of the model
pred_test2 <- predict(rf2, test2, type = "response")

# Variable importance
varImpPlot(rf2)

varimp2 <- cbind(var = row.names(rf2$importance), rf2$importance) %>% as_tibble(); varimp2
names(varimp2) <- c("Variable", "varImp")
varimp2$varImp <- as.numeric(varimp2$varImp)
varimp2 <- varimp2 %>% arrange(-varImp) %>% slice(1:10)

ggplot(varimp2, aes(reorder(Variable, -varImp), varImp)) + 
  geom_col(fill = "tomato") +
  geom_text(aes(label = round(varImp, 2), y = varImp + 100), vjust = 0) +
  coord_flip() +
  theme_wsj()

# Evaluate
conf_matrix2 <- table(test2$recommend, pred_test2); conf_matrix2
prop_matrix2 <- prop.table(conf_matrix2, 1); prop_matrix2
accuracy <- (conf_matrix2[1, 1] + conf_matrix2[2, 2])/ sum(conf_matrix); accuracy
specificity <- conf_matrix2[1, 1] / sum(conf_matrix2[1, 1] + conf_matrix2[1, 2]); specificity
sensitivity <- conf_matrix2[2, 2] / sum(conf_matrix2[2, 1] + conf_matrix2[2, 2]); sensitivity

