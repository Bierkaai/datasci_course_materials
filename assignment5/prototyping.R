library(caret)
library(ggplot2)

d.raw <- read.csv('seaflow_21min.csv')


# Question 3
file_id_f <- d.raw$file_id
partitions <- createDataPartition(file_id_f, times=2, p=0.5)

train <- d.raw[partitions[[1]],]
test <- d.raw[partitions[[2]],]


# Question 4: pico and nano particles are mixed with ultra
g <- ggplot(data = d.raw, aes(x = chl_small, y =  pe, color = pop))+
    geom_point()

print(g)

# Question 5,6,7
library(rpart)

fol <- formula(pop ~ fsc_small + fsc_perp + chl_small + pe + chl_big + chl_small)
model <- rpart(fol, data=train, method='class')
print(model)

# Question 8
predictions <- predict(model, test, type='class')
pred.accuracy <- sum(predictions==test$pop)/nrow(test)

# Question 9
library(randomForest)
rf.model <- randomForest(fol, data=train)
rf.predictions <- predict(rf.model, test, type='class')
rf.pred.accuracy <- sum(rf.predictions==test$pop)/nrow(test)

# Question 10
importance(rf.model)

# Question 11
library(e1071)

svm.model <- svm(fol, data=train)
svm.predictions <- predict(svm.model, test, type='class')
svm.pred.accuracy <- sum(svm.predictions==test$pop)/nrow(test)

# Question 12
table(pred = predictions, true = test$pop)
table(pred = rf.predictions, true = test$pop)
table(pred = svm.predictions, true = test$pop)

# Question 13
library(dplyr)
data.filter <- d.raw %>% filter(file_id != 208)
file_id_f <- d.raw$file_id
partitions <- createDataPartition(file_id_f, times=2, p=0.5)

train <- d.raw[partitions[[1]],]
test <- d.raw[partitions[[2]],]

svm.model <- svm(fol, data=train)
svm.predictions <- predict(svm.model, test, type='class')
svm.pred.accuracy <- sum(svm.predictions==test$pop)/nrow(test)