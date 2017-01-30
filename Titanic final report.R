#FIANL REPORT
titanic <- read.csv("https://storage.googleapis.com/r_rookies/kaggle_titanic_train.csv")
str(titanic)
summary(titanic)
library(ggplot2)

#-----------------------清除未知數------------------------------------------
titanic <- titanic[complete.cases(titanic), ]
titanic$Survived <- factor(titanic$Survived)
titanic$Embarked <- as.character(titanic$Embarked)
titanic$Embarked[titanic$Embarked == ""] <- "S"
titanic$Embarked <- factor(titanic$Embarked)
#------------------------分組---------------------------------------
n <- nrow(titanic)
set.seed(666)
shuffled_titanic <- titanic[sample(n), ]
train_indices <- 1:round(0.7 * n)
train <- shuffled_titanic[train_indices, ]
test_indices <- (round(0.7 * n) + 1):n
test <- shuffled_titanic[test_indices, ]
test

#-------------------準確度計算-------------------------------------
install.packages("randomForest")
library(randomForest)
RAD<- randomForest(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, ntree = 1000)


# calculate accuracy
prediction <- predict(rf_clf, test[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")])
confusion_matrix <- table(test$Survived, prediction)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
#--------------探索沒上傳資料--------------------------------------
#輸入 to_predict
url <- "https://storage.googleapis.com/py_ds_basic/kaggle_titanic_test.csv"
to_predict <- read.csv(url)
summary(to_predict)

#先將年齡用平均年齡填滿
install.packages("dplyr")
library(dplyr)
install.packages("magrittr")
library(magrittr)
mean_age_by_Pclass <- to_predict %>%
  group_by(Pclass) %>%
  summarise(mean_age = round(mean(Age, na.rm = TRUE)))

filter_1 <- is.na(to_predict$Age) & to_predict$Pclass == 1
filter_2 <- is.na(to_predict$Age) & to_predict$Pclass == 2
filter_3 <- is.na(to_predict$Age) & to_predict$Pclass == 3
mean_age_by_Pclass

to_predict[filter_1, ]$Age <- 41
to_predict[filter_2, ]$Age <- 29
to_predict[filter_3, ]$Age <- 24

#再將FARE用其平均值填滿
fare_mean <- mean(to_predict$Fare, na.rm = TRUE)
to_predict$Fare[is.na(to_predict$Fare)] <- fare_mean

# Summary after imputation
summary(to_predict)
#----------------------to_predict的NA已經清空------------------------------
#
predicted <- predict(RAD, newdata = to_predict[, c("Pclass", "Sex", "Age", "SibSp", "Parch", "Fare", "Embarked")])
result <- data.frame(to_predict[, "PassengerId"], predicted)
names(result) <- c("PassengerId", "Survived")
head(result, n = 10)

#寫出結論
write.csv(result, file = "result_of_the_Tiatanic_report.csv", row.names = FALSE)