library(MASS)
library(readxl)
library(e1071)

diabetes <- read_excel("diabetes.xlsx")
diabetes_svm <-read_excel("diabetes_SVM.xlsx")

index <- sample(2, nrow(diabetes),replace = TRUE, prob = c(0.7, 0.3))
training <- diabetes[index==1,]
testing <- diabetes[index==2,]

lda.fit <- lda(class~., data = training)
lda.pred <- predict(lda.fit, testing)
table(lda.pred$class, testing$class)
mean(lda.pred$class==testing$class)

qda.fit <- qda(class~., data = training)
qda.pred <- predict(qda.fit, testing)
table(qda.pred$class, testing$class)
mean(qda.pred$class==testing$class)

index <- sample(2, nrow(diabetes_svm),replace = TRUE, prob = c(0.7, 0.3))
training <- diabetes_svm[index==1,]
testing <- diabetes_svm[index==2,]
svm.fit <- svm(class~., data = training, type = 'C-classification', cost = 1, kernel= 'linear')
svm.pred <- predict(svm.fit,newdata =  testing)
table(svm.pred, testing$class)
mean(svm.pred==testing$class)