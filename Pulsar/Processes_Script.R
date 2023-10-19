library(MASS)
library(readxl)
library(e1071)

pulsar <- read_excel("Pulsar_Dataset.xlsx")

index <- sample(2, nrow(pulsar),replace = TRUE, prob = c(0.7, 0.3))
training <- pulsar[index==1,]
testing <- pulsar[index==2,]

lda.fit <- lda(class~., data = training)
lda.pred <- predict(lda.fit, testing)
table(lda.pred$class, testing$class)
mean(lda.pred$class==testing$class)

qda.fit <- qda(class~., data = training)
qda.pred <- predict(qda.fit, testing)
table(qda.pred$class, testing$class)
mean(qda.pred$class==testing$class)

svm.fit <- svm(class~., data = training, type = 'C-classification', kernel= 'linear')
svm.pred <- predict(svm.fit,newdata =  testing)
table(svm.pred, testing$class)
mean(svm.pred==testing$class)