library(MASS)
library(readxl)
library(e1071)

wholesale <- read_excel("Wholesale_customers_data.xlsx")

index <- sample(2, nrow(wholesale),replace = TRUE, prob = c(0.7, 0.3))
training <- wholesale[index==1,]
testing <- wholesale[index==2,]

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