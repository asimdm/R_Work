library(MASS)
library(readxl)
library(e1071)

wilt <- read_excel("Wilt_Dataset.xlsx")
wilt_svm <-read_excel("Wilt_Dataset_SVM.xlsx")

index <- sample(2, nrow(wilt),replace = TRUE, prob = c(0.7, 0.3))
training <- wilt[index==1,]
testing <- wilt[index==2,]

lda.fit <- lda(class~., data = training)
lda.pred <- predict(lda.fit, testing)
table(lda.pred$class, testing$class)
mean(lda.pred$class==testing$class)

qda.fit <- qda(class~., data = training)
qda.pred <- predict(qda.fit, testing)
table(qda.pred$class, testing$class)
mean(qda.pred$class==testing$class)

index <- sample(2, nrow(wilt_svm),replace = TRUE, prob = c(0.7, 0.3))
training <- wilt_svm[index==1,]
testing <- wilt_svm[index==2,]
svm.fit <- svm(class~., data = training, type = 'C-classification', cost = 1, kernel= 'linear')
svm.pred <- predict(svm.fit,newdata =  testing)
table(svm.pred, testing$class)
mean(svm.pred==testing$class)