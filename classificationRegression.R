#Use provided reviewer_withavg.csv dataset
filename <- file.choose()
mydata = read.csv(filename, header=TRUE)
#mydata is a data frame
#the dataset is small so it's easy to see there are no NAs
mydata$Popular <- NULL #add the popular column to data.frame
for (i in 1:length(mydata[,1])) {
  if (mydata$Top10[i] == 1 || mydata$Top50[i] == 1 || mydata$Top100[i] == 1) {
    mydata$Popular[i] <- 1
  }
  else {
    mydata$Popular[i] <- 0
  }
}
names(mydata)
head(mydata)
ncol(mydata)
nrow(mydata) #199 rows verifies we still have 199 unique reviewers
traindata <- mydata[1:140,]
validata <- mydata[141:199,]
nrow(traindata) #140- yes
nrow(validata) #59- yes
#Prepare libraries needing for the following
library(class)
library(klaR)
library(e1071)
library(leaps)
library(lars)
library(MASS)
#Begin CLASSIFICATION:::
#X input variables:
train <- cbind(traindata$avg_viewership, traindata$avg_content, traindata$avg_centrality, traindata$avg_enhcontent)
test <- cbind(validata$avg_viewership, validata$avg_content, validata$avg_centrality, validata$avg_enhcontent)
#Y classifier:
cl <- traindata$Advisor
cltest <- validata$Advisor
#Run k-nearest neighbors:
err <- rep(0,5)
for (i in 1:5) {
resknn <- knn(train, test, cl, k=i, prob=TRUE)
#error count
err[i] <- sum(abs(as.numeric(res)-1-cltest))
}
#predicted vs. true
cbind(seq(1,5), err)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Begin Naive Bayes:
resbay <- NaiveBayes(train, as.factor(traindata$Advisor))
pred <- predict(resbay, test)
pred
#predicted vs. true
cbind(as.numeric(pred$class)-1, validata$Advisor)
#error count
err <- sum(abs(as.numeric(pred$class)-1-validata$Advisor))
err
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Begin Support Vector Machine:
#normalize input variables
mydata2 <- mydata
head(mydata2)
mydata2[["avg_viewership"]] <- mydata2[["avg_viewership"]]/sd(mydata2[["avg_viewership"]])
mydata2[["avg_content"]] <- mydata2[["avg_content"]]/sd(mydata2[["avg_content"]])
mydata2[["avg_centrality"]] <- mydata2[["avg_centrality"]]/sd(mydata2[["avg_centrality"]])
mydata2[["avg_enhcontent"]] <- mydata2[["avg_enhcontent"]]/sd(mydata2[["avg_enhcontent"]])
traindata2 <- mydata2[1:140,]
validata2 <- mydata2[141:199,]
train2 <- cbind(traindata2$avg_viewership, traindata2$avg_content, traindata2$avg_centrality, traindata2$avg_enhcontent)
test2 <- cbind(validata2$avg_viewership, validata2$avg_content, validata2$avg_centrality, validata2$avg_enhcontent)
#SVM:
ressvm <- svm(train2, as.factor(traindata2$Advisor))
#different kernels:
#reslin <- svm(train2, as.factor(traindata2$Advisor), kernel="linear")
respoly <- svm(train2, as.factor(traindata2$Advisor), kernel="polynomial")
#resred <- svm(train2, as.factor(traindata2$Advisor), kernel="redial")
#resredbas <- svm(train2, as.factor(traindata2$Advisor), kernel="redial basis")
#ressig <- svm(train2, as.factor(traindata2$Advisor), kernel="sigmoid")
pred <- predict(ressvm, test) #change ressvm to reslin, respoly, etc. based on which condition to try
pred
#predicted vs. true
cbind(as.numeric(pred)-1, validata2$Advisor)
#error count
err <- sum(abs(as.numeric(pred)-1-validata2$Advisor))
err
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Begin REGRESSION:::
#using entire dataset mydata here
#want to predict avg_centrality
#using MASS:
fit <- lm(formula=avg_centrality ~ avg_content + avg_viewership + avg_enhcontent + Top10 + Top50 + Top100 + Advisor + Lead, data=mydata)
step <- stepAIC(fit, direction="both")
step$anova
#using leaps:
regleaps <- regsubsets(avg_centrality ~ avg_content + avg_viewership + avg_enhcontent + Top10 + Top50 + Top100 + Advisor + Lead, data=mydata, nbest=1, method="exhaustive")
regleaps
summary.out <- summary(regleaps)
as.data.frame(summary.out$outmat)
plot(regleaps, scale="adjr2", main="Adjusted R^2")
library(car)
res.legend <- subsets(regleaps, statistic="cp", legend=FALSE, main="Mallow Cp")
res.legend
which.max(summary.out$adjr2) #yeilds 7 of the 8 variables should be included in the model
summary.out$which[7,] #the only variable of the 8 input that is left out is Top10
#using lars:
cols <- c(6:8,13:16,17)
x <- as.matrix(mydata[,cols])
y <- mydata$avg_centrality
reglars1 <- lars(x,y, type="lasso") #lasso
summary(reglars1)
plot(reglars1)
reglars2 <- lars(x,y,type="lar") #lar
summary(reglars2)
plot(reglars2)
reglars3 <- lars(x,y,type="for") #forward.stagewise
summary(reglars3)
plot(reglars3)
reglars4 <- lars(x,y,type="stepwise") #stepwise
summary(reglars4)
plot(reglars4)
#can do additional regression modeling as needed with other packages