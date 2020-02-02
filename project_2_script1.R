#######################################
#' Project 2
#' @author Bita Nezamdoust
#######################################

install.packages("scales")
install.packages("corrplot")
install.packages("gridExtra")
install.packages("glmnet")
#################TASK 1####################
#Read Rdata file
dir = "Work/Study_2018/Stat 8670: Computational Methods in Statistics/Project_2/Part1:LinearReg/project_2_data_1.RData"
get(load(dir))
ls()
str(test)
#'data.frame':	4875 obs. of  22 variables:
names(test)
# [1] "V1"  "V2"  "V3"  "V4"  "V5"  "V6"  "V7"  "V8"  "V9"  "V10" "V11" "V12" "V13" "V14"
# [15] "V15" "V16" "V17" "V18" "V19" "V20" "V21" "V22"



#install.packages("scales")
library(scales)
#' want to rescale the data so that they're all between 0 and 1 (normalized)
#' and the interpretibility of the prediction errors is easier

X.train = rescale(as.matrix(train[,7:22]))
X.test = rescale(as.matrix(test[,7:22]))
Y.train = rescale(train[,5])
Y.test = rescale(test[ ,5])


#Multicollinearity plot
#install.packages("corrplot")
library(corrplot)
M<-cor(X.train)
head(round(M,2))

par(mfrow = c(1, 2))
corrplot(M, method="pie", type = "lower", tl.col = "black")
corrplot(M, method="number", type = "lower", number.cex = .7, tl.col = "black")
#title("Multicollinearity between Predictors", outer = TRUE, line = -3, cex = 5)
?"corrplot"

#' Interpretation: Seems that the predictor variables are from moderately to very
#' strongly correlated. That mean strong multicollinearity exists among the predictor
#'  variables which will lead to high prediction error,
#' unless we control for it.  
#' Also see below:

cor(train$V13, train$V15)
# [1] 0.9781401
cor(train$V16, train$V19)
# [1] -0.7805525
cor(train$V8, train$V11)
# [1] 0.8326512

#Correlation plot
#install.packages("gridExtra")
library(gridExtra)
library(ggplot2)
frame.for.plot = data.frame(X.train)
p1 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V8, y = X.train$V11, color = Y.train))
p2 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V13, y = X.train$V15, color = Y.train))
p3 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V16, y = X.train$V19, color = Y.train))
p4 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V13, y = X.train$V17, color = Y.train))
p5 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V16, y = X.train$V19, color = Y.train))
p6 = ggplot(data = frame.for.plot) +
  geom_point(mapping = aes(x = X.train$V20, y = X.train$V22, color = Y.train))

grid.arrange(p1+ labs(x = "X8", y = "X11"),p2+ labs(x = "X13", y = "X15")
             ,p3+ labs(x = "X16", y = "X19"),p4+ labs(x = "X13", y = "X17")
             ,p5+ labs(x = "X16", y = "X19"),p6+ labs(x = "X20", y = "X22"),
             ncol = 3)
?ggplot

#Investigate correlation between X's and Y
for(i in 1:ncol(X.train)){
  cor = cor(X.train[,i], Y.train)
  print(paste("i = ",  i, ", cor = ", cor))
}

# [1] "i =  1 , cor =  0.0774291944103845"
# [1] "i =  2 , cor =  0.026756233353953"
# [1] "i =  3 , cor =  0.0645911488215885"
# [1] "i =  4 , cor =  0.0673660077273427"
# [1] "i =  5 , cor =  0.0646006572068302"
# [1] "i =  6 , cor =  0.0776083313207561"
# [1] "i =  7 , cor =  0.0849986557442271"
# [1] "i =  8 , cor =  0.0596058212539527"
# [1] "i =  9 , cor =  0.0664688151058887"
# [1] "i =  10 , cor =  0.119396952349143"
# [1] "i =  11 , cor =  0.0596055076857064"
# [1] "i =  12 , cor =  0.071206958395959"
# [1] "i =  13 , cor =  -0.12773370949347"
# [1] "i =  14 , cor =  0.115509719289025"
# [1] "i =  15 , cor =  -0.156666713393043"
# [1] "i =  16 , cor =  0.1271207640929

#They are very weekly correlated. However, their combination can still effectively
# predict Y. Transformation methods are also applied to help the correlation.

par(mfrow = c(3, 3))
plot(X.train$V7, Y.train, col = "blue")
plot(X.train$V9, Y.train, col = "blue")
plot(X.train$V10, Y.train, col = "blue")
plot(X.train$V11, Y.train, col = "blue")
plot(X.train$V12, Y.train, col = "blue")
plot(X.train$V13, Y.train, col = "blue")
plot(X.train$V15, Y.train, col = "blue")
plot(X.train$V19, Y.train, col = "blue")
plot(X.train$V22, Y.train, col = "blue")



#Transformation
X.train.log = log10(X.train) 
X.train.sq = sqrt(X.train)

X.test.log = log10(X.test)
X.test.sq = sqrt(X.test)

for(i in 1:ncol(X.train)){
  cor = cor(X.train.sq[,i], Y.train)
  print(paste("i = ",  i, ", cor = ", cor))
}

for(i in 1:ncol(X.train)){
  cor = cor(X.train.log[,i], Y.train)
  print(paste("i = ",  i, ", cor = ", cor))
}
## Correlation stayed quite the same.

####################Usual Linear Regression model:######################
fit.usual = lm(Y.train ~ X.train)
summary(fit.usual)
fit.usual$coefficients

coef.usual = coef(fit.usual)
Y.pred = coef.usual[1] + X.test%*%coef.usual[-1]

error.usual = mean((Y.test - Y.pred)^2)
error.usual
#[1] 0.0688315

#################Linear regression with Ridge penalty##################
library(glmnet)
fit.ridge = cv.glmnet(X.train, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, X.test , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.06281705

fit.ridge$lambda.min
# [1] 0.007351743
range(fit.ridge$lambda)
# [1]  0.004206937 38.332046104

library(glmnet)
fit.ridge = cv.glmnet(X.train, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, X.test , s = 1)
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
#[1] 0.05527972

#Ridge fit with sqrt transformation
library(glmnet)
fit.ridge = cv.glmnet(X.train.sq, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, X.test.sq, s = 1)
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05500825

#################Linear regression with Lasso penalty##################
library(glmnet)
fit.lasso = cv.glmnet(X.train, Y.train)
Y.pred = predict(fit.lasso, X.test , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.0625924


fit.lasso$lambda.min
# [1] 0.002352019
range(fit.ridge$lambda)
# [1] 0.004206937 38.332046104

#refit Lasso with lambda = 0.04
library(glmnet)
fit.lasso = cv.glmnet(X.train, Y.train)
Y.pred = predict(fit.lasso, X.test , s = 0.04)
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05633234

#Lasso fit with sqrt transformation
library(glmnet)
fit.lasso = cv.glmnet(X.train.sq, Y.train)
Y.pred = predict(fit.lasso, X.test.sq , s = 0.04)
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05633234

######################### PCA ########################
A = cov(X.train)
eigen(A)$values
which.max(eigen(A)$values) #The first one has the biggest eigenvalue
#hence is the PC with most amount of information

par(mfrow = c(1,2))
plot(eigen(A)$values, type = "h", lwd = 5, col = "purple",
     xlab = "PC's", ylab = "Eigenvalues")
plot(eigen(A)$values[-1], type = "h", lwd = 5, col = "purple",
     xlab = "PC's (minus the 1st)", ylab = "Eigenvalues") 
title("The Significant PC's'", outer = TRUE, line = -2, cex = 3)
#PC1 to PC5 seem significant

C = eigen(A)$vectors
new.train = cbind(X.train, X.train%*%C[,c(1:5)])

A = cov(X.test)
eigen(A)$values
which.max(eigen(A)$values) #The first one has the biggest eigenvalue
#hence is the PC with most amount of information

C = eigen(A)$vectors
new.test = cbind(X.test, X.test%*%C[,c(1:5)])

dim(new.train)
#[1] 1000   20 #4 PC's added

new.xtrain = new.train[,17:21]
dim(new.xtrain)
# [1] 1000   5
new.xtest = new.test[,17:21]
dim(new.xtest)
#[1] 4875   5

#Ridge
library(glmnet)
fit.ridge = cv.glmnet(new.xtrain, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, new.xtest , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
#[1] 0.05460363

#Lasso
fit.lasso = cv.glmnet(new.xtrain, Y.train)
Y.pred = predict(fit.lasso, new.xtest , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05400543

###BEST FIT

fit.lasso$lambda.min
# [1] 0.00187675

coef(fit.lasso)
# 6 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  0.67986900
# -0.04557849
# .         
# .         
# 5.58207043
# 9.26479508


##################Repeat above for the second Y variable#############

library(scales)
#' want to rescale the data so that they're all between 0 and 1 (normalized)
#' and the interpretibility of the prediction errors is easier

X.train = rescale(as.matrix(train[,7:22]))
X.test = rescale(as.matrix(test[,7:22]))
Y.train = rescale(train[,6])
Y.test = rescale(test[ ,6])


#Investigate correlation between X's and Y
for(i in 1:ncol(X.train)){
  cor = cor(X.train[,i], Y.train)
  print(paste("i = ",  i, ", cor = ", cor))
}
# 
# [1] "i =  1 , cor =  0.0739802239448816"
# [1] "i =  2 , cor =  0.0534700980063472"
# [1] "i =  3 , cor =  0.0659329631046685"
# [1] "i =  4 , cor =  0.0620614598319494"
# [1] "i =  5 , cor =  0.0659538125182766"
# [1] "i =  6 , cor =  0.069413301364315"
# [1] "i =  7 , cor =  0.0754844983969396"
# [1] "i =  8 , cor =  0.0574491295062612"
# [1] "i =  9 , cor =  0.0586362496768223"
# [1] "i =  10 , cor =  0.102272961176441"
# [1] "i =  11 , cor =  0.0574490662629386"
# [1] "i =  12 , cor =  0.0624389209831174"
# [1] "i =  13 , cor =  -0.133570072042911"
# [1] "i =  14 , cor =  0.131124242383567"
# [1] "i =  15 , cor =  -0.161134776873336"
# [1] "i =  16 , cor =  0.131711832230074"

#They are very weekly correlated. However, their combination can still effectively
# predict Y. Transformation methods are also applied to help the correlation.

#Transformation
X.train.log = log10(X.train) 
X.train.sq = sqrt(X.train)
X.test.log = log10(X.test)
X.test.sq = sqrt(X.test)


####################Usual Linear Regression model:######################
fit.usual = lm(Y.train ~ X.train)
summary(fit.usual)
fit.usual$coefficients

coef.usual = coef(fit.usual)
Y.pred = coef.usual[1] + X.test%*%coef.usual[-1]

error.usual = mean((Y.test - Y.pred)^2)
error.usual
#[1] 0.0577305

#################Linear regression with Ridge penalty##################
library(glmnet)
fit.ridge = cv.glmnet(X.train, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, X.test , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05524565


#Fit with sqrt transformation
library(glmnet)
fit.ridge = cv.glmnet(X.train.sq, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, X.test.sq, s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05443571

#################Linear regression with Lasso penalty##################
library(glmnet)
fit.lasso = cv.glmnet(X.train, Y.train)
Y.pred = predict(fit.lasso, X.test , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.05506565


######################### PCA ########################

# A = cov(X.train)
# eigen(A)$values
# 
# par(mfrow = c(1,2))
# plot(eigen(A)$values, type = "h", lwd = 5, col = "purple",
#      xlab = "PC's", ylab = "Eigenvalues")
# plot(eigen(A)$values[-1], type = "h", lwd = 5, col = "purple",
#      xlab = "PC's (minus the 1st)", ylab = "Eigenvalues") 
# title("The Significant PC's'", outer = TRUE, line = -2, cex = 3)
# #PC1 to PC5 seem significant
# 
# C = eigen(A)$vectors
# new.train = cbind(X.train, X.train%*%C[,c(1:5)])
# 
# A = cov(X.test)
# eigen(A)$values
# C = eigen(A)$vectors
# new.test = cbind(X.test, X.test%*%C[,c(1:5)])
# 
# new.xtrain = new.train[,17:21]
# new.xtest = new.test[,17:21]
# 


# Using the new X variables (PC's) computed above as new.xtrain and new.xtest:

library(glmnet)
fit.ridge = cv.glmnet(new.xtrain, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, new.xtest , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
#[1] 0.0476928
# coef(fit.ridge)
# 6 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  0.59749903
# -0.04745375
# 0.46997583
# 0.03654323
# 3.59981162
# 4.75306913


fit.lasso = cv.glmnet(new.xtrain, Y.train)
Y.pred = predict(fit.lasso, new.xtest , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 
# [1] 0.04768863

fit.lasso$lambda.min
# [1] 0.001178654

coef(fit.lasso)
# 6 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  0.71780993
# -0.06315695
# .         
# .         
# 6.65172809
# 10.65443339

###BEST FIT

fit.lasso$lambda.min
# [1] [1] 0.002040315

coef(fit.lasso)
# 6 x 1 sparse Matrix of class "dgCMatrix"
# 1
# (Intercept)  0.64773879
# -0.04584834
# .         
# .         
# 6.15853225
# 8.36215970

#Variable selection is done

########################PCA 2 (using PCA function)########################
train.pca <- prcomp(X.train, center = TRUE, scale. = TRUE)
print(train.pca)
plot(train.pca, type = "l")
test.pca <- prcomp(X.test, center = TRUE, scale. = TRUE)

new.xtrain = train.pca$x[,1:6]
dim(new.xtrain)
# [1] 1000   5
new.xtest = test.pca$x[,1:6]
dim(new.xtest)

library(glmnet)
fit.ridge = cv.glmnet(new.xtrain, Y.train, alpha = 0)
coef(fit.ridge)
range(fit.ridge$lambda)
Y.pred = predict(fit.ridge, new.xtest , s = "lambda.min")
MSE.ridge = mean((Y.pred - Y.test)^2)
MSE.ridge 

# [1] 0.054
