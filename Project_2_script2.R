#######################################
#' Project 2
#' @author Bita Nezamdoust
#######################################

install.packages("corrplot")
install.packages("gridExtra")
install.packages("glmnet")
install.packages("rda")
install.packages("e1071")
#################TASK 2####################
#Read Rdata file
d =get(load("Work/Study_2018/Spring_2018/Stat 8670: Computational Methods in Statistics/Project_2/Paert2:LogisticReg/project_2_data_2.RData"))
ls()
str(d)
dim(d)

summary(X.train)
summary(Y.train)
table(Y.train)
# 1  2  3  4  5  6 
# 45 47 38 49 50 71 

cor(X.train[,1], X.train[,5])

#Correlation Analysis
# install.packages("corrplot")

#head(round(M,2))
 
library(corrplot)
par(mfrow = c(2, 2))
M<-cor(X.train[,c(20:40)])
corrplot(M, method="pie", type = "lower", tl.col = "black")
M<-cor(X.train[,c(60:80)])
corrplot(M, method="pie", type = "lower", tl.col = "black")
M<-cor(X.train[,c(180:200)])
corrplot(M, method="pie", type = "lower", tl.col = "black")
M<-cor(X.train[,c(440:460)])
corrplot(M, method="pie", type = "lower", tl.col = "black")
#corrplot(M, method="number", type = "lower", number.cex = .7, tl.col = "black")
title("MULTICOLLINEARITY BETWEEN PREDICTORS", outer = TRUE, line = -1, cex = 5)



library(ggplot2)
# install.packages("gridExtra")
library(gridExtra)
frame.for.plot = data.frame(X.train)
p1 = ggplot(data = frame.for.plot) +
geom_point(mapping = aes(x = V11, y = V13, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p2 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V20, y = V25, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p3 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V90, y = V92, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p4 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V170, y = V180, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p5 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V195, y = V196, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p6 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V330, y = V333, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p7 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V459, y = V460, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p8 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V424, y = V236, color = Y.train))
scale_color_manual(values = c("red", "blue"))
p9 = ggplot(data = frame.for.plot) +
 geom_point(mapping = aes(x = V510, y = V511, color = Y.train))
scale_color_manual(values = c("red", "blue"))
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9, ncol = 3)
 



#Fitting models
########Usual logistic Regression without Penalty######
library(glmnet)
usual.fit = glmnet(X.train, Y.train, family = "multinomial")
summary(usual.fit)
coef(usual.fit, s = 0)

Y.pred = predict(usual.fit, newx = X.test, s=0, type = "class")
error.usual = sum(Y.pred!=Y.test)/length(Y.test)

error.usual
#[1] 0.09

#########Logistic regression with Ridge penalty#########
#To shrink the parameter estimates to deal with the effect of multicolinearity
#"multinomial" for Y having more than 2 classes
ridge.fit = cv.glmnet(X.train, Y.train, family = "multinomial", alpha = 0)
Y.pred = predict(ridge.fit,newx = X.test, type = "class", s = "lambda.min")
error.ridge = sum(Y.pred!=Y.test)/length(Y.test)
error.ridge
#[1] 0.1533333
coef(ridge.fit)

#Plot
fit.all = glmnet(X.train, Y.train, family = "multinomial", alpha = 0)
plot(fit.all, xvar = "lambda")

#########Logistic regression with Lasso penalty#########
lasso.fit = cv.glmnet(X.train, Y.train, family = "multinomial")
Y.pred = predict(ridge.fit,newx = X.test, type = "class", s = "lambda.min")
error.lasso = sum(Y.pred!=Y.test)/length(Y.test)
error.lasso
# [1] 0.1533333

##########LDA#########
library(MASS)
lda.fit = lda(X.train, Y.train)
Y.pred = predict(lda.fit, X.test)$class
error.lda = sum(Y.pred!=Y.test)/length(Y.test)
error.lda
# [1] 0.2166667
summary(lda.fit)

#########QDA##########
library(MASS)
qda.fit = qda(X.train, Y.train)
Y.pred = predict(qda.fit, X.test)$class
error.qda = sum(Y.pred!=Y.test)/length(Y.test)
error.qda

# [1] 0.2166667

##########RDA##########
#install.packages("rda")
library("rda")
X.train.rda = t(X.train)
Y.train.rda = as.numeric(Y.train)
X.test.rda = t(X.test)
Y.test.rda = as.numeric(Y.test)

rda.fit = rda(X.train.rda, Y.train.rda)
cv.rda.fit = rda.cv(rda.fit, x = X.train.rda, y = Y.train.rda)
alpha = (cv.rda.fit$alpha)
delta = (cv.rda.fit$delta)
opt.alpha = alpha[which.min(apply(cv.rda.fit$cv.err, 1, min))]
opt.delta = delta[which.min(apply(cv.rda.fit$cv.err, 2, min))]
Y.pred = predict(rda.fit, x = X.train.rda, y = Y.train.rda, xnew = X.test.rda, 
                 alpha = opt.alpha, delta = opt.delta)
error.rda = sum(Y.pred!=Y.test.rda)/length(Y.test.rda)
error.rda
# 0.0666667

##BEST FIT
predict(rda.fit, x = X.train.rda, y = Y.train.rda, xnew = X.test.rda, 
        type = "nonzero")
str(rda.fit)
summary(rda.fit)
opt.alpha
# [1] 0.99
opt.delta
# [1] 0
rda.fit
# $nonzero
# delta
# alpha    0 0.333 0.667   1 1.333 1.667   2 2.333 2.667   3
# 0    561   234    51   1     0     0   0     0     0   0
# 0.11 561   174    28   5     1     0   0     0     0   0
# 0.22 561   162    25   5     1     0   0     0     0   0
# 0.33 561   162    26   6     5     1   0     0     0   0
# 0.44 561   179    28   7     5     1   1     0     0   0
# 0.55 561   195    34  12     6     5   1     1     1   0
# 0.66 561   220    58  19     8     6   6     5     1   1
# 0.77 561   288    98  39    17    10   6     6     6   5
# 0.88 561   431   218 100    49    31  19    12     9   7
# 0.99 561   561   557 543   527   502 466   429   391 356
# 
# $errors
# delta
# alpha   0 0.333 0.667   1 1.333 1.667   2 2.333 2.667   3
# 0    41   117   141 229   229   229 229   229   229 229
# 0.11 35   115   138 183   229   229 229   229   229 229
# 0.22 32   107   102 179   229   229 229   229   229 229
# 0.33 30   101    85 179   182   229 229   229   229 229
# 0.44 27    84    82 179   179   187 229   229   229 229
# 0.55 26    73    80 179   179   179 182   229   229 229
# 0.66 19    50    86  96   179   179 179   179   182 229
# 0.77 16    31    67  90   101   179 179   179   179 179
# 0.88  5    16    28  63    84    91  95   133   176 179
# 0.99  0     0     1   2     5     9  16    19    29  37


#More model fitting using Support Vector Machine
################ SVM ##################
#Linear:
# install.packages("e1071")
library(e1071)
fit = svm(X.train, Y.train, cost = 0.5, kernel = "linear", type = "C")
pred.class = predict(fit, X.test)
sum(Y.test!=pred.class)/length(Y.test)
#[1] 0.09333333
fit = svm(X.train, Y.train, cost = 0.05, kernel = "linear", type = "C")
pred.class = predict(fit, X.test)
sum(Y.test!=pred.class)/length(Y.test)
#[1] [1] 0.09333333
fit = svm(X.train, Y.train, cost = 1, kernel = "linear", type = "C")
pred.class = predict(fit, X.test)
sum(Y.test!=pred.class)/length(Y.test)
#[1] [1] 0.09333333
fit = svm(X.train, Y.train, cost = 10, kernel = "linear", type = "C")
pred.class = predict(fit, X.test)
sum(Y.test!=pred.class)/length(Y.test)
#[1] [1] 0.09333333


#Polynomial
library(e1071)
degree = 2
for(C in c(0.1, 1, 5)){
  for(gamma in c(0.0005, 5)){
    for(gamma0 in c(0, 0.1, 10)){
      fit = svm(X.train, Y.train, cost = C, degree = degree, gamma = gamma, 
                coef0 = gamma0, kernel = "polynomial", type = "C")
      pred.class = predict(fit, X.test)
      print(c("C, gamma, gamma0, error = ", C, gamma, gamma0, 
              sum(Y.test!=pred.class)/length(Y.test)))
    }
  }
}
# [1] "C, gamma, gamma0, error = " "1"                         
# [3] "5e-04"                      "0"                         
# [5] "0.76"                      
# [1] "C, gamma, gamma0, error = " "1"                         
# [3] "5e-04"                      "0.1"                       
# [5] "0.426666666666667"         
# [1] "C, gamma, gamma0, error = " "1"                         
# [3] "5e-04"                      "10"                        
# [5] "0.0866666666666667"        
# [1] "C, gamma, gamma0, error = " "1"                         
# [3] "5"                          "0"                         
# [5] "0.14" 
# ...

fit$coefs #????

#Sigmoid
library(e1071)
for(C in c(1, 5, 100)){
  for(gamma in c(0.0005, 0.2)){
    for(gamma0 in c(0, 0.1, 10)){
      fit = svm(X.train, Y.train, cost = C, degree = degree, gamma = gamma, 
                coef0 = gamma0, kernel = "sigmoid", type = "C")
      pred.class = predict(fit, X.test)
      print(c("C, gamma, gamma0, error = ", C, gamma, gamma0, 
              sum(Y.test!=pred.class)/length(Y.test)))
    }
  }
}

# [1] "C, gamma, gamma0, error = " "5"                         
# [3] "5e-04"                      "0"                         
# [5] "0.106666666666667"         
# [1] "C, gamma, gamma0, error = " "5"                         
# [3] "5e-04"                      "0.1"                       
# [5] "0.103333333333333"         
# [1] "C, gamma, gamma0, error = " "5"                         
# [3] "5e-04"                      "10"                        
# [5] "0.793333333333333"         
# [1] "C, gamma, gamma0, error = " "5"                         
# [3] "0.2"                        "0"                         
# [5] "0.566666666666667"  

#Radial
library(e1071)
for(C in c(1, 5, 8)){
  for(gamma in c(0.01, 0.0005)){
    fit = svm(X.train, Y.train, cost = C, gamma = gamma, kernel = "radial", type = "C")
    pred.class = predict(fit, X.test)
    print(c("C, gamma, error = ", C, gamma, 
            sum(Y.test!=pred.class)/length(Y.test)))
  }
}


# [1] "C, gamma, error = " "5"                  "0.01"              
# [4] "0.18"              
# [1] "C, gamma, error = " "5"                  "5e-04"             
# [4] "0.0866666666666667"
# [1] "C, gamma, error = " "8"                  "0.01"              
# [4] "0.18"              
# [1] "C, gamma, error = " "8"                  "5e-04"             
# [4] "0.0966666666666667"

##############Classification Tree#################
library(rpart)
A = data.frame(X.train, Y.train.factor = as.factor(Y.train))
tree.fit = rpart(Y.train.factor ~., data = A)
Y.pred = predict(tree.fit, data.frame(X.test), type="class")
sum(Y.pred != Y.test)/length(Y.pred)
# [1] 0.21


for(maxdepth in c(3, 6, 9))
{
  for(minobs in c(1, 5, 10, 20))
  {
    for(Cp in c(0, 0.001, 0.01))
    {
      A = data.frame(X.train, Y.train.factor = as.factor(Y.train))
      mycontrol = rpart.control(minsplit = minobs, cp = Cp, maxdepth =maxdepth)
      fit = rpart(Y.train.factor ~., data = A, control = mycontrol)
      a = predict(fit, data.frame(X.test), type = "class")
      print(c("maxdepth =", maxdepth, "minobs =", minobs, "Cp =", Cp, "error =",
            sum(a != Y.test)/length(a)))
    }
  }
}

# [1] "maxdepth ="        "6"                 "minobs ="          "1"                
# [5] "Cp ="              "0.01"              "error ="           "0.213333333333333"
# [1] "maxdepth ="        "6"                 "minobs ="          "5"                
# [5] "Cp ="              "0"                 "error ="           "0.223333333333333"
# [1] "maxdepth ="        "6"                 "minobs ="          "5"                
# [5] "Cp ="              "0.001"             "error ="           "0.223333333333333"
# [1] "maxdepth ="        "6"                 "minobs ="          "5"                
# [5] "Cp ="              "0.01"              "error ="           "0.213333333333333"
# [1] "maxdepth ="        "6"                 "minobs ="          "10"               
# [5] "Cp ="              "0"                 "error ="           "0.233333333333333"



