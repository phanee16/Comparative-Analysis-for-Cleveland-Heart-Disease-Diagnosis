---
title: "Comparative Analysis for Cleveland Heart Disease Diagnosis"
author: "Phaneesha Chilaveni"
date: "6/22/2022"
output: html_document
---

##**The Cleveland heart-disease study was conducted by the Cleveland Clinic Foundation. The response variable is “diag1” (diagnosis of heart disease: buff = healthy, sick = heart disease). There is a second “diag2” that contains stage information about the sick, this can be disregarded. There were 303 patients in the study, and 13 predictive variables, including age, gender, and a range of biological measurements. Fit a neural network, CART model and a random forest to the Cleveland heartdisease data. Compare the results, and comment on the performance.**## 

#Load the all the required libraries for the assignment
 
library(ISLR)
library(e1071)
library(ROCR)
library(caret)
library(kernlab)
library(randomForest)
library(NeuralNetTools)
library(klaR)
library("rpart")
library(neuralnet)
library(rpart.plot)
library(randomForest)
  


#_Loading the Cleveland data and copying it into a new variable boston for further manipulations on the data._
 
load("~/Downloads/cleveland.RData")
Cleveland = cleveland
  

#_Chechking for any NA's in the data_
 
which(is.na(Cleveland) == TRUE) 
  
##**Comment**: There are no NA's in the data.

#_Splitting the data into training and testing by holding out 30% of the random boston data into testing and the remaining as training._
 
set.seed(123)
index = sample(1:nrow(Cleveland),0.7*nrow(Cleveland))
training = Cleveland[index,]
testing = Cleveland[-index,]

#Converting all the data into numeric and dividing again into train and test
numeric_data = data.frame(lapply(Cleveland, as.numeric))
index_new = sample(1:nrow(numeric_data),0.7*nrow(numeric_data))
training_new = numeric_data[index_new,]
testing_new = numeric_data[-index_new,]
  

#_Fitting a Neural network on the Cleveland numeric data and finding the error._ 
 
nn0 <- neuralnet(diag1~., data = training_new, hidden = c(8,4), stepmax = 10^10, linear.output = FALSE)
pred <- predict(nn0, newdata = testing_new[,-15])
mean(pred-testing_new$diag1)^2

  
##**Comment**: The mean squared error is 22.27994%. the acccuracy is 77.72006%.

#_Plotting the neural network with normal plot function_
 
plot(nn0,rep = "best", x.entry = NULL, x.out = NULL,
  radius = 0.25, arrow.length = 0.2, intercept = TRUE,
  intercept.factor = 0.4, information = TRUE, col.entry.synapse = "darkgreen", col.entry = "deeppink1",
  col.hidden = "darkgoldenrod", col.hidden.synapse = "mediumturquoise",
  col.out = "midnightblue", col.out.synapse = "indianred",
  col.intercept = "mediumorchid4", fontsize = 10, dimension = 6,show.weights = FALSE, fill = 'blue',)
  

#_Plotting the neural network with plotnet function._
 
plotnet(nn0,
  rel_rsc = 1, circle_cex = 6, cex_val = 0.8, alpha_val = 1,
  circle_col = c("mediumturquoise","lightblue","slategray1","lavender","thistle"), pos_col = "black", neg_col = "gray",
  bord_col = "black", max_sp = TRUE, pad_x = 1, prune_col = NULL,
  prune_lty = "line", )
  

#_Classification and Regression Trees_

 

#Classification
set.seed(123)
model.control <- rpart.control(minbucket= 1,minsplit = 2, xval = 15, cp = 0)
fit_c <- rpart(diag1 ~ ., data = training[,-15], method = "class" ,control = model.control)

fit_c$cptable
plot(fit_c$cptable[,4], main = "Cp for model selection", ylab = "Cp")

min_cp = which.min(fit_c$cptable[,4])
pruned_fit <- prune(fit_c, cp = fit_c$cptable[min_cp,1])
  

##**plot the full tree and the pruned tree**
#_Full Tree_
 

rpart.plot(fit_c,uniform = TRUE, branch = .3, compress=T,main = "Full Tree using rpart.plot()")
plot(fit_c,uniform = TRUE, branch = .3, compress=T, main = "Full Tree using plot()")
text(fit_c,use.n=TRUE, all=TRUE, cex = .5)
  


#_Pruned Tree_
 
rpart.plot(pruned_fit,uniform = TRUE, branch = .3, compress=T,main = "Pruned Tree using rpart.plot()")
plot(pruned_fit,uniform = TRUE, branch = .3, compress=T, main = "Pruned Tree using plot()")
text(pruned_fit,use.n=TRUE, all=TRUE, cex = .7)

  


 
#Regression

set.seed(13)
model.controls <- rpart.control(minbucket = 1, minsplit = 2, xval = 15, cp = 0)
fit_r <- rpart(diag1~., data = training_new[,-15], control = model.controls)
plot(fit_r$cptable[,4], main = "Cp for model selection", ylab = "cv error")
  

##**plot the full tree and the pruned tree**
#_Full Tree_
 

rpart.plot(fit_r,uniform = TRUE, branch = .6, compress=T,main = "Full Tree")
plot(fit_r, uniform = TRUE,branch = .3, compress=T, main = "Full Tree")
text(fit_r,use.n=TRUE, all=TRUE, cex = .5)
rsq.rpart(fit_r)#to visualize cross-validation results
  

#_Pruned Tree_
 
min_cp = which.min(fit_r$cptable[,4])#4th column is the error
pruned_fit_r <- prune(fit_r, cp = fit_r$cptable[min_cp, 1])


rpart.plot(pruned_fit_r,uniform = TRUE, branch = .6, compress=T,main = "Pruned Tree")
plot(pruned_fit_r, uniform = TRUE,branch = .3, compress=T, main = "Pruned Tree")
text(pruned_fit_r,use.n=TRUE, all=TRUE, cex = .5)
rsq.rpart(pruned_fit_r)#to visualize cross-validation results

  


#_Testing Error_
 
pred = predict(fit_r,newdata = testing_new[,-15])
unique(pred)
mean(pred!=testing_new$diag1)
table(pred,testing_new$diag1)
  
##**Comment**: The error is 32.58427% and the accuracy is 67.41573%

#_Performing Random Forest on the cleveland data and observing the error_
 
set.seed(123)
rf.fit <- randomForest(diag1~., data = training[,-15],mtry=10, n.tree = 10000)
rf.fit
plot(rf.fit)
varImpPlot(rf.fit)

importance(rf.fit)
  
#_Error_
 
pred <- predict(rf.fit, newdata = testing[,-15], type = "response")

mean(pred!=testing$diag1)
  
##**Comment** : The error is 16.85393% and the accuracy is 83.14607%
########
##**Comments on which model is best**: Among all the models random Forest has highest accuracy followed by Neural network and CART.So for the cleveland data random Forest predicts the model correctly.
