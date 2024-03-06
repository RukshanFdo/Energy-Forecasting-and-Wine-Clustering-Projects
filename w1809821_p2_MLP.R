---
title: "R Notebook"
output: html_notebook
---
```{r}
library(readxl)
UoW_dataX=read_excel("c:/Users/Rukshan Fernando/OneDrive/Desktop/Level_5/2ndSem/5DATA001C.2 Machine Learning and Data Mining/Assesment/w1809821_p2_UoW_load.xlsx") 
UoW_dataX
```
```{r}
x <- UoW_dataX[,4] x normalization <- function(x) 
  {  return ((x - min(x)) / (max(x) - min(x))) 
} 
str(UoW_dataX) 
```
```{r}
UoW_dataX columnOfYear<-factor(UoW_dataX$Dates) 
columnOfYear<-as.numeric(columnOfYear) columnOfYear 
 
UoW_data_frame<-UoW_dataX.frame(columnOfYear,UoW_dataX['11:00'],UoW_dataX['10:00'],UoW_dataX['09:00']) 
UoW_data_frame names(UoW_data_frame)[1] <- "year" 
names(UoW_data_frame)[2] <- "h11" 
names(UoW_data_frame)[3] <- "h10" 
names(UoW_data_frame)[4] <- "h9" 
str(UoW_data_frame) 
UoW_data_Scaled <- as.UoW_dataX.frame(lapply(UoW_data_frame, normalization)) 
UoW_data_Scaled 
```
```{r}
#Training a NN model library("neuralnet") 
 
set.seed(2222) 
UoW_data_Train <- UoW_data_Scaled[1:400, ] UoW_data_Test <- UoW_data_Scaled[401:500, ] 
 
#nn <- neuralnet(h11 ~ year,hidden=c(10,5) , UoW_dataX = UoW_data_Train ,linear.output=TRUE) 
#nn <- neuralnet(h11 ~ year,hidden=c(128,9) , UoW_dataX = UoW_data_Train ,linear.output=TRUE) 
#best
#nn <- neuralnet(h11 ~ year,hidden=c(64,3) , UoW_dataX = UoW_data_Train ,linear.output=TRUE) 
#also best 
nn <- neuralnet(h11 ~ year,hidden=c(5,2) , UoW_dataX = UoW_data_Train ,linear.output=TRUE, learningrate = 0.01) plot(nn) 
```
```{r}
UoW_data_Test[2] ResultOfModel <- predict(nn, newdata = UoW_data_Test[2]) 
#ResultOfModel 
#model1Result1 = compute(nn, UoW_data_Test[1]) 
#model1Result1
```
```{r}
#Obtaining the original (non-normalized) training and testing the required Output
TrainOutput <- x[1:400,"11:00"]
#The 400 initial rows
TestOutput <- x[401:500,"11:00"] 
#Find the maximum and least values in the remaining rows.
min_1 <- min(TrainOutput) 
max_1 <- max(TrainOutput) 
#Present its contents 
#Create a renormalized version of a normalised function. 
no_normalizing <- function(x, min, max) {
  return( (max - min)*x + min ) 
} 
 
renormormalized_prediction_value <- no_normalizing(ResultOfModel, min_1, max_1) 
#renormormalized_prediction_value
```
```{r}
UoW_dataX
```
```{r}
library(Metrics) 
#The RMSE 
rmse(TestOutput$`11:00`,renormormalized_prediction_value) 
#The MAE 
mae(TestOutput$`11:00`,renormormalized_prediction_value) 
#The MAPE 
mape(TestOutput$`11:00`,renormormalized_prediction_value) 
#Investigate the relationship between expected and actual values
cor(renormormalized_prediction_value,TestOutput$`11:00`) 
#Plot for exchange_model 
```
```{r}
par(row_mf=c(1,1)) 
plot(TestOutput$`11:00`, renormormalized_prediction_value ,col='green',main='Renormalized  Prediction Graph',pch=18,cex=0.7) 
abline(0,1,lwd=2) 
legend('bottomright',legend='NN', pch=18,col='green', bty='n') fin_result1_1 <- cbind(TestOutput, renormormalized_prediction_value) 
fin_result1_1 plot(TestOutput$`11:00` , ylab = "Predicted vs Expected", type="l", col="red" ) 
par(new=TRUE) 
plot(renormormalized_prediction_value, ylab = " ", yaxt="n", type="l", col="green" ,main='Predicted  Value Vs Expected Value Graph') 
legend("topleft", c("Expected","Predicted"), fill=c("red","green")) 
```
```{r}
#in_nnnarx <- neuralnet(h11 ~ year+h10+h9,hidden=c(64,32,3), UoW_dataX = UoW_data_Train, linear.output=TRUE) 
in_nnnarx <- neuralnet(h11 ~ year+h10+h9,hidden=c(5,2),UoW_dataX = UoW_data_Train ,linear.output=TRUE) 
plot(in_nnnarx) 
```
```{r}
UoW_data_Test[2] 
result_nnnarx_model1 <- predict(in_nnnarx, newdata = UoW_data_Test[2:4]) 
#result_nnnarx_model1 TestOutput 
```
```{r}
nnnarx_renorm_pre_val <- no_normalizing(result_nnnarx_model1, min_1, max_1) 
 
library("Metrics") rmse(TestOutput$`11:00`,nnnarx_renorm_pre_val) 
# The MSE 
mae(TestOutput$`11:00`,nnnarx_renorm_pre_val) 
#The MAPE 
mape(TestOutput$`11:00`,nnnarx_renorm_pre_val) 
#Investigate the relationship between expected and actual values
cor(TestOutput$`11:00`,nnnarx_renorm_pre_val) 
#Exchange model plot
```
```{r}
par(row_mf=c(1,1)) 
plot(TestOutput$`11:00`, nnnarx_renorm_pre_val ,col='green',main='Renormalized  Prediction Graph',pch=18,cex=0.7) 
abline(0,1,lwd=2) 
legend('bottomright',legend='NN', pch=18,col='green', bty='n') 
fin_result1_1 <- cbind(TestOutput, nnnarx_renorm_pre_val) 
fin_result1_1 plot(TestOutput$`11:00` , ylab = "Predicted vs Expected", type="l", col="red" ) 
par(new=TRUE) 
plot(nnnarx_renorm_pre_val, ylab 	= 	" 	", 	yaxt="n", type="l",	col="green" ,main='Predicted  
Value Vs Expected Value Graph') 
legend("topleft", c("Expected","Predicted"), fill=c("red","green") ) 
```

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 



Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
