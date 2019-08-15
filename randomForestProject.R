#install.packages(c("ggplot2","statsr")
#install.packages("rattle")
#install.packages("rpart.plot")
#install.packages("randomForest")
#install.packages("RGtk2")

library(rattle)
library(rpart.plot)
library(class)
library(ggplot2)
library(randomForest)

setwd("C:/Users/Admin/Documents")
house_train <- read.csv("C:/Users/Admin/Documents/train.csv")

#write.csv(cut_data, "House_Data.csv")

options(scipen = 999)


#Cutting Data

cut_data <- house_train[,-c(1,2,3,4,6,7,8,9,10,11,12,13,14,15,16,17,18,21,22,23,24,25,26,27,28,29,30,31,32,34,35,36,37,38,39,40,41,42,43,46,47,49,51,52,53,54,56,57,58,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80)]
cut_data <- cut_data[,-c(4)]




#setting up my test and train data with a 70:30 split

#train <- cut_data[1:1000,]

#test <- cut_data[1001:1460,]


train_rows <- sample(nrow(cut_data), nrow(cut_data)*0.66)
train <- cut_data[train_rows, ]
test <- cut_data[-train_rows,]


#This linear model gives us information regarding if the data we have chosen
#is suitable for prediction. P value and error rate of the data is show here


model=lm(log(SalePrice) ~ OverallCond + YearBuilt + GarageCars
         + FullBath + BsmtFullBath + LotArea +X1stFlrSF +
           X2ndFlrSF + TotRmsAbvGrd , data = train)

summary(model)

#Creating different verisons of my random forest. 

# fit1 <- rpart(SalePrice ~ OverallCond + YearBuilt + GarageCars
#              + FullBath , data = cut_data, cp =.02)
# 
# fit2 <- rpart(SalePrice ~ OverallCond + YearBuilt + GarageCars
#              + FullBath + BsmtFullBath + LotArea +X1stFlrSF
#               , data = cut_data, cp =.02)


#Fit 3 is the forest that provides the lowest error pecantage at 1% error

# fit3 <- rpart(SalePrice ~ OverallCond + YearBuilt +
#              + FullBath + BsmtFullBath + LotArea +X1stFlrSF +
#                X2ndFlrSF + TotRmsAbvGrd, data = cut_data, cp =.02)
# 
# fit4 <- rpart(SalePrice ~ LotArea + YearBuilt + FullBath + TotRmsAbvGrd + GarageCars, data = train, cp =.02)

fit5 <- rpart(SalePrice ~ OverallCond + YearBuilt +
                + FullBath + BsmtFullBath + LotArea +X1stFlrSF +
                X2ndFlrSF + TotRmsAbvGrd , GarageCars, data = train, cp =.02)



#Plotting the random forest

rpart.plot(fit5, box.palette ="RdBu", shadow.col = "red", nn=TRUE)


#Prediction Based on the random forest model above

prediction = predict(fit5, test, type = "vector")
?predict

prediction

#This allows us to see how accurate our predictions are vs the actual sale price

vaild <- prediction - test$SalePrice


#Calcuating the root mean Squared Error

rmse <- function(actual, prediction){
  
 mean(sqrt(as.numeric(actual**2))) - mean(sqrt(as.numeric(prediction ** 2)))

}

rmse(test$SalePrice, prediction)


#calculating the error

err_per <- function (actual, pred) {
  (abs(actual-pred)/actual)*100
}

mean(err_per(test$SalePrice, prediction))


# error_percentage <- function() {
#   
#   #root square mean of two values
#   
#   abs(rmse(test$SalePrice, prediction)/mean(test$SalePrice))*100
#   
# }
# 
# error_percentage()
