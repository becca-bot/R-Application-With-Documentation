#install.packages(c("class", "ggplot2"))

library(class)
library(ggplot2)


setwd("C:/Users/Admin/Documents")
train <- read.csv("C:/Users/Admin/Documents/train.csv")


##setting the value in R to stop Scientific Notations from being displayed

options(scipen = 999)

#Cutting columns

cut_train <- train[,-c(1,4,5,6,7,8,9,11,12,14,15,16,22,24,25,26,27,33,34,35,36,37,38,39,44,45,46,47,49,51,53,59,60,61,63,64,65,67,68,69,70,71,72,73,74,75,76,77,78,79,80)]
cut_train <- cut_train [,-c(16)]

#Putting the sales prices into ranges

cut_price <- cut(cut_train$SalePrice,breaks = c(0,100000, 150000, 200000, 250000, 300000, 350000, 400000,500000, 600000, 700000,800000), ordered_results = TRUE)
cut_train<-cbind(cut_train,cut_price)
cut_train <- cut_train [,-c(29)]
cut_train$cut_price <- as.numeric(cut_train$cut_price)


#Ordering factors

cut_train$HouseStyle <- ordered(cut_train$HouseStyle, levels = c("1Story", "1.5Unf","1.5Fin", "2Story", "2.5Unf", "2.5Fin", "SFoyer", "SLvl"))
cut_train$ExterQual <- ordered(cut_train$ExterQual, levels = c( "Fa", "TA", "Gd", "Ex"))
cut_train$ExterCond <- ordered(cut_train$ExterCond, levels = c("Po","Fa","TA", "Gd", "Ex"))

levels(cut_train$BsmtQual) <- c("None","Fa","TA", "Gd", "Ex")
cut_train$BsmtQual[is.na(cut_train$BsmtQual)] <- "None"
cut_train$BsmtQual <- ordered(cut_train$BsmtQual, levels = c("None","Fa","TA", "Gd", "Ex"))

levels(cut_train$BsmtCond) <- c("None","Po","Fa","TA", "Gd")
cut_train$BsmtCond[is.na(cut_train$BsmtCond)] <- "None"
cut_train$BsmtCond <- ordered(cut_train$BsmtCond, levels = c("None","Po","Fa","TA", "Gd"))

cut_train$HeatingQC <- ordered(cut_train$HeatingQC, levels = c("Po","Fa","TA", "Gd","Ex"))
cut_train$KitchenQual <- ordered(cut_train$KitchenQual, levels = c("Fa","TA", "Gd","Ex"))

levels(cut_train$FireplaceQu) <- c("None","Po","Fa","TA", "Gd","Ex")
cut_train$FireplaceQu[is.na(cut_train$FireplaceQu)] <- "None"
cut_train$FireplaceQu <- ordered(cut_train$FireplaceQu, levels = c("None","Po","Fa","TA", "Gd","Ex"))

levels(cut_train$Electrical) <- c("None","Mix","FuseP","FuseF","FuseA","SBrkr")
cut_train$Electrical[is.na(cut_train$Electrical)] <- "None"
cut_train$Electrical <- ordered(cut_train$Electrical, levels = c("None","Mix","FuseP","FuseF","FuseA","SBrkr"))



#######################################################
par(mfrow=c(1,2))

#boxplot(train$SalePrice,train$MSZoning)
#boxplot(train$SalePrice ~ train$MSSubClass)
                           
#boxplot(train$SalePrice ~ train$OverallCond)

#(train$SalePrice, train$ExterCond)

#linmo<-lm(train$SalePrice~train$OverallQual,train)
#abline(linmo)

cor(cut_train$OverallQual, as.numeric(cut_train$ExterQual))
cor(cut_train$OverallCond, as.numeric(cut_train$ExterCond))

###########################################################################



hist(as.numeric(cut_train$SalePrice ~ as.numeric(cut_train$YearBuilt)))






############################################################################

#Creating KNN Mdoel that will be used to test data

#Normalising the data

cut_train[,-29] <- as.data.frame(lapply(cut_train[, -29],as.integer))
featureScaling <- function(x) { ((x - min(x)) / (max(x) - min(x))) } 

normalised <- featureScaling(cut_train[, -29])

cut_train[, -29] <- as.data.frame(lapply(cut_train[, -29,], featureScaling))

#Looking for Na data

na_counts <- as.data.frame(lapply(lapply(cut_train,is.na),sum))
na_containing_columns <- na_counts[, na_counts>0]
names(na_containing_columns)

#Creating out training and test data

training_data <- cut_train[1:990,]

test_data <- cut_train[991:1460,]

k_value <- 5

predictions <- knn(training_data[,-29],
test_data[,-29],training_data[,29], k = k_value)

results_table <- table(as.integer(test_data[,29]), as.integer(predictions))
print(results_table)

errors <- results_table[1,2] + results_table[2,1]
print(errors)

#Optimising K Value?

all_errors <- NULL
k_values <- 1:100

for(i in k_values){
  next_predictions <- knn(training_data[,-29],test_data[,-29],training_data[,29],k=i)
  
  next_results_table <- table(test_data[,29],
                              next_predictions)
  
  next_errors <- next_results_table[2,1]
  
  all_errors <- c(all_errors,next_errors)
}

plot(all_errors)

min(all_errors)


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(results_table)

ggplot(data.frame(k_values,all_errors), aes(x = k_values, y = all_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for sales price") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_color_manual(values = c("red","blue"))


#####################################################################################


less_data <- cut_train [,-c(1,2,3,5,7,8,9,10,12,13,14,15,17,18,21,24,26,28)]

training_data <- less_data[1:990,]

test_data <- less_data[991:1460,]

k_value <- floor(sqrt(nrow(less_data)))

predictions <- knn(training_data[,-11],
                   test_data[,-11],training_data[,11], k = k_value)

results_table <- table(as.integer(test_data[,11]), as.integer(predictions))
print(results_table)

errors <- results_table[1,2] + results_table[2,1]
print(errors)

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(results_table)

all_errors <- NULL
k_values <- 1:100

for(i in k_values){
  next_predictions <- knn(training_data[,-11],test_data[,-11],training_data[,29],k=i)
  
  next_results_table <- table(test_data[,29],
                              next_predictions)
  
  next_errors <- next_results_table[2,1]
  
  all_errors <- c(all_errors,next_errors)
}

plot(all_errors)

min(all_errors)


accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(results_table)

ggplot(data.frame(k_values,all_errors), aes(x = k_values, y = all_errors)) +
  geom_point() +
  geom_smooth(method = "loess",colour = "blue", size = 1) + 
  ggtitle("Error vs k-Value for sales price") +
  xlab("k-Values") +
  ylab("Error") +
  theme(axis.text.x=element_text(angle=-45, vjust=0.5)) +
  theme(axis.text.y=element_text(angle=-45, hjust=-0.1, vjust=-0.5)) +
  scale_color_manual(values = c("red","blue"))




