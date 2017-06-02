#R -  Source code using Support vector machines 

#loading library
library(e1071)
library(ineq)

# Importing dataset after removing after cleaning the data:
# Cleaning involved: 1. Introducing dummy variables for categorical. 
#                    2. Prediction missing values for age using regression.

#Code to generate regression equation for age using all other variables
#(Could have used imputation to fill the missing age values with mean or median but in that case we will be reducing variance as data set is not that large and 25% (1804/7414)of data is missing in age)
a <- read.csv("AgeWithoutBlanks.csv")
m <- step(lm(a$custAge~., data = a), direction = "both")

#Equation obtained:
#Age = -50.93 -(0.663)*blue-collar + (1.77)*entrepreneur + (4.713)*housemaid + (2.72)*management + (19.7)*retired + (1.27)*selfemployed - (8.53)*student + (5.98)*unknown - (7.1)*single + (2.68)*divorced - (3.78)*basic.6y - (4.3)*basic.9y - (4.3)*high.school - (3.9)*profession - (4.8)*university.degree - (3.79)*blank + (3.27)*unknown - (1.9)*jul - (2.06)*jun - (2.42)*may + (0.72)*mon + (0.7)*tue + (1.13)*con.price.idx + (.2)*cons.conf.idx - (0.5)*euribor.3m
#Prediction on age restores mean and median of the age column which is 38 and 39 respectively 

#Importing final clean data whcih has no missing values
original <- read.csv("FinalCleanData.csv")

#setting seed
set.seed(123)

#removing columns on basis of correlation matrix
original <- original [,-c(51, 55, 48,50,56,27,24)]

#importing reduced bias data- bias reduced by duplicating the 1's and doing oversampling by making no. of 0's equal to no. of 1's
ReducedBias <- read.csv("fiftyfifty.csv")
dim(ReducedBias)

#removing columns on basis of correlation matrix
ReducedBiasNoCorrelation <- ReducedBias [,-c(51, 55, 48,50,56,27,24)]

#Calculating gini predictive power of each variable
r <- c()
for(i in 1:ncol(ReducedBiasNoCorrelation)){
  r <-c(r, ineq(ReducedBiasNoCorrelation[,i], type = "Gini"))
  
}
r
w <- which(r>0.9)
data <- ReducedBiasNoCorrelation[,c(w,51)]

#randomly selecting train and test
n <- nrow(data)
shuffled_data <- data[sample(n), ]
train <- shuffled_data[1:6574, ]
test <- shuffled_data[6575:13294, ] 
dim(train)
dim(test)

#checking 1s in my dataset
sum(train$responded)
sum(test$responded)

#Running Support vecor machines on randomly generated train
svmfit <- svm(train$responded~., data = train, type='C-classification')
summary(svmfit)

#Predicting on randomly generated test
prediction <- predict(svmfit, test)
table(prediction)

table(test$responded, prediction)

#testing on original
predictionOriginal <- predict(svmfit, original)
table(predictionOriginal)
t <-table(original$responded, predictionOriginal)
t
