library(randomForest)
what <- "Random Forest"

#importing data
dat <- read.csv("FinalCleanData.csv")
sum(is.na(dat))
dim(dat)

#removing columns on basis of correlation matrix
d <- d [,-c(51, 55, 48,50,56,27,24)]

#shuffling
n <- nrow(dat)
shuffled_dat <- dat[sample(n), ]
tr <- shuffled_dat[1:5000, ]
te <- shuffled_dat[5001:7414, ] 
dim(tr)
dim(te)

FOREST_model <- randomForest(tr$responded~., data=tr, importance = T ,ntree=200)
varImpPlot(FOREST_model)
summary(FOREST_model)

Prediction <- predict(FOREST_model, te)
table(te$responded, Prediction)

train_pred <- predict(FOREST_model, tr, type="prob")[,2]
test_pred <- predict(FOREST_model, testset, type="prob")[,2]

display_results()