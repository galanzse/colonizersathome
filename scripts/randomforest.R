
library(tidyverse)
library(randomForest)
library(caTools)
library(party)

# exploratory
data <- read.csv("results/trait_final.txt", sep="")
str(data)

# format variables
data$family <- as.factor(data$family)
data$growth_form <- as.factor(data$growth_form)
data$lifeform <- as.factor(data$lifeform)
data$origin <- as.factor(data$origin)
data$invasiveness <- as.factor(data$invasiveness)
data$clonality <- as.factor(data$clonality)
data$archaeophyte <- as.factor(data$archaeophyte)
data$self.compatible <- as.factor(data$self.compatible)
data$pollination <- as.factor(data$pollination)
data$agochory <- as.factor(data$agochory)
data$autochory <- as.factor(data$autochory)
data$anemochory <- as.factor(data$anemochory)
data$hydrochory <- as.factor(data$hydrochory)
data$zoochory <- as.factor(data$zoochory)

summary(data)

# remove variables with too many NAs
data <- data %>% dplyr::select(-c('RD','SRA','self.compatible'))

# random forest
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
varout <- c('species','origin','invasiveness')
mypred <- setdiff(colnames(data), varout)

# imputacion NAs
data.imp <- rfImpute(family~., data=data[,mypred], iter=5)
data.imp$origin <- data$origin
data.imp$invasiveness <- data$invasiveness

# best ntry = 5
par(mfrow=c(1,1))
tuneRF(y=data$origin, x=data.imp, improve=0.0005, ntreeTry=300)
sqrt(ncol(data.imp))

sample = sample.split(data.imp$origin, SplitRatio = .80)
train = subset(data.imp, sample == TRUE)
test  = subset(data.imp, sample == FALSE)


# predict origin
rf1 <- randomForest(origin~., data=train[,-which(colnames(train)=='invasiveness')],
                    ntree=500, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance

varImpPlot(rf1, n.var=15, main='Origin ~')

pred = predict(rf1, newdata=test[-which(colnames(train)=='invasiveness')]) # cross-validation
(cm = table(test[,'origin'], pred))
(sum(diag(cm))/sum(cm))


# predict invasiveness
rf1 <- randomForest(invasiveness~., data=train[,-which(colnames(train)=='origin')],
                    ntree=500, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance

varImpPlot(rf1, n.var=15, main='Invasiveness ~')

pred = predict(rf1, newdata=test[-which(colnames(train)=='origin')]) # cross-validation
(cm = table(test[,'invasiveness'], pred))
(sum(diag(cm))/sum(cm))

