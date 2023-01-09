
library(tidyverse)
library(randomForest)
library(caTools)

# exploratory
data <- read.csv("results/trait_final.txt", sep="")
str(data)

# traits to analyse: remove correlated and little significant traits, and traits with many NAs
colnames(data)

# pairs(data[,c("SLA","LDMC","SRL","RDMC","dC13","height","seed_weight","onset_flowering","length_bloom","LCC","RD")], upper.panel=NULL)
colSums(is.na(data))
v_traits <- c("growth_form","family","lifeform","SLA","SRL","RDMC","dC13","height","seed_weight","onset_flowering","length_bloom","LCC","clonality","pollination","agochory","autochory","anemochory","hydrochory","zoochory","numb_disp","counts","climatic_div","climatic_ric")

data <- data %>% dplyr::select(c('origin','invasiveness',all_of(v_traits))) %>% filter(!(is.na(pollination)))
data$clonality[is.na(data$clonality)] <- 0

# format variables
data$family <- as.factor(data$family)
data$growth_form <- as.factor(data$growth_form)
data$lifeform <- as.factor(data$lifeform)
data$origin <- as.factor(data$origin)
data$invasiveness <- as.factor(data$invasiveness)
data$clonality <- as.factor(data$clonality)
data$pollination <- as.factor(data$pollination)
data$agochory <- as.factor(data$agochory)
data$autochory <- as.factor(data$autochory)
data$anemochory <- as.factor(data$anemochory)
data$hydrochory <- as.factor(data$hydrochory)
data$zoochory <- as.factor(data$zoochory)

# random forest
# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
varout <- c('species','origin','invasiveness')
mypred <- setdiff(colnames(data), varout)

# imputacion NAs
data.imp <- rfImpute(family~., data=data[,mypred], iter=5)
data.imp$origin <- data$origin
data.imp$invasiveness <- data$invasiveness

sqrt(ncol(data.imp)) # best ntry = 5

# split data
sample = sample.split(data.imp$origin, SplitRatio = .80)
train = subset(data.imp, sample == TRUE)

# predict origin
rf1 <- randomForest(origin~., data=train[,-which(colnames(train)=='invasiveness')], ntree=500, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance
varImpPlot(rf1, n.var=15, main='Origin ~')

# predict invasiveness
rf1 <- randomForest(invasiveness~., data=train[,-which(colnames(train)=='origin')], ntree=500, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance
varImpPlot(rf1, n.var=15, main='Invasiveness ~')
