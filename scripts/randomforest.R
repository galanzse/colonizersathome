
library(tidyverse)
# install.packages('randomForest')
library(randomForest)
# install.packages("caTools")
library(caTools)
# install.packages('party')
library(party)

# source("scripts/merge_all_data.R")

#  EXPLORATORY DATA ANALYSIS #### 
data <- read.csv("results/data_analyses.txt", sep="")
str(data)

# clasifico las variables correctamente
data$clonality <- as.factor(data$clonality)
data$archaeophyte <- as.factor(data$archaeophyte)
data$pollination <- as.factor(data$pollination)
data$agochory <- as.factor(data$agochory)
data$autochory <- as.factor(data$autochory)
data$anemochory <- as.factor(data$anemochory)
data$hydrochory <- as.factor(data$hydrochory)
data$zoochory <- as.factor(data$zoochory)
data$onset_flowering <- data$onset_flowering %>% round() %>% as.integer()
data$length_bloom <- data$length_bloom %>% round() %>% as.integer()
data$family <- as.factor(data$family)
data$growth_form <- as.factor(data$growth_form)
data$lifeform <- as.factor(data$lifeform)
data$origin <- as.factor(data$origin)
data$invasiveness <- as.factor(data$invasiveness)

summary(data)


pairs(data[,c("SLA","LDMC","SRL","RDMC",           
              "dC13","LNC","height","CN",         
              "seed_weight","onset_flowering","length_bloom")])

data$CN <- NULL # es lo mismo que LNC

# correlacion entre variables macro
pairs(data[,c("bio1.sd","bio7.m","bio12.sd",       
              "bio15.m","AOO","yrange")])

# boxplots por origen
vtraits <- colnames(data)[2:10]
par(mfrow=c(1,1))
for (tr in vtraits) {
  pl <- data %>% select(tr, origin); colnames(pl) <- c("trait","invasiveness")
  pl <- na.omit(pl)
  print(tr)
  print(table(pl$trait, pl$invasiveness))
}

vtraits <- colnames(data)[27:32]
par(mfrow=c(2,3))
for (tr in vtraits) {
  pl <- data %>% select(tr, origin); colnames(pl) <- c("trait","invasiveness")
  pl <- na.omit(pl)
  boxplot(pl$trait ~ pl$invasiveness, main=tr)
}


# RANDOM FOREST #### 

# https://towardsdatascience.com/random-forest-in-r-f66adf80ec9
varout <- c('species','invasiveness')
data.mod <- data %>% select(-varout)

# imputacion NAs en clonality y pollination
ss_y <- data.mod$origin
ss_x <- data.mod
ss_x$origin <- NULL
data.imp <- rfImpute(x=ss_x, y=ss_y, data=data.mod, iter=5)
colnames(data.imp)[colnames(data.imp)=="ss_y"] <- "origin"

# get best ntry
par(mfrow=c(1,1))
tuneRF(y=data.imp[,1], x=data.imp[,-1], improve=0.05, ntreeTry=50)

sample = sample.split(data.imp$origin, SplitRatio = .75)
train = subset(data.imp, sample == TRUE)
test  = subset(data.imp, sample == FALSE)

rf1 <- randomForest(origin~., data=train, ntree=501, mtry=5, replace=T, importance=T)
rf1
plot(rf1)
rf1$importance

varImpPlot(rf1)

pred = predict(rf1, newdata=test[-1]) # cross-validation
(cm = table(test[,1], pred))
(sum(diag(cm))/sum(cm))



