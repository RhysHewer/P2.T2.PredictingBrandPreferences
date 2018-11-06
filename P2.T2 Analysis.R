library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(corrplot)
library(caret)
library(tidyr)
library(parallel)
library(doParallel)
library(rattle)
library(kableExtra)
library(ROCR)
library(gridExtra)

#read in data
setwd("C:/Users/rhysh/Google Drive/Data Science/Ubiqum/Project 2/Task 2")
incomplete <- read.csv("SurveyIncomplete.csv")
origdata <- read_xlsx("Survey_Key_and_Complete_Responses_excel.xlsx", sheet = 2)
data <- origdata

#Check structure of data to see if alike
names(incomplete) == names(data)

incomplete %>% sample_n(5)
data %>% sample_n(5) 

str(incomplete) 
str(data)


#check for NAs
data %>% is.na() %>% sum()
incomplete %>% is.na() %>% sum()

#data types
data$elevel <- data$elevel %>% as.factor()
data$car <- data$car %>% as.factor()
data$zipcode <- data$zipcode %>% as.factor()
data$brand <- data$brand %>% as.factor()

##check for outliers
numericVars <- Filter(is.numeric, data)
outliers <- numericVars %>% sapply(function(x) boxplot(x, plot=FALSE)$out) %>% str()

#data distribution

qqnorm(data$salary)
qqnorm(data$age)
qqnorm(data$credit)

##Key feature is brand preference, begin with exploring this value.

g6 <- ggplot(data, aes(brand, fill = brand)) +
        geom_bar() +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Frequency") + 
        ggtitle("Brand Preference Frequencies")
g6

##review other histograms for skewdness
histData <- origdata %>% select(-brand)
g8 <- ggplot(gather(histData), aes(value)) + 
        geom_histogram(bins = 20, fill = "#D95F02", colour = "white") + 
        theme_bw() +
        facet_wrap(~key, scales = 'free_x') +
        xlab("Value") + 
        ylab("Count") + 
        ggtitle("Histograms of Numeric Variables")
g8

## Plot Brand choice v other variables

g1 <- ggplot(data, aes(brand, salary, fill = brand)) +
        geom_violin() +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Salary ($)") + 
        ggtitle("Brand Preference v Salary")
g1 <- ggplotly(g1)
g1

g2 <- ggplot(data, aes(brand, age, fill = brand)) +
        geom_violin() +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Age") + 
        ggtitle("Brand Preference v Age")
g2 <- ggplotly(g2)
g2

g3 <- ggplot(data, aes(brand, elevel, colour = brand)) +
        geom_count() +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Education Level") + 
        ggtitle("Brand Preference v Education Level")
g3

g4 <- ggplot(data, aes(brand, car, colour = brand)) +
        geom_count() +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Car") + 
        ggtitle("Brand Preference v Car")
g4

g5 <- ggplot(data, aes(brand, zipcode, colour = brand)) +
        geom_count() +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Zip Code") + 
        ggtitle("Brand Preference v Zip Code")
g5

g7 <- ggplot(data, aes(brand, credit, fill = brand)) +
        geom_violin() +
        theme_bw() +
        scale_fill_brewer(palette="Dark2") +
        xlab("Brand Preference") + 
        ylab("Credit") + 
        ggtitle("Brand Preference v Credit")
g7 <- ggplotly(g7)
g7


##Review correlation matrix numeric data
corrMatrix <- numericVars %>% cor()
corrMatrix %>% corrplot.mixed()

#near zero variance
nzv <- data %>% nearZeroVar(saveMetrics = TRUE)
nzv

#Feature Selection Decision Tree
##decision tree
featureDT <- rpart(brand ~ ., data = data)
fancyRpartPlot(featureDT)
featureDT$variable.importance


##explore brand, age, salary
plot(data$age, data$salary)

g12 <- ggplot(data, aes(salary, age, colour = brand)) +
        geom_point(show.legend = FALSE) +
        facet_grid(brand ~ .) +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Salary") + 
        ylab("Age") + 
        ggtitle("Salary v Age by Brand Preference")
g12

        

## Modelling


#Creating Testing/Training sets
set.seed(111)
trainIndex <- createDataPartition(data$brand, p = 0.75, list = FALSE)
training <- data[ trainIndex,]
testing  <- data[-trainIndex,]

concData <- data %>% select(brand, age, salary)
training.conc <- concData[ trainIndex,]


#set up parallel processing (requires parallel and doParallel libraries)
cluster <- makeCluster(detectCores() - 1) 
registerDoParallel(cluster)

#Cross Validation 10 fold
fitControl<- trainControl(method = "cv", number = 10, savePredictions = TRUE, 
                          allowParallel = TRUE)


##KNN
# Deploy KNN model 
model.KNN.brand <- train(brand ~ ., data = training, 
                         method = "knn", trControl = fitControl)
model.KNN.brand

# Deploy KNN model with concentrated feature set
model.KNN.brand.conc <- train(brand ~ ., data = training.conc, 
                              method = "knn", trControl = fitControl)
model.KNN.brand.conc

# Deploy KNN model with scaled features
model.KNN.brand.scale <- train(brand ~ ., data = training, 
                         method = "knn", trControl = fitControl, 
                         preProcess = c("center", "scale"))
model.KNN.brand.scale

# Deploy KNN model with scaled features and concentrated feature set
model.KNN.brand.conc.scale <- train(brand ~ ., data = training.conc, 
                              method = "knn", trControl = fitControl, 
                              preProcess = c("center", "scale"))
model.KNN.brand.conc.scale

#tune best KNN model further
tGridKNN <- expand.grid(k = c(6,7,8,9,10))
finalKNN <- train(brand ~ ., data = training.conc,
                  method = "knn", trControl = fitControl,
                  preProcess = c("center", "scale"),
                  tuneGrid = tGridKNN)
finalKNN


#test KNN model
#Predictions on the test set (model = model name, testing = test set)
predictions.KNN <- predict(finalKNN, testing)
testing$predictions.KNN <- predictions.KNN


#Confusion matrix
confMatrixKNN <- confusionMatrix(testing$brand, testing$predictions.KNN)
confMatrixKNN
metrics.KNN <- postResample(pred = testing$predictions.KNN, obs = testing$brand)
metrics.KNN
fourfoldplot(confMatrixKNN$table, conf.level = 0, margin = 2, main = "Confusion Matrix KNN")


## Random Forest
# Deploy RF model 
set.seed(111)
tGridRF <- expand.grid(mtry = c(16,18,20))
model.RF.brand.tune <- train(brand ~ ., data = training, 
                        method = "rf", trControl = fitControl,
                        tuneGrid = tGridRF)
model.RF.brand.tune

## Deploy RF model on concentrated feature set
tGridRF.conc <- expand.grid(mtry = c(1,2,3))
model.RF.brand.conc <- train(brand ~ ., data = training.conc, 
                         method = "rf", trControl = fitControl,
                         tuneGrid = tGridRF.conc)
model.RF.brand.conc 


## Test Random Forest
predictions.RF <- predict(model.RF.brand.tune, testing)
testing$predictions.RF <- predictions.RF

#Confusion matrix
confMatrixRF <- confusionMatrix(testing$brand, testing$predictions.RF)
confMatrixRF
metrics.RF <- postResample(pred = testing$predictions.RF, obs = testing$brand)
metrics.RF
fourfoldplot(confMatrixRF$table, conf.level = 0, margin = 1, main = "Confusion Matrix RF")


##GBM
tGridGBM <- expand.grid(n.trees = c(150,200,250), 
                        interaction.depth = c(3,4), shrinkage = 0.1, 
                        n.minobsinnode = c(5,10))

model.GBM.brand <- train(brand ~ ., data = training, 
                         method = "gbm", trControl = fitControl,
                         verbose = FALSE, tuneGrid = tGridGBM)
model.GBM.brand

#concentrated featureset
model.GBM.brand.conc <- train(brand ~ ., data = training.conc, 
                             method = "gbm", trControl = fitControl,
                             verbose = FALSE, tuneGrid = tGridGBM)

model.GBM.brand.conc


## Test GBM
predictions.GBM <- predict(model.GBM.brand.conc, testing)
testing$predictions.GBM <- predictions.GBM

#Confusion matrix
confMatrixGBM <- confusionMatrix(testing$brand, testing$predictions.GBM)
confMatrixGBM
metrics.GBM <- postResample(pred = testing$predictions.GBM, obs = testing$brand)
metrics.GBM
fourfoldplot(confMatrixGBM$table, conf.level = 0, margin = 2, main = "Confusion Matrix GBM")


##Convert predictions to numerics to allow calculations
testing$brand <- testing$brand %>% as.numeric()
testing$brand <- testing$brand -1

testing$predictions.KNN <- testing$predictions.KNN %>% as.numeric()
testing$predictions.KNN <- testing$predictions.KNN - 1

testing$predictions.RF <- testing$predictions.RF %>% as.numeric()
testing$predictions.RF <- testing$predictions.RF - 1

testing$predictions.GBM <- testing$predictions.GBM %>% as.numeric()
testing$predictions.GBM <- testing$predictions.GBM - 1

#AUC

#AUC KNN
pred.KNN <- prediction(testing$predictions.KNN,testing$brand)
auc.perf.KNN <- performance(pred.KNN, measure = "auc")
auc.perf.KNN@y.values[[1]]


#AUC RF
pred.RF <- prediction(testing$predictions.RF,testing$brand)
auc.perf.RF <- performance(pred.RF, measure = "auc")
auc.perf.RF@y.values[[1]]

#ROC GBM
pred.GBM <- prediction(testing$predictions.GBM,testing$brand)
auc.perf.GBM <- performance(pred.GBM, measure = "auc")
auc.perf.GBM@y.values[[1]]

#create results table
mNam <- c("KNN", "RF", "GBM")
Acc <- c(metrics.KNN[1], metrics.RF[1], metrics.GBM[1])
Kap <- c(metrics.KNN[2], metrics.RF[2], metrics.GBM[2])
Sens <- c(confMatrixKNN$byClass[1], confMatrixRF$byClass[1], confMatrixGBM$byClass[1])
Spec <- c(confMatrixKNN$byClass[2], confMatrixRF$byClass[2], confMatrixGBM$byClass[2])
auc <- c(auc.perf.KNN@y.values[[1]], auc.perf.RF@y.values[[1]], auc.perf.GBM@y.values[[1]])
modelResults <- data.frame(mNam,Acc,Kap,Sens,Spec,auc)
colnames(modelResults) <- c("Model", "Accuracy", "Kappa", "Sensitivity", "Specificity","AUC")
modelResults

#Resamples
reSamps <- resamples(list(GBM = model.GBM.brand.conc,
                          RF = model.RF.brand.tune,
                          KNN = finalKNN))

bwplot(reSamps)

##Turn off parallel processing
stopCluster(cluster)
registerDoSEQ()

##Explore Model Insights
summary(model.GBM.brand.conc)

testing$predictions.GBM <- testing$predictions.GBM %>% as.factor()
g13 <- ggplot(testing, aes(salary, age, colour = predictions.GBM)) +
        geom_point(show.legend = FALSE) +
        facet_grid(predictions.GBM ~ .) +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Salary") + 
        ylab("Age") + 
        ggtitle("Salary v Age by GBM prediction - test set")
g13

grid.arrange(g12,g13)

##Predicting Brand Preference
#Update data types
incomplete$elevel <- incomplete$elevel %>% as.factor()
incomplete$car <- incomplete$car %>% as.factor()
incomplete$zipcode <- incomplete$zipcode %>% as.factor()
incomplete$brand <- incomplete$brand %>% as.factor()

#make predictions
predictions.incomplete <- predict(model.GBM.brand.conc, incomplete)
incomplete$brand <- predictions.incomplete

g14 <- ggplot(incomplete, aes(salary, age, colour = brand)) +
        geom_point(show.legend = FALSE) +
        facet_grid(brand ~ .) +
        theme_bw() +
        scale_color_brewer(palette="Dark2") +
        xlab("Salary") + 
        ylab("Age") + 
        ggtitle("Salary v Age by Predicted Brand")
g14
