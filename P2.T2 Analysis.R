library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(corrplot)
library(caret)
library(tidyr)

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

##datasets are sufficiently similar. Will take the strategy of training, test and validation sets. 
##Training and testing from the complete responses. Validation from the incomplete responses.
##Remaining EDA on complete data but will need to perform same transformations on validation data prior to modelling this.
##need to combine sets at end!!!

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

#EDA



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
        geom_histogram(bins = 10, fill = "#D95F02", colour = "white") + 
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

##Review correlation matrix
corrMatrix <- origdata %>% cor()
corrMatrix %>% corrplot.mixed()

#near zero variance
nzv <- data %>% nearZeroVar(saveMetrics = TRUE)
nzv


##Preliminary Hypothesis!!!!


