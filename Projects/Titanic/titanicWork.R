# TITANIC STATS PROJECT

# Set working directory
setwd("T:/Data")

# Import training data
train <- read.csv("T:/Data/train.csv")
View(train)

# Import test data
test <- read.csv("T:/Data/test.csv")
View(test)

# View structure of different data sets
str(train)
str(test)

# Convert Survival and Pclass data from Integer to Factor for Train
train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)

# Rename Surival factors
levels(train$Survived) <- c(0,1)

# Convert names into Character format from Factor for Train
train$Name <- as.character(train$Name)

# Convert Pclass data from Integer to Factor for Train
test$Pclass <- as.factor(test$Pclass)

# Convert names into Character format from Factor for Train
test$Name <- as.character(test$Name)

# Check for duplicated data
sum(duplicated(train))

# Build a table for survival statistics
table(train$Survived)

# Load ggplot2
library(ggplot2)
ggplot(train, aes(x = Sex, fill = Survived)) + geom_bar() + xlab("Sex") + ylab("Total Count")

# Add new column called "Model Prediction"
train$ModelPrediction <- 0

# Calculate accuracy of Prediction based on assigning 0 to every result
sum(train$Survived==train$ModelPrediction)/nrow(train)

# Assign all females a survival rate
train$ModelPrediction[train$Sex=="female"] <- 1

# Calculate accuracy of Prediction based on assigning 1 to every Female (i.e. assuming all females survive)
sum(train$Survived==train$ModelPrediction)/nrow(train)

# Create a table that looks at survival and travel class
prop.table(table(train$Survived,train$Pclass))

# Create a chart to represent survival rate vs class
ggplot(train, aes(x=Sex, fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)

# Reset Model Prediction column to 0
train$ModelPrediction <- 0

# Set all females in Pclass 1 and 2, but not 3, to 1
train$ModelPrediction[(train$Sex=="female")&((train$Pclass)=="1"|(train$Pclass=="2"))] <- 1

# Check the accuracy of this model
sum(train$Survived==train$ModelPrediction)/nrow(train)

# PClass, Age, Sex, Survival Chart
ggplot(train, aes(Age, fill = Survived))+
  geom_bar(width=10)+
  facet_wrap(~Sex + ~Pclass)

# Reset prediction to 0
train$ModelPrediction <- 0

# Assign all females a value of 1 except for those in class 3
train$ModelPrediction[train$Sex=="female"&(train$Pclass=="1"|train$Pclass=="2")] <- 1

# Assign males in first and second class who are under 18 value 1
train$ModelPrediction[(train$Sex=="male")&((train$Pclass=="1")|(train$Pclass=="2"))&(train$Age<18)] <- 1

# Check accuracy of the model 
sum(train$ModelPrediction==train$Survived)/nrow(train)
