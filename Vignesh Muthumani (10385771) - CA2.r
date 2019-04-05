#----------------------------------------------------------------------------#
####################### Probability Modelling Of Data ########################
#----------------------------------------------------------------------------#
################ Assignment 2: Vignesh Muthumani (10385771) ##################
#----------------------------------------------------------------------------#


# Q1 ------------------------------------------------------------------------#


getwd()

# Pasted the dataset provided in the working directory and coverted the file
# to csv format

data1 <- read.csv(file.choose(), header = T)
View(data1)

male <- data1[data1$ï..gender == 1,]
female <- data1[data1$ï..gender == 0,]

# Step 1: setting hypothesis: [H0: P0 = 0.5] ; [H1: P0 != 0.5]

# Step 2: setting alpha = 0.05

# step 3: testing the proportion:

x <- nrow(male)
n <- nrow(data1)
p <- p0 <- 0.5

prop.test(x, n, p, correct = F)

# Decision: Since (p-value = 0.8474) > (alpha = 0.05), we accept the null 
# hypothesis, H0 (i.e., prop. of males is equal to prop. of females)

# [optional method - mathematically derived]: 

# Step 3: finding t-value:
# t-value = (phat-p0) / (sqrt(p0*(1-p0)/n))

phat <- nrow(male)/nrow(data1)

p0 <- 0.5 # since we assume that prop. of male = prop. of female

nr <- phat - p0

dr <- sqrt((p0*(1-p0))/nrow(data1))

tvalue <- nr/dr
tvalue # t-value = 0.192

# Step 4: finding c-value
# cvalue: z(alpha/2) = z(0.025) = -1.96   # calculated from z-table

# Step 5: Decision making
# Since tvalue < |cvalue|, we accept null hypothises, H0
# Based on this testing, we conclude that proportion of males is equal to the
# proportion of females


# Q2 ------------------------------------------------------------------------#


data2 <- read.csv(url('https://stats.idre.ucla.edu/stat/data/binary.csv'))
View(data2)

library(caTools)

# a. training 80% of data:
# splitting the dataset (80% of train data and 20% of test data)
split <- sample.split(data2[,1], SplitRatio = 0.8)
train <- subset(data2, split == T)  # training dataset
test <- subset(data2, split == F)   # test dataset

nrow(train)
nrow(test)

# modelling the train dataset
train.model <- glm(admit ~ ., data = train, family = "binomial")
summary(train.model)

# b. At the level of alpha = 0.05, we can see that all the variables 
#    are significant

# c. since all the variables are significant, we are proposing the above-model
# as the optimal one.

# d. predicting the test dataset using trained model
pred <- predict(train.model, test, type = "response")
pred

nrow(test) # =80

actual.values <- test[,1]

pred.values <- rep(0,80)

# probability of admit being 1, if pred < 0.5, then admit is 0
pred.values[pred >= 0.5] <- 1

# e. creating confusion matrix
table(pred.values, actual.values)

# f. finding the probability of correctness of the prediction
mean(pred.values == actual.values)

# We can see that there is a 70% accuracy

##############################################################################

