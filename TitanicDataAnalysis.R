# Load raw data
train <- read.csv("data/train.csv", header = TRUE)
test <- read.csv("data/test.csv", header = TRUE)

# Add a "Survived" variable to the test set to allow for combining data sets
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])

# Swap first two columns to match ordering in 'train' dataframe
test.survived <- test.survived[c(2, 1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)]

# Combine data sets
data.combined <- rbind(train, test.survived)

# A bit about R data types (e.g. factors)
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

# Take a look at gross survival rates
table(data.combined$Survived)

# Distribution across classes
table(data.combined$Pclass)

# Load up ggplot2 package to use for visualizations
library(ggplot2)

# Hypothesis - Rich folks survived at a higher rate
train$Pclass <- as.factor(train$Pclass)
ggplot(train, aes(x = Pclass, fill = factor(Survived))) + 
  geom_bar(width = 0.5) +
  xlab("Pclass") + 
  ylab("Total Count") + 
  labs(fill = "Survived")

# Examine the first few names in the training data set
head(as.character(train$Name))

# How many unique names are there across both train & test?
length(unique(as.character(data.combined$Name)))

# Two duplicate names, take a closer look
# First, get the duplicate names and store them as a vector
dup.names <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

# Next, take a look at the records in the combined data set
data.combined[which(data.combined$Name %in% dup.names),]

# What is up with the 'Miss.' and 'Mr.' thing?
library(stringr)

# Any correlation with other variables (e.g. SibSp)?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:5,]

# Hypothesis - Name titles correlate with Age
mrses <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(train$Sex == "male"),]
males[1:5,]

# Expand upon the relationships between 'Survived' and 'Pclass' by adding the new 'Title' 
# variable to the data set and then explore a potential 3D relationship
extractTitle <- function(name) {
  name <- as.character(name)
  
  if (length(grep("Miss.", name)) > 0) {
    return ("Miss.")
  }
  else if (length(grep("Master.", name)) > 0) {
    return ("Master.")
  }
  else if (length(grep("Mrs.", name)) > 0) {
    return ("Mrs.")
  }
  else if (length(grep("Mr.", name)) > 0) {
    return ("Mr.")
  }
  else {
    return ("Other")
  }
}

titles <- NULL

for (i in 1:nrow(data.combined)) {
  titles <- c(titles, extractTitle(data.combined[i, "Name"]))
}

data.combined$Title <- as.factor(titles)

# Since we only have survived lables for the train set, only use the
# first 891 rows
ggplot(data.combined[1:891,], aes(x = Title, fill = Survived)) +
  stat_count(width = 0.5) +
  facet_wrap(~Pclass) + 
  ggtitle("Pclass") +
  xlab("Title") +
  ylab("Total Count") +
  labs(fill = "Survived")

# OK, Age and Sex seem pretty important as derived from analysis of Title, let's take a
# look at the distributions of Age over entire data set
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])

# Just to be thorough, take a look at survival rates broken out by Sex, Pclass and Age
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
  facet_wrap(~Sex + Pclass) +
  geom_histogram(binwidth = 10) +
  xlab("Age") +
  ylab("Total Count")

# Validate that 'Master.' is a good proxy for male children
boys <- data.combined[which(data.combined$Title == "Master."),]
summary(boys$Age)

# We know that "Miss." is more complicated, let's examine further
misses <- data.combined[which(data.combined$Title == "Miss."),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
  facet_wrap(~Pclass) +
  geom_histogram(binwidth = 5) +
  ggtitle("Age for 'Miss.' by Pclass") +
  xlab("Age") +
  ylab("Total Count")

# OK, appears female children may have different survival rate,
# could be a candidate for feature engineering later
misses.alone <- misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))

# Move on to the SibSp variable, summarize the variable
summary(data.combined$SibSp)

# Can we treat as a factor?
length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

# We belive Title is predictive. Visualize survival rates by SibSp, Pclass and Title
ggplot(data.combined[1:891,], aes(x = SibSp, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("SibSp") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

# Treat the Parch variable as a factor and visualize
data.combined$Parch <- as.factor(data.combined$Parch)
ggplot(data.combined[1:891,], aes(x = Parch, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("ParCh") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

# Let's try some feature engineering. What about creating a Family size feature?
temp.sibsp <- c(train$SibSp, test$SibSp)
temp.parch <- c(train$Parch, test$Parch)
data.combined$Family.size <- as.factor(temp.sibsp + temp.parch + 1)

# Visualize it to see if it is predictive
ggplot(data.combined[1:891,], aes(x = Family.size, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Family.size") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

# Take a look at the Ticket variable
str(data.combined$Ticket)

# Based on the huge number of levels, Ticket really isn't a factor variable, it is a string.
# Convert it and display first 20
data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

# There's no immediately apparent structure in the data, let's see if we can find some.
# We'll start with taking a look at just the first char for each
ticket.first.char <- ifelse(data.combined$Ticket == "", " ", substr(data.combined$Ticket, 1, 1))
unique(ticket.first.char)

# OK, we can make a factor for analysis purposes and visualize
data.combined$Ticket.first.char <- as.factor(ticket.first.char)

# First, a high-level plot of data
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar(width = 1) +
  ggtitle("Survivability by Ticket.first.char") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 350) +
  labs(fill = "Survived")

# Ticket seems like it might be predictive, drill down a bit
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass) +
  ggtitle("Pclass") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 150) +
  labs(fill = "Survived")

# Lastly, see if we get a pattern when using combination of Pclass & Title
ggplot(data.combined[1:891,], aes(x = Ticket.first.char, fill = Survived)) +
  geom_bar(width = 1) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Ticket.first.char") +
  ylab("Total Count") +
  ylim(0, 200) +
  labs(fill = "Survived")

# Next up - the fares Titanic passengers paid
summary(data.combined$Fare)
length(unique(data.combined$Fare))

# Can't make Fare a factor, treat as numeric & visualize with histogram
ggplot(data.combined, aes(x = Fare)) +
  geom_histogram(binwidth = 5) +
  ggtitle("Combined Fare Distribution") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0, 200) 

# Let's check to see if Fare has predictive power
ggplot(data.combined[1:891,], aes(x = Fare, fill = Survived)) +
  geom_histogram(binwidth = 5) +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Fare") +
  ylab("Total Count") +
  ylim(0, 50) +
  labs(fill = "Survived")

# Analysis of the Cabin variable
str(data.combined$Cabin)

# Cabin really isn't a factor, make a string and then display first 100
data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with a "U"
data.combined[which(data.combined$Cabin == ""), "Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take a look at just the first char as a factor
cabin.first.char <- as.factor(substr(data.combined$Cabin, 1, 1))
str(cabin.first.char)
levels(cabin.first.char)

# Add to combined data set and plot
data.combined$Cabin.first.char <- cabin.first.char

# High level plot
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0, 750) +
  labs(fill = "Survived")

# Could have some predictive power, drill in
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass) +
  ggtitle("Survivability by Cabin.first.char") +
  xlab("Pclass") +
  ylab("Total Count") +
  ylim(0, 500) +
  labs(fill = "Survived")

# Does this feature improve upon Pclass + Title?
ggplot(data.combined[1:891,], aes(x = Cabin.first.char, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.first.char") +
  ylab("Total Count") +
  ylim(0, 500) +
  labs(fill = "Survived")

# What about folks with multiple cabins?
data.combined$Cabin.multiple <- as.factor(ifelse(str_detect(data.combined$Cabin, " "), "Y", "N"))

ggplot(data.combined[1:891,], aes(x = Cabin.multiple, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Cabin.multiple") +
  ylab("Total Count") +
  ylim(0, 350) +
  labs(fill = "Survived")

# Does survivability depend on where you got onboard the Titanic?
str(data.combined$Embarked)
levels(data.combined$Embarked)

# Plot data for analysis
ggplot(data.combined[1:891,], aes(x = Embarked, fill = Survived)) +
  geom_bar() +
  facet_wrap(~Pclass + Title) +
  ggtitle("Pclass, Title") +
  xlab("Embarked") +
  ylab("Total Count") +
  ylim(0, 300) +
  labs(fill = "Survived")

# ======================
# Exploratory Modeling 1
# ======================

library(randomForest)

# Train a Random Forest with the default parameters using Pclass & Title
rf.train.1 <- data.combined[1:891, c("Pclass", "Title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x = rf.train.1, y = rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)

# Train a Random Forest using Pclass, Title & SibSp
rf.train.2 <- data.combined[1:891, c("Pclass", "Title", "SibSp")]

set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)

# Train a Random Forest using Pclass, Title & Parch
rf.train.3 <- data.combined[1:891, c("Pclass", "Title", "Parch")]

set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

# Train a Random Forest using Pclass, Title, SibSp & Parch
rf.train.4 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Parch")]

set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)

# Train a Random Forest using Pclass, Title & Family.size
rf.train.5 <- data.combined[1:891, c("Pclass", "Title", "Family.size")]

set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

# Train a Random Forest using Pclass, Title, SibSp & Family.size
rf.train.6 <- data.combined[1:891, c("Pclass", "Title", "SibSp", "Family.size")]

set.seed(1234)
rf.6 <- randomForest(x = rf.train.6, y = rf.label, importance = TRUE, ntree = 1000)
rf.6
varImpPlot(rf.6)

# Train a Random Forest using Pclass, Title, Parch & Family.size
rf.train.7 <- data.combined[1:891, c("Pclass", "Title", "Parch", "Family.size")]

set.seed(1234)
rf.7 <- randomForest(x = rf.train.7, y = rf.label, importance = TRUE, ntree = 1000)
rf.7
varImpPlot(rf.7)

# ================
# Cross Validation
# ================

# Subset our test records and features
test.submit.df <- data.combined[892:1309, c("Pclass", "Title", "Family.size")]

# Make predictions 
rf.5.preds <- predict(rf.5, test.submit.df)
table(rf.5.preds)

# Write out a CSV file for submission to Kaggle
submit.df <- data.frame(PassengerId = rep(892:1309), Survived = rf.5.preds)

write.csv(submit.df, file = "RF_SUB_20180421_1.csv", row.names = FALSE)

# First try submission scores 0.79425
# Let's look into cross-validation using the caret package to see if we can get
# more accurate estimates
library(caret)
library(doSNOW)

# Leverage caret to create 100 total folds, but ensure that the ratio of those
# that survived and perished in each fold matches the overall training set. This
# is known as stratified cross validation and generally provides better results.
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k = 10, times = 10)

# Check stratification
table(rf.label)
table(rf.label[cv.10.folds[[33]]])

# Set up caret's trainControl object per above.
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.1)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.1

# The above is only slightly more pessimistic than the rf.5 OOB prediction, but
# not pessimistic enough. Let's try 5-fold CV repeated 10 times.
set.seed(5983)
cv.5.folds <- createMultiFolds(rf.label, k = 5, times = 10)

# Set up caret's trainControl object per above.
ctrl.2 <- trainControl(method = "repeatedcv", number = 5, repeats = 10, index = cv.5.folds)

# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(89472)
rf.5.cv.2 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.2)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.2

# 5-fold CV isn't better. Move to 3-fold CV repeated 10 times.
set.seed(37596)
cv.3.folds <- createMultiFolds(rf.label, k = 3, times = 10)

# Set up caret's trainControl object per above.
ctrl.3 <- trainControl(method = "repeatedcv", number = 3, repeats = 10, index = cv.3.folds)

# Set up doSNOW package for multi-core training. This is helpful as we're going
# to be training a lot of trees.
cl <- makeCluster(6, type = "SOCK")
registerDoSNOW(cl)

# Set seed for reproducibility and train
set.seed(94622)
rf.5.cv.3 <- train(x = rf.train.5, y = rf.label, method = "rf", tuneLength = 3,
                   ntree = 1000, trControl = ctrl.3)

# Shutdown cluster
stopCluster(cl)

# Check out results
rf.5.cv.3

# ======================
# Exploratory Modeling 2
# ======================

# Let's use a single decision tree to better understand what's going on with our
# features. Obviously Random Forest are far more powerful than single trees,
# but single trees have the advantage of being easier to understand.

# Load packages
library(rpart)
library(rpart.plot)

# Create utility function
rpart.cv <- function(seed, training, labels, ctrl) {
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  
  # Levarage formula interface for training
  rpart.cv <- train(x = training, y = labels, method = "rpart", tuneLength = 30, trControl = ctrl)
  
  # Shutdown cluster
  stopCluster(cl)
  
  return (rpart.cv)
}

# Grab features 
features <- c("Pclass", "Title", "Family.size")
rpart.train.1 <- data.combined[1:891, features]

# Run CV and check out results
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)
rpart.1.cv.1

# Plot
prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# The plot bring out some interesting lines of investigation. Namely:
#     1 - Titles of "Mr." and "Other" are predicted to perish at an
#         overall accuracy rate of 83.2%.
#     2 - Titles of "Master.", "Miss." & "Mrs." in 1st & 2nd class
#         are predicted to survive at an overall accuracy rate of 94.9%.
#     3 - Titles of "Master.", "Miss." & "Mrs." in 3rd class with
#         family sizes equal to 5, 6, 8 & 11 are predicted to perish
#         with 100% accuracy.
#     4 - Titles of "Master.", "Miss." & "Mrs." in 3rd class with 
#         family sizes not equal to 5, 6, 8 & 11 are predicted to
#         survive with 59.6% accuracy.

# Both rpart and rf confirm that title is important, let's investigate further
table(data.combined$Title)

# Parse out last Name and Title
data.combined[1:25, "Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits, "[", 1)
last.names[1:10]

# Add last names to dataframe in case we find it useful later
data.combined$Last.name <- last.names

# Now for titles
name.splits <- str_split(sapply(name.splits, "[", 2), " ")
titles <- sapply(name.splits, "[", 2)
unique(titles)

# What's up with a Title of "the"?
data.combined[which(titles == "the"),]

# Re-map titles to be more exact
titles[titles %in% c("Dona.", "the")] <- "Lady."
titles[titles %in% c("Ms.", "Mlle")] <- "Miss."
titles[titles == "Mme."] <- "Mrs."
titles[titles %in% c("Jonkheer.", "Don.")] <- "Sir."
titles[titles %in% c("Col.", "Capt.", "Major.")] <- "Officer"
table(titles)
 
# Make Title a factor
data.combined$New.title <- as.factor(titles)

# Visualize new version of Title
ggplot(data.combined[1:891,], aes(x = New.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for New.title by Pclass")

# Collapse titles based on visual analysis
indexes <- which(data.combined$New.title == "Lady.")
data.combined$New.title[indexes] <- "Mrs."

indexes <- which(data.combined$New.title == "Dr." |
                 data.combined$New.title == "Rev." |
                 data.combined$New.title == "Sir." |
                 data.combined$New.title == "Officer")
data.combined$New.title[indexes] <- "Mr."

# Visualize
ggplot(data.combined[1:891,], aes(x = New.title, fill = Survived)) +
  geom_bar() +
  facet_wrap(~ Pclass) +
  ggtitle("Survival Rates for Collapsed New.title by Pclass")

# Grab features
features <- c("Pclass", "New.title", "Family.size")
rpart.train.2 <- data.combined[1:891, features]

# Run CV and check out results
rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)
rpart.2.cv.1

# Plot
prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# Dive in on 1st class "Mr."
indexes.first.mr <- which(data.combined$New.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# One female?
first.mr.df[first.mr.df$Sex == "female",]

# Update New.title feature 
indexes <- which(data.combined$New.title == "Mr." &
                 data.combined$Sex == "female")
data.combined$New.title[indexes] <- "Mrs."

# Any other gender slip-ups?
length(which(data.combined$Sex == "female" &
            (data.combined$New.title == "Master." |
             data.combined$New.title == "Mr.")))

# Refresh data frame
indexes.first.mr <- which(data.combined$New.title == "Mr." & data.combined$Pclass == "1")
first.mr.df <- data.combined[indexes.first.mr, ]

# Let's look at surviving 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived == "1",])
View(first.mr.df[first.mr.df$Survived == "1",])

# Take a look at some of the high fares
indexes <- which(data.combined$Ticket == "PC 17755" |
                 data.combined$Ticket == "PC 17611" |
                 data.combined$Ticket == "113760")
View(data.combined[indexes,])

# Visualize survival rates for 1st class "Mr." by fare
ggplot(first.mr.df, aes(x = Fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("1st Class 'Mr.' Survival Rates by Fare")

# Engineer features based on all the passengers with the same Ticket
ticket.party.size <- rep(0, nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined))
tickets <- unique(data.combined$Ticket)

for (i in 1:length(tickets)) {
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket == current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"] / length(party.indexes)
  
  for (k in 1:length(party.indexes)) {
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
}

data.combined$Ticket.party.size <- ticket.party.size
data.combined$Avg.fare <- avg.fare

# Refresh 1st class "Mr." dataframe
first.mr.df <- data.combined[indexes.first.mr, ]
summary(first.mr.df)

# Visualize new features
ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = Ticket.party.size, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by Ticket.party.size")

ggplot(first.mr.df[first.mr.df$Survived != "None",], aes(x = Avg.fare, fill = Survived)) +
  geom_density(alpha = 0.5) +
  ggtitle("Survival Rates 1st Class 'Mr.' by Avg.fare")

# Hypothesis - Ticket.party.size is highly correlated with Avg.fare
summary(data.combined$Avg.fare)

# One missing value, take a look
data.combined[is.na(data.combined$Avg.fare), ]

# Get records for similar passengers and summarize Avg.fares
indexes <- with(data.combined, which(Pclass == "3" & Title == "Mr." & Family.size == 1 & Ticket != "3701"))
similar.na.passengers <- data.combined[indexes,]
summary(similar.na.passengers$Avg.fare)

# Use median since close to mean and a little higher than mean
data.combined[is.na(avg.fare), "Avg.fare"] <- 7.840

# Leverage caret's preProcess function to normalize data
preproc.data.combined <- data.combined[, c("Ticket.party.size", "Avg.fare")]
preProc <- preProcess(preproc.data.combined, method = c("center", "scale"))

postproc.data.combined <- predict(preProc, preproc.data.combined)

# Hypothesis refuted for all data
cor(postproc.data.combined$Ticket.party.size, postproc.data.combined$Avg.fare)

# How about for just 1st class all-up?
indexes <- which(data.combined$Pclass == "1")
cor(postproc.data.combined$Ticket.party.size[indexes], postproc.data.combined$Avg.fare[indexes])
# Hypothesis refuted again

# OK, let's see if our feature engineering has made any difference
features <- c("Pclass", "New.title", "Family.size", "Ticket.party.size", "Avg.fare")
rpart.train.3 <- data.combined[1:891, features]

# Run CV and check out results 
rpart.3.cv.1 <- rpart.cv(94622, rpart.train.3, rf.label, ctrl.3)
rpart.3.cv.1

# Plot
prp(rpart.3.cv.1$finalModel, type = 0, extra = 1, under = TRUE)








