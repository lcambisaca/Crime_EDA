library(softImpute)

# Read the dataset, treating "Unknown" as NA
Crime <- read.csv('Crime_DataSet.csv', header = TRUE, na.strings = "Unknown", stringsAsFactors = TRUE)
set.seed(1)
keep_cols1 <- c("Crime.Solved", "State",  "Victim. Sex",
                "Victim.Ethnicity" , "Year")  #Try to find which state is the most dangerous for minorities

Crime1 <- Crime[ , keep_cols1]
Crime1 <- Crime1[!is.na(Crime1$Crime.Solved), ] #Removes all na from specified col
Crime1 <- Crime1[!is.na(Crime1$Year), ] #Removes all na from specified col


# Now I need to prepare data for matrix completion where we fix missing data found in the data set :( painn)
# Cleaning Data

Crime1$Crime.Solved <- as.numeric(Crime1$Crime.Solved == "Yes")
#1 solved 0 not solved

Crime1$Victim.Sex <- as.numeric(Crime1$Victim.Sex  == "Male")
#1 solved 0 not solved

#1 male 0 female
#1 Hispanic 0 not hispanic
# State From Alaska down in alpha order


Crime1$Victim.Ethnicity <- as.numeric(Crime1$Victim.Ethnicity == "Hispanic")

Crime1$State <- as.numeric(Crime1$State) -1
# State From Alaska down in alpha order

Crime_matrix <- as.matrix(Crime1)


fit <- softImpute(Crime_matrix, rank.max = 4, lambda = 10)
Crime_completed <- complete(Crime_matrix, fit)


Crime_imputed <- as.data.frame(Crime_completed)

rm(Crime_completed,fit,Crime_matrix,Crime,Crime1)

Crime_imputed$Victim.Sex <- round(Crime_imputed$Victim.Sex)

Crime_imputed$Victim.Ethnicity <- round(Crime_imputed$Victim.Ethnicity)

# Data is Fixed
library(caTools)
Crime_imputed$Crime.Solved <- as.factor(Crime_imputed$Crime.Solved)

split = sample.split(Crime_imputed$Crime.Solved, SplitRatio = 0.75)
training_set = subset(Crime_imputed, split == TRUE)
test_set = subset(Crime_imputed, split == FALSE)

#Logistic Regression

glm.fits <- glm(Crime.Solved ~ ., data = training_set, family = binomial)
summary(glm.fits)
prob_pred = predict(glm.fits, type = 'response', newdata = test_set)
y_pred = ifelse(prob_pred > 0.6, 1, 0)
cm1 = table(Predicted = y_pred, Actual = test_set$Crime.Solved)
accuracy1 <- mean(y_pred == test_set$Crime.Solved)
cm1
accuracy1

#Naive Bayes
library(e1071)
nb_model <- naiveBayes(Crime.Solved ~ ., data = training_set)
nb_pred <- predict(nb_model, newdata = test_set)
cm_nb <- table(Predicted = nb_pred, Actual = test_set$Crime.Solved)
nb_accuracy <- mean(nb_pred == test_set$Crime.Solved)
cm_nb
nb_accuracy

summary(nb_model)

# Classifier Tree
library(tree)
training_set$Crime.Solved <- as.factor(training_set$Crime.Solved)
test_set$Crime.Solved <- as.factor(test_set$Crime.Solved)

tree.solved <-tree(Crime.Solved ~ ., data = training_set)
tree.solved <- tree(Crime.Solved ~ ., Crime_imputed, 
                    control = tree.control(nobs = nrow(Crime_imputed), mindev = 0.001))

tree.pred <- predict(tree.solved, newdata = test_set, type = "class")

cm2 <- table(test_set$Crime.Solved, tree.pred)
accuracy2 <- mean(tree.pred == test_set$Crime.Solved)
cm2
accuracy2
plot(tree.solved)
text(tree.solved , pretty = 0)
summary(tree.solved)
# Cry maybe boosting

#Bagging
library(randomForest)
bag.solved <-randomForest(
  Crime.Solved ~ ., 
  data = training_set,
  ntree = 50,
  mtry = 4 , 
  importance = TRUE)

bag.solved

yhat.bag <- predict(bag.solved, newdata = test_set , type = "class")
summary(yhat.bag)
cm3 <- table(test_set$Crime.Solved, yhat.bag)
accuracy3 <- mean(yhat.bag == test_set$Crime.Solved)
accuracy3
cm3

# Random Forest

rf.solved <-randomForest(
  Crime.Solved ~ ., 
  data = training_set,
  ntree = 100,
  mtry = 2 , 
  importance = TRUE)

yhat.rf <- predict(bag.solved, newdata = test_set , type = "class")
summary(yhat.rf)
cm4 <- table(test_set$Crime.Solved, yhat.rf)
accuracy4 <- mean(yhat.rf == test_set$Crime.Solved)
accuracy4
cm4
#Boosting
library(gbm)
training_set$Crime.Solved <- as.numeric(training_set$Crime.Solved) -1
test_set$Crime.Solved <- as.numeric(test_set$Crime.Solved) -1
# Train the Gradient Boosting Model
boost.solved <- gbm(Crime.Solved ~ ., 
                    data = training_set,
                    distribution = "bernoulli",  # Binary classification
                    n.trees = 50,                 # Number of trees (can increase later)
                    interaction.depth = 4,        # Tree depth
                    shrinkage = 0.1,              # Learning rate (default)
                    cv.folds = 5,                 # Optional: add cross-validation
                    verbose = FALSE)
summary(boost.solved)
best_iter <- gbm.perf(boost.solved, method = "cv")
pred_prob <- predict(boost.solved, newdata = test_set, n.trees = best_iter, type = "response")
y_pred <- ifelse(pred_prob > 0.6, 1, 0)

cm5 <- table(Predicted = y_pred, Actual = test_set$Crime.Solved)
accuracy5 <- mean(y_pred == test_set$Crime.Solved)

print(cm5)
print(accuracy5)
