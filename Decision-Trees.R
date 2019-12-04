## Load Libraries
library(tree) # To fit trees
library(randomForest) # For random forests and bagging
library(ISLR)

## Load Cleaned Dataset
data <- read.csv(file = "/Users/ivysandberg/Desktop/SML_PROJECT/Clean_CHSIdata.csv")

## Data Cleaning
data$Death_Rate = data$Total_Deaths/data$Population_Size * 100

data$Uninsured_Percent = data$Uninsured/data$Population_Size * 100
data$NoHS_Percent = data$No_HS_Diploma/data$Population_Size * 100
data$Depression_Rate = data$Major_Depression/data$Population_Size * 100
data$Drug_Rate = data$Recent_Drug_Use/data$Population_Size * 100
data$Older_Population = data$Age_65_84 + data$Age_85_and_Over

data <- data[data$Unemployed > 0,]
data$Unemployment_Rate = data$Unemployed/data$Population_Size * 100

data$Doctors = ifelse(data$Prim_Care_Phys_Rate > median(data$Prim_Care_Phys_Rate), 1, 0)

## Create Response Variable
data$Health = ifelse(data$Health_Status > median(data$Health_Status), "Poor", "Good") 
data$Health <- as.factor(data$Health)

# Data Visualizations
hist(data$Health_Status)
summary(data$Health_Status)
hist(data$Prim_Care_Phys_Rate)

par(mfrow=c(1,2))
boxplot(data$Uninsured~data$Health)
boxplot(data$Uninsured_Percent~data$Health)

par(mfrow=c(1,2))
plot(data$Uninsured, data$Death_Rate)
plot(data$Uninsured_Percent, data$Death_Rate)

par(mfrow=c(1,1))



#### Regression Tree
## Split data
set.seed(5)
sample.data<-sample.int(nrow(data), floor(.50*nrow(data)), replace = F) 
train<-data[sample.data, ]
test<-data[-sample.data, ]

## store the response variable for test data
pred.test <- test[,"Death_Rate"]

## Recursive Binary Splitting
tree.class.train <- tree(Death_Rate ~ Poverty + Health_Status + Suicide + 
                           Uninsured_Percent + Dentist_Rate + Unemployment_Rate + NoHS_Percent + 
                           Depression_Rate + Drug_Rate + Diabetes + Smoker + Obesity + Older_Population +  
                           Prim_Care_Phys_Rate, data = train)
summary(tree.class.train)

plot(tree.class.train) 
text(tree.class.train, cex=0.75)

tree.pred.test<-predict(tree.class.train, newdata=test)
mean((tree.pred.test - pred.test)^2)

## Pruning
set.seed(5)
cv.class<-cv.tree(tree.class.train, K=10) 
trees.num<-cv.class$size[which.min(cv.class$dev)] 
trees.num

prune.full<-prune.tree(tree.class.train, best=trees.num) 
prune.full

plot(prune.full)
text(prune.full, cex=0.75)

prune.pred.test<-predict(prune.full, newdata=test)
mean((prune.pred.test - pred.test)^2)

## Bagging
library(randomForest)

set.seed(5)
bag.tree <- randomForest(Death_Rate ~ Poverty + Health_Status + Suicide + 
                           Uninsured_Percent + Dentist_Rate + Unemployment_Rate + NoHS_Percent + 
                           Depression_Rate + Drug_Rate + Diabetes + Smoker + Obesity + Older_Population +  
                           Prim_Care_Phys_Rate, data = train, mtry=14, importance=TRUE)
bag.tree
importance(bag.tree) 
varImpPlot(bag.tree)

pred.bag<-predict(bag.tree, newdata=test) 
mean((pred.bag - pred.test)^2)

## Random Forests
set.seed(5)
rf.tree <- randomForest(Death_Rate ~ Poverty + Health_Status + Suicide + 
                          Uninsured_Percent + Dentist_Rate + Unemployment_Rate + NoHS_Percent + 
                          Depression_Rate + Drug_Rate + Diabetes + Smoker + Obesity + Older_Population +  
                          Prim_Care_Phys_Rate, data = train, mtry=4, importance=TRUE)

rf.tree
importance(rf.tree) 
varImpPlot(rf.tree)

pred.rf<-predict(rf.tree, newdata=test) 
mean((pred.rf - pred.test)^2)



#### Classification Tree
# Split Data (same way it was split for the Classification Milestone)
set.seed(1)
sample.data <- sample.int(nrow(data), floor(.80*nrow(data)), replace = F)
train <- data[sample.data, ]
test <- data[-sample.data, ]


# Run first tree for Classification
class_tree <- tree(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + 
                     NoHS_Percent + Smoker + Drug_Rate + Obesity + Diabetes + 
                     Doctors, data=train)
summary(class_tree)
plot(class_tree)
text(class_tree, cex=0.65, pretty = 0)
# 16 terminal nodes

# Use Cross Validation to find the optimal tree size
set.seed(1)
cv_class_tree <- cv.tree(class_tree, K=10, FUN = prune.misclass)
cv_class_tree

optimal_size <- cv_class_tree$size[which.min(cv_class_tree$dev)]
optimal_size 
# optimal tree size = 2

# Prune the tree
prune_class_tree <- prune.misclass(class_tree, best = optimal_size)
prune_class_tree
plot(prune_class_tree)
text(prune_class_tree, cex=0.65, pretty = 0)


# First tree error rate
pred_class_tree <- predict(class_tree, newdata=test, type="class")
accuracy = mean(pred_class_tree == test$Health) 
# Accuracy rate =  0.84
1 - accuracy
# error rate = 0.16


# Pruned tree error rate
pred_prune_tree <- predict(prune_class_tree, newdata=test, type="class")
accuracy2 = mean(pred_prune_tree == test$Health) 
# Accuracy rate = 0.849
1 - accuracy2
# error rate = 0.151

### Bagging
set.seed(1)
# mtry = number of predictors
bag_class_tree <- randomForest(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + 
                                 NoHS_Percent + Smoker + Drug_Rate + Obesity + Diabetes + 
                                 Doctors, data = train, mtry = 9, importance=TRUE)
importance(bag_class_tree)

pred_bag_tree <- predict(bag_class_tree, newdata=test, type="class")
accuracy3 = mean(pred_bag_tree == test$Health) 
# Accuracy rate = 0.866
1 - accuracy3
# error rate = 0.134


### Random Forests
set.seed(1)
# mtry = p/3 = 9/3 = 3
forest_class_tree <- randomForest(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + 
                                    NoHS_Percent + Smoker + Drug_Rate + Obesity + Diabetes + 
                                    Doctors, data = train, mtry=3, importance=TRUE)
importance(forest_class_tree)


pred_forest_tree <- predict(forest_class_tree, newdata = test, type="class")
accuracy4 = mean(pred_forest_tree == test$Health) 
# Accuracy rate = 0.891
1 - accuracy4
# error rate = 0.109
















