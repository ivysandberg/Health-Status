library(ROCR) # For AUC metrics
library(boot) # For Cross-Validation
library(MASS) # For LDA
library(ICS) # For kurtosis and skewness tests
library(ipred) ##for errorest function


data <- read.csv(file = "/Users/ivysandberg/Desktop/SML_PROJECT/CLEAN_CHSIdata.csv")

# Create Response Variable
hist(data$Health_Status)

# 0 = "Poor Health" , 1 = "Good Health"
data$Health = ifelse(data$Health_Status > median(data$Health_Status), 0, 1) 

# Additional Cleaning
data$Death_Rate = data$Total_Deaths/data$Population_Size * 100
data$Uninsured_Percent = data$Uninsured/data$Population_Size * 100
data$NoHS_Percent = data$No_HS_Diploma/data$Population_Size * 100
data$Depression_Rate = data$Major_Depression/data$Population_Size * 100
data$Drug_Rate = data$Recent_Drug_Use/data$Population_Size * 100
data$Older_Population = data$Age_65_84 + data$Age_85_and_Over
data$Unemployment_Rate = data$Unemployed/data$Population_Size * 100

# Add a categorical variable to our logistic regression
# use Prim_Care_Phys_Rate = how many doctors does a county have per 10,000 people
hist(data$Prim_Care_Phys_Rate)
data$Doctors = ifelse(data$Prim_Care_Phys_Rate > median(data$Prim_Care_Phys_Rate), 1, 0) 
# highly skewed right


# Visualizations
boxplot(data$Unemployment_Rate~data$Health) # Good
boxplot(data$Poverty~data$Health) # Good
boxplot(data$Uninsured_Percent~data$Health) # Good
boxplot(data$Depression_Rate~data$Health) # No separation, which is interesting
boxplot(data$NoHS_Percent~data$Health) # Good
boxplot(data$Drug_Rate~data$Health) # Opposite of expected
boxplot(data$Smoker~data$Health) # Slight separation
boxplot(data$Obesity~data$Health) # Good
boxplot(data$Older_Population~data$Health) # Slight Separation
boxplot(data$Diabetes~data$Health) # Good
boxplot(data$Uninsured~data$Health) 

# Split the Data
set.seed(1)
sample.data <- sample.int(nrow(data), floor(.80*nrow(data)), replace = F)
train <- data[sample.data, ]
test <- data[-sample.data, ]


# Logistic Regression Model 1
log1 <- glm(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + 
              NoHS_Percent + Smoker + Drug_Rate + Obesity + Diabetes, family = binomial, data = train)
summary(log1)


# ROC of Model 1
predict <- predict.glm(log1, newdata = test, type = "response")
rates <- prediction(predict, test$Health)
roc <- performance(rates, measure="tpr", x.measure="fpr")

plot(roc, main="ROC Curve for Logistic Regression")
lines(x = c(0,1), y = c(0,1), col="red")

# AUC of Model 1
auc<-performance(rates, measure = "auc")
auc


# Cross Validation
set.seed(1)
model <- glm(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + NoHS_Percent + 
                 Smoker + Drug_Rate + Obesity + Diabetes, family = binomial, data = data)
summary(model)
five <- cv.glm(data = data, model, K = 5)
five$delta

ten <- cv.glm(data = data, model, K = 10)
ten$delta


## Check Assumptions for LDA
poor_health <- data[data$Health == 0,]
good_health <- data[data$Health == 1,]

# Kurtosis and Skewness for Poor Health
# Null Hyp is that the constant variance assumption is satisfies. 
mvnorm.kur.test(poor_health[,c("Unemployment_Rate", "Poverty", "Uninsured_Percent", "NoHS_Percent", "Smoker", "Obesity", "Diabetes")])
mvnorm.skew.test(poor_health[,c("Unemployment_Rate", "Poverty", "Uninsured_Percent", "NoHS_Percent", "Smoker", "Obesity", "Diabetes")])

# Kurtosis and Skewness for Good Health
mvnorm.kur.test(good_health[,c("Unemployment_Rate", "Poverty", "Uninsured_Percent", "NoHS_Percent", "Smoker", "Obesity", "Diabetes")])
mvnorm.skew.test(good_health[,c("Unemployment_Rate", "Poverty", "Uninsured_Percent", "NoHS_Percent", "Smoker", "Obesity", "Diabetes")])


## LDA Model
lda_model <- lda(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent 
                  + NoHS_Percent + Smoker + Obesity + Diabetes + Drug_Rate, data = train)
lda_model


# ROC of LDA Model
predict_lda <- predict(lda_model, newdata = test)

preds <- predict_lda$posterior[,2]
rates2 <- prediction(preds, test$Health)
roc2 <- performance(rates2, measure="tpr", x.measure="fpr")
plot(roc2, main="LDA ROC Curve")
lines(x = c(0,1), y = c(0,1), col="red")

# AUC of LDA Model
auc2 <- performance(rates2, measure = "auc")
auc2


# Cross Validation of LDA Model
cv.da <- function(object, newdata) 
  
{
  
  return(predict(object, newdata = newdata)$class)
  
} 

# K = 5
set.seed(1)
data$Health <- as.factor(data$Health)
errorest(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + NoHS_Percent + 
           Smoker + Obesity + Diabetes + Drug_Rate, data=data, model=lda, 
         estimator="cv", est.para=control.errorest(k=5), predict=cv.da)$err 

# K = 10
set.seed(1)
data$Health <- as.factor(data$Health)
errorest(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + NoHS_Percent + 
           Smoker + Obesity + Diabetes + Drug_Rate, data=data, model=lda, 
         estimator="cv", est.para=control.errorest(k=10), predict=cv.da)$err 

l

# Add a categorical variable to our logistic regression = Doctors
log2 <- glm(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + 
              NoHS_Percent + Smoker + Drug_Rate + Obesity + Diabetes + 
              Doctors, family = binomial, data = train)
summary(log2)

# ROC for Logistic Regression Model 2
predict <- predict.glm(log2, newdata = test, type = "response")
rates3 <- prediction(predict, test$Health)
roc3 <- performance(rates3, measure="tpr", x.measure="fpr")

plot(roc3, main="ROC Curve for Logistic Regression with Doctors Classification Variable")
lines(x = c(0,1), y = c(0,1), col="red")

# AUC for Logistic Reg Model 2
auc3 <- performance(rates3, measure = "auc")
auc3


# Cross Validation
set.seed(1)
model2 <- glm(Health ~ Unemployment_Rate + Poverty + Uninsured_Percent + NoHS_Percent + Smoker + Drug_Rate + Obesity + 
               Diabetes + Doctors, family = binomial, data = data)
summary(model2)
five <- cv.glm(data = data, model2, K = 5)
five$delta

ten <- cv.glm(data = data, model2, K = 10)
ten$delta


