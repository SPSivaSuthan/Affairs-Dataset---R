#Logistic Regression: Classification Problem
#Affairs dataset

affairs <- read.csv(choose.files())
dim(affairs)
head(affairs)
affairs <- affairs[,-1]
head(affairs)

#missing data checks
colSums(is.na(affairs))

str(affairs)

#If its a binary value don't need to check outlier, and don't need to do featurescaling & encoding

#splitting the data into training and test for building model and prediction

library(caTools)
set.seed(123)
split <- sample.split(affairs$extramaterial.affairs, SplitRatio = 0.75)
split
table(split)
training <- subset(affairs, split==T)
test <- subset(affairs, split==F)
nrow(training)
nrow(test)

#building Logistic regression model
# model name - generalised linear model (glm)

#logit <- glm(dv~idv, data = training, family='binomial')

names(affairs)
logit <- glm(extramaterial.affairs~., data = training, family ='binomial')
logit
summary(logit)

#significant variable = vryunhap+unhap+antirel+yrsmarr1+yrsmarr2

logit1 <- glm(extramaterial.affairs~vryunhap+unhap+antirel+yrsmarr1+yrsmarr2, 
             data = training, family = 'binomial')
logit1
summary(logit1)

# predict the model with test dataset 

logit_pred <- predict(logit1, newdata = test, type='response')
logit_pred

# threshold value = 50%

logit_pred_thr <- ifelse(logit_pred>=0.5,1,0)
logit_pred_thr

#confusion Matrix

cm <- table(test$extramaterial.affairs,logit_pred_thr)
cm

(108+7)/(108+5+31+7)

install.packages('caret')
library(caret)

confusionMatrix(cm)
           
#Accuracy for 50% Threshold value is 0.7616
   
