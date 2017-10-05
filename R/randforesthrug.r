##
## [randforesthrug.r]
##
## author     : Ed Goodwin
## project    : randforesthrug
## createdate : 10.01.2017
##
## description:
##    Random Forest Talk for HRUG
##
## version: 0.01
## changelog:
##

require(randomForest)
require(caret)
require(dplyr)
require(mlbench) # data set


## data set
data(PimaIndiansDiabetes2)
diabetesdat = PimaIndiansDiabetes2
diabetesdat = na.omit(diabetesdat)

trainsize = 0.66
testsize = 0.34
indexes = sample(1:nrow(diabetesdat), size=trainsize*nrow(diabetesdat))

# Split data into test/train set
testset = diabetesdat[-indexes,]
dim(testset)
trainset = diabetesdat[indexes,]
dim(trainset)

summary(trainset)
sapply(trainset, class)

# some data plots
ggplot(diabetesdat, aes(x=diabetes, y=insulin)) + geom_jitter() +
  ggtitle("Insulin Levels and Diabetes Incidence")

ggplot(diabetesdat, aes(x=diabetes, y=mass)) + geom_jitter() +
  ggtitle("BMI and Diabetes Incidence")

ggplot(diabetesdat, aes(x=mass, y=triceps, colour=diabetes)) + geom_point() +
  ggtitle("BMI vs Tricep skin fold thickness with Diabetes Incidence")

ggplot(diabetesdat, aes(x=age, y=diabetes)) + geom_jitter() +
  ggtitle("Age and Diabetes Incidence")

ggplot(diabetesdat, aes(x=pressure, y=glucose, colour=diabetes)) + geom_point() +
  ggtitle("Glucose levels, Blood Pressure and Diabetes Incidence")

with(diabetesdat, table(age, diabetes))

## build our model
set.seed(314)
model = train(diabetes ~ pregnant + glucose + age +
                mass + pressure,
              data = trainset,
              method = "rf")

## predict with our model
testset$diabetespred = predict(model, newdata = testset)

## how did our model do? use a confusion matrix
table(testset$diabetes, testset$diabetespred)

model2 = train(diabetes ~ pregnant + glucose + pressure +
                 triceps + insulin + mass + pedigree + age,
              data = trainset,
              method = "rf")
model2

model3 = train(diabetes ~ pregnant + glucose + pedigree + age,
               data = trainset,
               method = "rf",
               trControl=trainControl(method="cv",number=5))

testset$diabetespred3 = predict(model3, newdata = testset)
table(testset$diabetes, testset$diabetespred3)
