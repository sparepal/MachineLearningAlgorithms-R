
# Importing the dataset
dataset = read.csv('50_Startups.csv')

#Encodinf categorical data
dataset$State=factor(dataset$State,
                       levels = c('New York','California','Florida'),
                       labels = c(1,2,3) 
)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = .8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

#Fitting Multiple Linear Tegression to The training set
regressor=lm(formula = Profit ~ .,data=training_set)# . represents linear combination of all the idependent variables else use +
summary(regressor)

#Predicting 
y_pred = predict(regressor,newdata = test_set)

#Backward elimination for optimal model
regressor=lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State,data=dataset)
summary(regressor)

regressor=lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend ,data=dataset)
summary(regressor)

regressor=lm(formula = Profit ~ R.D.Spend + Marketing.Spend ,data=dataset)
summary(regressor)
#Spend has some significance but(Using the Rsquare adjusted method this elimination would not be appropriate)
regressor=lm(formula = Profit ~ R.D.Spend,data=dataset)
summary(regressor)
