dataset=read.csv('Position_Salaries.csv')
dataset=dataset[2:3]

#Fitting Linear regression to the dataset
lin_reg= lm(formula = Salary~.,
            data=dataset)
summary(lin_reg)


#Fitting Polynomial Regression to the dataset
dataset$Level2=dataset$Level^2

dataset$Level3=dataset$Level^3
dataset$Level4=dataset$Level^4#More the degree more the accuracy of the poly curve 
poly_reg=lm(formula = Salary ~ .,
            data = dataset)
summary(poly_reg)

#install ggplot2 if not installed
library(ggplot2)
#Visualising the Linear Regression results to compare
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour ='red')+
  geom_line(aes(x=dataset$Level,y=predict(lin_reg,newdata = dataset)),
            colour ='blue')+
  ggtitle('Linear regression results')+
  xlab('Level')+
  ylab('Salary')

#Visualising the Polynomial Regression results
x_grid=seq(min(dataset$Level),max(dataset$Level),0.1)
ggplot()+
  geom_point(aes(x=dataset$Level,y=dataset$Salary),
             colour ='red')+
  geom_line(aes(x=dataset$Level,y=predict(poly_reg,newdata = dataset)),
            colour ='blue')+
  ggtitle('Linear regression results')+
  xlab('Level')+
  ylab('Salary')

#Predicting with linear model
y_pred=predict(lin_reg,data.frame(Level=6.5))
#Predicting with polynomial model
y_pred=predict(poly_reg,data.frame(Level=6.5,Level2=6.5^2,Level3=6.5^3,Level4=6.5^4))

