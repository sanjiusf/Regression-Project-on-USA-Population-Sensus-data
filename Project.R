#Importing the libraries
library(readxl)
library(car)
library(carData)
library(corrplot)
#Setting the directory
setwd("C:\\Users\\sanji\\Documents\\MS-USF\\Semester 2\\AMB\\Project")
#Loading the data
pop_data = read_excel("6304 Regression Project Data.xlsx")
#Pre-Processing of the data-calculating the variariables "popcollege"(number of population that has attended college) and "popprof"(Number of population that has professional experience)
pop_data$popcollege = (pop_data$percollege * pop_data$poptotal)/100
pop_data$popprof = (pop_data$perprof * pop_data$poptotal)/100
#Calculating Ratio of population of children to adult
pop_data$popratio = (pop_data$popchild / pop_data$popadult)
#Calculating the number of children living in poverty
pop_data$popchildpoverty = (pop_data$popchild * pop_data$perchildpoverty)/100
#Dividing the dataset into two parts : with rural counties and with metropolitian counties
rural_popdata=subset(pop_data,pop_data$inmetro==0)
metropolitian_popdata=subset(pop_data,pop_data$inmetro==1)
#Taking a random sample of 30 counties from the metro poverty data set. 
set.seed(44317266)  
sample.rural_popdata=rural_popdata[sample(1:nrow(rural_popdata),60,replace=FALSE),]
sample.metropolitian_popdata=metropolitian_popdata[sample(1:nrow(metropolitian_popdata),30,replace=FALSE),]
#Analysis of the data
#Calculating the correlation matrix and plot
x_cor<- cor(cbind(sample.rural_popdata[,4:20]))
corrplot(x_cor,method = "number")
#Applying the Linear Regression models
#Model 1-Kitchen sink model
m1<-lm(sample.rural_popdata$perelderlypoverty~sample.rural_popdata$area+sample.rural_popdata$poptotal+sample.rural_popdata$popdensity+sample.rural_popdata$popwhite+sample.rural_popdata$popblack+sample.rural_popdata$popasian+sample.rural_popdata$popadult+sample.rural_popdata$popchild+sample.rural_popdata$popcollege+sample.rural_popdata$popprof+sample.rural_popdata$popratio+sample.rural_popdata$popchildpoverty)
summary(m1)
#Model 2-Feature selection based on correlation matrix and model application
m2<-lm(sample.rural_popdata$perelderlypoverty~sample.rural_popdata$popdensity+sample.rural_popdata$popblack+sample.rural_popdata$popasian+sample.rural_popdata$popadult+sample.rural_popdata$popcollege+sample.rural_popdata$popchildpoverty)
summary(m2)
#Model 3-Normalizing the y variable by applying log transformation 
hist(log(sample.rural_popdata$perelderlypoverty))
m3<-lm(log(sample.rural_popdata$perelderlypoverty)~sample.rural_popdata$popdensity+sample.rural_popdata$popblack+sample.rural_popdata$popasian+sample.rural_popdata$popadult+sample.rural_popdata$popcollege+sample.rural_popdata$popchildpoverty)
summary(m3)
#Checking the regression assumptions for model 3
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popdensity)
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popblack)
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popasian)
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popadult)
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popchildpoverty)
plot(sample.rural_popdata$perelderlypoverty,sample.rural_popdata$popcollege)
#Checking if the the model satisfies normality criteria using QQPlot
qqnorm(sample.rural_popdata$perelderlypoverty)
qqline(sample.rural_popdata$perelderlypoverty,col="red")
#Checking if the the model satisfies independance criteria 
durbinWatsonTest(m3)
#Checking if the the model satisfies linearity criteria
plot(m3)
plot(sample.rural_popdata$perelderlypoverty,m3$fitted.values )
abline(0,0,lwd=3,col="Red") 
#Checking if the the model satisfies homoskedacity criteria
plot(m3$residuals,m3$fitted.values )
abline(0,0,lwd=3,col="Red") 
#Checking if the the model satisfies multicollinearity criteria
vif(m3)
#Checking for leverages in the data
lev=hat(model.matrix(m3))
plot(lev,pch=19)
abline(3*mean(lev),0,col="red",lwd=3)
sample.rural_popdata[lev>(3*mean(lev)),2:3]
#Model 4- Removing the leverages from data
m4<-lm(log(sample.metropolitian_popdata$perelderlypoverty)~sample.metropolitian_popdata$popdensity+sample.metropolitian_popdata$popblack+sample.metropolitian_popdata$popasian+sample.metropolitian_popdata$popadult+sample.metropolitian_popdata$popcollege+sample.metropolitian_popdata$popchildpoverty)
summary(m4)
plot(m4)
durbinWatsonTest(m4)
plot(sample.metropolitian_popdata$perelderlypoverty,m4$fitted.values )
abline(0,0,lwd=3,col="Red") 