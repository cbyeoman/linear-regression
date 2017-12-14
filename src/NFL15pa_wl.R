## Chris Yeomans
## Linear Regression Analysis on NHL 

## file path to the csv file
dir="C:/Users/Christopher/Desktop/linear-regression-master/data/"

#Function to concatenate file name to dir, then read the data
reader = function(csv){
  fl = paste(dir,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}

#Read the data into R
football.df = reader("NFL_15.csv")

#trend scatter plot
library(s20x)
trendscatter(WL~PA, data = football.df, f = 0.8)

#linear model for the data
football.lm = lm(WL~PA, data = football.df)

#Check for normality in the residuals
normcheck(football.lm, shapiro.wilk = TRUE)

#Plot residuals vs fitted values of the linear model
plot(football.lm, which = 1)

#Summary of the linear model
summary(football.lm)

#Get the estimates for the parameters
football.lm$coefficients

#Confidence interval for B0 and B1
ciReg(football.lm)

#Make predictions using the linear model
predict(football.lm, data.frame(PA=c(280, 320, 360, 400, 440)))

#Plot the predictions with the actual data
with(football.df,plot(WL~PA,bg="Black",main="Football Linear Model",
    pch=22,cex=1.2,ylim=c(0,1),xlim=c(250,1.1*max(PA))))
abline(football.lm,col='Red',lwd=2)
predictions = c(280, 320, 360, 400, 440)

points(predictions, predict(football.lm, data.frame(PA=c(280, 320, 360, 400, 440))),
       col='Blue',cex=1.6,pch=19)

#Cooks distance plot
cooks20x(football.lm)
