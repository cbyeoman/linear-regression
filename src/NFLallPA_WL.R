## Chris Yeomans
## Linear Regression Analysis on NHL 

##  directory of the csv file
dir="C:/Users/Christopher/Desktop/linear-regression-master/data/"

#Function to create filepath to csv file, then read the data  
reader = function(csv){
  fl = paste(dir,csv,sep="")
  read.table(fl,header=TRUE,sep=",")
}

#Call the above function to read data from selected file into R 
football.df = reader("NFL_all.csv")

#trend scatter plot to check visually that data has some linear distribution 
library(s20x)
trendscatter(WL~PA, data = football.df, f = 0.9)

#create the linear model for the data
football.lm = lm(WL~PA, data = football.df)

#Check for normality visually in the residuals using q-q and histogram 
normcheck(football.lm, shapiro.wilk = TRUE)

#Plot residuals vs fitted values of the linear model checking for uniformity about the origin
plot(football.lm, which = 1)

#Summary of the linear model
summary(football.lm)

#estimates for the parameters
football.lm$coefficients

#Confidence interval
ciReg(football.lm)

#Make predictions using the linear model
predict(football.lm, data.frame(PA=c(280, 320, 360, 400, 440)))

#Plot the predictions made, along with the real data
with(football.df,plot(WL~PA,bg="Black",main="Football Linear Model",
    pch=22,cex=1.2,ylim=c(0,1),xlim=c(250,1.1*max(PA))))
abline(football.lm,col='Red',lwd=2)
predictions = c(280, 320, 360, 400, 440)

points(predictions, predict(football.lm, data.frame(PA=c(280, 320, 360, 400, 440))),
       col='Blue',cex=1.6,pch=19)

#Cooks distance plot to determine any outliers in data. 
cooks20x(football.lm)
