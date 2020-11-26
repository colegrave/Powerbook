#Script 6.5
#Generates data for a study where the values of the predictor
#are drawn from a normal distribution with defined mean and sd.
#Y values are assumed to be linearly related to X, and the slope
#and intercept are specified by the researcher.

#Clear out R
rm(list=ls())

#Variables to define the imaginary world
slope <- 14
intercept <- 0
sd <-2

#The parameters for the population we will
#sample our X values from
xmean <- 4.5
xsd <- 0.3


#Specify the total sample size of our study
reps <- 20

#Set up empty vectors to store our x and y values
xval<-c()
yval <-c()

#Start of a loop set to the number of replicates
for(r in 1:reps) {

#Generate the current X value by drawing from a normal
#distribution, and add this to the vector of X values
thisx <- rnorm(1, xmean, xsd)
xval <- append(xval, thisx)
#Generate the predicted value for this replicate using
#the defined linear relationship
predicted <- (intercept + (slope*thisx))
#Generate a random deviation for this individual
residual <- rnorm(1, 0, sd)
#Generate the y value by adding the deviation to the predicted
#value
yval <- append(yval, (predicted + residual))

	}
 

#Combine our vectors into a data frame
dataset <- data.frame(xval, yval)


#Analyse the data and store the result in an object 
#called analysis
analysis <- anova(lm(yval~xval, data=dataset))


#display the data and the analysis
print(dataset)
print(analysis)

#plot the data
plot(dataset)




