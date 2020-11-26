#Script 6.1 
#Generates data for a continuous predictor with levels
#set by the researcher. Assumes a linear relationship 
#between Y and X, with a defined slope and intercept. 

#Clear out R.
rm(list=ls())

#Define the slope, intercept and sd of our imaginary world.
slope <- -0.1
intercept <- 230
sd <-30

#The next 3 lines define our design.
#Set up a vector containing the  levels of our predictor.
levels <- c(0,37.5,75,112.5,150,187.5,225,262.5,300)
#Set up a vector containing the number of reps at each level.
#N.B make sure these two vectors are of the same length.
reps <- c(4,4,4,4,4,4,4,4,4)
#Combine our vectors into a single matrix.
design <- rbind(levels,reps)

#We use the length function to automatically count the 
#number of levels in our design,to be used in our loop below
nlevels <- length(levels)

#Set up empty vectors to store our x and y values
xval<-c()
yval <-c()


#The nested loops to generate the data set
#The outer loop goes through each level in turn
#The inner loop is set to the number of replicates at
#current level
	for(x in 1:nlevels) {
		for(r in 1:design[2,x]) {

#Generate a random deviation for the current data point
#by drawing from a normal distribution with mean of zero
#and sd as defined by user.
residual <-rnorm(1, 0, sd)
#Generate the predicted value for the current data point
#by substituting the current value of the predictor variable
#into the equation of the linear relationship.
predicted <- (intercept + (slope * design[1,x]))
#Generate the actual data point by adding the deviation
#to the predicted value, and add it to our y vector.
yval <-append(yval, (predicted + residual))

#Extract the current value of the predictor from the design
#matrix and add that value to the x vector.
xval <- append(xval, design[1,x])

		}
	}
#Nested loops end here. 

#Combine our vectors into a data frame.
dataset <-data.frame(xval, yval)


#Analyse the data and store the result in an object
#called analysis.
analysis<- anova(lm(yval~xval, data= dataset))


#Display the data frame and results of the analysis.
print(dataset)
print(analysis)

#Produce a plot of the data.
plot(dataset)






