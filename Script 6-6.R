#Script 6.6
#Generate data for a design with a continuous predictor, but
#a binary response variable. The relationship between the 
#probability of infection and the value of the predictor
#is assumed to be logistic, with coefficients defined by
#the researcher

rm(list=ls())

#We set up two variables containing the model parameters
#for our logistic curve. m1 is the rate turn, whilst
#m2 is the inflection point
m1 <- 0.1
m2 <- 20

#Set up our design
levels <- c(5,10,15,20,25,30,35,40,45)
reps <- c(4,4,4,4,4,4,4,4,4)
design <- rbind(levels,reps)
nlevels <- length(levels)

#Set up empty vectors to store our x and y values
xval<-c()
yval <-c()

#The loops to generate the dataset start here
#The outer loops goes through the number of levels
#whilst the inner loop goes around the number of
#replicates of the current level
	for(x in 1:nlevels) {
		for(r in 1:design[2,x]) {

#Generates the probability of infection of the current
#replicate based on the logistic curve
predicted <- 1/(1 + exp(-m1*(design[1,x] - m2)))
#Generates a y value by drawing a number from a binomial
#distribution. This will be a value of either 1 
#or 0, where 1 is infected, 0 is uninfected.
yval <-append(yval, (rbinom(1,1,predicted)))

#Adds the value for the predictor of the current replicate
#to the X vector.
xval <- append(xval, design[1,x])

		}
	}
#Nested loops end here. 

#Combines our vectors into a data frame
dataset <-data.frame(xval, yval)

#Carry out the analysis.Note we have split the line
#of code over two lines to make it easier to read 
#R will read it as a single line
analysis<- anova(glm(yval~xval, 
  family=binomial(link='logit'),data= dataset), test = 'Chisq')

#Display the data and the results of the analysis.
print(dataset)
print(analysis)
#Plot the data
plot(dataset)






