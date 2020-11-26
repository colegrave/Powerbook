#Script 6.3
#Generates data for a continuous predictor with levels 
#defined by the researcher. Assumes a quadratic relationship
#between Y and X, with coefficients m1, m2 and C defined by 
#the researcher.

#Clear out R
rm(list=ls())

#These lines define our imaginary world.
#We have three coefficients to define the quadratic 
#function: Y = C + m1*x + m2*x^2
C <- 230
m1 <- -0.1667
m2 <- 0.00022
sd <-30

#Sets up the design of our study
levels <- c(0,37.5,75,112.5,150,187.5,225,262.5,300)
reps <- c(4,4,4,4,4,4,4,4,4)
design <- rbind(levels,reps)
nlevels <- length(levels)


#Creates two empty lists to store p-values for the 
#linear and quadratic terms in the model
plist1 <- c()
plist2 <- c()

#Empty vectors to store our response and predictor variable
#Note, we have an extra vector which will allow us to have 
#an additional column in our data frame for the X^2 values.
#This will make it easier to fit our model at the end.
xval<-c()
x2val<-c()
yval <-c()


#The nested loops start here. 
	for(x in 1:nlevels) {
		for(r in 1:design[2,x]) {

#These lines generate the Y value for the current replicate
#using the quadratic relationship defined above and a
#random deviation drawn from a normal distribution.
residual <-rnorm(1, 0, sd)
predicted <- (C + (m1 * design[1,x]) + (m2 * (design[1,x]^2)))
yval <-append(yval, (predicted + residual))
xval <- append(xval, design[1,x])
x2val <- append(x2val, (design[1,x]^2))

		}
	}
#Nested loops end here. 

#Combine our vectors into a data frame
dataset <-data.frame(xval, yval, x2val)

#Carry out the analysis. 
#We actually fit two linear models to the data. 
#The first is the quadratic model (with results stored in
#analysis1). The second is the linear model (with
#results stored in analysis 2). This allows us to compare the
#power to discriminate the linear and quadratic relationships.
analysis1<- anova(lm(yval~ xval + x2val, data= dataset))
analysis2<- anova(lm(yval~ xval, data= dataset))


#Display the data from a and analysis results.
print(dataset)
print(analysis1)
print(analysis2)








