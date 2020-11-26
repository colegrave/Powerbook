#Script 6.2
#A version of Script 6.1 embedded in a loop to allow multiple
#runs of the experiment to be carried out automatically to
#estimate power. Annotation is only provided for aspects that
#are different to Script 6.1

rm(list=ls())

#Define our world
slope <- -0.1
intercept <- 230
sd <-30

#The number of experiments we will use to estimate power.
runs <- 100000

#Define our design.
levels <- c(0,37.5,75,112.5,150,187.5,225,262.5,300)
reps <- c(4,4,4,4,4,4,4,4,4)
design <- rbind(levels,reps)
nlevels <- length(levels)


#Create an empty vector to store our p-values.
plist <- c()

#The start of the loop that repeats the whole experiment.
for(experimentloop in 1:runs) {

xval<-c()
yval <-c()

	for(x in 1:nlevels) {
			for(r in 1:design[2,x]) {

#Generate the data.
residual <-rnorm(1, 0, sd)
predicted <- (intercept + (slope * design[1,x]))
yval <-append(yval, (predicted + residual))
xval <- append(xval, design[1,x])

		}
	}
 
dataset <-data.frame(xval, yval)
analysis<- anova(lm(yval~xval, data= dataset))

#Extracts the appropriate p-value and stores it
#as a variable called p.
p <- analysis[1,5]

#Adds this p-value to our p-value vector.
plist <- append(plist, p)

#The big loop ends here.
}

#Counts the number of significant p-values
significant <- length(which(plist<0.05))

##Calculates power as significant/runs and sends 
#the result to the Console
print(paste("power =", (significant/runs)))
