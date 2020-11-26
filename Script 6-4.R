##Script 6.4
#A version of Script 6.3 embedded in a loop to allow multiple
#runs of the experiment to be carried out automatically to
#estimate power. Annotation is only provided for aspects that
#are different to Script 6.3

rm(list=ls())

#Define our world
C <- 230
m1 <- -0.1667
m2 <- 0.00022
sd <-30

#Defines the number of experiments used to estimate
#power.
runs <- 100000

#Our design.
levels <- c(0,37.5,75,112.5,150,187.5,225,262.5,300)
reps <- c(4,4,4,4,4,4,4,4,4)
design <- rbind(levels,reps)
nlevels <- length(levels)


#Create empty vectors to store the P values from the 
#quadratic and linear analyses.
plist1 <- c()
plist2 <- c()

#The outer loop to repeat the experiment starts here.
for(experimentloop in 1:runs) {

xval<-c()
x2val<-c()
yval <-c()

	for(x in 1:nlevels) {
		for(r in 1:design[2,x]) {

#Generate the current data point.
residual <-rnorm(1, 0, sd)
predicted <- (C + (m1 * design[1,x]) + (m2 * (design[1,x]^2)))
yval <-append(yval, (predicted + residual))
xval <- append(xval, design[1,x])
x2val <- append(x2val, (design[1,x]^2))

		}
	}

dataset <-data.frame(xval, yval, x2val)
analysis1<- anova(lm(yval~ xval + x2val, data= dataset))
analysis2<- anova(lm(yval~ xval, data= dataset))

#Extract our p-values.
p1 <- analysis1[2,5]
p2 <- analysis2[1,5]

#Add the p-values to the p-value vectors.
plist1 <- append(plist1, p1)
plist2 <- append(plist2, p2)

#The big loop ends here.
}

#Counts the number of significant P values in
#our p-value vectors and stores these in variables called
#significant1 (qudratic) and significant2 (linear).
significant1 <- length(which(plist1<0.05))
significant2 <- length(which(plist2<0.05))

print(paste("power to detect quadratic =", (significant1/runs)))
print(paste("power to detect straight line =", (significant2/runs)))

