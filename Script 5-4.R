#Script 5.4
#This is a modification of Script 5.3 to allow for the 
#possibility pf drop-outs due to contamination
#detailed annotation can be found on Script 5.3
#Annotation here is limited to new lines of code.

rm(list=ls())

cell.means <-rbind(c(5.3,5,4.9), c(5.1, 4.8, 4.7))
sd <-0.3
runs <- 100000

#Create a variable called dropout that contains 
#the probability of contamination.
dropout <- 0.1

reps <- 15
plist <- c()

for(experimentloop in 1:runs) {

treatmentnames <- c("sensitive", "resistant")
shelfnames <- c("top","middle","bottom")
strain<-c()
shelf<-c()
growth <-c()

for(s in 1:3) {
	for(t in 1:2) {
		for(r in 1:reps) {

#Generates a random number by drawing from a uniform 
#distribution between 0 an 1 to be used to test for
#contamination
u <- runif(1)

#A conditional statement to compare the random number u
#to the dropout rate.If it is greater than the probability  
#of infection, generate a measurement normally. If not 
#then add an NA (R's missing value indicator) instead.
#[note we have split the first line of code over two lines
#to make easier to read, but R reads it as a single line]
if(u > dropout) {growth <-append(growth, 
                                 rnorm(1,cell.means[t,s],sd))
	} else {growth <- append(growth, NA)}

strain <- append(strain, treatmentnames[t])
shelf <- append(shelf, shelfnames[s])

		}
	}
}
growth.data <-data.frame(strain, shelf, growth)
analysis<- anova(lm(growth~shelf+strain))
p <- analysis[2,5]
plist <- append(plist, p)


}
significant <- length(which(plist<0.05))
print(paste("power =", (significant/runs)))
