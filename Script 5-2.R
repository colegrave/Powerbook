#Script 5.2
#This is the same script as 5.1 but embeded in a loop to 
#allow multiple runs of the experiment. Detailed annotation
#of the main body can be found in Script 5.1
#only new items or changes are annotated here.

rm(list=ls())

cell.means <-rbind(c(5.3,5,4.9), c(5.1, 4.8, 4.7))
sd <-0.3
runs <- 100000
reps <- 45

#create an empty vector where we will store the p values.
plist <- c()

#big loop starts here.
for(experimentloop in 1:runs) {


shelflist <- rep(1:3, ((2*reps)/3))
randomshelf <- sample(shelflist, (reps*2), 
                      replace = FALSE, prob = NULL)
treatmentnames <- c("sensitive", "resistant")
shelfnames <- c("top","middle","bottom")

strain<-c()
shelf<-c()
growth <-c()
count<-0

	for(t in 1:2) {
		for(r in 1:reps) {

count<-count+1
s <- randomshelf[(count)]

growth <-append(growth, rnorm(1,cell.means[t,s],sd))
strain <- append(strain, treatmentnames[t])
shelf <- append(shelf, shelfnames[s])

		}
	}

growth.data <-data.frame(strain, shelf, growth)

analysis<- anova(lm(growth~shelf+strain))

#Extract the appropriate p value and store 
#it as a variable called p
p <- analysis[2,5]

#Add this p value to our running list of p values
plist <- append(plist, p)

}

#Count the number of items in plist that are 
#less than 0.05 (i.e. significant) and store this 
#in a variable called significant.
significant <- length(which(plist<0.05))

#Calculate power by dividing the number of significant
#results by the total number of runs, and sends the 
#result to the Console.
print(paste("power =", (significant/runs)))
