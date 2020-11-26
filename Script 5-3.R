#Script 5.3
#Generates a balanced randomised block design where
#two treatments are allocated in equal numbers of replicates
#on each of 3 shelves in a growth chamber

#Clear out R
rm(list=ls())

#These lines specify the imaginary world.
cell.means <-rbind(c(5.3,5,4.9), c(5.1, 4.8, 4.7))
sd <-0.3
runs <- 100000

#Define the number of reps per treatment per shelf
#so that total sample size is reps*6. NB
#this is different from in Scripts 5.1 and 5.2.
reps <- 15

#Set up an empty vector to store our p values.
plist <- c()

#The start of the loop for repeating the experiments.
for(experimentloop in 1:runs) {

#Labels for the two levels of the treatment factor
#and three levels of shelf
treatmentnames <- c("sensitive", "resistant")
shelfnames <- c("top","middle","bottom")

#3 empty vectors so store values in later. 
strain<-c()
shelf<-c()
growth <-c()

#Nested loops to generate the data set
for(s in 1:3) {
	for(t in 1:2) {
		for(r in 1:reps) {

#Generate the growth value for this rep and add it to 
#the vector (growth).see Script 5.1 for more details.
growth <-append(growth, rnorm(1,cell.means[t,s],sd))

#Add the label for the treatment and shelf for the 
#current replicate to the appropriate vector.
strain <- append(strain, treatmentnames[t])
shelf <- append(shelf, shelfnames[s])

		}
	}
}

#Create the data frame.
growth.data <-data.frame(strain, shelf, growth)

#Analyse the data, extract the P value and add
#it to our P value vector.
analysis<- anova(lm(growth~shelf+strain))
p <- analysis[2,5]
plist <- append(plist, p)

}

#Determine the number of P values in our list
#that are significant.
significant <- length(which(plist<0.05))

#Output the estimated power to the console.
print(paste("power =", (significant/runs)))
