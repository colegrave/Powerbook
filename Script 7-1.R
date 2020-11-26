#Script 7.1
#A two-factor design to estimate the power of both of
#the main effects and the interaction

#Matrix of means for the 4 combinations of genotype
#and environment indexed as follows
#(sens/optimal, sens/stress), c(res/optimal, res/stress)
cell.means <-rbind(c(5,4.6), c(4.8, 4.2))

#The value for the standard deviation
sd <-0.3

#The number of imaginary experiments
runs <- 100000

#The number of replicates per combination of genotype
#and environment.
reps <- 9

#Three empty vectors to store the p-values in
strainplist <- c()
environmentplist <-c()
interactionplist <-c()

#Big loop starts here.
for(experimentloop in 1:runs) {

#Labels to be used for the genotypes and environments 
treatmentnames <- c("sensitive", "resistant")
environmentnames <- c("optimal","stressful")

#Empty vectors to store the columns of our data frame
strain<-c()
environment<-c()
growth <-c()

#Nested loops to generate the data. 
for(s in 1:2) {
	for(t in 1:2) {
		for(r in 1:reps) {

#Generates the growth value for this rep and adds it to 
# the vector (growth)

growth <- append(growth, rnorm(1,cell.means[t,s],sd))

#Adds the label for the strain and environment for
#this replicate to the appropriate vectors
strain <- append(strain, treatmentnames[t])
environment <- append(environment, environmentnames[s])

		}
	}
}

#Combine our three vectors into a data frame
growth.data <- data.frame(strain, environment, growth)


#Analyse the data, and save in an Object called analysis
analysis <- anova(lm(growth~strain*environment))

#Extract the appropriate p-values
strainp <- analysis[1,5]
environmentp <- analysis[2,5]
interactionp <- analysis[3,5]

#Adds the current p-values to our p-value vectors
strainplist <- append(strainplist, strainp)
environmentplist <- append(environmentplist, environmentp)
interactionplist <- append(interactionplist, interactionp)

#The big loop ends here
}

#Counts the number of significant p-values in each vector
significantstrain <- length(which(strainplist<0.05))
significantenvironment <- length(which(environmentplist<0.05))
significantinteraction <- length(which(interactionplist<0.05))


#Outputs our power estimates to the Console
print(paste("power:strain =", (significantstrain/runs)))
print(paste("power:environment =", (significantenvironment/runs)))
print(paste("power:interaction =", (significantinteraction/runs)))
