#Script 7.2
#A one-factor design where the factor has 3 levels
#Analysis based on Anova followed by a planned pairwise
#comparison. Power estimated for the Omnibus test
#and the comparison of interest


#The means and sd of the three levels of our factor
treatment.means <-c(45, 52, 57)
sd <- 7

#The number of imaginary experiments that we want
runs <- 10000


#The number of replicates per level of the treatment
reps <- 10

#Empty vectors to store the p-values for the Omnibus test
#and planned comparison
omnibusplist <- c()
plist <- c()

#Experiment loop starts here
for(experimentloop in 1:runs) {

#Labels for the levels of our treatment
treatmentnames <- c("control", "traditional", "novel")



#Vectors to store the columns in our data frame
treatment <-c()
yield <-c()



#The nested loops start here. 
for(t in 1:3) {
		
		for(r in 1:reps) {

#Generates the yield value for this rep and adds it to 
#the appropriate vector (yield)

yield <-append(yield, rnorm(1,treatment.means[t],sd))

#Adds the label for the treatment for this replicate
#to the appropriate vector (treatment)
treatment <- append(treatment, treatmentnames[[t]])

		}
	}

#Nested loops end here. 

#Combine our vectors into a data frame
yield.data <-data.frame(treatment, yield)


#Carry out the analysis. We use
#a linear model to fit our anova and generate the 
#Omnibus p-value.
analysis <- anova(lm(yield~treatment))

#Extracts the Omnibus p-value and stores it as a variable 
#called p, and also adds this to our list of Omnibus
#p-values
p <- analysis[1,5]
omnibusplist <- append(omnibusplist, p)

#If the omnibus result is significant, we do a pairwise 
#comparison between traditional and novel and replace our 
#Omnibus p-value with the result of the comparison
if(p < 0.05){
comparison <- pairwise.t.test(yield, treatment, p.adj = "none")
p <- comparison$p.value[4]
}

#Adds the current p-value to our list of pariwise
#comparison p-values
plist <- append(plist, p)

#The big loop ends here
}

#Counts the number of significant p-values for each test
significant <- length(which(plist<0.05))
omnibussignificant <- length(which(omnibusplist<0.05))


#Outputs the estimated powers to the Console
print(paste("power =", (significant/runs)))
print(paste("omnibuspower =", (omnibussignificant/runs)))


