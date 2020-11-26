#Script 5.1
#Script to generate data for a design where equal
#replicates of two treatments are randomly allocated to 
#spaces across three shelves in a growth chamber with equal 
#numbers of tubes on each shelf.

#Clear out R.
rm(list=ls())


#Matrix to store the means of the 6 combinations of 
#factors, indexed as follows
#(sens/Shelf1, sens/Shelf2, sens/Shelf3), 
#(res/shelf1, Res/shef2, Res/Shef3).
cell.means <-rbind(c(5.3,5,4.9), c(5.1, 4.8, 4.7))

#Set the value for the standard deviation.
sd <-0.3

#Set up a variable (reps) that contains the number of  
#replicates per treatment. So total sample size is reps*2
#N.B.this Script will only work if this is a multiple of 3.
reps <- 45


#Set up a vector containing the number of 
#1s, 2s and 3s for the design corresponding to the
#number of replicates on each shelf
shelflist <- rep(1:3, ((2*reps)/3))

#Shuffle the shelflist vector so that tubes  will be  
#assigned to shelf in a random order. NB For ease of
#reading we have split this line of code over two, but
#R will read it as a single line
randomshelf <- sample(shelflist, (reps*2), 
                      replace = FALSE, prob = NULL)


#Labels for the two levels of the treatment factor
treatmentnames <- c("sensitive", "resistant")

#Lables for the different levels of the shelf variables
shelfnames <- c("top","middle","bottom")

#Set up 3 empty vectors that we will use to store values
#in later. These will  be used to generate our data frame.
strain<-c()
shelf<-c()
growth <-c()

#set up a counter to keep track of the number of reps
#created so far. This is used to randomly pick shelves.
count<-0

#The nested loops start here.
#t loops through the two treatments whilst r loops 
#through the replicates for each treatment
	for(t in 1:2) {
		for(r in 1:reps) {

#Add 1 to the counter each time we start the loop 
#To keep track of the current replicate's number.
count<-count+1


#Sets the shelf number for this rep to the value drawn 
#from the random shelf allocation list created above.
s <- randomshelf[(count)]

#Generates the growth value for this rep and adds it to 
#the vector (growth).
#Does so by drawing a random number from a normal
#distribution with the approproate mean for the current
#factor  combination (extracted from the cell.means table)
#and sd defined above.
growth <-append(growth, rnorm(1,cell.means[t,s],sd))

#Adds the label for the treatment and shelf for the 
#current replicate to the appropriate vector.
strain <- append(strain, treatmentnames[t])
shelf <- append(shelf, shelfnames[s])

		}
	}
#Nested loops end here.

#now to combine vectors into a data frame
growth.data <-data.frame(strain, shelf, growth)


#Carry out out the analysis useing a linear model
#store the result in an object called analysis.
analysis<- anova(lm(growth~shelf+strain))


##Display the data frame and analysis results.
print(growth.data)
print(analysis)







