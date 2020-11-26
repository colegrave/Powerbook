#Script 4.1 
#Simulates a single factor study with two levels 
#that will be analysed with a t-test.

#This line clears out R (see intro ESM for more details).
rm(list=ls())

#Sets up a counter that we will use to count significant 
#results and sets it to zero.
count <- 0


#The start of the loop. The code within the loop is
#repeated 100000 times, generating a data set and P value
#for each iteration. Thus, our estimated power will be 
#based on 100000 inginary experiments.
for(i in 1:100000) {

#Generate the random values for our control mice.
control <- rnorm(10, mean = 100, sd = 10)

#Generate the random values for our treatment mice.
wheel <- rnorm(10, mean = 90, sd = 10)

#Carry out a t-test on our imaginary data, and store the
#output in an object called my.result.
my.result <- t.test(control, wheel, var.equal = TRUE)
#Extract the P value from my.result, and store it in a 
#variable called pvalue.
pvalue <- my.result[3]


#Check to see whether our P value is significant.
#If it is, it adds one to the value of the counter.
if(pvalue<0.05){count = count+1}

#The end of our loop. 
}

#Once the specified number of imaginary experiments
#have run, send the value of the count variable
#to the Console to give us our answer
print(paste("count =",count))


