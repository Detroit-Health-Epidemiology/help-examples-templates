

library(tidyverse)

# Create a sample dataframe with some repeat patient ids
myData <- data.frame(c(rep(10, 50), 11:60), rnorm(1:100))
colnames(myData) <- c("Patient_Id", "Result") # Name the columns
head(myData) # View head to verify it is set up correctly

# Add variables to test different ways of marking duplicate Ids
myData <- myData %>% mutate(
  test1 = duplicated(Patient_Id), # This will mark all repeats of the patient ID after the first instance
  test2 = duplicated(Patient_Id, fromLast = TRUE), # This works backwards, and will mark all repeats starting at the last instance
  test3 = duplicated(Patient_Id)|duplicated(Patient_Id, fromLast = TRUE), # This will mark ALL duplicates because it checks from both directions
  test4 = duplicated(Patient_Id)&duplicated(Patient_Id, fromLast = TRUE) # Why don't we want to use & instead of | ? This will leave off BOTH the first and last instance
)

# We know that we should have 50 patients with Id "10"
# Let's see what the different methods captured:
table(myData$test1)

table(myData$test2)

table(myData$test3)

table(myData$test4)

# Take a look at the data and see how it works!
View(myData)