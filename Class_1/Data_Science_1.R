#### Haley's Data Science Class #1 ####
# Areas Covered 
# 1. Variables
# 2. Functions
# 3. Conditional Statements (if / else statements)
# 4. For loops


# Load in packages
library(tidyverse)


# Make up some data to play with

ebert_cobian<- data.frame(Member = c("Haley", "Tyler", "Curry"),
                       Species = c("Human", "Human", "Dog"),
                       Hair_Color = c("Brown","Blonde", "Brown"),
                       Age = c(23, 24, 2.5),
                       Birth_Place = c("San Mateo", "Ventura", "Merced"))

haley.rules<- data.frame(Member = c("Haley", "Myself"))


# Conditional Statments
family_age<- function(){if (ebert_cobian$Age < 10){
return("Dog")
} else {
  return("Parents")
}}




# Fucntions

# For loops
prob_3<- data.frame(A = c(1:25))

for (i in length(prob_3$A)) {
  prob_3$B = (prob_3$A)^2
}

sum(prob_3$B)

install.packages(cars)




