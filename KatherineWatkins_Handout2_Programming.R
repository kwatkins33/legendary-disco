#### Katherine Watkins
#### May 14th, 2018
#### Handout 2 for Computational Modeling - Programming
#### wnv.csv from the CDC and US Geological survey

#Exercise. Write a script to load the West Nile virus data and use ggplot to create a histogram for the total number of cases in each state in each year.
#Follow the format of the prototypical script advocated in the presentation: Header, Load Packages, Declare Functions, Load Data, Perform Analysis.

# Load Packages
library(lubridate)
library(ggplot2)
library(tidyverse)

# Create a working directory containing the script with the data file in the same folder.

#For Windows
#setwd("C:/Users/workshop/Documents/wnv")
#For Mac
#setwd("/Users/katherinewatkins/Documents/UGA Grad School/Computational Modeling/Part 1/wnv")
getwd()
list.files()
wnv <- read_csv("wnv.csv")
head(wnv)
# Double checking included so I could figure out what Windows was calling the file, it came up just "wnv" but didn't recognize it as such.

# Scripts
# Exercise(1). Write a script to load the West Nile virus data and use ggplot to create a histogram for the total number of cases in each state in each year. Follow the format of the prototypical script advocated in the presentation: Header, Load Packages, Declare Functions, Load Data, Perform Analysis.
# Don't overcomplicate things
ggplot(data=wnv,aes(x=wnv$Total))+
  geom_histogram()
  
# Exercise(2). The state-level and case burden is evidently highly skewed. Plot a histogram for the logarithm of the number of cases. Do this two different ways.
# one way to do this: log transform the data
wnv$logtotal <- log(wnv$Total)
ggplot(data=wnv,aes(x=wnv$logtotal))+
  geom_histogram()

# another way to do this: log transform the scale
ggplot(data=wnv,aes(x=log(wnv$Total)))+
  geom_histogram()

# Exercise(3). Use arithmetic operators to calculate the raw case fatality rate (CFR) in each state in each year. Plot a histogram of the calcated CFRs.
cfr <- wnv$Fatal/wnv$Total

ggplot(data=wnv,aes(x=cfr, fill=State))+
  geom_histogram()+
  facet_wrap(~ Year)+
  labs(x='Case fatality rate', y='Frequency',title='Number of WNV fatalities by state',caption='Data from:https://diseasemaps.usgs.gov/')

# Exercise(4). Use arithmetic operators, logical operators, and the function sum to verify that the variable Total is simply the sum of the number of febrile cases, neuroinvasive cases, and other cases.
head(wnv)
wnv$var.total <- sum(wnv$EncephMen + wnv$Fever + wnv$Other - wnv$Total)
wnv
# because new column var.total is 0, the variable Total is the sum of the cases that are febrile, neuroinvasive, and other

# Exercise(5). Use modular arithmetic to provide an annual case count for each state rounded (down) to the nearest dozen. Use modular arithmetic to extract the rounding errors associated with this calculate, then add the errors to obtain the total error.
rounded.total <- (wnv$Total %/% 12) * 12
# This gives the total cases for each state rounded down to the nearest 12 (dozen)
rounded.total
total.rounding.error <- sum(wnv$Total %% 12)
# This gives a sum of all of the rounding errors for each state in each year which is 1241
total.rounding.error

# Functions
# Exercise(6). Write a function to calculate the mean and standard error (standard deviation divided by the square root of the sample size) of the neuroinvasive disease rate for all the states in a given list and given set of years. Follow the Google R style and remember to place the function near the top of your script. Use your function to calculate the average severe disease rate in California, Colorado, and New York.
  # Part 1: Calculate mean and standard error of neuroinvasive disease rate for all states in a given list and given set of years

mean_std_error <- function(x){
  # Computes the mean and the standard error of a sample
  #
  # Args:
  #   x: vector of values that will be averaged and the standard error will be determined for
  #
  # Returns:
  #   a combination of the mean and the standard error 
  
  # calculates the mean of the sample
  m <- mean(x)
  
  # calculates the standard deviation of the sample
  std_dev <- sd(x)
  
  # calculates the square root of the length of the sample
  sq_rt <- sqrt(length(x))
  
  # calculates the standard error, the standard deviation divided by the square root
  # Computation
  s_e <- std_dev/sq_rt
  return(c(m,s_e))
}

neuroinvas_rate <- function(x){
  # Computes the rate of neuroinvasive disease for states for each year 
  niv <- x$EncephMen
  tot <- x$Total
  niv_r <- niv/tot
  return(niv_r)
# This could've also been accomplished by making a new column in the wnv file as done in the first few exercises.
# Maybe that would make this simpler so there are fewer functions within the function. 
# This probably won't end up being one function, it will be a series of functions that will need to be executed one after another.
  }

states <- c("California", "Colorado", "New York")
  
state_neuroinvas_rate <- function(x,states,years){
  x1 <- x[x$Year %in% years,]
  x2 <- x1[x1$State %in% states,]
  nir <- neuroinvas_rate(x2)
#This calculates the rate of neuroinvasive disease for the selected states states in the selected years
  mean_std_error(nir) 
  
}
state_neuroinvas_rate(wnv,states,years)
#returns state_neuroinvas_rate(wnv,states,years)
# rearranging so return is outside of the nested functions, but inside the overall function results in the response: Error in View : object 'm' not found
# It seems like I won't be able to do this in one function, but I think I can do this using multiple funcions

#Exercise(7). Use ggplot to show the neurovinvasive disease rate for these states as a bar graph with error bars to show the standard deviation.

#Exercise(8). Use your function and ggplot to show the neurovinvasive disease rate for all states.

#PIPES
#Exercise(9). Use pipes to produce the same plots without using your function.

#CONTROL OF FLOW
#Exercise(10). Choose a longitude to designate the “center” of the country. Use the function ifelse to assign each state to an “Eastern” region or a “Western” region.
long <- wnv$Longitude
long.sorter <- function(long){
  if(long >= 95.0){
    return("Western")
  } else {
    return("Eastern")
  }
}

#Exercise(11). Analyse your data to compare case fatality rates in the Eastern vs. Weestern United States.

#Exercise(12). Is there evidence for a latitudinal gradient in case fatality rate?

# Loops
times <- seq(1:10)
some.algorithm <- function(t){ y <- t*10 # a silly example
}
output <- c() 
# Question: What is this line doing? for(t in times)
# I believe it should concatonate the result, but it is empty, when it is c(t), it gives information on the function
# in general, c() creates a string and is necessary when giving multiple results separated by a comma because the return function will only work with one value
for(t in times){
  output <- c(output, some.algorithm(t)) 
}
plot(times, output, type='p')
#Exercise(13). Loop over all the years in the WNV data set (1999-2007) and compute the following statistics: Total number of states reporting cases, total number of reported cases, total number of fatalities, and case fatality rate. Produce some plots to explore how these quantities change over and with respect to each other. Explain what you have learned or suspect based on these plots.
years <- seq(1997:2007)
wnv$cfr <- wnv$Fatal/wnv$Total
# finally making a cfr column to make this easier
totals <- function(y){
  tot.states <- sum(wnv$State)
  tot.cases <- sum(wnv$Total)
  tot.fatal <- sum(wnv$Fatal)
  tot.cfr <- sum(wnv$cfr)
}
tot.output <- c()
for (y in years) {
  tot.output <- c(tot.output, totals(y))
}
tot.output
# Error Message: Error in sum(wnv$State) : invalid 'type' (character) of argument
# Not sure how to fix this error.

#Exercise(14). How does your choice of longitudinal breakpoint matter to the evidence for a geo- graphic difference in case fatality rate? Combine conditional execution and looping to study the difference in case fatality rate over a range of breakpoints. What is the “best” longitude for separating case fatality rate in the East vs. West?

# Using Help
#Exercise(15). We may interpret raw case fatality rate (i.e. ratio of the number of deaths, x, to number of infections, n) as a realization from a binomial process with n trials and x “successes” generated by an unknown rate parameter p. This p may be the quantity truly of interest (for instance, if we wish to ask if the case fatality rate in California is significantly different from the case fatality rate in Colorado. In R, the estimated rate and its confidence interval can be obtained using the function prop.test for testing equal proportions. Use the help to determine the proper usage of prop.test and calculate confidence intervals for the case fatality rates in all states for which there have been reported cases of WNV.

#Exercise(16). The “See Also” section of the help for prop.test states that a different function might be useful for an exact test of our hypotheses. Use the help to identify what this function is, learn how to use it, and compare the differences.


