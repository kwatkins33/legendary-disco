#### Katherine Watkins
#### May 15th, 2018
#### Handout 4 for Computational Modeling - Data Modeling

library(tidyverse) 
library(magrittr) 
library(GGally)
# After this, I kinda abandoned keeping all the libraries at the top to help with my understanding the flow of the assignment.

#setwd("/Users/katherinewatkins/Documents/UGA Grad School/Computational Modeling/Part 1")
# Don't need this anymore because it's in the project folder
getwd()

#Task 1: Using either read_csv (note: the underscore version, not read.csv) or load, import the data set you put together in the module on ‘Data wrangling in R’.

ld.prism.pop <- read_csv("ld_prism_pop.csv")
sort(names(ld.prism.pop))
#checking to see I have the same things as my neighbors by making sure the names of the columns are all the same

# Task 2: Use the ggpairs function to obtain a 4x4 summary plot of precipitation (prcp), average temperature (avtemp), population size (size), number of Lyme disease cases (cases). Note: it may take several seconds for this plot to appear as there are nearly 50,000 data points.
ggpairs(ld.prism.pop,columns=c("prcp","avtemp","size","cases"))

# Task 3: Create two new columns for log10(size) and log10(cases+1) and substitute these for the original size and cases supplied when you recreate the ggpairs plot. Why do we add 1 to the number of cases?
library(dplyr)
ld.prism.pop %<>% mutate(log10size=log10(size))
ggpairs(ld.prism.pop,columns = c("prcp","avtemp","log10size","cases"))

ld.prism.pop %<>% mutate(log10cases=log10(cases+1))
ggpairs(ld.prism.pop,columns = c("prcp","avtemp","log10size","log10cases"))
#naming the column "log10cases+1" resulted in an error, so it was named "log10cases"
# +1 is added becauses you can't take the log of 0

# A simple linear model
# Task 4: Using set.seed(222) for reproducibility, create a new data frame to be a random sample (n=100 rows) of the full data frame and plot precipitation (x-axis) vs average temperature (y-axis).
library(ggplot2)
set.seed(222)
smallsample <- ld.prism.pop %>% sample_n(100)
myPlot <- ggplot(smallsample) + geom_point(aes(x=prcp,y=avtemp))+
  # Task 5: Add the best straight line to the plot using geom_smooth.
  geom_smooth(aes(prcp,avtemp),method='lm')
myPlot

# Task 6: Create a linear model (lm) object with a call like myModel <- lm(y ~ x, data = myData) for the subsetted data, where y=avtemp and x=prcp. In addition, view the summary with a call along the lines of summary(myModel)
myModel <- lm(avtemp~prcp,data=smallsample)
myModel
summary(myModel)

# Task 7: What is the slope of the line you plotted in Task 5, and is the slope significantly different from 0 (p<0.05)?
# From the summary table, the slope is [2,1] which is 0.00672 and it is significant because the p value < 0.05 (3.19e-06) found in [2,4]

# Task 8: Write a single line of code to generate a ggplot of total population size by year.
ld.prism.pop %>% group_by(year) %>% summarize(total=sum(size)) %>% 
  ggplot(.)+geom_point(aes(x=year,y=total))

library(modelr)

# Task 9: Create a data frame called “by_state” from the main data frame, that groups by state, and inspect it.
by_state <- ld.prism.pop %>% ungroup %>% group_by(state)

# Task 10: Next, update this new data frame so that it is nested (simply pass it to nest). Again, inspect the data frame by typing its name in the console so see how things changed.
by_state %<>% nest 
by_state

# Task 11: Display the Georgia data in the console window.
# Georgia data displayed by entering: by_state$data[[10]] in the console

# Task 12: Write a function that takes a data frame as its argument and returns a linear model object that predicts size by year.
linGrowth_model <- function(by_state){
  lm(size~year, data=by_state)
}

models <- purrr::map(by_state$data, linGrowth_model)
models

detach("package:maps", unload=TRUE)
#maps::map
#purrr::map

# Task 13: Add a column to the by_state dataframe, where each row (state) has its own model object.
by_state %<>% mutate(model = map(data, linGrowth_model))
by_state
# shows the new column "model" has been added
by_state %<>% mutate(resids = map2(data, model, add_residuals))
by_state
# shows that the new column "resids" has been added

# Task 14: Run these commands and inspect “resids”. What is the structure of “resids”?
# resids is <list> and a tibble

# Task 15: Write a function that accepts an object of the type in the resids list, and returns a sum of the absolute values, i.e. ignoring sign: abs(3)+abs(-2)=5. Use the function to add a column called totalResid to by_state that provides the total size of residuals summed over counties and years.
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% mutate(totalResid = map(resids,sum_resids))
by_state

# Task 16: Write a function that accepts a linear model and returns the slope (model M has slope M$coefficients[2]) and then use this function to create a new column called slope in the by_state data frame, that is the slope for each state.
get_slope <- function(model){ 
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))

#un nest data structures slope in data frame by_state
slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)
by_state

# Task 17: Plot the growth rate (slope value) for all states.
slopes %>% ggplot(aes(state,slope))+geom_point()+ 
# puts the labels vertically so they don't run into each other
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Task 18: Plot the total resisduals for all states.
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Task 19: Repeat Tasks 9 and 10 using a different data frame name, by_state2.
# Task 9: Create a data frame called “by_state2” from the main data frame, that groups by state, and inspect it.
by_state2 <- ld.prism.pop %>% ungroup %>% group_by(state)
by_state2
# Task 10: Next, update this new data frame so that it is nested (simply pass it to nest). Again, inspect the data frame by typing its name in the console so see how things changed.
by_state2 %<>% nest
by_state2
# Task 20: Write a function that accepts an element of the by_state2$data list-column and returns the
# spearman correlation coefficient between Lyme disease cases and precipitation
runCor <- function(df){ 
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$estimate)
}
by_state2 %<>% mutate(spCor = purrr::map(data, runCor))
by_state2

spCors <- unnest(by_state2,spCor)
spCors %<>% arrange(desc(spCor))
spCors

spCors$state <- factor(spCors$state, levels=unique(spCors$state)) 
ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))













