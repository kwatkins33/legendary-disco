#### Katherine Watkins
#### May 15, 2018
#### "Wrangling" Module Handout

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)

#setwd("/Users/katherinewatkins/Documents/UGA Grad School/Computational Modeling/Part 1")
# Don't need this when using the project folder.

# Task 1: Read in all three csv files as tibble data frames. For consistency with these notes, we’ll assign their dataframes to be called “ld”, “pop” and “prism”, resectively.
prism <- read_csv("climate.csv")
ld <- read_csv("lyme.csv")
pop <- read_csv("pop.csv")

# Task 2: By inspecting the ‘pop’ data, and talking with your neighbors and instructors, articulate in which
# way(s) these data fail to conform to the tidy data format?
# The pop file has the years in different columns rather than in one column for simple comparison - the time variable is in multiple columns instead of one
# 

pop %<>% select(fips,starts_with("pop2"))
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit
pop %<>% mutate(year=str_replace_all(str_year,"pop",""))
pop %<>% mutate(year=as.integer(year))
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))
pop %<>% mutate(fips=as.integer(fips))

# How would you do remove state-wide summaries at this stage?
# Maybe the FIPS code tells a state-wide summary and those could be removed
# Also we could remove the character column ‘str_year’ now that we’ve converted year to integer (How would you do this?)
# use na.omit

# Task 4: Write a code chunk to convert the Lyme disease data to tidy data format.

ld %<>% gather(starts_with("Cases"),key="str_year",value="cases") 
ld %<>% mutate(year=str_replace_all(str_year,"Cases",""))
ld %<>% mutate(year=as.integer(year))
ld %<>% rename(state=STNAME,county=CTYNAME)

fips.builder<-function(st,ct){ if (str_length(ct)==3){
  fips<-paste(as.character(st),as.character(ct),sep="") %>% as.integer }
  else if (str_length(ct)==2){ 
    fips<-paste(as.character(st),"0",as.character(ct),sep="") %>% as.integer
  }
  else {
    fips<-paste(as.character(st),"00",as.character(ct),sep="") %>% as.integer
  }
  return(fips) 
}

ld %<>% rowwise() %>% mutate(fips=fips.builder(STCODE,CTYCODE)) 
# takes about 10 seconds 
ld %<>% select(-c(STCODE,CTYCODE,str_year))

# Task 5: Join the Lyme disease data frame and PRISM data frame together to form a new data frame, and in such a way that it retains county-year combinations for which we have both disease and clime data.

ld.prism <- inner_join(ld,prism)

# Task 6: Write a line of code to additionally combine the demographic data with the Lyme disease and climate
# data.

ld.prism.pop <- inner_join(ld.prism,pop)

# Task 7: Write two lines of code that create two new data frames: 
#(1) to determine how many cases of Lyme disease were reported each year, 

cases_by_year <- ld %>% ungroup %>% group_by(year) %>% 
  summarize(total=sum(cases)) %>% arrange(desc(total))

#(2) the average number of cases in each state - averaged across county and year. What was the worst year? Which three states have been most impacted on average?
# The worst year in the US was 2009 (line 72-73 give this)
# The worst year by state was 2008 for New York (lines 82-83)

#cases_by_state <- ld.prism.pop %>% ungroup %>% group_by(state,county,year) %>%
#  summarize(avCases=mean(cases)) %>% arrange(desc(avCases))

#cases_by_state <- ld.prism.pop %>% ungroup %>% group_by(state,year) %>%
#  summarize(total=sum(cases)) %>% arrange(desc(total))

cases_by_state <- ld.prism.pop %>% ungroup %>% group_by(state) %>%
  summarize(avCases=mean(cases)) %>% arrange(desc(avCases))
# The states impacted the most have been Connecticut, Massachusetts, and Delaware (lines 85-86)

# Task 8: use save to create an Rda file of the data frame and use write_csv to create a csv file of the same (remember to try ?save and ?write_csv in the console if you’re not sure how these functions work).
save(ld.prism.pop,file='ld_prism_pop.Rda')

write_csv(ld.prism.pop, "ld_prism_pop.csv")

# Task 9: Add annotations to the following lines of code with comment lines to explain what they are achieving. 
# Note: in the code line with “group_by(ld.prism.pop)” you need to replace the data frame in parentheses with the name of your data frame in which you combined Lyme disease, climate and demography data (Task 6)



county_map <- map_data("county")
state_map <- map_data("state")

ag.fips <- group_by(ld.prism.pop,fips)
# groups the ld.prism.pop data frame by fips

ld.16y<-summarize(ag.fips,all.cases=sum(cases))
# summarizes cases of lyme disease across all years

ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)
# joins ld.prism.pop with ld.16y by the fips code and shows only state, county, fips, and cases

ld.16y<-distinct(ld.16y)
# removes duplicate rows of data

ld.16y %<>% rename(region=state,subregion=county)
# renamed the state column region and the county column subregion

ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
# removes when the county column has "county" in it, replaces Clarke county with just Clarke

ld.16y$region<-tolower(ld.16y$region)
# makes the region lowercased - lowercases the state name

ld.16y$subregion<-tolower(ld.16y$subregion)
# makes the subregion lowercased

ld.16y %<>% mutate(log10cases=log10(1+all.cases))
# adds a column that log transforms the cases for each county

map.ld.16y<-left_join(county_map,ld.16y)
# joins the ld.16y df with the couty_map df by the region and subregion

ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
# maps out the log10(cases) of lyme disease by latitude and longitude


