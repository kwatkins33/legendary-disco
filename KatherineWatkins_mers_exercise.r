
#### handout 1
#### Katherine Watkins

#imported dataset

# renamed cases mers

setwd("/Users/katherinewatkins/Documents/UGA Grad School/Computational Modeling/Part 1/mers")
getwd()
list.files()
mers <- read_csv("cases.csv")
# to confirm renaming
head(mers)

class(mers$onset)
# returns [1] "character"

# errors corrected
mers$hospitalized[890]<-c('2015-02-20')
mers<-mers[-471,]

# downloaded lubridate package with install.packages("lubridate")

library(lubridate)

mers$onset2<-ymd(mers$onset)
mers$hospitalized2<-ymd(mers$hospitalized)
# warning message received: 5 failed to parse.

class(mers$onset2)
# returns [1] "Date"
day0<-min(na.omit(mers$onset2))

# Question. Why do we use the function na.omit? What happens if we neglect this command?
#day0<-min((mers$onset2))
# na.omit allows for some manipulation based on the onset day of the epidemic, neglecting na.omit keeps rows where there are no values (missing data)


mers$epi.day<-as.numeric(mers$onset2-day0)

# Question: what purpose does the command as.numeric serve?
# it creates a new number which is the days since the onset - day0 for each value in the onset column

library(ggplot2)

ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day))+
  labs(x='Epidemic day',y='Case count', title='Global count of MERS cases by date of symptom onset',caption="Data from:http://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# Warning message received: Removed 535 rows containing non-finite values (stat_count).
# Not ending each line when "+" prevented each line to be built upon and resulted in just a grey box - perhaps the first layer only
# Removing the "+" before labs caused the labs line to run as its own line of code giving what each variable is along with the title, captions, at labels

# Distinguishing between countries done using aesthetic fill.

ggplot(data=mers)+
  geom_bar(mapping = aes(x=epi.day,fill=country))+
  labs(x='Epidemic day',y='Case count',title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

# Warning messages received:
# 1: Removed 535 rows containing non-finite values (stat_count). 
# 2: position_stack requires non-overlapping x intervals

# Exercise. Modify the epidemic curve using the argument position="fill". What does this plot show?
ggplot(data=mers)+
  geom_bar(mapping = aes(x=epi.day,fill=country),position="fill")+
  labs(x='Epidemic day',y='Case count',title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# I have learned that to use the position="fill" function, the aes bracket must be closed, but position="fill" must go within the mapping bracket. 
# This argument causes the bars to be filled to 1.00 for each country during each epidemic day to represent that there is no day during the outbreak where more than one country had a case of MERS, it would have been represented as multiple case counts, splitting either 0.50 and 0.50 or 0.33, and so on.

# Exercise. Another way to modify a bar plot is to change the coordinates. This can be done by "adding" coord_flip() and coord_polar() to the plot. What does this plot show?
ggplot(data=mers)+
  geom_bar(mapping = aes(x=epi.day,fill=country))+coord_polar()+
  labs(x='Epidemic day',y='Case count',title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# making the coordinates polar shows the epidemic curve in a circle which is not useful for this type of data.
ggplot(data=mers)+
  geom_bar(mapping = aes(x=epi.day,fill=country))+coord_flip()+
  labs(x='Epidemic day',y='Case count',title='Global count of MERS cases by date of symptom onset',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# this function caused the x and y axes to flip.

## Univariate Plots
mers$infectious.period<-mers$hospitalized2-mers$onset2
class(mers$infectious.period)
# returned: [1] "difftime"
mers$infectious.period<-as.numeric(mers$infectious.period,units="days")
ggplot(data=mers)+
  geom_histogram(aes(x=infectious.period))+
  labs(x='Infectious period',y='Frequency',title='Distribution of calculated MERS infectious period',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# returned: `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
#Warning message:
#  Removed 727 rows containing non-finite values (stat_bin).

mers$infectious.period2<-ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers)+
  geom_histogram(aes(x=infectious.period2))+
  labs(x='Infectious period',y='Frequency',title='Distribution of calculated MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Exercise. Investigate the frequency of hospital-acquired infections of MERS
#density plot
ggplot(data=mers)+
  geom_density(mapping=aes(x=infectious.period2))+
  labs(x='Infectious period',y='Frequency', title='Probability density for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#area plot
ggplot(data=mers)+
  geom_area(stat='bin',mapping=aes(x=infectious.period2))+
  labs(x='Infectious period',y='Frequency',title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
#Exercise. Use the infectious period data calculated in mers$infectious.period2 to experiment with other univariate plot types like geom_dotplot and geom_bar
ggplot(data=mers)+
  geom_dotplot(mapping=aes(x=infectious.period2))+
  labs(x='Infectious period',y='Frequency',title='Dot plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# The dots go past 1.00 on the y axis
ggplot(data=mers)+
  geom_bar(mapping=aes(x=infectious.period2))+
  labs(x='Infectious period',y='Frequency',title='Bar plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
# Both plots look roughly like the the standard histogram though the dot plot represents the data as dots while the geom_bar represents the data as a bar chart

## Bivariate plots
#Exercise. Use our corrected infectious period variable (infectious.period2) to study the change in the infectious period over the course of the MERS epidemic.
# Figure out what kind of two variable plot to use = maybe geom_point()
ggplot(data=mers)+
  geom_bar(mapping = aes(x=infectious.period2))+
  labs(x='Corrected infectious period', y='Case count', title='Count of MERS cases throughout infectious period', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')
  # As the infectious period goes on, there are fewer cases

#Exercise. In data from many outbreaks it can be seen that there is a kind of societal learning. When the infection first emerges it is not quickly recognized, public health resources have not been mobilized, it is not known what symptoms are diagnostic, how to treat, etc. But, quickly, this information is collected and the outbreak is contained. Is there evidence of this kind of societal learning in the mers data. Add a curve fit using geom_smooth to explore this question. Hint: We solved using the loess method because the default smoother (gam) failed.
ggplot(data=mers)+
  geom_smooth(method=loess, size=1.5, mapping=aes(x=epi.day, y=infectious.period2))+
  labs(x='Epi day', y='Corrected infectious period', title='Corrected infectious period over epi days', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

#Exercise. Plot infectious.period2 against time, as before, but this time add a separate smooth fit for each country.
ggplot(data=mers)+
  geom_smooth(method=loess, size=1.5, mapping=aes(x=epi.day, y=infectious.period2, color=country))+
  labs(x='Epi day', y='Corrected infectious period', title='Corrected infectious period over epi days', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

# Faceting
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) + geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period', title='MERS infectious period (positive values only) over time', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

ggplot(data=subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping = aes(x=epi.day, y=infectious.period2, color=country)) +
  facet_grid(gender ~ country)+
  scale_y_continuous(limits = c(0,50))+
    labs(x='Epidemic day', y='Infectious period', title='MERS infectious period by gender and country', caption='Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv')

# Exercise. Study variation in the case fatality rate (the fraction of cases that end in death) over time and across countries.


# More





