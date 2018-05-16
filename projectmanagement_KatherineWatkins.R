
#### Katherine Watkins
#### handout 1


library(lubridate)
library(ggplot2)

#Exercise(1). Copy the MERS data file cases.csv and paste it into your working directory.
setwd("/Users/katherinewatkins/legendary-disco")
getwd()
list.files()

#Exercise(2). Create a new script following the prototype we introduced. Your script should load
#the MERS data and make a plot.
mers <- read.csv("cases.csv")
# to confirm renaming
head(mers)

class(mers$onset)
# returns [1] "character"

# errors corrected
mers$hospitalized[890]<-c('2015-02-20')
mers<-mers[-471,]

# downloaded lubridate package with install.packages("lubridate")

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

ggplot(data=mers)+
  geom_bar(mapping=aes(x=epi.day))+
  labs(x='Epidemic day',y='Case count', title='Global count of MERS cases by date of symptom onset',caption="Data from:http://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
