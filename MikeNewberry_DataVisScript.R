####Data Visualization Exercise###
##May 14 2018##
##Mike Newberry##


rm(list=ls())
#set working 
setwd("C:/Users/workshop/Desktop/Comp Workshop/mers")


library(ggplot2)
library(dplyr)

mers <- read.csv('cases.csv')

#look at the data
head(mers)

#what class is the onset?
class(mers$onset)

#fix some of the data
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]

#convert dates to a continuous varialbe
#install and load lubridate
library(lubridate)
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)

#what class is the onset2?
class(mers$onset2)
#it is Date

##What is the earliest onset date?
day0 <- min(na.omit(mers$onset2))
#date returned is 2012-03-21

## we use na.omit in order to prevent blank dates from
#being returned as the first day.
#if we do not do this, an "na" value will be returned as 
#the earliest day. 

#now make a new value for the epidemic day of each case

mers$epi.day <- as.numeric(mers$onset2 - day0)
mers$epi.day

## the as.numeric ensures that the returned value is a 
## numeric value instead of a date value

##now use ggplot() to visualize the mers data

ggplot(data = mers) +
  geom_bar(mapping = aes(x = epi.day)) +
  labs(x = 'Epidemic day', y = 'Case count', title ='Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##If you fail to use the "+", then elements of the chart are not created

##now represent our epidemic day data by country

ggplot(data = mers) +
  geom_bar(mapping = aes(x = epi.day, fill = country)) +
  labs(x = 'Epidemic day', y = 'Case count', title ='Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

## Now modify the epidemic curve with the fill position, choose a 
##new variable to represent with the bar color

#although the postition = 'fill' does not represent anything new, as there are not multiple types of cases to stack

ggplot(data = mers) +
  geom_bar(mapping = aes(x = epi.day, position = 'fill')) +
  labs(x = 'Epidemic day', y = 'Case count', title ='Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##now alter the country/epidmeic day graph with a coordinate flip

ggplot(data = mers) +
  geom_bar(mapping = aes(x = epi.day, fill = country)) +
  labs(x = 'Epidemic day', y = 'Case count', title ='Global count of MERS cases by date of symptom onset',
       caption = "Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") +
        coord_flip()

##Univariate Plots
##Look at Infectious period

#calculate the number of raw infectious period
mers$infectious.period <- mers$hospitalized2 - mers$onset2

class(mers$infectious.period)

#the data type for infectious.period is "difftime"
#this means that this new data column exists only as a difference of 2 values
#and we want there to be a singgle integer

mers$infectious.period <- as.numeric(mers$infectious.period, units = "days")
#in additon to converting these difftime to numbers, assign "days" as the column's unit
head(mers)

ggplot(data = mers) +
  geom_histogram(aes(x = infectious.period)) +
  labs(x = 'Infectious period', y = 'Frequency', title = 'Distribution of Calculated MERS infectious period', 
  caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#The plot shows negative infectious periods because many infections were 
##acquired in hospitals, hence hospitalizations preceed onset of symptoms


#goal now is to set these negative infectious period values to zero

mers$infectious.period2 <- ifelse(mers$infectious.period < 0, 0, mers$infectious.period)

mers$infectious.period2

# now plot again
ggplot(data = mers) +
  geom_histogram(aes(x = infectious.period2)) +
  labs(x = 'Infectious period', y = 'Frequency', 
       title = 'Distribution of calculated MERS infectious period (positive values only)',
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")


##Look at the density plot

ggplot(data = mers) + 
  geom_density(mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', y = 'Frequency',
       titel = 'Probability density for MERS infectious period (positive values only)', 
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Now make an area plot

ggplot(data = mers) +
  geom_area(stat = 'bin', mapping = aes(x = infectious.period2)) +
  labs(x = 'Infectious period', y = 'Frequency',
      title = 'Area plot for MERS infectious period (positive values only)', 
      caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##Now experiment with other univariate plot types (geom_dotplot, geom_bar)
ggplot(data = mers) +
  geom_dotplot(mapping = aes(x = infectious.period2)) + 
  labs(x = 'Infectious period', y = 'Frequency',
       titel = 'Probability density for MERS infectious period (positive values only)', 
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##now try geom_bar
ggplot(data = mers) +
  geom_bar(mapping = aes(x = infectious.period2)) + 
  labs(x = 'Infectious period', y = 'Frequency',
       title = 'Probability density for MERS infectious period (positive values only)', 
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
######################
##Now moving on to Bivariate plots

ggplot(data = mers) +
  geom_point(mapping = aes(x = epi.day, y = infectious.period2)) +
    labs(x = 'Epidemic day', y = 'Infectious period',
         title = 'Infectious period over the course of MERS epidemic', 
         caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

##There does not seem to be a strong decrease in infectious period as the epidemic day progresses

## Now add a curve fit to see if there is evidence of societal learning

ggplot(data = mers) +
  geom_point(mapping = aes(x = epi.day, y = infectious.period2)) +
  geom_smooth(mapping = aes(x= epi.day, y = infectious.period2)) +
  labs(x = 'Epidemic day', y = 'Infectious period',
       title = 'Infectious period over the course of MERS epidemic', 
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
  
  
#Now break the smooth fit by country. HoW?
ggplot(data = mers) +
    geom_bar(mapping = aes(x = epi.day)) +
    geom_smooth(mapping = aes(x= epi.day, y = infectious.period2, color = country), method = "loess", se = FALSE) +
    labs(x = 'Epidemic day', y = 'Infectious period',
    title = 'Infectious period over the course of MERS epidemic', 
    caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")  

##There appears to be some degree of societal learning in Qatar and the UAE, but other smooth trendlines are sinusoidal or flat
#    facet_wrap(~country) +
##########Faceting#########

#use facet_wrap() and facet_grid

ggplot(data = mers, mapping = aes( epi.day, y = infectious.period2)) +
  geom_point(mapping = aes(color = country)) +
  facet_wrap(~country) +
  scale_y_continuous(limits = c(0, 50)) +
    labs(x = 'Epidemic day', y = 'Infectious period', title = 'MERS infectious period (positive values only) over time',
         caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Now clean up the data to select for high incidence countries and to exclude vague gender data
#subset the data to accomplish this

ggplot(data = subset(mers, gender %in% c('M', 'F') & country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea', 'UAE'))) +
  geom_point(mapping = aes(epi.day, y = infectious.period2, color = country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limit = c(0, 50)) +
  labs(x = 'Epidemic day', y = 'Infectious period',
       title = 'MERS infectious period by gender and country',
       caption = "Data from https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

#Exercise: Study variation in the case fatality rate over time and across countries  
#So assign a new vriable whether or not a case ended in fatality
#Look to aggregate the values, perhaps with a bar plot

mers$fatality <- mers$outcome 

library(plotly)
