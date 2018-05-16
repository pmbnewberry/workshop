########################
##Data Wrangling Module
##15 May 2018
##Mike Newberry
########################

#rm(list=ls())
#set working 
setwd("C:/Users/workshop/Desktop/Comp Workshop/Wrangling")

library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)


#str_replace_all
#paste (base R)

#library tidyverse
#
#the Tibble adds a descriptor above each column of a table

##import the datasets with read_csv()

prism <- read_csv('climate.csv')
pop <- read_csv('pop.csv')
ld <- read_csv('lyme.csv')

#Tidy up Pop Data

#call original pop
pop

#subset the fips values that occur from the year 2000
pop %<>% select(fips,starts_with("pop2"))
pop

#Retain only the columns for fips, str_year, and size.
#this is to "remove columns that are not variables"
#also removes all the NAs
pop %<>% gather(starts_with("pop2"), key = "str_year", value = "size") %>% na.omit
pop

#Remove the pop in front of the year names, put the new year names
#into a new column named "year"
pop %<>% mutate(year = str_replace_all(str_year, "pop",""))
pop

#Turns the year column into integers
pop %<>% mutate(year = as.integer(year))
pop

#Remove the excess "0" padding in front of the fips codes
pop %<>% mutate(fips = str_replace_all(fips, "^0",""))
pop

#how would I remove columns size or str_year?
#can use select() and (-)the str_year, or select all other 2 columns

pop %<>% select(-str_year)
#pop %<>% select(fips, size, year)
pop
####################

#Lyme Disease Data
#convert this into tidy format

#call original data frame
ld

#turn the horizontal data (columns by year) into vertical data 
#input as rows. converts "glide" to "format"
#the "key" and "format" outputs are the new columns that contain the 
#formerly horizontal column years

ld %<>% gather(starts_with("Cases"), key = "str_year", value = "cases")
ld

#create a new column that removes "Cases" part of the str_year
#str_year is retained however, "year" is the new column
ld %<>% mutate(year = str_replace_all(str_year, "Cases", ""))
ld

#convert the year column numbers into integers
ld %<>% mutate(year = as.integer(year))
ld

#Rename the state and county names to be more readable
#this changes the name of existing columns, does not
#generate new columns

ld %<>% rename(state = STNAME, county = CTYNAME)
ld

#create a function that combines the state and county codes 
#into a single fips code
#to do this, county codes that do not have 3 digits must be padded
#with either one "0" or two "0"'s to ge tto 3 digits
#the code then combines the state and county into a fips output

fips.builder <- function(st,ct) {
  if (str_length(ct)==3) {
    fips <- paste(as.character(st), as.character(ct), sep = "") %>% as.integer
  }
  #if the county code already has 3 digits, simply paste the 
  #state and county codes together, with no space separating
  else if (str_length(ct) == 2) {
    fips <- paste(as.character(st), "0", as.character(ct), sep = "") %>% as.integer  
  #if the county code has 2 digits, add a single zero between state and county
    #in order to pad the fips code
  }
  else {
    fips <- paste(as.character(st), "00", as.character(ct), sep = "") %>% as.integer
  #if the code has one disit (the last option, so "else"), buffer with
    # two "0"'s
  }
  return(fips)
}

#
ld %<>% rowwise() %>% mutate(fips = fips.builder(STCODE, CTYCODE))  
#this process takes several seconds
#the flow of data is the mutate function creating the fips column
#then this is piped into the rowwise function
#the piping to ld is the last step

ld %<>% select(-c(STCODE,CTYCODE,str_year))
#Now remove the excess STCODE, CTYCODE, and str_year
ld

##Now join the Lyme disease and PRISM data
#use "inner_join" function in dplyr
#base R version of this is "merge"

ld.prism <- inner_join(ld,prism)
ld.prism

#now combine the demographic data (pop) with ld.prism

#need to change fips in pop to integer to allow them to join on 'fips' x 'fips'
pop %<>% mutate(fips = as.integer(fips))
pop

ld.prism.pop <- inner_join(ld.prism,pop)
ld.prism.pop

###################
##now use the dplyr function "summarize" to obtain summary information
##base R can do this with "aggregate"

#create 2 data frames: one determines # lyme cases each year
# and the other showing the average number of lyme cases in each state
#(averaged across county and year)

cases_by_year <- ld %>% ungroup %>% group_by(year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
cases_by_year

#the worst year nationwide was 2009

#now group by state and by year, descending order
cases_by_state <- ld.prism %>% ungroup %>% group_by(state, year) %>%
  summarize(total=sum(cases)) %>% arrange(desc(total))
cases_by_state

#the three most impacted states are Pennsylvania, New York, and
#New Jersey

###########################################
#Saving data frames as objects and files

#save(ld.prism.pop, file = "ld_prism_pop.rda")
#test <- load("ld_prism_pop")

#test

#now save as .csv
write_csv(ld.prism.pop,"ld_prism_pop.csv")

###########################################
##Visualizing geographic data

#get map data for US counties and states
county_map <- map_data("county")
state_map <- map_data("state")

## ##grouping the lyme disease, prism, and population data 
#by the respective fips codes
ag.fips <- group_by(ld.prism.pop,fips)

#summarizes all of the cases during the time period by fip codes
ld.16y<-summarize(ag.fips,all.cases=sum(cases))

#combine the summarized total cases from ld.16y with the original ld.prism.pop
#data set rows "state" and "county", joined by "fips"
ld.16y<-left_join(select(ld.prism.pop,c(state,county,fips)),ld.16y)

#remove any potential repeats of rows
ld.16y<-distinct(ld.16y)

#renames the state and county column names
ld.16y %<>% rename(region=state,subregion=county)

#for each row, remove the gratuitus "county" label
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")

#change the region(state) names to lower case
ld.16y$region<-tolower(ld.16y$region)

#change the subregion(county) names to lower case
ld.16y$subregion<-tolower(ld.16y$subregion)

#log transform the case data, put it in a new column
ld.16y %<>% mutate(log10cases=log10(1+all.cases))

#join the geographic coordinate data
map.ld.16y<-left_join(county_map,ld.16y)

##generate the map
ggplot(map.ld.16y)+geom_point(aes(long,lat,color=log10cases),size=0.1) +
  scale_colour_gradientn(colours=rev(rainbow(4)))
