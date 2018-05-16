##########################
##    Mike Newberry     
##    Programming Worksheet    
##    14 MAY 2018    
##########################

rm(list=ls())

library(ggplot2)
library(dplyr)
library(magrittr)

#set working 
setwd("C:/Users/workshop/Desktop/Comp Workshop 15MAY18/mers")

wnv <- read.csv('wnv.csv')

##Exercise: make hostogram of total cases per state per year

ggplot(data = wnv) +
  geom_histogram(mapping = aes(x = Total, fill = State)) +
  facet_wrap(~Year)
                 

##Exercise: plot the state-level and case burden histogram of the log
## Add to script

#log transform by placing log10 inside aes(x =...)
ggplot(data = wnv) +
  geom_histogram(mapping = aes(x = log10(Total), fill = State)) +
  facet_wrap(~Year)

#second way to log transform, add scale_x_log10 (adding a layer)
ggplot(data = wnv) +
  geom_histogram(mapping = aes(x = Total, fill = State)) +
  scale_x_log10()

#now calculate the raw case fatality rates in each state in each year

wnv$CFR <- (wnv$'Fatal')/(wnv$'Total')

ggplot(data = wnv) +
  geom_histogram(mapping = aes(x = CFR, fill = State)) +
  facet_wrap(~Year) #keep facet wrapping to represent each year separately

#Exercise: Now verify that the Total for each row is actually the sum of the febrile cases,
#neuroinvasive cases, and other cases

wnv$temptotal <- (wnv$EncephMen + wnv$Fever + wnv$Other) 

all(wnv$temptotal==wnv$Total)
#returns TRUE
#so all values in Total are consistent with component cases 

#Exercise: get annual case count for each state rounded down to nearest dozen

wnv$totalrounded <- wnv$Total %/% 12
wnv$rounderror <- wnv$Total %% 12

#now add total error across all states 
totalerror <- sum(wnv$rounderror)

#total error is returned as 1241

###################
#Functions

#the ratio of wnv$Encephmen / wnv$Total is the 
#Neuroinvasive Disease Rate (NDR)


wnv$NDR <- wnv$EncephMen/wnv$Total


#calculate mean and se (stdev/(n^(1/2))) of neuroinvasive rate 

#make a funciton for calculating the mean of NDR

NDR_mean <- function(x) {
  #computes the neuroinvasive disease rate mean
  #
  #arguments:
  #  x: vector of NDRs by state by year
  #
  #returns:the mean NDR 
  
  
  #computation
  m <- sum(x)/length(x)
  return(m)
}

NDRmean <- NDR_mean(wnv$NDR)

NDRmean
#returns 0.5643397

#now make a function for the standard error

NDR_se <- function(x) {
  #computes the neuroinvasive disease rate standard error
  #
  #arguments:
  #  x: vector of NDR ratesby state by year
  #
  #returns:the standard error of NDRs 
  
  
  #computation
  m <- sd(x)/(length(x)^(1/2))
  return(m)
}

NDRse <- NDR_se(wnv$NDR)

NDRse

#returns 0.01730868

wnv[wnv$State == "California",]

##create a function to subset states
NDR_subset <- function(data, state) {
  #computes the neuroinvasive disease rate standard error
  #
  #arguments:
  #  
  #
  #returns:
  
  dfsub <- data[data$State == state,]
  return(dfsub)
}
NDR_subset(wnv, "California")

#dfsub <- data[data$State %in% state,]
wnv[wnv$State %in% c("California","New York"),]



##create a function to subset states, modified to accept multiple states
NDR_subset2 <- function(data, state) {
  #computes the neuroinvasive disease rate standard error
  #
  #arguments:
  #  
  #
  #returns:
  
  dfsub <- data[data$State %in% state,]
  return(dfsub)
}
NDR_subset2(wnv, c("California", "Colorado"))

##Now combine the subsetting and NDR formulas to create a new function
#that gives the average NDR with one or more states

NDR_mean_state <- function(data, state) {
  #computes the neuroinvasive disease rate standard error
  #
  #arguments:
  #  
  #
  #returns:
  
  dfsub <- data[data$State %in% state,]
  dfsub <- sum(dfsub$NDR)/length(dfsub$NDR)
  
  return(dfsub)
}

NDR_mean_state(wnv, c("California", "Colorado","New York"))

#The neuroinvasive disease rate mean across CA, CO, and NY is 0.5594912

####
##Loops


##Example
#times <- seq(1:10)
#some.algorithm <- function(t){
#  y <- t*10 # a silly example
#}
#output <- c() # Question: What is this line doing? This line combines the outputs of the function as the sequence continues
#for(t in times){
#  output <- c(output, some.algorithm(t))
#}
#plot(times, output, type='p')



#Generate a sequence to run the funtions

#times <- seq(1999,2007, by = 1)



#statecases <- c()
#statecases <- rep(NA, times = length(unique(wnv$Year)))

Year <- unique(wnv$Year)
u_
n_States <- rep(NA, length(Year))
total_case <- rep(NA, length(Year))
Fatal <- rep(NA, length(Year))
CFR <- rep(NA, length(Year))


#indexing statement: [wnv$Year == Year[i]]

for(i in c(1:length(Year))){
  n_states[i] <- length(unique(wnv$State)(wnv$Year == Year$[i])}


