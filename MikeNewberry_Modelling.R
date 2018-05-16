#############################################
###Modelling Module R Computational Workshop
##Mike Newberry
##15 May 2018
#############################################

rm(list=ls())
#set working 
setwd("C:/Users/workshop/Desktop/Comp Workshop/Wrangling")

#packages for this module specifically
library(tidyverse)
library(magrittr)
library(GGally)

#other packages
#library(dplyr) in tidyverse
library(stringr)
library(maptools)
library(ggmap)
library(maps)
library(ggplot2)
#library(purrr) in tidyverse



ld.prism.pop <- read_csv('wranglingresults.csv')
#names(ld.prism.pop)[1] <- "state"
#ld.prism.pop

#ggpairs(df,columns=c("x","y","z"))

ggpairs(ld.prism.pop, columns = c("prcp","avtemp","size","cases"))

ld.prism.pop %<>% mutate(log10size = log10(size),
                         log10cases = log10(1+cases))

#had to add 1 to the log10cases because otherwise the
#arithmetic returns negative infinity values

##Linear MOdel
#generate random numbers

set.seed(222)

#sample out 100 rows from the data, make plot1
plot1 <- sample_n(ld.prism.pop, 100)

#plot this using the 100 randomly selected rows
#also add the geom_smooth line, using the "lm" method instead of "loess"
ggplot(data = plot1) +
    geom_point(mapping= aes(x= prcp, y = avtemp)) +
    labs(x = 'Precipitation', y = 'Average Temp') +
    geom_smooth(mapping = aes(x = prcp, y = avtemp), method = "lm")

#now create a linear model object, function is "lm()" to fit linear model

lpp_model <- lm(avtemp ~ prcp, data = plot1)

lpp_summary = summary(lpp_model)
lpp_summary

####summary returns:
#Call:
#  lm(formula = avtemp ~ prcp, data = plot1)
#
#Residuals:
#  Min      1Q  Median      3Q     Max 
#-9.5195 -3.2711 -0.3451  2.1937 10.8865 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  6.02541    1.36916   4.401 2.75e-05 ***
#  prcp         0.00672    0.00136   4.942 3.19e-06 ***
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Residual standard error: 4.397 on 98 degrees of freedom
#Multiple R-squared:  0.1995,	Adjusted R-squared:  0.1913 
#F-statistic: 24.42 on 1 and 98 DF,  p-value: 3.19e-06
####################

#what is the slope of the best fit line of the average temp vs precipitation plot?
##The slope is 0.00672
#The slope p-value is 3.19e-06, so significantly different from 0.


###modelr package

#write a line of code, total US pop by year

#ld.prism.pop$year <- as.integer(ld.prism.pop$year)
#sort(unique(ld.prism.pop$year))

ld.prism.pop %>% group_by(year) %>% summarize(total = sum(size)) %>% ggplot(.) +
    geom_point(aes(x=year, y=total))

#Make grouped data frame and nested data frame
#create "by_state" from the data frame, group by state

by_state <- ld.prism.pop
by_state %<>% group_by(state)
by_state

#nest the new data frame
by_state %<>% nest
by_state

#Display GA county data
by_state$data[[10]]

#Make a function  that has a data frame as an argument and returns a linear model
#object that predicts size by year

linGrowth_model <- function(df){
  lm(size ~ year, data = df)
}


models <- purrr::map(by_state$data, linGrowth_model)

#can detatch packages as needed by detach("package:maps", unload=TRUE),
#in case another package also has a purr
# or use the (name of package)::(function) to specify which package to pull the 
#function from

#add column in by_state to hold the new model object, better than
#creating a new data frame

by_state %<>% 
  mutate(model = purrr::map(by_state$data, linGrowth_model))

#now the goal is to pass 2 arguments to a function, need map2

library(modelr)
by_state %<>% mutate(resids = map2(data, model, add_residuals))
by_state

#"resids" is in the tribble structure

get_slope <- function(model){
  model$coefficients[2]
}
by_state %<>% mutate(slope = purrr::map(model, get_slope))


#function that returns sum of absolute values
sum_resids <- function(x){
  sum(abs(x$resid))
}
by_state %<>% dplyr::mutate(totalResid = purrr::map(resids,sum_resids))

slopes <- unnest(by_state, slope)
totalResids <- unnest(by_state, totalResid)

#plot growth rate (slopes) for all states
slopes %>% ggplot(aes(state,slope))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot total residuals for all states now
totalResids %>% ggplot(aes(state,totalResid))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

################################
##now make by_state2 
#create "by_state" from the data frame, group by state

by_state2 <- ld.prism.pop
by_state2 %<>% group_by(state)
by_state2

#nest the new data frame
by_state2 %<>% nest
by_state2


models <- purrr::map(by_state2$data, linGrowth_model)

by_state2 %<>% 
  mutate(models = purrr::map(by_state$data, linGrowth_model))

####################################################
#function to take by_state2$data and return spearman correlation coefficient
runCor <- function(df){
  suppressWarnings(cor.test(df$cases,df$prcp,method="spearman")$models)
}

by_state2 %<>% mutate(spCor = purrr::map(data, runCor))

spCors <- unnest(by_state2,spCor)

spCors %<>% arrange(desc(spCor))

spCors$state <- factor(spCors$state, levels=unique(spCors$state))

ggplot(spCors,aes(state,spCor))+geom_point()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



