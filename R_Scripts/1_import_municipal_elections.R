#load libraries
library(here)
library(tidyverse)
#load data
load(file=here("data/elec.RData"))
#rename
elections<-table
#remove table
rm(table)
#Check crosstabls of provinces by elections
table(elections$province, elections$election_year)

elections %>% 
  filter(election_year>2000)->elections
#Repeat
table(elections$province, elections$election_year)


#newspaper closures
#This includes mergers that reduced the number of newspapers in a municipality by 1
daily_newspaper_closed<-rep(0, nrow(elections))
community_weekly_newspaper_closed<-rep(0, nrow(elections))
#newspaper reduced service
#This includes going from 7 days to 6, transitioning to a community from a paid paper
daily_newspaper_reduced_service<-rep(0, nrow(elections))
community_weekly_newspaper_reduced_service<-rep(0, nrow(elections))

#To ignore
#university newspape
#Anything that opened and closed between elections (i.e. less than three years)
#Anything in 
# Montreal
#Quebec City
#Toronto
#Ottawa
# Winnipeg
# REgina
# Saskatchewan
# Victoria
# Vancouver
# Edmonton 
# Calgary
# Halifax
# Saint John
# Moncton
# Frederiction


##### Sample code for closure #### 
#search for the municipality
#Find the municipality you are looking for in two ways
#Look for keywords and partial matches

elections %>% 
  filter(str_detect(municipality, "Moose"))
#Look for exact matches
elections %>% 
  filter(municipality=="Moose Jaw")
#
#Assume a newspaper closed in Moose Jaw in 2010
elections %>% 
  mutate(newspaper_closed=case_when(
    municipality=="Moose Jaw" & election_year > 2009 ~ 1
  ))->elections

#Provide a check
elections %>% 
  filter(municipality=="Moose Jaw" & election_year> 2009) %>% 
  select(municipality, election_year, newspaper_closed)


