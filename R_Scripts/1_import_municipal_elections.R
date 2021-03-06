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


#### Set up empty variables to be coded  ####
#newspaper closures
#This includes mergers that reduced the number of newspapers in a municipality by 1
daily_newspaper_closed<-rep(0, nrow(elections))
community_weekly_newspaper_closed<-rep(0, nrow(elections))
#newspaper reduced service
#This includes going from 7 days to 6, transitioning to a community from a paid paper
daily_newspaper_reduced_service<-rep(0, nrow(elections))
community_weekly_newspaper_reduced_service<-rep(0, nrow(elections))

# To ignore
# university newspapers
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


#### Sample code for closure #### 
#search for the municipality
#Find the municipality you are looking for in two ways
#Look for keywords and partial matches

elections %>% 
  filter(str_detect(municipality, "Sainte-Marie")) %>% 
  View()
#Look for exact matches
elections %>% 
  filter(municipality=="Rouyn-Noranda") %>% View()
#
#### Daily Newspaper Closures 
elections %>% 
  mutate(daily_newspaper_closed=case_when(
    # municipality=="Sainte-Marie" & election_year > 2014 ~ 1,
    # municipality=="Rouyn-Noranda" & election_year > 2014 ~ 1,
    # municipality=="Selkirk" &election_year> 2020 ~1,
    municipality=="Chilliwack" &election_year> 2016 ~1,
    TRUE ~ 0
  ))->elections

#### Community weekly daily free Newspaper Closures ####
elections %>% 
  mutate(community_weekly_daily_free_newspaper_closed=case_when(
    municipality=="Sainte-Marie" & election_year > 2014 ~ 1,
    municipality=="Rouyn-Noranda" & election_year > 2014 ~ 1,
    municipality=="Selkirk" &election_year> 2020 ~1,
    municipality=="Abbotsford" &election_year> 2013 ~ 2
    municipality=="Chilliwack" &election_year> 2013 ~ 1
    TRUE ~ 0
  ))->elections

#### Daily newspaper Service Reductions ####
elections %>% 
  mutate(daily_newspaper_reduced_service=case_when(
    # municipality=="Sainte-Marie" & election_year > 2014 ~ 1,
    # municipality=="Rouyn-Noranda" & election_year > 2014 ~ 1,
    # municipality=="Selkirk" &election_year> 2020 ~1,
    # TRUE ~ 0
  ))->elections

#### Community and Weekly newspaper Service Reductions ####
elections %>% 
  mutate(community_weekly_newspaper_reduced_service=case_when(
    # municipality=="Sainte-Marie" & election_year > 2014 ~ 1,
    # municipality=="Rouyn-Noranda" & election_year > 2014 ~ 1,
    # municipality=="Selkirk" &election_year> 2020 ~1,
    # TRUE ~ 0
    municipality=="Yarmouth" > election_year> 2016 ~ 1,
    municipality=="Digby" > election_year> 2016 ~ 1
  ))->elections


elections %>% 
  filter(str_detect(municipality, "Sainte-Marie| Rouyn")) %>% 
  View()
#Provide a check
elections %>% 
  filter(municipality=="Moose Jaw" & election_year> 2009) %>% 
  select(municipality, election_year, newspaper_closed)

#View(elections)
#write_csv(elections, file="data/canadian_municipal_elections.csv")
