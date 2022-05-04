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
