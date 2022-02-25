#Read in data
library(here)
library(did)
library(tidyverse)

elections<-read.csv(here("Data/elections_help.csv"))
summary(elections)
names(elections)
glimpse(elections)

did_turnout<-att_gt(yname="turnout", tname="year", idname="CSD.code", gname="group_name", data=elections)

#checks
class(elections$CSD.code)
elections %>% 
  group_by(CSD.code, municipality) %>% 
  summarize(n=n()) %>% 
View()

elections %>% 
  select(CSD.code, turnout, year, group_name) %>% 
  summary()
