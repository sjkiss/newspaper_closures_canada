#Read in data
library(here)
library(did)
library(tidyverse)

elections<-read.csv(here("Data/elections_help.csv"))
summary(elections)
names(elections)
glimpse(elections)

did_turnout<-att_gt(yname="Turnout", tname="year", idname="CSD.code", gname="period", 
                    data=subset(elections, is.na(Turnout)==F))
did_turnout<-att_gt(yname="Turnout", tname="year", idname="CSD.code", gname="period", data=elections)

#checks
class(elections$CSD.code)
elections %>% 
  group_by(CSD.code, municipality) %>% 
  summarize(n=n()) %>% 
View()

elections %>% 
  select(CSD.code, turnout, year, group_name) %>% 
  summary()
