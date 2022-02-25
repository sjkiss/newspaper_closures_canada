#Read in data
library(here)
library(did)

elections<-read.csv(here("Data/elections_help.csv"))
summary(elections)
names(elections)
did_turnout<-att_gt(yname="turnout", tname="year", idname="CSD.code", gname="group_name", data=elections)
