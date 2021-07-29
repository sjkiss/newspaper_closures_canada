####Load libraries####
library(tidyverse)
library(haven)
#This is the command to read in a stata file:
#Notethe file file="" must specify exactly where the file is. 
elec<-read_dta(file="elec.dta", encoding="utf-8")
#
##Look up different ways of exporting characters with accents
## key is encodings. Try utf-8 encoding in your searches?
## This is tricky and often trial and error is the best way 
## rio package` `


#### Calculations of Stats ####

## Try to calculate margin
elec %>% 
  filter(str_detect(race_id, "")) %>% 
  group_by(race_id) %>% 
  arrange(race_id, position, magnitude,votes_received, .by_group=T) %>% 
  mutate(margin=(votes_received-lag(votes_received))/total_votes) %>% 
  select(votes_received, total_votes, elected,margin, position, ward, magnitude) %>% 
  View()


## This works for races that are single candidates, but it won't really work f

#### export ####
## openxlsx package
library(openxlsx)

elec %>% 
  mutate(position=str_to_lower(position)) %>% 
 # filter(str_detect(position, "mayor|mairie")) %>% 
write.xlsx(., file="election_cleaned.xlsx")