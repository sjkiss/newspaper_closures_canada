####Load libraries####
library(tidyverse)
library(haven)
#This is the command to read in a stata file:
#Notethe file file="" must specify exactly where the file is. 

elec<-read_csv(file="data/canadian_municipal_elections.csv")
nrow(elec)
elec %>% 
  filter(str_detect(municipality, "Beaup"))

sgc<-read.csv(file="data/sgc_2016_structure.csv")
#Merge sgc with elec to get csd code
sgc %>% 
  filter(Level==4) ->sgc
sgc %>% 
  rename(municipality=4)->sgc
sgc %>% 
  filter(str_detect(municipality, "Beaup"))

library(tidylog)
elec %>% 
  left_join(., sgc, by="municipality") %>% 
  filter(is.na(Code)) %>% 
  filter(province=="Quebec") %>% 
  View()

elec %>% 
  left_join(., sgc, by="municipality") %>% 
  filter(str_detect(municipality, "Beaup")) %>% 
  View()
##Look up different ways of exporting characters with accents
## key is encodings. Try utf-8 encoding in your searches?
## This is tricky and often trial and error is the best way 
## rio package` `

View(elec)

#### Calculations of Stats ####
elec %>% 
  merge()
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