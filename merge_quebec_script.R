library(here)
library(readr)
library(tidyverse)
# this section reads in the Statistics Canada's SGC file.
sgc<-read.csv(file=here("data/sgc_2016_structure.csv"))
sgc %>% 
  slice(2173:2175)
sgc %>% 
  filter(Level==4) ->sgc
sgc %>% 
  rename(municipality=4)->sgc


sgc %>% 
  filter(str_detect(municipality, "Bégin"))
qc %>% 
  select(municipality) %>% 
  filter(str_detect(municipality, "Lac"))

Encoding(sgc$municipality)<-'UTF-8'
Encoding(qc$municipality)<-'UTF-8'

#This extracts the 4-digit
sgc %>% 
mutate(mun_c=str_sub(Code,4,7))->sgc
sgc$mun_c
#This script reads in Quebec's data files
qc_2013<-read_csv(file=here("data/quebec_2013.csv"), col_types=c("n", "c", "n", "c", "c"))
qc_2017<-read_csv(file=here("data/quebec_2017.csv"), col_types=c("n", "c", "n", "c", "c"))
qc_2021<-read_csv(file=here("data/quebec_2021.csv"), col_types=c("n", "c", "c", "n", "c", "n", "c", "c"))
qc_2017$poste_c<-as.numeric(qc_2017$poste_c)

# qc_2013<-select(qc_2013, -poste_c)
# qc_2017<-select(qc_2017, -poste_c)
Encoding(qc_2013$reg_n)<-'utf-8'
list1<-list(qc_2013, qc_2017, qc_2021)


names(list1)<-c("2013", "2017", "2021")
list1 %>% 
  bind_rows(., .id="Year")->qc


#Make turnout
qc$turnout<-(qc$nb_votes_candidat+qc$nb_votes_rejetes)/qc$nb_electeurs_maire
# Make CSD Code
qc %>% 
  rename(municipality=3)->qc
#This section keeps only the mayorlty elections
qc %>% 
  filter(str_detect(type_poste_d, "Maire"))->qc
qc %>% 
  mutate(position=str_extract_all("Maire", type_poste_d))->qc
#This writes the data out
qc %>% 
  group_by(Year, mun_c, type_poste_d) %>% 
#write.csv(qc, file="Data/qc_results_2013_2017.csv")

#Now we try the merge 

sgc$mun_c
qc$mun_c
library(tidylog)
qc %>% 
  left_join(sgc, by="municipality", suffix=c("_results", "_sgc"))->qc_left
?left_join
names(qc)
qc %>% 
  full_join(sgc, by="municipality", suffix=c("_results", "_sgc"))->qc_full
names(qc_full)
qc_left%>% 
  select(Year, mun_c_results, mun_c_sgc, municipality, `Code`, `nb_electeurs_maire`, `nb_votes_candidat`) %>% 
  #filter(is.na(Code)) %>% 
filter(is.na(Code)) %>% 
  View()
# Calculate 
#### The next thing to do will be to rename the variables and prep for merging with Sam's database!!!!

#calculate number of candidates
qc_left %>% 
  group_by(municipality, Year) %>% 
  summarize(n=n()) %>% 
  right_join(., qc_left, by=c("municipality", "Year")) %>% 
rename(`number of candidates`=3)->qc_left

#Define acclaimed status
qc_left$acclaimed<-ifelse(qc_left$`number of candidates`==1,1,0)
