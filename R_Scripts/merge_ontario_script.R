#Read in Ontario files
on_2014<-read.csv(file="data/ontario_2014.csv")
on_2018<-read.csv(file="data/ontario_2018.csv")
names(on_2014)

#Necessary to strip out the type of municipality designations. 
on_2014$municipality

on_2014 %>% 
  mutate(municipality=str_remove_all(MUNICIPALITY.NAME, " T$| Tp$| C$| M$| Co$| M$| R$| V$")) ->on_2014
on_2018 %>% 
  mutate(municipality=str_remove_all(MUNICIPALITY.NAME, " T$| Tp$| C$| M$| Co$| M$| R$| V$")) ->on_2018
list1<-list(on_2014, on_2018)

names(list1)<-c('2014', '2018')
class(list1)
list1%>% 
  bind_rows(., .id="Year") ->on
names(on)
?bind_rows
library(janitor)
on<-clean_names(on)
on %>% rename("Year"=1)->on
names(on)
on %>% 
  filter(name_of_office=="MAYOR")

glimpse(on)
on %>% 
  mutate(votes=as.numeric(str_replace_all(number_of_votes_cast_for_the_candidate, ",", ""))) ->on
on %>% 
  mutate(total_votes=as.numeric(str_replace_all(number_of_votes_cast_for_the_office, ",", "")))->on
names(on)
#define acclaimed
on %>% 
  mutate(acclaimed=case_when(
    str_detect(elected_or_acclaimed, "ACCLAIMED")~1,
    str_detect(elected_or_acclaimed, "ELECTED")~0,
  )) ->on
on %>% 
  select(municipality, Year, votes, total_votes, acclaimed) 
