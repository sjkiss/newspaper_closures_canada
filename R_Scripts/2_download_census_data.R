
#Execute this command on your end Once.
#set_api_key("CensusMapper_287500bb91a374ec69fdcf270fb20ff7", install=T, overwrite=T)
#options(cancensus.api_key = "CensusMapper_287500bb91a374ec69fdcf270fb20ff7")

#### Get Population 2001
source("R_Scripts/1_read_in_data_file.R")
library(cancensus)
# set_cache_path('/Users/skiss/OneDrive - Wilfrid Laurier University/cancensus_cache_path', install = TRUE)
# readRenviron("~/.Renviron")
population_1996<-get_census(dataset="CA1996", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            geo_format=NA, level="CSD", use_cache = T)
population_2001<-get_census(dataset="CA01", 
           regions=list(CSD=c(elections$`CSD code`)), 
           geo_format=NA, level="CSD", use_cache = T)
population_2006<-get_census(dataset="CA06", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            geo_format=NA, level="CSD", use_cache = T)
population_2011<-get_census(dataset="CA11", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            geo_format=NA, level="CSD", use_cache = T)
population_2016<-get_census(dataset="CA16", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            geo_format=NA, level="CSD", use_cache = T)
population_2021<-get_census(dataset="CA21", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            geo_format=NA, level="CSD", use_cache = T)
census.list<-list(population_1996,population_2001, population_2006, population_2011, population_2016, population_2021)
names(census.list)<-c('1996', '2001', '2006', '2011', '2016', '2021')
names(census.list)
census.list %>% 
  bind_rows(., .id="census_year")->population
?bind_rows
glimpse(population)
population %>% 
  mutate(Density=Population/`Area (sq km)`)->population

population %>% 
  arrange(`GeoUID`) %>% 
 group_by(`GeoUID`) %>% 
  summarize(count=n()) %>% 
  filter(count<6) ->changed_csd
changed_csd
population %>% 
  filter(`GeoUID` %in% changed_csd$`GeoUID`)

population %>% 
  arrange(`GeoUID`) %>% 
  group_by(GeoUID) %>% 
  mutate(delta=(Population-lag(Population))/lag(Population))->population
names(population)
population %>% 
  select(census_year, `CSD code`=GeoUID, Population, Density, delta, CD_UID, PR_UID)->population

elections %>% 
  mutate(census_year=case_when(
    year<2004 ~ 2001,
    year>2003 & year<2009 ~ 2006,
    year> 2008 & year<2014 ~ 2011,
    year> 2013 & year< 2019 ~ 2016,
    year>2018 ~ 2021
  ))->elections
names(elections)
population$census_year<-as.numeric(population$census_year)
elections$census_year<-as.numeric(elections$census_year)
population$`CSD code`<-as.numeric(population$`CSD code`)
elections %>%
left_join(., population, by=c("census_year", "CSD code"))->elections


