
#Execute this command on your end Once.
#options(cancensus.api_key = "CensusMapper_287500bb91a374ec69fdcf270fb20ff7")
#### Get Population 2001
source("R_Scripts/1_read_in_data_file.R")
library(cancensus)
population_2001<-get_census(dataset="CA01", 
           regions=list(CSD=c(elections$`CSD code`)), 
           vectors=c("v_CA01_2","v_CA01_3"), 
           geo_format=NA, level="CSD", use_cache = T)
names(population_2001)
population_2001$GeoUID<-as.numeric(population_2001$GeoUID)
population_2001 %>% 
  select(1,8,9,10,11,12)->population_2001


  elections %>% 
  left_join(., population_2001, by=c(`CSD code`="GeoUID"))->elections

elections %>% 
select(`CSD code`,municipality, year,starts_with('v_')) %>% 
  filter(is.na("v_CA01_3: Population percentage change, 1996-2001"))
# Set the non 2001 years to be NA
elections %>% 
  rename("population_2001"="v_CA01_2: Population, 2001", 
         "delta_pop_1996_2001"="v_CA01_3: Population percentage change, 1996-2001")->elections
elections %>% 
  mutate(population_2001=ifelse(year>2003, NA, population_2001),
         delta_pop_1996_2001=ifelse(year>2003, NA,delta_pop_1996_2001))->elections
         
#### Get Population 2006
population_2006<-get_census(dataset="CA06", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            vectors=c("v_CA06_1"), 
                            geo_format=NA, level="CSD", use_cache = T)

population_2006$GeoUID<-as.numeric(population_2006$GeoUID)
population_2006 %>% 
  select(1,8:11)->population_2006

elections %>% 
  left_join(., population_2006, by=c(`CSD code`="GeoUID"))->elections

names(elections)
elections %>% 
  rename("population_2006"="v_CA06_1: Population, 2006 - 100% data")->elections

names(elections)
elections %>% 
  mutate(delta_pop_2001_2006=(population_2006-population_2001/population_2006)*100)->elections

#Delete non 2006 years
names(elections)
elections %>% 
  mutate(population_2006=ifelse(year>2003 & year <2009, population_2006, NA), 
         delta_pop_2001_2006=ifelse(year>2003 & year<2009, delta_pop_2001_2006, NA))->elections


####Get Population 2011
population_2011<-get_census(dataset="CA11", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            vectors=c("v_CA11F_1"), 
                            geo_format=NA, level="CSD", use_cache = T)

population_2011$GeoUID<-as.numeric(population_2011$GeoUID)
names(population_2011)
population_2011 %>% 
  select(1,11)->population_2011

elections %>% 
  left_join(., population_2011, by=c(`CSD code`="GeoUID"))->elections
names(elections)
elections %>% 
  rename(population_2011=37)->elections

#Calculate delta_pop
elections %>% 
  mutate(delta_pop_2006_2011=((population_2011-population_2006)/population_2006)*100)->elections
names(elections)
#Delete non2011 population values
elections %>% 
  mutate(population_2011=ifelse(year>2008 & year <2014, population_2011, NA), 
         delta_pop_2001_2006=ifelse(year>2008 & year<2014, delta_pop_2006_2011, NA))->elections

#Get POpulation 2016 data
names(elections)
population_2016<-get_census(dataset="CA16", 
                            regions=list(CSD=c(elections$`CSD code`)), 
                            vectors=c("v_CA16_401","v_CA16_403"), 
                            geo_format=NA, level="CSD", use_cache = T)

population_2016$GeoUID<-as.numeric(population_2016$GeoUID)
names(population_2016)
population_2016 %>% 
  select(1,11:12)->population_2016

elections %>% 
  left_join(., population_2016, by=c(`CSD code`="GeoUID"))->elections
names(elections)
elections %>% 
  rename(population_2016=39, delta_pop_2011_2016=40)->elections

#Delete non2016 population values
elections %>%
  mutate(population_2016=ifelse(year>2013 & year <2019, population_2016, NA),
         delta_pop_2011_2016=ifelse(year>2013 & year<2019, delta_pop_2011_2016, NA))->elections


names(elections)
# #Get POpulation 2021 data
# 
# population_2021<-get_census(dataset="CA21", 
#                             regions=list(CSD=c(elections$`CSD code`)), 
#                             vectors=c("v_CA16_401","v_CA16_403"), 
#                             geo_format=NA, level="CSD", use_cache = T)
# 
# population_2016$GeoUID<-as.numeric(population_2016$GeoUID)
# names(population_2016)
# population_2016 %>% 
#   select(1,11:12)->population_2016
# 
# elections %>% 
#   left_join(., population_2016, by=c(`CSD code`="GeoUID"))->elections
# names(elections)
# elections %>% 
#   rename(population_2016=38, delta_pop_2011_2016=39)->elections
# 
# #Delete non2016 population values
# elections %>% 
#   mutate(population_2016=ifelse(year>2013 & year <2019, population_2016, NA), 
#          delta_pop_2011_2016=ifelse(year>2013 & year<2019, delta_pop_2011_2016, NA))->elections
names(elections)
