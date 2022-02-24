### Warning:
# Set the working directory to be the directory where this R-script is stored on your computer
#Or, more simply, open the Rstudio PRoject file in the folder. It will do that automatically
#Checks
#getwd()
#list.files()

#Install necessary packages  
to.install<-c('readxl', 'tidyverse' ,'tidylog', 'here')                                    # Specify your packages
not_installed <- to.install[!(to.install %in% installed.packages()[ , "Package"])]    # Extract not installed packages
if(length(not_installed)) install.packages(not_installed)     

#Load libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(tidylog)
library(here)
#This reads in the newspaper file
elections<-read_xlsx(path=here("Data", "working_data_file.xlsx"))

#Call the code to add in the Statistics Canada data
#source(here("R_Scripts/2_download_census_data.R"))
#Set variables to be numeric
elections$turnout<-as.numeric(elections$turnout)
elections$Total_votes<-as.numeric(elections$`Election Total Votes`)

#Check what's been imported
names(elections)

#drop last name
elections %>% 
  select(-`Last Name`, -12)->elections

#Create Unique ID by combining city name and year
elections<-elections %>% 
  unite("id", 5:6, remove=F)


##### Define acclaimed candidates
names(elections)
elections$acclaimed<-ifelse(elections$`number of candidates`==1, 1, 0)
##### Calculate Margin
elections %>% 
  #Form groups by municipality and year
  group_by(municipality, year) %>% 
  #Arrange the data by province, then city then year, moving from largest vote-getter (e.g. the winner), to the least
  arrange(province, municipality, year, desc(Votes), by_group=T) %>% 
  mutate(place=row_number()) %>% 
  #calculate the margin as a function of Votes minus the next votegetter in the group (e.g. the second-place candidate)
mutate(margin=Votes-lead(Votes),
       #convert to a percent
       margin_percent=margin/Total_votes) ->elections

elections %>% 
  filter(place==1) ->elections

#### Calculate first Differences
names(elections)

#Calculate the change in turnout
# elections %>% 
#   group_by(municipality) %>% 
#   mutate(delta_turnout=turnout-lag(turnout),
#          delta_margin=margin_percent-lag(margin_percent), 
#          delta_n=`number of candidates`-lag(`number of candidates`)) ->elections

#Identify the first closure
elections %>%
  group_by(municipality, closure= status<0) %>% 
  arrange(closure) %>% 
  mutate(closure_count= row_number(), 
         first_closure = closure & closure_count==1) ->elections



elections %>% 
  group_by(municipality) %>% 
  mutate(group_name=match(TRUE, first_closure)) %>% 
mutate(group_name=car::Recode(group_name, "NA=0")) ->elections


#calculate the previous margin of victory
names(elections)
# elections %>% 
#   group_by(municipality) %>% 
#   mutate(previous_margin=lag(margin_percent)) ->elections
#Check
qplot(elections$margin_percent, geom="histogram")
summary(elections)

#### How many complete observations#### 
#Remove extraneous variables
names(elections)
elections %>% 
  select(-c("Votes", "Election Total Votes", "Total_votes", "margin"))->elections
names(elections)

#Add # of missing values 
elections %>% 
  #Perform the next functions by row
  rowwise() %>% 
  #Count the number of missings in each row for the DVs turnout to margin_percent
  mutate(na=sum(is.na(c_across(c(turnout, `number of candidates`, margin_percent)))))->elections

#Examine
names(elections)


#Check Jasper
 elections%>% 
  filter(municipality=="Jasper") 
 elections %>% 
   filter(municipality=="Grandview")
 names(elections)
elections %>% 
  select(turnout, margin_percent, `number of candidates`) %>% 
  is.na() %>% 
  colSums()


elections$na
#556 Observations.
nrow(elections)
elections %>% 
  filter(na==0) %>% 
  nrow() 
# 462 complete cases

elections %>% 
  filter(na<2) %>% 
  nrow()
#516 with at least two DVs

elections %>% 
  filter(na<3) %>% 
  nrow()
#542 election years with at least 1 DV




elections %>%   group_by(treatment) %>% 
  summarize(n=n())
# 82 post-treatment election-years, 471 non-treatment election-years
#How many election years prior to the treatment


#Assign To Treatment Group
#This assigns 1 to any city-year where a newspaper has been closed
elections %>% 
  group_by(municipality) %>% 
  mutate(treat_group=case_when(
    sum(status)==0 ~ 0,
    sum(status)<0 ~1
  ))->elections
.

#This tries to add a time variable t with 0 for the year a newspaper closes, and then -1 for the year before, and +1 for the year after; But by definition all untreated cities will then have a 0 all years
elections %>% 
group_by(municipality) %>%
  mutate(t=data.table::rleid(status)) %>% 
  group_by(t, .add=T) %>% 
  mutate(timetotreat=- (t==1) * (n()+1) + row_number() - 1*(t==2))->elections
names(elections)


#Distribution of treatment values by year
#0 equals no newspaper closure
#-1 equals 1 ewspaper has closed
#-2 equals 2, etc. etc. 
table(elections$year, elections$status)


#### Show distribution of variables ####

elections%>% 
  pivot_longer(cols=c(status, turnout, `number of candidates`, margin_percent)) %>% 
  ggplot(., aes(x=value))+geom_histogram()+facet_grid(~name, scales="free_x")
ggsave(here("Plots", "distribution_of_variables.png"), width=8, height=4)

#Detect outlier with 6 newspaper closures, it is the city of Montreal. 
elections %>% 
  filter(status< -5) 
elections<-ungroup(elections)

#### Write Out Descriptives ####
#install.packages("DescTools")

elections %>% 
  select(municipality) %>% 
  tbl_summary(statistic=municipality~"{N}") 

length(unique(elections$municipality))

elections %>% 
  filter(municipality=="Saskatoon")

write.csv(elections, file=here("data/elections_help.csv"))
