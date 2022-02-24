#This reads in the 2016 statistics canada file
sgc_2016<-read_csv(file="/Users/skiss/OneDrive - Wilfrid Laurier University/projects_folder/News_deserts/sgc_2016_structure.csv")

sgc_2016 %>%
  #This renames the variables int he statistics canada file to match the 
  #variables in the newspaper_closure_file
  #Note that if a person were to manually rename the variables int he statitsics canada
  #file this would be unnecessary and would not work 
  rename(., `CSD code`="Code", "municipality"="Class title") %>% 
  #we also need to assign a province code to the sgc data base. 
  #The province code is the first two digits of the CSD code
  #This creates a new variable called province_code by extracting
  # the first two digits from CSD code
  mutate(province_code=str_extract(`CSD code`, "^[0-9]{2}")) %>% 
  mutate(province=case_when(
    province_code==10 ~ "Newfoundland and Labrador",
    province_code==11 ~ "Prince Edward Island",
    province_code==12 ~ "Nova Scotia",
    province_code==13~ "New Brunswick",
    province_code==24 ~ "Quebec",
    province_code==35 ~ "Ontario",
    province_code==46 ~ "Manitoba",
    province_code==47 ~ "Saskatchewan",
    province_code==48 ~ "Alberta",
    province_code==59 ~ "British Columbia")
  ) ->sgc_2016
class(sgc_2016$`CSD code`)
summary(elections$`CSD code`)
summary(sgc_2016$`CSD code`)
names(sgc_2016)


#How many rows have a CSD Code before the  merge
table(elections$`CSD code`, useNA = "ifany")
#There are 768 missing rows without a CSD code.
# Let's see if we can reduce that. 
nrow(elections)
elections %>% 
  get_dupes(municipality, `CSD code`) %>% 
  View()
?get_dupes
View(elections[duplicated(elections),])

elections %>% 
  left_join(., sgc_2016, by=c("province", "municipality")) 
table(test$`CSD code.x`, useNA="ifany")
table(test$`CSD code.y`, useNA="ifany")
names(test)
nrow(test)
?full_join
View(test)
elections %>% 
  filter(municipality=="Grandview")
test %>% 
  distinct() %>% 
  nrow()
write_csv(test, file="merge_text.csv")