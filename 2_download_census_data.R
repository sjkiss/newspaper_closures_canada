source("1_read_in_data_file.R")
#install.packages("cancensus")
elections
library(cancensus)

#Execute this command on your end Once.

#set_api_key('Put Your census mapper key here', install = TRUE)
elections %>% 
  filter(~is.na(`CSD code`))
map(elections$`CSD code`, 
    function(x) 
      get_census(dataset='CA16', regions=list(CSD=x), vectors=c("v_CA16_408")))
census_data <- get_census(dataset='CA16', 
                          regions=list(CMA="59933"),
                          vectors=c("v_CA16_408","v_CA16_409","v_CA16_410"),
                          level='CSD', use_cache = FALSE, geo_format = NA)

