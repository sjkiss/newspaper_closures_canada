#Source
library(here)
source(here("R_Scripts/2_download_census_data.R"))
#### Recreate Koop####


#### TWFE Model 1 #### 
names(elections)
turnout<-lm(turnout~treatment+as.factor(year)+as.factor(municipality), data=elections)
summary(turnout)
margin<-lm(margin_percent~treatment+as.factor(year)+as.factor(municipality), data=elections)
summary(margin)
candidates<-lm(log(`number of candidates`)~treatment+as.factor(year)+as.factor(municipality), data=elections)
summary(candidates)
#Print First s
library(stargazer)

#### Diagnostics for TWFE1#### 

qplot(1:length(turnout$residuals),turnout$residuals,  geom="point")
qplot(1:length(margin$residuals),margin$residuals,  geom="point")
qplot(1:length(candidates$residuals),candidates$residuals,  geom="point")

qplot(turnout$fitted.values, turnout$resid, main="Residuals and fitted values for Turnout")
ggsave(here("Plots", "residuals_fitted_values_turnout.png"))
qplot(margin$fitted.values, margin$resid, main="Residuals and fitted values for Margin Percent")
ggsave(here("Plots", "residuals_fitted_values_margins.png"))
qplot(candidates$fitted.values, candidates$resid, main="Residuals and fitted values for N candidates")
ggsave(here("Plots", "residuals_fitted_values_turnout.png"))


qqnorm(turnout$resid, main="QQ Plot for Turnout")  #Normal Quantile to Quantile plot
qqline(turnout$resid) 
ggsave(here("Plots", "qqnorm_turnout_full.png"))
qqnorm(margin$resid, main="QQPlot for Margin")  #Normal Quantile to Quantile plot
qqline(margin$resid) 
ggsave(here("Plots", "qqnorm_turnout_full.png"))

qqnorm(candidates$resid, main="QQPlot for candidates")  #Normal Quantile to Quantile plot
qqline(candidates$resid) 
ggsave(here("Plots", "qqnorm_candidates_full.png"))

#### TWFE Filter out outliers ####
#Filter out huge numbers of newspaper closures and largenumbers of candidatets
elections %>% 
  filter(status > -6 & `number of candidates` <20)->elections2
turnout2<-lm(turnout~treatment+as.factor(year)+as.factor(municipality), data=elections2)
summary(turnout2)
margin2<-lm(margin_percent~treatment+as.factor(year)+as.factor(municipality), subset(elections2, margin_percent<2))
summary(margin2)
candidates2<-lm(log(`number of candidates`)~treatment+as.factor(year)+as.factor(municipality), data=elections2)
summary(candidates2)
summary(turnout2)
summary(margin2)
summary(candidates)
#Filter out margins
#Check residuals of 

qplot(turnout2$fitted.values, turnout2$resid, main="Residuals and fitted values for Turnout 2\n Subsetted data on closures and candidates")

ggsave(here("Plots", "residuals_fitted_values_turnout_subsetted.png"))
qplot(margin2$fitted.values, margin2$resid, main="Residuals and fitted values for Margin Percent\nSubsetted data on closures, candidates and margin <2")
ggsave(here("Plots", "residuals_fitted_values_margins_subsetted.png"))
qplot(candidates2$fitted.values, candidates2$resid, main="Residuals and fitted values for N candidates\nSubsetted data on closures, candidates and margin <2")
ggsave(here("Plots", "residuals_fitted_values_turnout_subsetted.png"))


qqnorm(turnout2$resid, main="QQ Plot for Turnout Subsetted")  #Normal Quantile to Quantile plot
qqline(turnout2$resid) 
ggsave(here("Plots", "qqnorm_turnout_subsetted.png"))
qqnorm(margin2$resid, main="QQPlot for Margin Subsetted")  #Normal Quantile to Quantile plot
qqline(margin2$resid) 
ggsave(here("Plots", "qqnorm_turnout_subsetted.png"))

qqnorm(candidates2$resid, main="QQPlot for candidates Subsetted")  #Normal Quantile to Quantile plot
qqline(candidates$resid) 
ggsave(here("Plots", "qqnorm_candidates_subsetted.png"))


#stargazer(turnout, turnout2, margin, margin2, candidates, candidates2, omit=c("^as.factor"), out=here("Tables", "model1_model3.html"))


#### Check Raw Statistics #### 


#### Filter the politics through newspaper closures ####
#turnout6<-lm(turnout~previous_margin, data=elections)

# DiD

library(did)
names(elections)

elections$id

names(elections)

write.csv(elections, here("Data/elections_help.csv"))
names(elections)
#did_turnout<-att_gt(yname="turnout", tname="year", idname="CSD code", gname="group_name", data=subset(elections, !is.na(turnout)))
#did_margin<-att_gt(yname="margin_percent", tname="year", idname="CSD code", gname="group_name", data=elections)

