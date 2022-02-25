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
candidates<-lm(`number of candidates`~treatment+as.factor(year)+as.factor(municipality), data=elections)
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
candidates2<-lm(`number of candidates`~treatment+as.factor(year)+as.factor(municipality), data=elections2)
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


stargazer(turnout, turnout2, margin, margin2, candidates, candidates2, omit=c("^as.factor"), out=here("Tables", "model1_model3.html"))


#### TWFE With log of margin and number of candidates ####
names(elections)
candidates
candidates3<-update(candidates, log(`number of candidates`)~.)
margin3<-update(margin, log(`margin_percent`)~.)

#### TWFE Categorical #### 
names(elections)
turnout4<-lm(turnout~status+as.factor(year)+as.factor(municipality), data=elections)
candidates4<-lm(`number of candidates`~status+as.factor(year)+as.factor(municipality), data=elections)
margin4<-lm(margin_percent~status+as.factor(year)+as.factor(municipality), data=elections)

#### TWFE Log candidates log margin dichotomous ####
margin5<-update(margin4, log(margin_percent)~.)
candidates5<-update(margin4, log(`number of candidates`)~.)
#Print the giant table

stargazer(turnout, turnout2, turnout4, title="Turnout in Elections As A Function of Newspaper Closures", type="html", omit=c("^as.factor"), column.labels = c("Turnout", "Turnout Minus Unusual Cases", "Turnout Dichotomous Treatment Variable"), out=here("Tables", "turnout_models.html"), dep.var.labels.include = F)


stargazer(margin, margin2, margin3, margin4, margin5, title="Victory Margin in Elections As A Function of Newspaper Closures", type="html", omit=c("^as.factor"), column.labels=c("Margin", "Margin minus unusual cases", "Log Margin", "Margin, Dichotomous Treatment Variable", "Log Margin, Dichotomous Treatment"), out=here("Tables", "margin_models.html"), dep.var.labels.include = F)

stargazer(candidates, candidates2, candidates3, candidates4, candidates5, title="Number of Canddiates in Elections As A Function of Newspaper Closures", type="html", omit=c("^as.factor"), column.labels=c('Candidates', 'Candidates minus unusual cases', 'Log Candidates', 'Candidates, Dichotomous Treatment Variable', "Log Candidates, Dichotomous"), out=here("Tables", "candidate_models.html"), dep.var.labels.include = F)


#### Check Raw Statistics #### 

elections %>% 
  select(turnout, margin_percent, `number of candidates`)

#### Filter the politics through newspaper closures ####
#turnout6<-lm(turnout~previous_margin, data=elections)
names(elections)
summary(turnout6)
 
# DiD

library(did)
names(elections)
elections$group_name
elections$id
write.csv(elections, here("data/elections_help.csv"))
did_turnout<-att_gt(yname="turnout", tname="year", idname="CSD code", gname="group_name", data=subset(elections, !is.na(turnout)))
did_margin<-att_gt(yname="margin_percent", tname="year", idname="CSD code", gname="group_name", data=elections)

