library(lubridate)
library(lme4)
library(cowplot)
library(MuMIn)
library(Rmisc)
library(tidyverse)
library(spsurvey)
library(ggridges)
library(ggrepel)
library(reshape2)
library(MuMIn)

set.seed(207)

########### Trend slopes
difference_no3 <- read.csv("./data/clean_data/difference_no3.csv")
difference_nh4 <- read.csv("./data/clean_data/difference_nh4.csv")
difference_tn <- read.csv("./data/clean_data/difference_tn.csv")
difference_tp <- read.csv("./data/clean_data/difference_tp.csv")
difference_tn_lake <- read.csv("./data/clean_data/difference_tn_lake.csv")
difference_tp_lake <- read.csv("./data/clean_data/difference_tp_lake.csv")

########################## Modeling -- loading variables first
loading.alt<-read.csv("./data/predictor_data/alternative_loading_predictors.csv")


#### Stream NO3

stream.no3<-merge(difference_no3,loading.alt,by="state")

stream.no3.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.no3,na.action = "na.fail")
#options(na.action = "na.fail")
best<-dredge(stream.no3.global)
subset(best,delta<2)
best.stream.no3<-lm(SLRW_Trend_Estimate~feed,stream.no3)

### Stream NH4
stream.nh4<-merge(difference_nh4,loading.alt,by="state")

stream.nh4.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.nh4,na.action = "na.fail")
best<-dredge(stream.nh4.global)
subset(best,delta<2)

best.stream.nh4<-lm(SLRW_Trend_Estimate~feed+urban.delta,data=stream.nh4)

### stream TP
stream.tp<-merge(difference_tp,loading.alt,by="state")

stream.tp.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                        fertilizer+fertilizer.delta+feed+feed.delta,stream.tp,na.action = "na.fail")
best<-dredge(stream.tp.global)
subset(best,delta<2)
best.stream.tp<-lm(SLRW_Trend_Estimate~ag.delta,data=stream.tp)

### stream TN
stream.tn<-merge(difference_tn,loading.alt,by="state")

stream.tn.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                       fertilizer+fertilizer.delta+feed+feed.delta,stream.tn,na.action = "na.fail")
best<-dredge(stream.tn.global)
subset(best,delta<2)

best.stream.tn<-lm(SLRW_Trend_Estimate~feed+feed.delta,data=stream.tn)
summary(best.stream.tn)

### lake TP
lake.tp<-merge(difference_tp_lake,loading.alt,by="state")

lake.tp.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                     fertilizer+fertilizer.delta+feed+feed.delta,lake.tp,na.action = "na.fail")
best<-dredge(lake.tp.global)
subset(best,delta<2)
best.lake.tp<-lm(SLRW_Trend_Estimate~ag+fertilizer.delta+undeveloped,data=lake.tp)
summary(best.lake.tp)

### lake TN
lake.tn<-merge(difference_tn_lake,loading.alt,by="state")

lake.tn.global<-lm(SLRW_Trend_Estimate~pop+pop.delta+urban+undeveloped+ag+urban.delta+undeveloped.delta+ag.delta+
                     fertilizer+fertilizer.delta+feed+feed.delta,lake.tn,na.action = "na.fail")

best<-dredge(lake.tn.global)
subset(best,delta<2)

best.lake.tn<-lm(SLRW_Trend_Estimate~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta,lake.tn)
summary(best.lake.tn)

#################################### Fitting policy variables.

state.areas<-read.csv("./data/predictor_data/state_areas.csv")
state.areas$states<-state.areas$State
state.areas$state.jurs.land<-state.areas$state.jurs.land*1000*0.00404686 ## converting from 1000 acres to km2

nutrient_criteria<-read.csv("./data/predictor_data/regulations.csv")
nutrient_criteria_short<-data.frame(states=nutrient_criteria$state,lake.p.criteria=nutrient_criteria$plakes.aggregate,
                                    lake.n.criteria=nutrient_criteria$n.lakes.aggregate,stream.n.criteria=
                                      nutrient_criteria$nstream.aggregate,stream.p.criteria=nutrient_criteria$pstream.aggregate)
nutrient.criteria.melt<-melt(nutrient_criteria_short)

s319<-read.csv("./data/predictor_data/summarized_319.csv") %>% 
  dplyr::rename("states"="State") %>% 
  inner_join(state.areas, by="states") %>% 
  mutate(total.money.sq.km= Total.State.Money/state.jurs.land)
s319$states<-factor(s319$states,levels=s319$states[order(s319$total.money.sq.km)])

tmdl<-read.csv("./data/predictor_data/TMDL_data_summary.csv") %>% inner_join(state.areas,by="states") %>% mutate(tmdl.sites.sq.km=tally/state.jurs.land)
tmdl$states <-factor(tmdl$states,levels=tmdl$states[order(tmdl$tmdl.sites.sq.km)])

####### merge_predictors
loading.pca$states<-loading.pca$state

# TODO: Figure out why all states don't have loading scores
# The ones that don't are AK and HI which aren't included in the NLCD 
# Make this clear in the text if you do show AK and HI in the figures but don't include them in the models
policy.loading <- full_join(tmdl, s319, by =c("states", "State", "total.area.th.acre","federal.land.th.acre", "tribal.land.th.acre", "state.jurs.land")) %>% 
  dplyr::select(-State) %>% 
  full_join(nutrient_criteria_short, by = "states") # %>% 
  #full_join(loading.pca %>% dplyr::select(-State, -state), by = "states") %>% dplyr::rename("state"= "states")

# Why doesn't PA have tmdl data? Not sure, I've dropped it from analysis lower down
policy.loading$tmdl.z<-(policy.loading$tmdl.sites.sq.km-mean(policy.loading$tmdl.sites.sq.km, na.rm=T))/sd(policy.loading$tmdl.sites.sq.km, na.rm=T)
policy.loading$money.z<-(policy.loading$total.money.sq.km-mean(policy.loading$total.money.sq.km))/sd(policy.loading$total.money.sq.km)
policy.loading$lake.criteria.p.z<-(policy.loading$lake.p.criteria-mean(policy.loading$lake.p.criteria))/sd(policy.loading$lake.p.criteria)
policy.loading$lake.criteria.n.z<-(policy.loading$lake.n.criteria-mean(policy.loading$lake.n.criteria))/sd(policy.loading$lake.n.criteria)
policy.loading$stream.n.criteria.z<-(policy.loading$stream.n.criteria-mean(policy.loading$stream.n.criteria))/sd(policy.loading$stream.n.criteria)
policy.loading$stream.p.criteria.z<-(policy.loading$stream.p.criteria-mean(policy.loading$stream.p.criteria))/sd(policy.loading$stream.p.criteria)

policy.loading$state<-policy.loading$states
####################### modeling the effects of policy

# stream nitrate
difference_no3<-left_join(stream.no3, policy.loading, by="state")

stream.nitrate.policy.wlr<-data.frame()
stream.nitrate.model.selction.wlr<-data.frame()

stream.nitrate.policy.slr<-data.frame()
stream.nitrate.model.selction.slr<-data.frame()

stream.nitrate.z.scores.slr<-data.frame()
stream.nitrate.z.scores.wlr<-data.frame()

null.coef.stream.no3<-data.frame()

nsims<-10000

for (i in 1:nsims){
  d<-difference_no3
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)

  model.money.wlr<-lm(slope.wlr~feed+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~feed+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~feed+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~feed+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~feed+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~feed+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~feed+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~feed+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~feed+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~feed+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~feed+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~feed+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~feed,data=d)
  s.null.wlr<-summary(null.wlr)
  null.slr<-lm(slope.slr~feed,data=d)
  
  null.output<-data.frame(feed=coef(null.wlr)[2],feed.se=s.null.wlr$coefficients[2,2],rsq=s.null.wlr$r.squared)
  
  null.coef.stream.no3<-rbind(null.coef.stream.no3,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.tmdl.wlr.z.summary<-summary(model.tmdl.wlr.z)
  model.money.wlr.z.summary<-summary(model.money.wlr.z)
  model.criteria.wlr.z.summary<-summary(model.criteria.wlr.z)
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                              coef=c(coef(model.tmdl.wlr.z)[3],coef(model.money.wlr.z)[3],
                              coef(model.criteria.wlr.z)[3]),
                              coef.se=c(model.tmdl.wlr.z.summary$coefficients[3,2],
                                        model.money.wlr.z.summary$coefficients[3,2],
                                        model.criteria.wlr.z.summary$coefficients[3,2]))
  
  model.tmdl.slr.z.summary<-summary(model.tmdl.slr.z)
  model.money.slr.z.summary<-summary(model.money.slr.z)
  model.criteria.slr.z.summary<-summary(model.criteria.slr.z)
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[3],
                                         coef(model.money.slr.z)[3],
                                         coef(model.criteria.slr.z)[3]),
                                  se.coef=c(model.tmdl.slr.z.summary$coefficients[3,2],
                                          model.money.slr.z.summary$coefficients[3,2],
                                          model.criteria.slr.z.summary$coefficients[3,2]))
  
  model.tmdl.wlr.summary<-summary(model.tmdl.wlr)
  model.money.wlr.summary<-summary(model.money.wlr)
  model.criteria.wlr.summary<-summary(model.criteria.wlr)
  
  wlr.models.output<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                                ceof=c(coef(model.tmdl.wlr)[3],
                                       coef(model.money.wlr)[3],
                                       coef(model.criteria.wlr)[3]),
                                coef.se=c(model.tmdl.wlr.summary$coefficients[3,2],
                                          model.money.wlr.summary$coefficients[3,2],
                                          model.criteria.wlr.summary$coefficients[3,2]))
  
  model.tmdl.slr.summary<-summary(model.tmdl.slr)
  model.money.slr.summary<-summary(model.money.slr)
  model.criteria.slr.summary<-summary(model.criteria.slr)
  
  slr.models.output<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                                coef=c(coef(model.tmdl.slr)[3],
                                       coef(model.money.slr)[3],
                                       coef(model.criteria.slr)[3]),
                                coef.se=c(model.tmdl.slr.summary$coefficients[3,2],
                                          model.money.slr.summary$coefficients[3,2],
                                          model.criteria.slr.summary$coefficients[3,2]))
  
  stream.nitrate.z.scores.slr<-rbind(stream.nitrate.z.scores.slr,z.scores.models.slr)
  stream.nitrate.z.scores.wlr<-rbind(stream.nitrate.z.scores.wlr,z.scores.models.wlr)
  stream.nitrate.policy.wlr<-rbind(stream.nitrate.policy.wlr,wlr.models.output)
  stream.nitrate.policy.slr<-rbind(stream.nitrate.policy.slr,slr.models.output)
  
  stream.nitrate.model.selction.wlr<-rbind(stream.nitrate.model.selction.wlr,model.selction.wlr)
  
  ###slr
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.nitrate.model.selction.slr<-rbind(stream.nitrate.model.selction.slr,model.selction.slr)
  

}


summarySE(stream.nitrate.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")
summarySE(stream.nitrate.model.selction.slr,measurevar = "delta.AICc",groupvars="model")


#### Stream ammonium

stream.ammonium.slopes.predictors<-merge(stream.nh4, policy.loading,by="state")

stream.ammonium.policy.wlr<-data.frame()
stream.ammonium.model.selction.wlr<-data.frame()

stream.ammonium.policy.slr<-data.frame()
stream.ammonium.model.selction.slr<-data.frame()

stream.ammonium.z.scores.slr<-data.frame()
stream.ammonium.z.scores.wlr<-data.frame()

null.coef.stream.ammonium<-data.frame()

for (i in 1:nsims){
  d<-stream.ammonium.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)
  
  model.money.wlr<-lm(slope.wlr~feed+urban.delta+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~feed+urban.delta+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~feed+urban.delta+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~feed+urban.delta+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~feed+urban.delta+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~feed+urban.delta+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~feed+urban.delta+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~feed+urban.delta+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~feed+urban.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~feed+urban.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~feed+urban.delta+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~feed+urban.delta+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~feed+urban.delta,data=d)
  null.slr<-lm(slope.slr~feed+urban.delta,data=d)
  
  null.wlr.summary<-summary(null.wlr)
  
  null.output<-data.frame(feed=coef(null.wlr)[2],feed.se=null.wlr.summary$coefficients[2,2],
                          urban.delta=coef(null.wlr)[3],urban.delta.se=null.wlr.summary$coefficients[3,2],
                          rsq=null.wlr.summary$r.squared)
  null.coef.stream.ammonium<-rbind(null.coef.stream.ammonium,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  stream.ammonium.model.selction.wlr<-rbind(stream.ammonium.model.selction.wlr,model.selction.wlr)
  
  wlr.z.tmdl.summary<-summary(model.tmdl.wlr.z)
  wlr.z.money.summary<-summary(model.money.wlr.z)
  wlr.z.criteria.summary<-summary(model.criteria.wlr.z)
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.wlr.z)[4],
                                         coef(model.money.wlr.z)[4],
                                         coef(model.criteria.wlr.z)[4]),
                                  coef.se=c(wlr.z.tmdl.summary$coefficients[4,2],
                                            wlr.z.money.summary$coefficients[4,2],
                                            wlr.z.criteria.summary$coefficients[4,2]))
  
  slr.z.tmdl.summary<-summary(model.tmdl.slr.z)
  slr.z.money.summary<-summary(model.money.slr.z)
  slr.z.criteria.summary<-summary(model.criteria.slr.z)
  
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[4],
                                         coef(model.money.slr.z)[4],
                                         coef(model.criteria.slr.z)[4]),
                                  coef.se=c(slr.z.tmdl.summary$coefficients[4,2],
                                            slr.z.money.summary$coefficients[4,2],
                                            slr.z.criteria.summary$coefficients[4,2]))
  
  stream.ammonium.z.scores.slr<-rbind(stream.ammonium.z.scores.slr,z.scores.models.slr)
  stream.ammonium.z.scores.wlr<-rbind(stream.ammonium.z.scores.wlr,z.scores.models.wlr)
  
  slr.tmdl.summary<-summary(model.tmdl.slr)
  slr.money.summary<-summary(model.money.slr)
  slr.criteria.summary<-summary(model.criteria.slr)
  
  output.slr<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.slr)[4],
                                coef(model.money.slr)[4],
                                coef(model.criteria.slr)[4]),
                         coef.se=c(slr.tmdl.summary$coefficients[4,2],
                                   slr.money.summary$coefficients[4,2],
                                   slr.criteria.summary$coefficients[4,2]))
  
  stream.ammonium.policy.slr<-rbind(stream.ammonium.policy.slr,output.slr)
  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[4],
                                coef(model.money.wlr)[4],
                                coef(model.criteria.wlr)[4]),
                         coef.se=c(wlr.tmdl.summary$coefficients[4,2],
                                   wlr.money.summary$coefficients[4,2],
                                   wlr.criteria.summary$coefficients[4,2]))
  stream.ammonium.policy.wlr<-rbind(stream.ammonium.policy.wlr,output.wlr)
  
  

  

  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.ammonium.model.selction.slr<-rbind(stream.ammonium.model.selction.slr,model.selction.slr)
  
  }


summarySE(stream.ammonium.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")
summarySE(stream.ammonium.model.selction.slr,measurevar = "delta.AICc",groupvars="model")

### stream TN

stream.tn.slopes.predictors<-merge(stream.tn,policy.loading,by="state")

stream.tn.policy.slr<-data.frame()
stream.tn.policy.wlr<-data.frame()

stream.tn.model.selction.wlr<-data.frame()
stream.tn.model.selction.slr<-data.frame()

stream.tn.z.scores.slr<-data.frame()
stream.tn.z.scores.wlr<-data.frame()

null.model.fit.stream.tn<-data.frame()
for (i in 1:nsims){
  
  d<-stream.tn.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)
  
  model.money.wlr<-lm(slope.wlr~feed+feed.delta+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~feed+feed.delta+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~feed+feed.delta+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~feed+feed.delta+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~feed+feed.delta+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~feed+feed.delta+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~feed+feed.delta+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~feed+feed.delta+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~feed+feed.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~feed+feed.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~feed+feed.delta+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~feed+feed.delta+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~feed+feed.delta,data=d)
  null.slr<-lm(slope.slr~feed+feed.delta,data=d)
  
  null.wlr.summary<-summary(null.wlr)
  
  null.output<-data.frame(feed=coef(null.wlr)[2],feed.se=null.wlr.summary$coefficients[2,2],
                          feed.delta=coef(null.wlr)[3],feed.delta.se=null.wlr.summary$coefficients[3,2],
                          rsq=r.squaredGLMM(null.wlr)[1])
  
  null.model.fit.stream.tn<-rbind(null.model.fit.stream.tn,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  stream.tn.model.selction.wlr<-rbind(stream.tn.model.selction.wlr,model.selction.wlr)
  
  wlr.z.tmdl.summary<-summary(model.tmdl.wlr.z)
  wlr.z.money.summary<-summary(model.money.wlr.z)
  wlr.z.criteria.summary<-summary(model.criteria.wlr.z)
  
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.wlr.z)[4],
                                         coef(model.money.wlr.z)[4],
                                         coef(model.criteria.wlr.z)[4]),
                                  coef.se=c(wlr.z.tmdl.summary$coefficients[4,2],
                                            wlr.z.money.summary$coefficients[4,2],
                                            wlr.z.criteria.summary$coefficients[4,2]))
  
  slr.z.tmdl.summary<-summary(model.tmdl.slr.z)
  slr.z.money.summary<-summary(model.money.slr.z)
  slr.z.criteria.summary<-summary(model.criteria.slr.z)
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[4],
                                         coef(model.money.slr.z)[4],
                                         coef(model.criteria.slr.z)[4]),
                                  coef.se=c(slr.z.tmdl.summary$coefficients[4,2],
                                            slr.z.money.summary$coefficients[4,2],
                                            slr.z.criteria.summary$coefficients[4,2]))
  
  
  stream.tn.z.scores.slr<-rbind(stream.tn.z.scores.slr,z.scores.models.slr)
  stream.tn.z.scores.wlr<-rbind(stream.tn.z.scores.wlr,z.scores.models.wlr)
  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                     coef=c(coef(model.tmdl.wlr)[4],
                            coef(model.money.wlr)[4],
                            coef(model.criteria.wlr)[4]),
                     coef.se=c(wlr.tmdl.summary$coefficients[4,2],
                               wlr.money.summary$coefficients[4,2],
                               wlr.criteria.summary$coefficients[4,2]))
  
  stream.tn.policy.wlr<-rbind(stream.tn.policy.wlr,output.wlr)
  
  slr.tmdl.summary<-summary(model.tmdl.slr)
  slr.money.summary<-summary(model.money.slr)
  slr.criteria.summary<-summary(model.criteria.slr)
 
  output.slr<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                        coef=c(coef(model.tmdl.slr)[4],
                               coef(model.money.slr)[4],
                               coef(model.criteria.slr)[4]),
                        coef.se=c(slr.tmdl.summary$coefficients[4,2],
                                  slr.money.summary$coefficients[4,2],
                                  slr.criteria.summary$coefficients[4,2]))
  
  stream.tn.policy.slr<-rbind(stream.tn.policy.slr,output.slr)
  
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.tn.model.selction.slr<-rbind(stream.tn.model.selction.slr,model.selction.slr)
  
  
}


summarySE(stream.tn.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")
summarySE(stream.tn.model.selction.slr,measurevar = "delta.AICc",groupvars="model")

## stream tp

stream.tp.slopes.predictors<-merge(stream.tp, policy.loading,by="state")

stream.tp.policy.wlr<-data.frame()
stream.tp.model.selction.wlr<-data.frame()

stream.tp.policy.slr<-data.frame()
stream.tp.model.selction.slr<-data.frame()

stream.tp.z.scores.slr<-data.frame()
stream.tp.z.scores.wlr<-data.frame()

null.coef.stream.tp<-data.frame()

for (i in 1:nsims){
  
  d<-stream.tp.slopes.predictors
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)
  
  model.money.wlr<-lm(slope.wlr~ag.delta+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~ag.delta+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~ag.delta+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~ag.delta+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~ag.delta+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~ag.delta+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~ag.delta+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~ag.delta+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~ag.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~ag.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~ag.delta+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~ag.delta+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~ag.delta,data=d)
  null.slr<-lm(slope.slr~ag.delta,data=d)
  
  null.wlr.summary<-summary(null.wlr)
  
  null.output<-data.frame(ag.delta=coef(null.wlr)[2],ag.delta.se=null.wlr.summary$coefficients[2,2],
                          rsq=null.wlr.summary$r.squared)
  null.coef.stream.tp<-rbind(null.coef.stream.tp,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  stream.tp.model.selction.wlr<-rbind(stream.tp.model.selction.wlr,model.selction.wlr)
  
  wlr.z.tmdl.summary<-summary(model.tmdl.wlr.z)
  wlr.z.money.summary<-summary(model.money.wlr.z)
  wlr.z.criteria.summary<-summary(model.criteria.wlr.z)
  
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.wlr.z)[3],
                                         coef(model.money.wlr.z)[3],
                                         coef(model.criteria.wlr.z)[3]),
                                  coef.se=c(wlr.z.tmdl.summary$coefficients[3,2],
                                            wlr.z.money.summary$coefficients[3,2],
                                            wlr.z.criteria.summary$coefficients[3,2]))
  
  slr.z.tmdl.summary<-summary(model.tmdl.slr.z)
  slr.z.money.summary<-summary(model.money.slr.z)
  slr.z.criteria.summary<-summary(model.criteria.slr.z)
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[3],
                                         coef(model.money.slr.z)[3],
                                         coef(model.criteria.slr.z)[3]),
                                  coef.se=c(slr.z.tmdl.summary$coefficients[3,2],
                                            slr.z.money.summary$coefficients[3,2],
                                            slr.z.criteria.summary$coefficients[3,2]))

  
  
  stream.tp.z.scores.slr<-rbind(stream.tp.z.scores.slr,z.scores.models.slr)
  stream.tp.z.scores.wlr<-rbind(stream.tp.z.scores.wlr,z.scores.models.wlr)
  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[3],
                                coef(model.money.wlr)[3],
                                coef(model.criteria.wlr)[3]),
                         coef.se=c(wlr.tmdl.summary$coefficients[3,2],
                                   wlr.money.summary$coefficients[3,2],
                                   wlr.criteria.summary$coefficients[3,2]))
  
  
  slr.tmdl.summary<-summary(model.tmdl.slr)
  slr.money.summary<-summary(model.money.slr)
  slr.criteria.summary<-summary(model.criteria.slr)
  
  output.slr<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.slr)[3],
                                coef(model.money.slr)[3],
                                coef(model.criteria.slr)[3]),
                         coef.se=c(slr.tmdl.summary$coefficients[3,2],
                                   slr.money.summary$coefficients[3,2],
                                   slr.criteria.summary$coefficients[3,2]))
  
  
  stream.tp.policy.wlr<-rbind(stream.tp.policy.wlr,output.wlr)
  stream.tp.policy.slr<-rbind(stream.tp.policy.slr,output.slr)
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.tp.model.selction.slr<-rbind(stream.tp.model.selction.slr,model.selction.slr)
  
  
  
}


summarySE(stream.tp.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")
summarySE(stream.tp.model.selction.slr,measurevar = "delta.AICc",groupvars="model")

## lake tp

lake.tp.slopes.policy<-merge(lake.tp,policy.loading,by="state")

lake.tp.policy.slr<-data.frame()
lake.tp.policy.wlr<-data.frame()

lake.tp.model.selction.slr<-data.frame()
lake.tp.model.selction.wlr<-data.frame()

lake.tp.z.scores.slr<-data.frame()
lake.tp.z.scores.wlr<-data.frame()

lake.tp.z.scores.slr<-data.frame()
lake.tp.z.scores.wlr<-data.frame()

null.coef.lake.tp<-data.frame()

for (i in 1:nsims){
  
  d<-lake.tp.slopes.policy
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)
  

  model.money.wlr<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~ag+fertilizer.delta+undeveloped+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~ag+fertilizer.delta+undeveloped+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~ag+fertilizer.delta+undeveloped+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~ag+fertilizer.delta+undeveloped+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~ag+fertilizer.delta+undeveloped+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~ag+fertilizer.delta+undeveloped+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~ag+fertilizer.delta+undeveloped+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~ag+fertilizer.delta+undeveloped,data=d)
  null.slr<-lm(slope.slr~ag+fertilizer.delta+undeveloped,data=d)
  
  null.wlr.summary<-summary(null.wlr)
  
  null.output<-data.frame(ag=coef(null.wlr)[2],ag.se=null.wlr.summary$coefficients[2,2],
                          fertilizer.delta=coef(null.wlr)[2],
                          fertilizer.delta.se=null.wlr.summary$coefficients[3,2],
                          undeveloped=coef(null.wlr)[4],undeveloped.se=null.wlr.summary$coefficients[4,2],
                          rsq=null.wlr.summary$r.squared)

  null.coef.lake.tp<-rbind(null.coef.lake.tp,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  lake.tp.model.selction.wlr<-rbind(lake.tp.model.selction.wlr,model.selction.wlr)
  lake.tp.model.selction.slr<-rbind(lake.tp.model.selction.slr,model.selction.slr)
  

  wlr.z.tmdl.summary<-summary(model.tmdl.wlr.z)
  wlr.z.money.summary<-summary(model.money.wlr.z)
  wlr.z.criteria.summary<-summary(model.criteria.wlr.z)
  
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.wlr.z)[5],
                                         coef(model.money.wlr.z)[5],
                                         coef(model.criteria.wlr.z)[5]),
                                  coef.se=c(wlr.z.tmdl.summary$coefficients[5,2],
                                            wlr.z.money.summary$coefficients[5,2],
                                            wlr.z.criteria.summary$coefficients[5,2]))
  
  slr.z.tmdl.summary<-summary(model.tmdl.slr.z)
  slr.z.money.summary<-summary(model.money.slr.z)
  slr.z.criteria.summary<-summary(model.criteria.slr.z)
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[5],
                                         coef(model.money.slr.z)[5],
                                         coef(model.criteria.slr.z)[5]),
                                  coef.se=c(slr.z.tmdl.summary$coefficients[5,2],
                                            slr.z.money.summary$coefficients[5,2],
                                            slr.z.criteria.summary$coefficients[5,2]))
  
  
  lake.tp.z.scores.slr<-rbind(lake.tp.z.scores.slr,z.scores.models.slr)
  lake.tp.z.scores.wlr<-rbind(lake.tp.z.scores.wlr,z.scores.models.wlr)
  
  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[5],
                                coef(model.money.wlr)[5],
                                coef(model.criteria.wlr)[5]),
                         coef.se=c(wlr.tmdl.summary$coefficients[5,2],
                                   wlr.money.summary$coefficients[5,2],
                                   wlr.criteria.summary$coefficients[5,2]))
  
  
  slr.tmdl.summary<-summary(model.tmdl.slr)
  slr.money.summary<-summary(model.money.slr)
  slr.criteria.summary<-summary(model.criteria.slr)
  
  output.slr<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.slr)[5],
                                coef(model.money.slr)[5],
                                coef(model.criteria.slr)[5]),
                         coef.se=c(slr.tmdl.summary$coefficients[5,2],
                                   slr.money.summary$coefficients[5,2],
                                   slr.criteria.summary$coefficients[5,2]))
  
  
  
  lake.tp.policy.slr<-rbind(lake.tp.policy.slr,output.slr)
  lake.tp.policy.wlr<-rbind(lake.tp.policy.wlr,output.wlr)
}



summarySE(lake.tp.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")

## lake tn

lake.tn.slopes.policy<-merge(lake.tn,policy.loading,by="state")

lake.tn.policy.slr<-data.frame()
lake.tn.policy.wlr<-data.frame()

lake.tn.model.selction.slr<-data.frame()
lake.tn.model.selction.wlr<-data.frame()

lake.tn.z.scores.slr<-data.frame()
lake.tn.z.scores.wlr<-data.frame()

lake.tn.z.scores.slr<-data.frame()
lake.tn.z.scores.wlr<-data.frame()

null.coef.lake.tn<-data.frame()

for (i in 1:nsims){
  
  d<-lake.tn.slopes.policy
  d<-d[complete.cases(d),]
  d$slope.wlr<-rnorm(nrow(d),d$SLRW_Trend_Estimate,d$SLRW_Trend_Error)
  d$slope.slr<-rnorm(nrow(d),d$SLR_Trend_Estimate,d$SLR_Trend_Error)
  
  
  model.money.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+total.money.sq.km,data=d)
  model.money.slr<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+total.money.sq.km,data=d)
  model.money.wlr.z<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+money.z,data=d)
  model.money.slr.z<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+money.z,data=d)
  
  model.criteria.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+stream.n.criteria,data=d)
  model.criteria.slr<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+stream.n.criteria,data=d)
  model.criteria.wlr.z<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+stream.n.criteria.z,data=d)
  model.criteria.slr.z<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+stream.n.criteria.z,data=d)
  
  model.tmdl.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.slr<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+tmdl.sites.sq.km,data=d)
  model.tmdl.wlr.z<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+tmdl.z,data=d)
  model.tmdl.slr.z<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta+tmdl.z,data=d)
  
  null.wlr<-lm(slope.wlr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta,data=d)
  null.slr<-lm(slope.slr~ag+feed.delta+fertilizer.delta+pop.delta+urban.delta,data=d)
  
  null.wlr.summary<-summary(null.wlr)
  
  null.output<-data.frame(ag=coef(null.wlr)[2],ag.se=null.wlr.summary$coefficients[2,2],
                          feed.delta=coef(null.wlr)[3],feed.delta.se=null.wlr.summary$coefficients[3,2],
                          fertilizer.delta=coef(null.wlr)[4],fertilizer.delta.se=null.wlr.summary$coefficients[4,2],
                          pop.delta=coef(null.wlr)[5],pop.delta.se=null.wlr.summary$coefficients[5,2],
                          urban.delta=coef(null.wlr)[6],urban.delta.se=null.wlr.summary$coefficients[6,2],
                          rsq=null.wlr.summary$r.squared)
  
  null.coef.lake.tn<-rbind(null.coef.lake.tn,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  lake.tn.model.selction.wlr<-rbind(lake.tn.model.selction.wlr,model.selction.wlr)
  lake.tn.model.selction.slr<-rbind(lake.tn.model.selction.slr,model.selction.slr)
  
  
  wlr.z.tmdl.summary<-summary(model.tmdl.wlr.z)
  wlr.z.money.summary<-summary(model.money.wlr.z)
  wlr.z.criteria.summary<-summary(model.criteria.wlr.z)
  
  
  z.scores.models.wlr<-data.frame(model.type="WLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.wlr.z)[7],
                                         coef(model.money.wlr.z)[7],
                                         coef(model.criteria.wlr.z)[7]),
                                  coef.se=c(wlr.z.tmdl.summary$coefficients[7,2],
                                            wlr.z.money.summary$coefficients[7,2],
                                            wlr.z.criteria.summary$coefficients[7,2]))
  
  slr.z.tmdl.summary<-summary(model.tmdl.slr.z)
  slr.z.money.summary<-summary(model.money.slr.z)
  slr.z.criteria.summary<-summary(model.criteria.slr.z)
  
  z.scores.models.slr<-data.frame(model.type="SLR.z",policy=c("tmdl","319","criteria"),
                                  coef=c(coef(model.tmdl.slr.z)[7],
                                         coef(model.money.slr.z)[7],
                                         coef(model.criteria.slr.z)[7]),
                                  coef.se=c(slr.z.tmdl.summary$coefficients[7,2],
                                            slr.z.money.summary$coefficients[7,2],
                                            slr.z.criteria.summary$coefficients[7,2]))

  
  lake.tn.z.scores.slr<-rbind(lake.tp.z.scores.slr,z.scores.models.slr)
  lake.tn.z.scores.wlr<-rbind(lake.tp.z.scores.wlr,z.scores.models.wlr)
  
  wlr.tmdl.summary<-summary(model.tmdl.wlr)
  wlr.money.summary<-summary(model.money.wlr)
  wlr.criteria.summary<-summary(model.criteria.wlr)
  
  output.wlr<-data.frame(model.type="WLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.wlr)[7],
                                coef(model.money.wlr)[7],
                                coef(model.criteria.wlr)[7]),
                         coef.se=c(wlr.tmdl.summary$coefficients[7,2],
                                   wlr.money.summary$coefficients[7,2],
                                   wlr.criteria.summary$coefficients[7,2]))
  
  
  slr.tmdl.summary<-summary(model.tmdl.slr)
  slr.money.summary<-summary(model.money.slr)
  slr.criteria.summary<-summary(model.criteria.slr)
  
  output.slr<-data.frame(model.type="SLR",policy=c("tmdl","319","criteria"),
                         coef=c(coef(model.tmdl.slr)[7],
                                coef(model.money.slr)[7],
                                coef(model.criteria.slr)[7]),
                         coef.se=c(slr.tmdl.summary$coefficients[7,2],
                                   slr.money.summary$coefficients[7,2],
                                   slr.criteria.summary$coefficients[7,2]))
  
  
  lake.tn.policy.slr<-rbind(lake.tn.policy.slr,output.slr)
  lake.tn.policy.wlr<-rbind(lake.tn.policy.wlr,output.wlr)
}



summarySE(lake.tn.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")

##### merging all the policy data to make plots

######################### z scored predictors
lake.tn.z.scores.wlr$nutrient_type<-"Lake TN"
lake.tp.z.scores.wlr$nutrient_type<-"Lake TP"
stream.tn.z.scores.wlr$nutrient_type<-"Stream TN"
stream.tp.z.scores.wlr$nutrient_type<-"Stream TP"
stream.nitrate.z.scores.wlr$nutrient_type<-"Stream Nitrate"
stream.ammonium.z.scores.wlr$nutrient_type<-"Stream Ammonium"

z_scores_plot<-bind_rows(lake.tn.z.scores.wlr,lake.tp.z.scores.wlr,stream.tn.z.scores.wlr,stream.tp.z.scores.wlr,
                          stream.nitrate.z.scores.wlr,stream.ammonium.z.scores.wlr)
z_scores_plot_new<-data.frame(policy=z_scores_plot$policy,nutrient_type=z_scores_plot$nutrient_type,
                              new_coef=rnorm(nrow(z_scores_plot),z_scores_plot$coef,z_scores_plot$coef.se)) # does this do the randomization right?
z_scores_plot_melt<-melt(z_scores_plot_new)

write.csv(z_scores_plot_melt,"./data/clean_data/z_scores_estiamtes.csv")

#### non z-scored predictors

lake.tn.policy.wlr$nutrient_type<-"Lake TN"
lake.tp.policy.wlr$nutrient_type<-"Lake TP"
stream.tn.policy.wlr$nutrient_type<-"Stream TN"
stream.tp.policy.wlr$nutrient_type<-"Stream TP"
stream.nitrate.policy.wlr$nutrient_type<-"Stream Nitrate"
stream.ammonium.policy.wlr$nutrient_type<-"Stream Ammonium"

non_z_summary<-bind_rows(lake.tn.policy.wlr,lake.tp.policy.wlr,stream.tn.policy.wlr,stream.tp.policy.wlr,
                         stream.nitrate.policy.wlr,stream.ammonium.policy.wlr)
non_z_summary_new<-data.frame(policy=non_z_summary$policy,nutreint_type=non_z_summary$nutrient_type,new_value=rnorm(nrow(non_z_summary),non_z_summary$coef,non_z_summary$coef.se))
non_z_melt<-melt(non_z_summary_new)

write.csv(non_z_melt,"./data/clean_data/non_z_scored_estiamtes.csv")

#### loading variables
loading.lake.tn<-melt(data.frame(nutrient_type="Lake TN",ag=rnorm(nsims,null.coef.lake.tn$ag,null.coef.lake.tn$ag.se),
                            feed.delta=rnorm(nsims,null.coef.lake.tn$feed.delta,null.coef.lake.tn$feed.delta.se),
                            fertilizer.delta=rnorm(nsims,null.coef.lake.tn$fertilizer.delta,null.coef.lake.tn$fertilizer.delta.se),
                            pop.delta=rnorm(nsims,null.coef.lake.tn$pop.delta,null.coef.lake.tn$pop.delta.se),
                            urban.delta=rnorm(nsims,null.coef.lake.tn$urban.delta,null.coef.lake.tn$urban.delta.se),
                            rsq=null.coef.lake.tn$rsq))
loading.lake.tp<-melt(data.frame(nutrient_type="Lake TP",ag=rnorm(nsims,null.coef.lake.tp$ag,null.coef.lake.tp$ag.se),
                                 fertilizer.delta=rnorm(nsims,null.coef.lake.tp$fertilizer.delta,null.coef.lake.tp$fertilizer.delta.se),
                                 undeveloped=rnorm(nsims,null.coef.lake.tp$undeveloped,null.coef.lake.tp$undeveloped.se),
                                 rsq=null.coef.lake.tp$rsq))
loading.stream.tn<-melt(data.frame(nutrient_type="Stream TN",feed=rnorm(nsims,null.model.fit.stream.tn$feed,null.model.fit.stream.tn$feed.se),
                        feed.delta=rnorm(nsims,null.model.fit.stream.tn$feed.delta,null.model.fit.stream.tn$feed.delta.se),
                        rsq=null.model.fit.stream.tn$rsq))
loading.stream.tp<-melt(data.frame(nutrient_type="Stream TP",
                                   ag.delta=rnorm(nsims,null.coef.stream.tp$ag.delta,null.coef.stream.tp$ag.delta.se),
                                   rsq=null.coef.stream.tp$rsq))
loading.stream.nitrate<-melt(data.frame(nutrient_type="Stream nitrate",
                                        feed=rnorm(nsims,null.coef.stream.ammonium$feed,null.coef.stream.ammonium$feed.se),
                                        urban=rnorm(nsims,null.coef.stream.ammonium$urban.delta,null.coef.stream.ammonium$urban.delta.se),
                                                    rsq=null.coef.stream.ammonium$rsq))
loading.stream.ammonium<-melt(data.frame(nutrient_type="Stream ammonium",
                                         feed=rnorm(nsims,null.coef.stream.no3$feed,null.coef.stream.no3$feed.se),
                                         rsq=null.coef.stream.no3$rsq))
loading_coeficients<-bind_rows(loading.lake.tn,loading.lake.tp,loading.stream.tn,loading.stream.tp,
                               loading.stream.nitrate,loading.stream.ammonium)

write.csv(loading_coeficients,"./data/clean_data/loading_variables.csv")

tiff(filename="./figures/z_scored_predictors.tiff",units="in",res=600,width=8,height=12,compression="lzw")
z_scores_plot_melt %>% 
  mutate(nutrient=as.factor(nutrient_type)) %>% 
  ggplot(aes(x=value, y=variable))+
  geom_density_ridges(scale=1, rel_min_height=0.01) +
  facet_grid(rows=vars(nutrient_type))+
  theme_bw()
dev.off()

### for correlation matrix
cor(policy.loading$total.money.sq.km,policy.loading$tmdl.sites.sq.km, use="complete.obs")
cor(policy.loading$total.money.sq.km,policy.loading$lake.p.criteria, use="complete.obs")
cor(policy.loading$total.money.sq.km,policy.loading$PC1, use="complete.obs")
cor(policy.loading$total.money.sq.km,policy.loading$PC2, use="complete.obs")

cor(policy.loading$tmdl.sites.sq.km,policy.loading$lake.p.criteria, use="complete.obs")
cor(policy.loading$tmdl.sites.sq.km,policy.loading$PC1, use="complete.obs")
cor(policy.loading$tmdl.sites.sq.km,policy.loading$PC2, use="complete.obs")

cor(policy.loading$lake.p.criteria,policy.loading$PC1, use="complete.obs")
cor(policy.loading$lake.p.criteria,policy.loading$PC2, use="complete.obs")




