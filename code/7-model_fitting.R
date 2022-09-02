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

state_tmdl_plot<-ggplot(tmdl,aes(x=tmdl.sites.sq.km,y=states))+geom_point(size=2)+
  theme_bw()+xlab(expression("TMDL sites visited (2-year-cycle"^-1~"km"^-2*")"))+
  ylab("")+theme(text = element_text(size=20))

state_319_plot<-ggplot(s319,aes(x=total.money.sq.km,y=states))+geom_point(size=2)+
  theme_bw()+xlab(expression("319 Dollars (Dollars"~"km"^-2*")"))+
  ylab("")+theme(text = element_text(size=20))

state_nutrient_criteria <- ggplot(nutrient.criteria.melt %>% 
                                    mutate(variable = recode(variable, "lake.p.criteria"="Lake P", "lake.n.criteria"="Lake N", "stream.n.criteria"="Stream N", "stream.p.criteria"="Stream P")),
                                  aes(x=value,y=states))+
    geom_bar(stat="identity", position = position_dodge())+
  theme_bw()+
  facet_grid(~variable) + 
  ylab("") + 
  xlab("Nutrient Criteria Score")+ 
  theme(text = element_text(size=20))
  
jpeg(filename="./figures/predictors.jpeg",units="in",res=600,width=17,height=12)
plot_grid(state_319_plot,state_tmdl_plot,state_nutrient_criteria,ncol=3,labels="AUTO", label_size = 24)
dev.off()

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

nsims<-1000

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
  null.slr<-lm(slope.slr~feed,data=d)
  
  null.output<-data.frame(feed=coef(null.wlr)[2],rsq=r.squaredGLMM(null.wlr)[1])
  
  null.coef.stream.no3<-rbind(null.coef.stream.no3,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.tmdl.wlr.z.summary<-summary(model.tmdl.wlr.z)
  model.money.wlr.z.summary<-summary(model.money.wlr.z)
  model.criteria.wlr.z.summary<-summary(model.criteria.wlr.z)
  
  z.scores.models.wlr<-data.frame(model.type="WLR",coef=c(coef(model.tmdl.wlr.z)[3],coef(model.money.wlr.z)[3],
                              coef(model.criteria.wlr.z)[3]),policy=c("tmdl","319","criteria"),
                              se.coef=c(model.tmdl.wlr.z.summary$coefficients[3,2],
                                        model.money.wlr.z.summary$coefficients[3,2],
                                        model.criteria.wlr.z.summary$coefficients[3,2]))
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  
  stream.nitrate.z.scores.slr<-rbind(stream.nitrate.z.scores.slr,z.scores.models.slr)
  stream.nitrate.z.scores.wlr<-rbind(stream.nitrate.z.scores.wlr,z.scores.models.wlr)
  
  
  stream.nitrate.model.selction.wlr<-rbind(stream.nitrate.model.selction.wlr,model.selction.wlr)
  
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  stream.nitrate.policy.wlr<-rbind(stream.nitrate.policy.wlr,output.wlr)
  
  ###slr
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.nitrate.model.selction.slr<-rbind(stream.nitrate.model.selction.slr,model.selction.slr)
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  stream.nitrate.policy.slr<-rbind(stream.nitrate.policy.slr,output.slr)
}


stream.nitrate.policy.wlr$nutrient<-"stream_nitrate"
stream.nitrate.policy.slr$nutrient<-"stream_nitrate"
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
  
  null.output<-data.frame(feed=coef(null.wlr)[2],urban.delta=coef(null.wlr)[3],rsq=r.squaredGLMM(null.wlr)[1])
  null.coef.stream.ammonium<-rbind(null.coef.stream.ammonium,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  z.scores.models.wlr<-data.frame(model.type="WLR",tmdl.wlr.z=coef(model.tmdl.wlr.z)[3],
                                  money.wlr.z=coef(model.money.wlr.z)[3],
                                  criteria.wlr.z=coef(model.criteria.wlr.z)[3])
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  stream.ammonium.z.scores.slr<-rbind(stream.ammonium.z.scores.slr,z.scores.models.slr)
  stream.ammonium.z.scores.wlr<-rbind(stream.ammonium.z.scores.wlr,z.scores.models.wlr)
  
  
  stream.ammonium.model.selction.wlr<-rbind(stream.ammonium.model.selction.wlr,model.selction.wlr)
  
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  stream.ammonium.policy.wlr<-rbind(stream.ammonium.policy.wlr,output.wlr)
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.ammonium.model.selction.slr<-rbind(stream.ammonium.model.selction.slr,model.selction.slr)
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  stream.ammonium.policy.slr<-rbind(stream.ammonium.policy.slr,output.slr)
}

stream.ammonium.policy.wlr$nutrient<-"stream_ammonium"
stream.ammonium.policy.slr$nutrient<-"stream_ammonium"
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
  
  null.output<-data.frame(feed=coef(null.wlr)[2],feed.delta=coef(null.wlr)[3],rsq=r.squaredGLMM(null.wlr)[1])
  
  null.model.fit.stream.tn<-rbind(null.model.fit.stream.tn,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  z.scores.models.wlr<-data.frame(model.type="WLR",tmdl.wlr.z=coef(model.tmdl.wlr.z)[3],
                                  money.wlr.z=coef(model.money.wlr.z)[3],
                                  criteria.wlr.z=coef(model.criteria.wlr.z)[3])
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  stream.tn.z.scores.slr<-rbind(stream.tn.z.scores.slr,z.scores.models.slr)
  stream.tn.z.scores.wlr<-rbind(stream.tn.z.scores.wlr,z.scores.models.wlr)
  
  
  stream.tn.model.selction.wlr<-rbind(stream.tn.model.selction.wlr,model.selction.wlr)
  
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  stream.tn.policy.wlr<-rbind(stream.tn.policy.wlr,output.wlr)
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.tn.model.selction.slr<-rbind(stream.tn.model.selction.slr,model.selction.slr)
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  stream.tn.policy.slr<-rbind(stream.tn.policy.slr,output.slr)
}

stream.tn.policy.wlr$nutrient<-"stream_TN"
stream.tn.policy.slr$nutrient<-"stream_TN"
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
  
  null.output<-data.frame(ag.delta=coef(null.wlr)[2],rsq=r.squaredGLMM(null.wlr)[1])
  null.coef.stream.tp<-rbind(null.coef.stream.tp,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  stream.tp.model.selction.wlr<-rbind(stream.tp.model.selction.wlr,model.selction.wlr)
  
  z.scores.models.wlr<-data.frame(model.type="WLR",tmdl.wlr.z=coef(model.tmdl.wlr.z)[3],
                                  money.wlr.z=coef(model.money.wlr.z)[3],
                                  criteria.wlr.z=coef(model.criteria.wlr.z)[3])
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  stream.tp.z.scores.slr<-rbind(stream.tp.z.scores.slr,z.scores.models.slr)
  stream.tp.z.scores.wlr<-rbind(stream.tp.z.scores.wlr,z.scores.models.wlr)
  
  
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  stream.tp.policy.wlr<-rbind(stream.tp.policy.wlr,output.wlr)
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  stream.tp.model.selction.slr<-rbind(stream.tp.model.selction.slr,model.selction.slr)
  
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  stream.tp.policy.slr<-rbind(stream.tp.policy.slr,output.slr)
}
stream.tp.policy.wlr$nutrient<-"stream_TP"
stream.tp.policy.slr$nutrient<-"stream_TP"

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
  
  null.output<-data.frame(fertilizer.delta=coef(null.wlr)[2],undeveloped=coef(null.wlr)[3],rsq=r.squaredGLMM(null.wlr)[1])

  null.coef.lake.tp<-rbind(null.coef.lake.tp,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  
  
  lake.tp.model.selction.wlr<-rbind(lake.tp.model.selction.wlr,model.selction.wlr)
  lake.tp.model.selction.slr<-rbind(lake.tp.model.selction.slr,model.selction.slr)
  
  z.scores.models.wlr<-data.frame(model.type="WLR",tmdl.wlr.z=coef(model.tmdl.wlr.z)[3],
                                  money.wlr.z=coef(model.money.wlr.z)[3],
                                  criteria.wlr.z=coef(model.criteria.wlr.z)[3])
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  lake.tp.z.scores.slr<-rbind(lake.tp.z.scores.slr,z.scores.models.slr)
  lake.tp.z.scores.wlr<-rbind(lake.tp.z.scores.wlr,z.scores.models.wlr)
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                     coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  lake.tp.policy.slr<-rbind(lake.tp.policy.slr,output.slr)
  lake.tp.policy.wlr<-rbind(lake.tp.policy.wlr,output.wlr)
}

lake.tp.policy.wlr$nutrient<-"lake_TP"

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
  
  null.output<-data.frame(ag=coef(null.wlr)[2],feed.delta=coef(null.wlr)[3],
                          fertilizer.delta=coef(null.wlr)[4],pop.delta=coef(null.wlr)[5],
                          urban.delta=coef(null.wlr)[6],rsq=r.squaredGLMM(null.wlr)[1])
  
  null.coef.lake.tn<-rbind(null.coef.lake.tn,null.output)
  
  model.selction.wlr<-data.frame(AICc(null.wlr,model.money.wlr,model.criteria.wlr,model.tmdl.wlr))
  model.selction.wlr$model<-row.names(model.selction.wlr)
  model.selction.wlr$delta.AICc<-model.selction.wlr$AICc-model.selction.wlr[model.selction.wlr$model=="null.wlr","AICc"]
  
  model.selction.slr<-data.frame(AICc(null.slr,model.money.slr,model.criteria.slr,model.tmdl.slr))
  model.selction.slr$model<-row.names(model.selction.slr)
  model.selction.slr$delta.AICc<-model.selction.slr$AICc-model.selction.slr[model.selction.slr$model=="null.slr","AICc"]
  
  
  
  lake.tn.model.selction.wlr<-rbind(lake.tn.model.selction.wlr,model.selction.wlr)
  lake.tn.model.selction.slr<-rbind(lake.tn.model.selction.slr,model.selction.slr)
  
  z.scores.models.wlr<-data.frame(model.type="WLR",tmdl.wlr.z=coef(model.tmdl.wlr.z)[3],
                                  money.wlr.z=coef(model.money.wlr.z)[3],
                                  criteria.wlr.z=coef(model.criteria.wlr.z)[3])
  
  z.scores.models.slr<-data.frame(model.type="SLR",tmdl.slr.z=coef(model.tmdl.slr.z)[3],
                                  money.slr.z=coef(model.money.slr.z)[3],
                                  criteria.slr.z=coef(model.criteria.slr.z)[3])
  lake.tn.z.scores.slr<-rbind(lake.tp.z.scores.slr,z.scores.models.slr)
  lake.tn.z.scores.wlr<-rbind(lake.tp.z.scores.wlr,z.scores.models.wlr)
  
  output.slr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.slr)[3],coef(model.criteria.slr)[3],coef(model.tmdl.slr)[3]))
  output.wlr<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                         coefficient=c(coef(model.money.wlr)[3],coef(model.criteria.wlr)[3],coef(model.tmdl.wlr)[3]))
  lake.tn.policy.slr<-rbind(lake.tn.policy.slr,output.slr)
  lake.tn.policy.wlr<-rbind(lake.tn.policy.wlr,output.wlr)
}

lake.tn.policy.wlr$nutrient<-"lake_TN"

summarySE(lake.tn.model.selction.wlr,measurevar = "delta.AICc",groupvars="model")

##### merging all the policy data to make plots
policy_effects<-bind_rows(lake.tn.policy.wlr,lake.tp.policy.wlr,stream.tn.policy.wlr,stream.tp.policy.wlr,
                          stream.nitrate.policy.wlr,stream.ammonium.policy.wlr)


alt.effect.size<-data.frame(policy.variable=c("319 dollars","Nutrient Criteria","tmdl"),
                            best.state=c(max(policy.loading$total.money.sq.km),26,max(policy.loading$tmdl.sites.sq.km, na.rm=T)))

policy_effects_alt<-merge(policy_effects,alt.effect.size,by="policy.variable")
policy_effects_alt$decade.nutrient.change<-policy_effects_alt$best.state*10*policy_effects_alt$coefficient

################## reviewing policy effects

z_df <- policy_effects_alt %>% group_by(policy.variable,nutrient) %>% summarize(mean=mean(coefficient), sd=sd(coefficient)) %>% as.data.frame()

# policy_effects_alt <- policy_effects_alt %>% mutate(z_scored = case_when(
#   policy.variable=="tmdl"~ (coefficient-z_df[3,2])/z_df[3,3],
#   policy.variable=="Nutrient Criteria"~ (coefficient-z_df[2,2])/z_df[2,3],
#   policy.variable=="319 dollars"~ (coefficient-z_df[1,2])/z_df[1,3]
# )) 
# 
# policy_effects_alt %>% 
#   group_by(policy.variable, nutrient) %>% 
#   summarize(bottom= quantile(coefficient, 0.025), 
#             first= quantile(coefficient, 0.25), 
#             median= quantile(coefficient, 0.5), 
#             third= quantile(coefficient, 0.75), 
#             top=quantile(coefficient, 0.975))
# 
# 
# policy_effects_alt$nutrient_fct <- factor(policy_effects_alt$nutrient, 
#                                           levels=c("lake_TN", "lake_TP", "stream_ammonium", "stream_nitrate", "stream_TN", 
#                                                                          "stream_TP"), 
#                                           labels= c("Lake TN", "Lake TP", "Stream NH4+", "Stream NO3-", 
#                                                     "Stream TN","Stream TP"))

policy_effects_alt %>% 
  mutate(nutrient_fct=as.factor(nutrient), ) %>% 
  ggplot(aes(x=coefficient, y=policy.variable))+
  geom_density_ridges(scale=1, rel_min_height=0.01) +
  facet_grid(rows=vars(nutrient_fct))+
  theme_bw()
  
######################### z scored predictors
lake.tn.z.scores.wlr$nutrient_type<-"Lake TN"
lake.tp.z.scores.wlr$nutrient_type<-"Lake TP"
stream.tn.z.scores.wlr$nutrient_type<-"Stream TN"
stream.tp.z.scores.wlr$nutrient_type<-"Stream TP"
stream.nitrate.z.scores.wlr$nutrient_type<-"Stream Nitrate"
stream.ammonium.z.scores.wlr$nutrient_type<-"Stream Ammonium"

z_scores_plot<-bind_rows(lake.tn.z.scores.wlr,lake.tp.z.scores.wlr,stream.tn.z.scores.wlr,stream.tp.z.scores.wlr,
                          stream.nitrate.z.scores.wlr,stream.ammonium.z.scores.wlr)
z_scores_plot_melt<-melt(z_scores_plot)

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



### HOW ARE WE USING THE DELTA AICc?
### Should we use mean from the runs?
### Need to summarize best state units 


############## Loading variables
null.coef.lake.tn$type<-"Lake TN"
null.coef.lake.tp$type<-"Lake TP"
null.coef.stream.ammonium$type<-"Stream Ammonium"
null.coef.stream.no3$type<-"Stream Nitrate"
null.coef.stream.tp$type<-"Stream TP"
null.model.fit.stream.tn$type<-"Stream TN"


predictor.estimates<-bind_rows(melt(null.coef.lake.tn),melt(null.coef.lake.tp),
                               melt(null.coef.stream.ammonium),melt(null.coef.stream.no3),
                               melt(null.coef.stream.tp),melt(null.model.fit.stream.tn))

predictor.estimates.sum<-aggregate(value~type+variable,predictor.estimates,quantile,c(0.025,0.5,0.975))


ggplot(predictor.estimates.sum[predictor.estimates.sum$variable!="rsq",],
       aes(x=value[,2],y=variable,color=type))+geom_point(size=3, position=position_dodge(width=0.3))+theme_classic()+
  xlab("Paramater estimate")+ylab("")+geom_vline(xintercept = 0,linetype="dashed")+
  geom_errorbar(aes(xmin=value[,1],xmax=value[,3]),width=0.0001, position=position_dodge(width=0.3))
