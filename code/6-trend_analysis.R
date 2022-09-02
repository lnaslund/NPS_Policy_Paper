library(tidyverse)
library(lubridate)
library(spsurvey)
library(ggplot2)
library(ggrepel)

############## Recast year function
############## This is needed because the trend analysis function estimates a variance for each sampling year
############## thus, treating the 2008/2009 sampling efforts as discrete events reduces the sample size,
############## and becomes problematic in some state/ year combinations

alter_year <- function(x){
  if(x=="2009"){
    return("2008")
  }
  if(x=="2014"){
    return("2013")
  }
  if(x=="2019"){
    return("2018")
  }
  else{
    return(x)
  }
}

#############  Stream nitrate trend analysis
stream_nitrate<-read.csv("data/clean_data/clean_stream_nitrate.csv")

stream_nitrate$siteID<-stream_nitrate$site
stream_nitrate$year<-year(ymd(stream_nitrate$date))
stream_nitrate$single_year <- year(ymd(sapply(as.character(stream_nitrate$year), alter_year), truncated = 2L))
stream_nitrate$Wyear <- stream_nitrate$single_year - min(stream_nitrate$single_year)

# summary of the number of observations per survey year per state
year_no3_state_n <- stream_nitrate %>% group_by(state, Wyear) %>% dplyr::summarize(count=n())

# trend analysis function weights the estimates based on the design of the survey (stratified random sampling)
nitrate.model_slr<-trend_analysis(stream_nitrate,vars_cont = "no3",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat", year="single_year") 
s_no3_slr <- nitrate.model_slr$contsum

# check difference between SLR and weighted SLR (note SLR is simple linear regression)
output_build <- data.frame()
for(i in 1:length(levels(as.factor(stream_nitrate$state)))){
  est <- lm(no3 ~ Wyear, data= stream_nitrate[stream_nitrate$state==levels(as.factor(stream_nitrate$state))[i],])$coeff[2]
  est_std <- summary(lm(no3 ~ Wyear, data= stream_nitrate[stream_nitrate$state==levels(as.factor(stream_nitrate$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(no3 ~ Wyear, data= stream_nitrate[stream_nitrate$state==levels(as.factor(stream_nitrate$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(stream_nitrate$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build <- rbind(output_build, output)
}

difference_no3 <-  s_no3_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename("state" = "Subpopulation", "SLRW_Trend_Estimate" = "Trend_Estimate","SLRW_Trend_Error"="Trend_Std_Error", "SLRW_Intercept_Estimate" = "Intercept_Estimate") %>% 
  left_join(output_build %>% dplyr::rename(SLR_Trend_Estimate=est, SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)

## going to compile these for a supplemental figure
compare_trends_nitrate<-ggplot(difference_no3, aes(SLR_Trend_Estimate,SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))


summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_no3))
write.csv(difference_no3, "data/clean_data/difference_no3.csv", row.names =F)


############### stream ammonium 

stream_ammonium<-read.csv("data/clean_data/clean_stream_nh4.csv")

stream_ammonium$siteID<-stream_ammonium$site
stream_ammonium$year<-year(ymd(stream_ammonium$date))
stream_ammonium$single_year <- year(ymd(sapply(as.character(stream_ammonium$year), alter_year), truncated = 2L))
stream_ammonium$Wyear <- stream_ammonium$single_year - min(stream_ammonium$single_year)

year_nh4_state_n <- stream_ammonium %>% group_by(state, Wyear) %>% dplyr::summarize(count=n())

ammonium.model_slr<-trend_analysis(stream_ammonium,vars_cont = "nh4",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat",year="single_year")
s_nh4_slr<-ammonium.model_slr$contsum

# check difference between SLR and weighted SLR
output_build_nh4 <- data.frame()
for(i in 1:length(levels(as.factor(stream_ammonium$state)))){
  est <- lm(nh4 ~ Wyear, data= stream_ammonium[stream_ammonium$state==levels(as.factor(stream_ammonium$state))[i],])$coeff[2]
  est_std <- summary(lm(nh4 ~ Wyear, data= stream_ammonium[stream_ammonium$state==levels(as.factor(stream_ammonium$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(nh4 ~ Wyear, data= stream_ammonium[stream_ammonium$state==levels(as.factor(stream_ammonium$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(stream_ammonium$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build_nh4 <- rbind(output_build_nh4, output)
}

difference_nh4 <-  s_nh4_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename(state = Subpopulation, SLRW_Trend_Estimate = Trend_Estimate,SLRW_Trend_Error=Trend_Std_Error, SLRW_Intercept_Estimate = Intercept_Estimate) %>% 
  left_join(output_build_nh4 %>% dplyr::rename(SLR_Trend_Estimate=est,SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)

stream_nh4_trend_compare<-ggplot(difference_nh4, aes(SLR_Trend_Estimate,SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_nh4))

write.csv(difference_nh4, "data/clean_data/difference_nh4.csv", row.names =F)

############### stream total nitrogen

stream_tn<-read.csv("data/clean_data/clean_stream_tn.csv")

stream_tn$siteID<-stream_tn$site
stream_tn$year<-year(ymd(stream_tn$date))
stream_tn$single_year <- year(ymd(sapply(as.character(stream_tn$year), alter_year), truncated = 2L))
stream_tn$Wyear <- stream_tn$single_year - min(stream_tn$single_year)


stream.tn.model_slr<-trend_analysis(stream_tn,vars_cont = "tn",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat", year="single_year")
s_tn_slr<-stream.tn.model_slr$contsum

# check difference between SLR and weighted SLR
output_build_tn <- data.frame()
for(i in 1:length(levels(as.factor(stream_tn$state)))){
  est <- lm(tn ~ Wyear, data= stream_tn[stream_tn$state==levels(as.factor(stream_tn$state))[i],])$coeff[2]
  est_std <- summary(lm(tn ~ Wyear, data= stream_tn[stream_tn$state==levels(as.factor(stream_tn$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(tn ~ Wyear, data= stream_tn[stream_tn$state==levels(as.factor(stream_tn$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(stream_tn$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build_tn <- rbind(output_build_tn, output)
}

difference_tn <-s_tn_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename(state = Subpopulation, SLRW_Trend_Estimate = Trend_Estimate,SLRW_Trend_Error=Trend_Std_Error, SLRW_Intercept_Estimate = Intercept_Estimate)%>% 
  left_join(output_build_tn %>% dplyr::rename(SLR_Trend_Estimate=est,SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)


stream_tn_trend_compare<-ggplot(difference_tn, aes(SLR_Trend_Estimate,SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_tn))
write.csv(difference_tn, "data/clean_data/difference_tn.csv", row.names =F)

############### stream total phosphorus

stream_tp<-read.csv("data/clean_data/clean_stream_tp.csv")

stream_tp$siteID<-stream_tp$site
stream_tp$year<-year(ymd(stream_tp$date))
stream_tp$single_year <- year(ymd(sapply(as.character(stream_tp$year), alter_year), truncated = 2L))
stream_tp$Wyear <- stream_tp$single_year - min(stream_tp$single_year)

tp.model_slr<-trend_analysis(stream_tp,vars_cont = "tp",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat", year="single_year")
s_tp_slr<-tp.model_slr$contsum

output_build_tp <- data.frame()
for(i in 1:length(levels(as.factor(stream_tp$state)))){
  est <- lm(tp ~ Wyear, data= stream_tp[stream_tp$state==levels(as.factor(stream_tp$state))[i],])$coeff[2]
  est_std <- summary(lm(tp ~ Wyear, data= stream_tp[stream_tp$state==levels(as.factor(stream_tp$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(tp ~ Wyear, data= stream_tp[stream_tp$state==levels(as.factor(stream_tp$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(stream_tn$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build_tp <- rbind(output_build_tp, output)
}

difference_tp <-  s_tp_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename(state = Subpopulation, SLRW_Trend_Estimate = Trend_Estimate, SLRW_Trend_Error=Trend_Std_Error, SLRW_Intercept_Estimate = Intercept_Estimate) %>% 
  left_join(output_build_tp %>% dplyr::rename(SLR_Trend_Estimate=est, SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)


stream_tp_trend_compare<-ggplot(difference_tp, aes(SLR_Trend_Estimate,SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_tp))

write.csv(difference_tp, "data/clean_data/difference_tp.csv", row.names =F)

############# lake TP

lake_tp<-read.csv("data/clean_data/clean_lake_tp.csv")

lake_tp$year<-year(ymd(lake_tp$date))
lake_tp$single_year <- year(ymd(sapply(as.character(lake_tp$year), alter_year), truncated = 2L))
lake_tp$Wyear <- lake_tp$single_year - min(lake_tp$single_year)

lake_tp.model_slr<-trend_analysis(lake_tp,vars_cont = "tp",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat", year="single_year")
l_tp_slr<-lake_tp.model_slr$contsum

output_build_tp_lake <- data.frame()
for(i in 1:length(levels(as.factor(lake_tp$state)))){
  est <- lm(tp ~ Wyear, data= lake_tp[lake_tp$state==levels(as.factor(lake_tp$state))[i],])$coeff[2]
  est_std <- summary(lm(tp ~ Wyear, data= lake_tp[lake_tp$state==levels(as.factor(lake_tp$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(tp ~ Wyear, data= lake_tp[lake_tp$state==levels(as.factor(lake_tp$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(lake_tp$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build_tp_lake <- rbind(output_build_tp_lake, output)
}

difference_tp_lake <-  l_tp_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename(state = Subpopulation, SLRW_Trend_Estimate = Trend_Estimate, SLRW_Trend_Error=Trend_Std_Error, SLRW_Intercept_Estimate = Intercept_Estimate) %>% 
  left_join(output_build_tp_lake %>% dplyr::rename(SLR_Trend_Estimate=est,SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)


lake_tp_trend_compare<-ggplot(difference_tp_lake, aes(SLR_Trend_Estimate, SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_tp_lake))
write.csv(difference_tp_lake, "data/clean_data/difference_tp_lake.csv", row.names =F)


################ lake TN

lake_tn<-read.csv("data/clean_data/clean_lake_tn.csv")

lake_tn$year<-year(ymd(lake_tn$date))
lake_tn$single_year <- year(ymd(sapply(as.character(lake_tn$year), alter_year), truncated = 2L))
lake_tn$Wyear <- lake_tn$single_year - min(lake_tn$single_year)

lake_tn.model_slr<-trend_analysis(lake_tn,vars_cont = "tn",subpops = "state",model_cont="SLR",xcoord="lon",ycoord="lat", year="single_year")
l_tn_slr<-lake_tn.model_slr$contsum

output_build_tn_lake <- data.frame()
for(i in 1:length(levels(as.factor(lake_tn$state)))){
  est <- lm(tn ~ Wyear, data= lake_tn[lake_tn$state==levels(as.factor(lake_tn$state))[i],])$coeff[2]
  est_std <- summary(lm(tn ~ Wyear, data= lake_tn[lake_tn$state==levels(as.factor(lake_tn$state))[i],]))$coeff[[4]]
  slr_intercept <- lm(tn ~ Wyear, data= lake_tn[lake_tn$state==levels(as.factor(lake_tn$state))[i],])$coeff[1]
  output <- data.frame(state=levels(as.factor(lake_tn$state))[i], est=est, est_std=est_std, slr_intercept=slr_intercept)
  output_build_tn_lake <- rbind(output_build_tn_lake, output)
}

#### lake tn and tp differences need error around estimates to be included

difference_tn_lake <- l_tn_slr %>% select(Subpopulation, Trend_Estimate, Trend_Std_Error,Intercept_Estimate) %>% dplyr::rename(state = Subpopulation, SLRW_Trend_Estimate = Trend_Estimate,SLRW_Trend_Error=Trend_Std_Error, SLRW_Intercept_Estimate = Intercept_Estimate) %>% 
  left_join(output_build_tn_lake %>% dplyr::rename(SLR_Trend_Estimate=est,SLR_Trend_Error=est_std, SLR_Intercept_Estimate = slr_intercept), by = "state") %>% 
  mutate(diff_slrw_slr = round(abs(SLRW_Trend_Estimate-SLR_Trend_Estimate), 6), sign_switch_slrw_slr = (SLRW_Trend_Estimate >0 & SLR_Trend_Estimate <0)| (SLRW_Trend_Estimate <0 & SLR_Trend_Estimate >0)) %>% 
  select(state, SLRW_Trend_Estimate, SLR_Trend_Estimate, diff_slrw_slr, sign_switch_slrw_slr, SLRW_Intercept_Estimate, SLR_Intercept_Estimate, SLRW_Trend_Error, SLR_Trend_Error) %>% 
  filter(is.nan(SLRW_Trend_Error)==F)

lake_tn_trend_compare<-ggplot(difference_tn_lake, aes(SLR_Trend_Estimate,SLRW_Trend_Estimate))+
  geom_smooth(method="lm")+
  geom_point()+
  geom_abline(slope = 1, linetype="dashed")+
  geom_text_repel(size=5, aes(label=state))+
  labs(x="Unweighted Trend Estimate", y="Weighted Trend Estimate")+
  theme_bw()+
  theme(axis.title = element_text(size=24, color='black', face="bold"), axis.text = element_text(size=14, color='black'), strip.text.y = element_text(
    size = 14, color = "black", face = "bold"
  ))

summary(lm(SLRW_Trend_Estimate~SLR_Trend_Estimate, data=difference_tn_lake))
write.csv(difference_tn_lake, "data/clean_data/difference_tn_lake.csv", row.names =F)

##################### saving figures

tiff(filename="./figures/methods.compare.streams.tiff",units="in",res=600,width=16,height=12,compression="lzw")
plot_grid(compare_trends_nitrate,stream_nh4_trend_compare,stream_tn_trend_compare,stream_tp_trend_compare,ncol=2,labels="AUTO",label_x=0.12,label_y=0.98,label_size=25)
dev.off()

tiff(filename="./figures/methods.compare.lakes.tiff",units="in",res=600,width=16,height=6,compression="lzw")
plot_grid(lake_tn_trend_compare,lake_tp_trend_compare,ncol=2,labels="AUTO",label_x=0.12,label_y=0.98,label_size=25)
dev.off()

################## How correlated are the different nutrient trends

snitrate<-data.frame(state=difference_no3$state,stream.nitrate.trend=difference_no3$SLRW_Trend_Estimate)
sammonium<-data.frame(state=difference_nh4$state,stream.ammonium.trend=difference_nh4$SLRW_Trend_Estimate)
stp<-data.frame(state=difference_tp$state,stream.tp.trend=difference_tp$SLRW_Trend_Estimate)
stn<-data.frame(state=difference_tn$state,stream.tn.trend=difference_tn$SLRW_Trend_Estimate)
ltn<-data.frame(state=difference_tn_lake$state,lake.tn.trend=difference_tn_lake$SLRW_Trend_Estimate)
ltp<-data.frame(state=difference_tp_lake$state,lake.tp.trend=difference_tp_lake$SLRW_Trend_Estimate)

cma<-merge(snitrate,sammonium,by="state")
cmb<-merge(cma,stp,by="state")
cmc<-merge(cmb,stn,by="state")
cmd<-merge(cmc,ltn,by="state")
cmf<-merge(cmd,ltp,by="state")

correlation.matrix.trends<-cor(cmf[,2:7],method="pearson",use="complete.obs")
plot(cmf[,2:7])

write.csv(correlation.matrix.trends,file="data/correlation.matrix.nutrient.trends.csv")

#rm(list=ls())


########### Plots of trends
coloring_bars<-function(mean.trend,se.trend){
  temp<-data.frame(mean.trend,se.trend)
  temp$bar.color<-"black"
  for (i in 1:nrow(temp)){
  if (temp$mean.trend[i]-temp$se.trend[i]>0){temp$bar.color[i]<-"Red"}
  if (temp$mean.trend[i]+temp$se.trend[i]<0){temp$bar.color[i]<-"Blue"}
  }
  return(temp$bar.color)
}



difference_no3$state<-factor(difference_no3$state,levels=difference_no3$state[order(difference_no3$SLRW_Trend_Estimate)])
difference_no3$bar_color<-coloring_bars(difference_no3$SLRW_Trend_Estimate,difference_no3$SLRW_Trend_Error)

stream_no3_plot<-ggplot(difference_no3,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("NO3 trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_no3_plot

difference_nh4$state<-factor(difference_nh4$state,levels=difference_nh4$state[order(difference_nh4$SLRW_Trend_Estimate)])
difference_nh4$bar_color<-coloring_bars(difference_nh4$SLRW_Trend_Estimate,difference_nh4$SLRW_Trend_Error)

stream_nh4_plot<-ggplot(difference_nh4,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("NH4 trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_nh4_plot

difference_tn$state<-factor(difference_tn$state,levels=difference_tn$state[order(difference_tn$SLRW_Trend_Estimate)])
difference_tn$bar_color<-coloring_bars(difference_tn$SLRW_Trend_Estimate,difference_tn$SLRW_Trend_Error)


stream_tn_plot<-ggplot(difference_tn,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TN trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_tn_plot

difference_tp$state<-factor(difference_tp$state,levels=difference_tp$state[order(difference_tp$SLRW_Trend_Estimate)])
difference_tp$bar_color<-coloring_bars(difference_tp$SLRW_Trend_Estimate,difference_tp$SLRW_Trend_Error)

stream_tp_plot<-ggplot(difference_tp,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TP trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
stream_tp_plot

jpeg(filename="./figures/all_streams.jpeg",units="in",res=600,width=16,height=12)
plot_grid(stream_tn_plot,stream_tp_plot,stream_no3_plot,stream_nh4_plot,ncol=4,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()

#############

difference_tn_lake$state<-factor(difference_tn_lake$state,levels=difference_tn_lake$state[order(difference_tn_lake$SLRW_Trend_Estimate)])
difference_tn_lake$bar_color<-coloring_bars(difference_tn_lake$SLRW_Trend_Estimate,difference_tn_lake$SLRW_Trend_Error)

lake_tn_plot<-ggplot(difference_tn_lake,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TN trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
lake_tn_plot

difference_tp_lake$state<-factor(difference_tp_lake$state,levels=difference_tp_lake$state[order(difference_tp_lake$SLRW_Trend_Estimate)])
difference_tp_lake$bar_color<-coloring_bars(difference_tp_lake$SLRW_Trend_Estimate,difference_tp_lake$SLRW_Trend_Error)


lake_tp_plot<-ggplot(difference_tp_lake,aes(x=SLRW_Trend_Estimate,y=state,color=bar_color))+geom_point()+
  geom_errorbarh(aes(xmin=SLRW_Trend_Estimate-SLRW_Trend_Error,xmax=SLRW_Trend_Estimate+SLRW_Trend_Error))+
  theme_classic()+geom_vline(xintercept = 0,linetype="dashed")+xlab(expression("TP trend ("*mu*g~L^-1~"year"^-1*")"))+
  ylab("")+theme(text = element_text(size=20))+scale_color_manual(values = c("gray50","blue","red"))+
  theme(legend.position = "none")
lake_tp_plot


jpeg(filename="./figures/all_lakes.jpeg",units="in",res=600,width=8,height=12)
plot_grid(lake_tn_plot,lake_tp_plot,ncol=2,labels="AUTO",label_x=0.9,label_y=0.95)
dev.off()