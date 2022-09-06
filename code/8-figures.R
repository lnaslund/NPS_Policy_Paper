
library(ggplot2)
library(cowplot)
library(Rmisc)

########### Plots of trends
########### Trend slopes
difference_no3 <- read.csv("./data/clean_data/difference_no3.csv")
difference_nh4 <- read.csv("./data/clean_data/difference_nh4.csv")
difference_tn <- read.csv("./data/clean_data/difference_tn.csv")
difference_tp <- read.csv("./data/clean_data/difference_tp.csv")
difference_tn_lake <- read.csv("./data/clean_data/difference_tn_lake.csv")
difference_tp_lake <- read.csv("./data/clean_data/difference_tp_lake.csv")
###


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


####################### Plotting policy variable ranges

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

########################Plotting coeficients of loading variable.names

loading<-read.csv("./data/clean_data/loading_variables.csv")

rsq<-loading[loading$variable=="rsq",]
aggregate(value~nutrient_type,rsq,mean) ### median r2 values of models

loading2<-loading[loading$variable!="rsq",]

loading.means<-aggregate(value~variable+nutrient_type,loading2,mean)
loading.lower<-aggregate(value~variable+nutrient_type,loading2,quantile,0.025)
names(loading.lower)[names(loading.lower)=="value"]<-"lower.ci"
loading.upper<-aggregate(value~variable+nutrient_type,loading2,quantile,0.975)
names(loading.upper)[names(loading.upper)=="value"]<-"upper.ci"

loading3<-merge(loading.means,loading.upper,by=c("variable","nutrient_type"))
loading.summary<-merge(loading3,loading.lower,by=c("variable","nutrient_type"))

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#D55E00")

loading.plot<-ggplot(loading.summary,aes(x=value,y=variable,color=nutrient_type))+
  geom_point(size=2,position=position_dodge(width=0.3))+
  geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci),height=0.001,position=position_dodge(width=0.3))+
  theme_classic()+
  geom_vline(xintercept = 0,linetype="dashed")+ylab("")+xlab("Parameter Estimate")+
  theme(text = element_text(size = 20),legend.position = c(0.12,0.9),legend.text.align = 0)+
  scale_color_manual(values = cbbPalette,name="",labels=c('Lake TN', 'Lake TP',
                                                          expression("Stream NH"[4]),
                                                          expression("Stream NO"[3]),
                                                          "Stream TN",
                                                          "Stream TP"))+
  scale_y_discrete(labels=c("urban.delta" = expression(Delta*"Urban"), "urban" = "Urban",
                            "undeveloped" = "Undeveloped","pop.delta"=expression(Delta*"Population"),
                            "fertilizer.delta"=expression(Delta*"Fertilizer"),
                            "feed.delta"=expression(Delta*"Animal feed"),
                            "feed"="Animal Feed","ag.delta"=expression(Delta*"Agriculture"),
                            "ag"="Agriculture"))
loading.plot


jpeg(filename="./figures/loading_coef.jpeg",units="in",res=600,width=8,height=10)
loading.plot
dev.off()


###################### plotting z scored coeficients

zs<-read.csv("./data/clean_data/z_scores_estiamtes.csv")

z.means<-aggregate(value~policy+nutrient_type,zs,mean)
z.lower<-aggregate(value~policy+nutrient_type,zs,quantile,0.025)
names(z.lower)[names(z.lower)=="value"]<-"lower.ci"
z.upper<-aggregate(value~policy+nutrient_type,zs,quantile,0.975)
names(z.upper)[names(z.upper)=="value"]<-"upper.ci"

zs2<-merge(z.means,z.lower,by=c("nutrient_type","policy"))
z.summary<-merge(zs2,z.upper,by=c("nutrient_type","policy"))

policy.plot<-ggplot(z.summary,aes(x=value,y=nutrient_type,color=policy))+geom_point(size=3,position=position_dodge(width=0.3))+
  geom_errorbarh(aes(xmin=lower.ci,xmax=upper.ci),height=0.0001,position=position_dodge(width=0.3))+
  theme_classic()+
  geom_vline(xintercept = 0,linetype="dashed")+ylab("")+xlab("Parameter Estimate")+
  theme(text = element_text(size = 20),legend.position = c(0.17,0.95))+
  scale_color_manual(values = cbbPalette,name="",labels=c('319 funding', 'Nutrient Criteria',"TMDL site visits"))
policy.plot

jpeg(filename="./figures/policy_coef.jpeg",units="in",res=600,width=8,height=10)
policy.plot
dev.off()

###########

nz<-read.csv("./data/clean_data/non_z_scored_estiamtes.csv")

summarized.non.z<-aggregate(value~policy+nutreint_type,nz,quantile,c(0.025,0.5,0.975))

write.csv(summarized.non.z,"./data/clean_data/non_z_scored_estiamtes_summarized.csv")
