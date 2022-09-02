library(vegan)
library(ggplot2)

####landcover

lc2008<-read.csv("data/predictor_data/NCLD2008.csv")
lc2019<-read.csv("data/predictor_data/NCLD2019.csv")

lc2008$urban<-lc2008$Developed__High_Intensity+lc2008$Developed__Low_Intensity+lc2008$Developed__Medium_Intensity
lc2008$undeveloped<-lc2008$Mixed_Forest+lc2008$Deciduous_Forest+lc2008$Evergreen_Forest+lc2008$Barren_Land+lc2008$Shrub_Scrub+lc2008$Herbaceous+lc2008$Woody_Wetlands+lc2008$Emergent_Herbaceous_Wetlands+lc2008$Perennial_Snow_Ice
lc2008$ag<-lc2008$Hay_Pasture+lc2008$Cultivated_Crops


lc2019$urban<-lc2019$Developed__Low_Intensity+lc2019$Developed__Medium_Intensity+lc2019$Developed__High_Intensity
lc2019$undeveloped<-lc2019$Mixed_Forest+lc2019$Deciduous_Forest+lc2019$Evergreen_Forest+lc2019$Barren_Land+lc2019$Shrub_Scrub+lc2019$Herbaceous+lc2019$Woody_Wetlands+lc2019$Emergent_Herbaceous_Wetlands+lc2019$Perennial_Snow_Ice
lc2019$ag<-lc2019$Hay_Pasture+lc2019$Cultivated_Crops

short.2008lc<-data.frame(State=toupper(lc2008$STATE_NAME),urban.2008=lc2008$urban,undeveloped.2008=lc2008$undeveloped,ag.2008=lc2008$ag)
short.2019lc<-data.frame(State=toupper(lc2019$ï..STATE_NAM),urban.2019=lc2019$urban,undeveloped.2019=lc2019$undeveloped,ag.2019=lc2019$ag)


lc.merged<-merge(short.2008lc,short.2019lc,by="State")
lc.merged$delta.urban<-lc.merged$urban.2019-lc.merged$urban.2008
lc.merged$delta.undeveloped<-lc.merged$undeveloped.2019-lc.merged$undeveloped.2008
lc.merged$delta.ag<-lc.merged$ag.2019-lc.merged$ag.2008

### Population

population<-read.csv("data/predictor_data/census_population_estimates.csv")

population2<-data.frame(State=toupper(population$state),postal.code=population$postal.code,population.delta=population$population2019-population$population2010,pop2010=population$population2010)


### agriculture

### animal feed first

feed<-read.csv("data/predictor_data/feed_expenses.csv")

feed.total<-feed[feed$Domain=="TOTAL",]

feed.total.sums<-aggregate(Value~Year+State,data=feed.total,sum)

feed.2007<-feed.total.sums[feed.total.sums$Year==2007,]
feed.2017<-feed.total.sums[feed.total.sums$Year==2017,]

names(feed.2007)[names(feed.2007) == "Value"] <- "feed.dollars.2007"
names(feed.2017)[names(feed.2017) == "Value"] <- "feed.dollars.2017"

feed.change<-merge(feed.2007,feed.2017,by="State")
feed.change$delta.animal.feed<-feed.change$feed.dollars.2017-feed.change$feed.dollars.2007

fert<-read.csv("data/predictor_data/fertilizer_expenses.csv")

fert.agg<-aggregate(Value~Year+State,data=fert[fert$Data.Item=="FERTILIZER & CHEMICAL TOTALS - EXPENSE, MEASURED IN $",],sum)

fert.2007<-fert.agg[fert.agg$Year==2007,]
fert.2017<-fert.agg[fert.agg$Year==2017,]

names(fert.2007)[names(fert.2007) == "Value"] <- "fertilizer.dollars.2007"
names(fert.2017)[names(fert.2017) == "Value"] <- "fertilizer.dollars.2017"

fertilizer.change<-merge(fert.2007,fert.2017,by="State")
fertilizer.change$delta.fertilizer<-fertilizer.change$fertilizer.dollars.2017-fertilizer.change$fertilizer.dollars.2007

#################### Merging datasets

d1<-merge(population2,lc.merged,by="State")
d2<-merge(d1,fertilizer.change,by="State")
d3<-merge(d2,feed.change,by="State")

write.csv(d3,"./data/alternative_predictors.csv")

###### normalizing for state area

state.areas<-read.csv("data/predictor_data/state_areas.csv")
state.areas$postal.code<-state.areas$State

d4<-merge(d3,state.areas,by="postal.code")


pca.data<-data.frame(state=d4$postal.code,undeveloped=d4$delta.undeveloped,urban=d4$delta.urban,population=d4$population.delta,
                     fertilizer=d4$delta.fertilizer,feed=d4$delta.animal.feed,ag.land=d4$delta.ag,state.area=d4$state.jurs.land)

z.scores<-pca.data

for (i in 2:7){
  one.col<-pca.data[,i]/pca.data$state.area
  colmean<-mean(one.col)
  colsd<-sd(one.col)
  trans<-(one.col-colmean)/colsd
  z.scores[,i]<-trans
}

states.pca<- vegan::rda(z.scores[,2:7])
states.summary<-summary(states.pca)  

predictor.vectors<-data.frame(states.summary$species[,1:2])
predictor.vectors$variable<-rownames(predictor.vectors)
p<-predictor.vectors

site.locations<-data.frame(states.summary$sites[,1:2])
site.locations$State<-pca.data$state

write.csv(site.locations,file="data/clean_data/loading.pca.data.csv")


transformed.pca<-ggplot(site.locations,aes(x=PC1,y=PC2,label=State))+
  geom_text()+
  theme_classic()+
  geom_hline(yintercept=0,linetype=2)+
  geom_vline(xintercept = 0,linetype=2)+theme(text = element_text(size=20))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[1],yend=p$PC2[1]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[2],yend=p$PC2[2]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[3],yend=p$PC2[3]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[4],yend=p$PC2[4]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[5],yend=p$PC2[5]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_segment(aes(x=0,y=0,xend=p$PC1[6],yend=p$PC2[6]),arrow = arrow(length = unit(0.3, "inches")))+
  geom_text(data=predictor.vectors,aes(x=PC1,y=PC2,label=variable),size=6)+
  xlab("PC1 (42%)")+ylab("PC2 (24%)")
transformed.pca

tiff(filename="figures/transformed.pca.tiff",units="in",res=300,width=8,height=8,compression="lzw")
transformed.pca
dev.off()

rm(list=ls())

