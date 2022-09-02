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

d<-merge(d3,state.areas,by="postal.code")



z.score<-function(x,state.area){
  t<-x/state.area
  (t-mean(t))/sd(t)
  } #### z score and normalize to state area


predictors<-data.frame(state=d$postal.code,pop=z.score(d$pop2010,d$state.jurs.land),pop.delta=z.score(d$population.delta,d$state.jurs.land),
                       urban=z.score(d$urban.2008,d$state.jurs.land),undeveloped=z.score(d$undeveloped.200,d$state.jurs.land),
                       ag=z.score(d$ag.2008,d$state.jurs.land),urban.delta=z.score(d$delta.urban,d$state.jurs.land),
                       undeveloped.delta=z.score(d$delta.undeveloped,d$state.jurs.land),
                       ag.delta=z.score(d$delta.ag,d$state.jurs.land),
                       fertilizer=z.score(d$fertilizer.dollars.2007,d$state.jurs.land),
                       fertilizer.delta=z.score(d$delta.fertilizer,d$state.jurs.land),
                       feed=z.score(d$feed.dollars.2007,d$state.jurs.land),
                       feed.delta=z.score(d$delta.animal.feed,d$state.jurs.land))

write.csv(predictors,"./data/predictor_data/alternative_loading_predictors.csv")
