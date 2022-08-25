library(Rmisc)
library(lubridate)
library(dplyr)

################### Read in and merge 2007 NLA chemistry and site information 

d1<-read.csv("data/lake_chemistry_data/nla2007_chemical_conditionestimates_20091123.csv")

d1.site.info<-read.csv("data/lake_chemistry_data/nla2007_sampledlakeinformation_20091113.csv")

d1<-merge(d1,d1.site.info,by=c("SITE_ID","VISIT_NO"))

d1.subset<-data.frame(date=mdy(d1$DATE_COL),state=d1$ST.x,tn=d1$NTL,tn.flag=NA,tn.qa.flag=NA, tp=d1$PTL,tp.flag=NA,tp.qa.flag=NA, site.type=d1$SITE_TYPE.x,
                      lat=d1$LAT_DD.x,lon=d1$LON_DD.y,siteID=d1$SITE_ID,weight=d1$WGT_NLA.x) 
## EPA downloaded TN data is in ug/l
## EPA downloaded TP data is in ug/l

## No data quality flags included in downloaded EPA data

################# Read in and merge 2012 NLA chemistry and site information 

d2<-read.csv("data/lake_chemistry_data/nla2012_waterchem_wide.csv")

d2.site.info<-read.csv("data/lake_chemistry_data/nla2012_wide_siteinfo_08232016.csv")

d2<-merge(d2,d2.site.info,by=c("UID"))

# combine different data quality flags into single column to exclude later, keeping a separate column for QA flag to recover ND points
d2$tn.flag<-paste(d2$NTL_FLAG, d2$NTL_LAB_FLAG, d2$NTL_QA_FLAG,sep="")
d2$tp.flag<-paste(d2$PTL_QA_FLAG, d2$PTL_LAB_FLAG, d2$PTL_FLAG,sep="")

d2.subset<-data.frame(date=mdy(d2$DATE_COL),state=d2$STATE,tn=d2$NTL_RESULT,tn.flag=d2$tn.flag, tn.qa.flag = d2$NTL_QA_FLAG, tp=d2$PTL_RESULT,tp.flag=d2$tp.flag,
                      tp.qa.flag = d2$PTL_QA_FLAG, site.type=d2$SITETYPE,lat=d2$LAT_DD83,lon=d2$LON_DD83,siteID=d2$SITE_ID,weight=d2$WGT_ALL)

d2.subset$tn<-d2.subset$tn*1000

## EPA downloaded TN data is in mg/l, column in d2.subset converted to ug/L
## EPA downloaded TP data is in ug/l

################# Read in and merge 2017 NLA chemistry and site information 

d3<-read.csv("data/lake_chemistry_data/nla_2017_water_chemistry_chla-data.csv")

d3.site.info<-read.csv("data/lake_chemistry_data/nla_2017_site_information-data.csv")

d3.tn<-d3[d3$ANALYTE=="NTL",]
d3.tn.small <-data.frame(UID=d3.tn$UID,tn=d3.tn$RESULT,tn.flag=d3.tn$NARS_FLAG, tn.qa.flag = d3.tn$NARS_FLAG) # mg/L

d3.tp<-d3[d3$ANALYTE=="PTL",]
d3.tp.small<-data.frame(UID=d3.tp$UID,tp=d3.tp$RESULT,tp.flag=d3.tp$NARS_FLAG, tp.qa.flag = d3.tp$NARS_FLAG) # ug/l

nutrients.together<-merge(d3.tp.small,d3.tn.small,by="UID")

d3.with.site<-merge(nutrients.together,d3.site.info,by="UID")

d3.subset<-data.frame(date=mdy(d3.with.site$DATE_COL),state=d3.with.site$PSTL_CODE,tn=as.numeric(d3.with.site$tn),tn.flag=d3.with.site$tn.flag,tn.qa.flag=d3.with.site$tn.qa.flag,
                      tp=as.numeric(d3.with.site$tp),tp.flag=d3.with.site$tp.flag, tp.qa.flag=d3.with.site$tp.qa.flag,site.type=d3.with.site$SITETYPE,
                      lat=d3.with.site$LAT_DD83,lon=d3.with.site$LON_DD83,siteID=d3.with.site$SITE_ID,weight=d3.with.site$WGT_TP_CORE)

d3.subset$tn<-d3.subset$tn*1000

## EPA downloaded TN data is in mg/l, column in d3.subset converted to ug/L
## EPA downloaded TP data is in ug/l

################### Merge different years

data<-bind_rows(d1.subset,d2.subset,d3.subset)

# discard nonprobabilistic sites
keep<-c("PROB","PROB_Lake")
data<-data[data$site.type%in%keep,]

# discard observations without states or with data quality flags other than ND
data<-data[data$state!="None",]
data$tp.flag<-as.factor(data$tp.flag)
data$tn.flag<-as.factor(data$tn.flag)

lake.TN.data<-data[data$tn.flag=="" | is.na(data$tn.flag)==TRUE | data$tn.qa.flag=="ND",]
lake.TN.data<-lake.TN.data[is.na(lake.TN.data$tn)==FALSE,]

write.csv(lake.TN.data,file="data/clean_data/clean_lake_tn.csv", row.names=FALSE)

lake.TP.data<-data[data$tp.flag=="" | is.na(data$tp.flag)==TRUE | data$tp.qa.flag=="ND",]
lake.TP.data<-lake.TP.data[is.na(lake.TP.data$tp)==FALSE,]

write.csv(lake.TP.data,file="data/clean_data/clean_lake_tp.csv", row.names=FALSE)

rm(list = ls())
