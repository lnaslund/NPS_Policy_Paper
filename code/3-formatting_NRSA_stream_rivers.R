library(Rmisc)
library(dplyr)
library(ggplot2)
library(lubridate)

#### load and merge NRSA surveys 

## 2008-2009 survey
d1<-read.csv("data/stream_chemistry_data/chem.csv")
d1$DATE_COL<-dmy(d1$DATE_COL)

site.info.2008<-read.csv("data/stream_chemistry_data/siteinfo_0.csv")

d1<-merge(d1,site.info.2008,by="UID")

d1$order<-as.numeric(recode_factor(as.factor(d1$STRAHLERORDER),"1st"="1","2nd"="2","3rd"="3","4th"="4","5th"="5",
                        "6th"="6","7th"="7","8th+"="8"))
d1$weight<-d1$WGTNRSA09

## 2018-2019 survey
d2<-read.csv("data/stream_chemistry_data/nrsa_1819_water_chemistry_chla_-_data.csv")
d2$state<-d2$STATE
d2$DATE_COL<-mdy(d2$DATE_COL)

site.info.d2<-read.csv("data/stream_chemistry_data/nrsa_1819_site_information_-_data.csv")
d2<-merge(d2,site.info.d2,by="UID")

d2$weight<-d2$WGT_TP_EXTENT
d2$order<-d2$STRAH_ORD

# 2013-2014 survey
d3<-read.csv("data/stream_chemistry_data/nrsa1314_widechem_04232019.csv")
d3$state<-d3$PSTL_CODE
d3$DATE_COL<-mdy(d3$DATE_COL)

site.info.d3<-read.csv("data/stream_chemistry_data/nrsa1314_siteinformation_wide_04292019.csv")
d3<-merge(d3,site.info.d3,by="UID")

d3$weight<-d3$WGT_EXT_SP
d3$order<-d3$STRAH_ORD

#### 2008-2009 survey
d1.subset<-data.frame(state=d1$STATE,site=d1$SITE_ID.x,date=d1$DATE_COL.x,nh4=d1$NH4,nh4.flag=d1$NH4_ALERT,
                      no3=d1$NO3,no3.flag=d1$NO3_ALERT,tp=d1$PTL,tp.flag=d1$PTL_ALERT,tn=d1$NTL,
                      tn.flag=d1$NTL_ALERT,site.type=d1$SITE_CLASS,order=d1$order,weight=d1$weight,
                      lat=d1$LAT_DD83,lon=d1$LON_DD83)

d1.subset$nh4<-d1.subset$nh4*1000 ## converting to ug/L
d1.subset$no3<-d1.subset$no3*1000 ## converting to ug/L

### 2018-2019 data. merging the flag (what they call it when a point is bad) into a column to make the formats more similar
### may want to handle points below detection differently? maybe call them zeros instead of dropping them?
# TODO: change bdl sites to 1/2 dl and run analysis again. Do the same for lakes

d2$no3.flag<-paste(d2$NITRATE_N_NARS_FLAG,d2$NITRATE_N_QA_FLAG,sep="")
d2$nh4.flag<-paste(d2$AMMONIA_N_NARS_FLAG,d2$AMMONIA_N_QA_FLAG,sep="")
d2$ptl.flag<-paste(d2$PTL_NARS_FLAG,d2$PTL_QA_FLAG,sep="")
d2$ntl.flag<-paste(d2$NTL_NARS_FLAG,d2$NTL_QA_FLAG,sep="")

d2.subset<-data.frame(state=d2$state,site=d2$SITE_ID.x,date=d2$DATE_COL.x,nh4=d2$AMMONIA_N_RESULT,
                      nh4.flag=d2$nh4.flag, no3=d2$NITRATE_N_RESULT,no3.flag=d2$no3.flag,
                      tp=d2$PTL_RESULT,tp.flag=d2$ptl.flag,
                      tn=d2$NTL_RESULT,tn.flag=d2$ntl.flag,site.type=d2$SITETYPE,order=d2$order,weight=d2$weight,
                      lat=d2$LAT_DD83,lon=d2$LON_DD83)

d2.subset$nh4<-d2.subset$nh4*1000 ## converting to ug/L
d2.subset$no3<-d2.subset$no3*1000 ## converting to ug/L

d2.subset$tn<-d2.subset$tn*1000 ## converting to ug/L -- total P is already in ug/L

##### 2013-2014 data
d3$no3.flag<-paste(d3$NITRATE_N_NARS_FLAG,d3$NITRATE_N_QA_FLAG,d3$NITRATE_N_SHIP_FLAG,sep="")
d3$nh4.flag<-paste(d3$AMMONIA_N_NARS_FLAG,d3$AMMONIA_N_QA_FLAG,d3$NITRATE_N_SHIP_FLAG,sep="")
d3$ptl.flag<-paste(d3$PTL_NARS_FLAG,d3$PTL_QA_FLAG,d3$PTL_SHIP_FLAG,sep="")
d3$ntl.flag<-paste(d3$NTL_NARS_FLAG,d3$NTL_QA_FLAG,d3$NTL_SHIP_FLAG,sep="")



d3.subset<-data.frame(state=d3$state,site=d3$SITE_ID.x,date=d3$DATE_COL.x,nh4=d3$AMMONIA_N_RESULT,
                      nh4.flag=d3$nh4.flag,
                      no3=d3$NITRATE_N_RESULT,no3.flag=d3$no3.flag,
                      tp=d3$PTL_RESULT,tp.flag=d3$ptl.flag,
                      tn=d3$NTL_RESULT,tn.flag=d3$ntl.flag,site.type=d3$SITETYPE.x,order=d3$order,weight=d3$weight,
                      lat=d3$LAT_DD83.x,lon=d3$LON_DD83.x)

d3.subset$nh4<-d3.subset$nh4*1000 ##converting to ug/L
d3.subset$no3<-d3.subset$no3*1000 ##converting to ug/L

d3.subset$tn<-d3.subset$tn*1000 ## converting to ug/L -- total P is already in ug/L

# merge survey years
data<-bind_rows(d1.subset,d2.subset,d3.subset)


# remove observations with NO3 data quality flags
nitrate.data<-data[data$no3.flag=="" & data$site.type=="PROB",]
nitrate.data<-nitrate.data[is.na(nitrate.data$no3)==FALSE,]

write.csv(nitrate.data,file="data/clean_data/clean_stream_nitrate.csv", row.names=FALSE)

tp.data<-data[data$tp.flag=="" & data$site.type=="PROB",]
tp.data<-tp.data[is.na(tp.data$tp)==FALSE,]

write.csv(tp.data,file="data/clean_data/clean_stream_tp.csv", row.names=FALSE)


tn.data<-data[data$tn.flag=="" & data$site.type=="PROB",]
tn.data<-tn.data[is.na(tn.data$tn)==FALSE,]

write.csv(tn.data,file="data/clean_data/clean_stream_tn.csv", row.names=FALSE)

nh4.data<-data[data$nh4.flag=="" & data$site.type=="PROB",]
nh4.data<-nh4.data[is.na(nh4.data$nh4)==FALSE,]

write.csv(nh4.data,file="data/clean_data/clean_stream_nh4.csv", row.names=FALSE)

rm(list=ls())
