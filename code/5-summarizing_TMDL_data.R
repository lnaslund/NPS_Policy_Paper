tmdl<-read.csv("data/predictor_data/TMDL_data.csv")
tmdl$tally<-1

tmdl_sum<-aggregate(tally~STATE+CYCLE_YEAR,data=tmdl,sum)

names(tmdl_sum)[names(tmdl_sum)=="STATE"]<-"states"
write.csv(tmdl_sum,"data/clean_data/TMDL_data_summary.csv", row.names=FALSE)

rm(list=ls())
