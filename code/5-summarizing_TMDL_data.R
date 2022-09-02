tmdl<-read.csv("data/predictor_data/TMDL_data.csv")

unique.tmdl<-data.frame(id=tmdl$ASSESSMENT_UNIT_ID,states=tmdl$STATE,CYCLE_YEAR=tmdl$CYCLE_YEAR)
unique.tmdl<-unique(unique.tmdl)
unique.tmdl$tally<-1

tmdl_sum<-aggregate(tally~states+CYCLE_YEAR,data=unique.tmdl,sum)


write.csv(tmdl_sum,"data/clean_data/TMDL_data_summary.csv", row.names=FALSE)

rm(list=ls())
