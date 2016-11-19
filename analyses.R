# analyses script

source("R/install.package.R")
source("R/data_merging.R")
source("R/cpc_distribution.R")
source("R/trend_analysis.R")
source("R/timeseries.R")
source("R/search_calc.R")
source("R/international_survey.R")
source("R/comparision_GvsI.R")
#################################################################################
#data translation
merge_init()
merger_init()

#################################################################################
data1<-read.csv("output/integrated/phrase.csv")
data2<-read.csv("output/integrated/related.csv")
#date_Search.Volume(data1,"phrase")
#date_Search.Volume(data2,"related")
#year_Search.Volume(data1,"phrase")
#year_Search.Volume(data2,"related")

###################################################################################
#trends calculation
dir_list<-list.dirs("./output")
temp_dir<-grep("./output/",dir_list,value = T)
temp_dir<-subset(temp_dir,temp_dir!="./output/integrated")

readfunction<-function(dirlist){
  
  for(file in dirlist){
    r1<-read.csv(paste0(file,"/related.csv"))
    r2<-read.csv(paste0(file,"/phrase.csv"))
    year_trend(r1,"related")
    year_trend(r2,"phrase")
    }
  
}

#readfunction(temp_dir)

################################################################################
dir_list1<-list.dirs("./data")
temp_dir1<-grep("./data/",dir_list1,value = T)
country_name<-gsub("./data/data reports ","",temp_dir1,fixed = T)
#lapply(country_name,ts_fillData)
lapply(country_name,search_calc)

#for international
dt1<-read.csv("output/integrated/related.csv")
dt2<-read.csv("output/integrated/phrase.csv")

dtt1<-aggregate(Search.Volume~Keyword+Date,data=dt1,sum,na.rm=T)
dtt2<-aggregate(Search.Volume~Keyword+Date,data=dt2,sum,na.rm=T)
international(dtt1,"related")
international_comp(dtt1,dt1)
#international(dtt2,"phrase")
###############################################################################
#dirlist1<-paste0(temp_dir1,"/related.csv")
#dirlist2<-paste0(temp_dir1,"/phrase.csv")
#dir_list1<-paste0("output/",country_name)
#for(f in temp_dir1){
 # country_name<-gsub("./data/data reports ","",f,fixed = T)
#  dlist1<-grep("2012 ",list.files(f),value = T)
#  if(identical(dlist1, character(0)) )
#    dlist1<-grep("2013 ",list.files(f),value = T)
#  s1<-grep("related",dlist1,value = T)
#  dlist2<-grep("2016",list.files(f),value = T)
#  if(identical(dlist2, character(0)) )
#    dlist1<-grep("2015 ",list.files(f),value = T)
  
#  s2<-grep("related",dlist2,value = T)
#  s1<-paste0(f,"/",s1)
#  s2<-paste0(f,"/",s2)
#  k1<-iterate(s1,country_name)
#  k2<-iterate(s2,country_name)
#  tkey<-intersect(k1$Keyword,k2$Keyword)
#  ll<-paste0("output/",country_name,"/related.csv")
#  dl<-read.csv(ll)
#  dat<-subset(dl,dl$Keyword %in% tkey & (dl$Search.Volume >=1000 |dl$Search.Volume >=500))
# datst<-dat$Keyword[!duplicated(x$Keyword)] 
  
  
#  tmp<-dl[order(dl$Search.Volume,decreasing = T),]
# t2<-datst
#  for(t in t2){
 #   newtmp<-subset(dataset,dataset$Keyword==t ,select =c(Search.Volume,Date))
#    newtmp<-subset(newtmp,as.Date(newtmp$Date) > as.Date("2011-01-01"),select =c(Search.Volume,Date))
    
#    newtmp<-newtmp[!duplicated(newtmp),]
#    newtmp<-newtmp[complete.cases(newtmp), ]
#    nats<-fill_missing(newtmp)
#    if(nrow(nats)>=3){
#      nats$Search.Volume<-na.ma(nats$Search.Volume)
      
#      ggsave(filename = paste0("figs/",country_name,"/",t,"-ts.png"),timeseries_Search.Volume(nats,t))
 #     print(t)
#      ggsave(filename = paste0("figs/",country_name,"/",t,"-fs.png"),timeseries_calc(nats,t))
#    } else print("not enough data")
    #timeseries_Search.Volume(nats,fg)
# }
#}


