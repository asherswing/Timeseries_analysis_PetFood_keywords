library("zoo")

#subfilenametemp<-grep("\\(phrase match\\)\\.csv$",filename,value = T)  


extract_date<-function(filename){
  
subfilename<-gsub("\\(.*$","",filename)
subfilename<-as.character(lapply(strsplit(subfilename,"/"),'[[',4))
name<-as.yearmon(as.character(subfilename), "%B %Y")
name
}

readfile_updated_file<-function(filename,ccname){
  temp<-read.csv(filename)
  Timestamp<-extract_date(filename)
# print(Timestamp)
  temp$Date<-as.Date(Timestamp)
#  print(temp$Date)
  temp$Country<-ccname
# print(cname)
  temp
}


iterate<-function(file_list,cname){
    for (file in file_list){
      if (!exists("xvf")){
        xvf <- readfile_updated_file(file,cname)
          }
      if (exists("dataset")){
      temp_dataset <-readfile_updated_file(file,cname)
      dataset<-rbind.fill(xvf, temp_dataset)
      rm(temp_dataset)
          }
      }
  dataset
}

combined_data_set<-function(directory){
  file_list <- list.files(directory)
  
  countryname<-gsub("./data/data reports ","",directory,fixed = T)
subfilePhrase<-grep("\\(phrase match\\)\\.csv$",file_list,value = T)  
subfileRelated<-grep("\\(related key\\)\\.csv$",file_list,value = T)  
subfilePhrase<-paste0(directory,"/",subfilePhrase)
subfileRelated<-paste0(directory,"/",subfileRelated)
datapharas<-iterate(subfilePhrase,countryname)
datarelated<-iterate(subfileRelated,countryname)
if("Keyword.Difficulty.Index" %in% colnames(datapharas))
{
  temp1<-subset(datapharas, select= -c(Keyword.Difficulty.Index))
} else temp1<-datapharas
if("Keyword.Difficulty.Index" %in% colnames(datarelated))
{
  temp2<-subset(datarelated,select= -c(Keyword.Difficulty.Index))
}else temp2<-datarelated

temp1<-subset(temp1,select=c(Keyword,Search.Volume,CPC,Competition,Number.of.Results,Trends,Date,Country))
temp2<-subset(temp2,select=c(Keyword,Search.Volume,CPC,Competition,Number.of.Results,Trends,Date,Country))
tmp1<-temp1[!duplicated(temp1),]
tmp2<-temp2[!duplicated(temp2),]
temp<-rbind.fill(tmp1,tmp2)
temp<-temp[!duplicated(temp),]

write.csv(temp2,paste0("output","/",countryname,"/related.csv"),row.names = F,append = FALSE)
write.csv(temp1,paste0("output","/",countryname,"/phrase.csv"),row.names = F,append = FALSE)
write.csv(temp,paste0("output","/",countryname,"/allkey.csv"),row.names = F,append = FALSE)
}




merge_init<-function(){
  
  dir_list<-list.dirs("./data")
  temp_dir<-grep("./data/",dir_list,value = T)
  country_name<-gsub("./data/data reports ","",temp_dir,fixed = T)
  if(!(sum(as.logical(lapply(paste0("figs","/",country_name),file.exists)))>0))
  {
    dir.create("figs/compare")
    
    dir.create("figs/search.vol")
    dir.create("figs/search.vol/global")
    dir.create("figs/search.vol/country")
    }
    
    if(!(sum(as.logical(lapply(paste0("output","/",country_name),file.exists)))>0))
  {lapply(paste0("output","/",country_name),dir.create)
    }
    
if(length(temp_dir)==length(country_name)) 
lapply(temp_dir,combined_data_set)
}

merger_init<-function(){
  dir_list<-list.dirs("./output")
  temp_dir<-grep("./output/",dir_list,value = T)
  phrasepath<-paste0(temp_dir,"/","phrase.csv")
  relatedpath<-paste0(temp_dir,"/","related.csv")
  allkeypath<-paste0(temp_dir,"/","allkey.csv")
  data1<-iterate_long(phrasepath)
  data2<-iterate_long(relatedpath)
  data3<-iterate_long(allkeypath)
  if(!file.exists(paste0("output","/","integrated")))
     dir.create(paste0("output","/","integrated"))
  write.csv(data2,paste0("output","/","integrated","/related.csv"),row.names = F)
  write.csv(data1,paste0("output","/","integrated","/phrase.csv"),row.names = F)
  write.csv(data3,paste0("output","/","integrated","/allkey.csv"),row.names = F)
  
  }  
  
iterate_long<-function(file_list){
  for (file in file_list){
    if (!exists("dataset")){
      dataset <- read.csv(file)
    }
    if (exists("dataset")){
      temp_dataset <-read.csv(file)
      dataset<-rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
  
  dataset
}

iteratemerge<-function(file_list){
  for (file in file_list){
    if (!exists("xvf")){
      xvf <- read.csv(file)
    }
    if (exists("dataset")){
      temp_dataset <-read.csv(file)
      xvf<-rbind.fill(dataset, temp_dataset)
      rm(temp_dataset)
    }
  }
xvf
}
