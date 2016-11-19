if(!file.exists(paste0("figs","/","DateCalculation")))
 { dir.create("figs/DateCalculation")
  dir.create("figs/DateCalculation/phrase")
  dir.create("figs/DateCalculation/related")
}
if(!file.exists(paste0("figs","/","YearCalculation")))
{dir.create("figs/YearCalculation")
  dir.create("figs/YearCalculation/phrase")
  dir.create("figs/YearCalculation/related")}


date_Search.Volume<-function(subdata,phrase){
  tempdat<-split(subdata,format.Date(subdata$Date,"%m-%Y"))
x<-format.Date(subdata$Date,"%m-%Y")
x<-x[!duplicated(x)]
x<-x[!is.na(x)]
iterate_date_Search.Volume<-function(namp){
  s1<-aggregate(tempdat[[namp]]$Search.Volume, by=list(Category=tempdat[[namp]]$Country), FUN=sum)
  s2<-aggregate(tempdat[[namp]]$Search.Volume, by=list(Category=tempdat[[namp]]$Country), FUN=mean)
  s3<-aggregate(tempdat[[namp]]$Search.Volume, by=list(Category=tempdat[[namp]]$Country), FUN=max)
  
  p1<-ggplot(s1,aes(y=s1$x,x=s1$Category))+geom_bar(aes(fill=s1$Category),stat = "identity")+coord_flip()+labs(title="Total Distribution",x="",y="")
  p2<-ggplot(s2,aes(y=s2$x,x=s2$Category))+geom_bar(aes(fill=s2$Category),stat = "identity")+coord_flip()+labs(title="Mean Distribution",x="",y="")
  p3<-ggplot(s3,aes(y=s3$x,x=s3$Category))+geom_bar(aes(fill=s3$Category),stat = "identity")+coord_flip()+labs(title="Maximum Distribution",x="",y="")
  png(filename = paste0("figs/DateCalculation/",phrase,"/",namp,".png"),width = 1200,height = 1000)
  grid.arrange(p1, p3,p2, nrow=3,top=paste0("Comparision of mean max and total Search.Volume distribution in ",namp),left="Region",bottom="Search.Volume Distribution")
  dev.off()
}
lapply(x,iterate_date_Search.Volume)


}


year_cpc<-function(subdata,phrase){
  tempdat<-split(subdata,format.Date(subdata$Date,"%Y"))
  x<-format.Date(subdata$Date,"%Y")
  x<-x[!duplicated(x)]
  x<-x[!is.na(x)]
  iterate_year_cpc<-function(namp){
    s1<-aggregate(tempdat[[namp]]$CPC, by=list(Category=tempdat[[namp]]$Country), FUN=sum)
    s2<-aggregate(tempdat[[namp]]$CPC, by=list(Category=tempdat[[namp]]$Country), FUN=mean)
    s3<-aggregate(tempdat[[namp]]$CPC, by=list(Category=tempdat[[namp]]$Country), FUN=max)
    
    p1<-ggplot(s1,aes(y=s1$x,x=s1$Category))+geom_bar(aes(fill=s1$Category),stat = "identity")+coord_flip()+labs(title="Total Distribution",x="",y="")
    p2<-ggplot(s2,aes(y=s2$x,x=s2$Category))+geom_bar(aes(fill=s2$Category),stat = "identity")+coord_flip()+labs(title="Mean Distribution",x="",y="")
    p3<-ggplot(s3,aes(y=s3$x,x=s3$Category))+geom_bar(aes(fill=s3$Category),stat = "identity")+coord_flip()+labs(title="Maximum Distribution",x="",y="")
    
    png(filename = paste0("figs/YearCalculation/",phrase,"/",namp,".png"),width = 1200,height = 1000)
    grid.arrange(p1, p3,p2, nrow=3,top=paste0("Comparision of mean max and total CPC distribution in ",namp),left="Region",bottom="CPC Distribution")
    dev.off()
  }
  lapply(x,iterate_year_cpc)
  
  
}

