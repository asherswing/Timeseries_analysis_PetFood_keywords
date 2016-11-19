
trend_calc <- function(trend_data, func_name = mean)
{
  trend_data <- unname(sapply(trend_data, function(w) func_name(as.numeric(strsplit(w, ",")[[1]]))))
  return(trend_data)
}

trend_extract <- function(trend_data)
{
  trend_data <- unname(sapply(trend_data, function(w) as.numeric(strsplit(w, ",")[[1]])))
  return(trend_data)
}

wavelet_coef <- function(data) 
{
  data_coef <- dwt(as.ts(data), filter = "haar", boundary = "periodic")
  data_coef <- unname(unlist(c(data_coef@W,data_coef@V[[data_coef@level]])))
  return(data_coef)
}


# Trend analysis#amap
Domain_trend_analyses<-function(subdata,phrase){
  d<-subdata$Trends[subdata$Trends!= ""]
  subD<-d[!is.na(d)]
  
  temp_trends <- trend_extract(as.character(subD))## getting trend numeric data to use further 
  #tempdat<-as.data.frame(temp_trends[sapply(temp_trends, length) > 0])
  g<-grep(max(colSums(temp_trends)),colSums(temp_trends),perl = T)
  gg<-temp_trends[,g]
  if(length(gg)<=nrow(temp_trends)){
    plot1<-as.data.frame(gg)
      }
  else  plot1<-as.data.frame(rowMeans(gg))
  plot2<-as.data.frame(rowMeans(temp_trends))
  colnames(plot1)<-"col"
  colnames(plot2)<-"col"
  p1<-ggplot(plot1,aes(y=plot1$col,x=1:12))+
    geom_bar(stat = "identity")+
    labs(title="Maximum trend")+geom_smooth()+
    theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none",
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank())+
    ylim(0,1)
  
  
  p2<-ggplot(plot2,aes(y=plot2$col,x=1:12))+
    geom_bar(stat = "identity")+
    labs(title="Average trend")+geom_smooth()+
     theme(axis.line=element_blank(),axis.text.x=element_blank(),axis.text.y=element_blank(),axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank())+
    ylim(0,1)
  
  
  
  grid.arrange(p1,p2,top="Trend comparision ")
  }



year_trend<-function(subdata,phrase){
  tempdat<-split(subdata,format.Date(subdata$Date,"%Y"))
  x<-format.Date(subdata$Date,"%Y")
  x<-x[!duplicated(x)]
  x<-x[!is.na(x)]
  iterate_year_trends<-function(namp){
    tmptrend<-tempdat[[namp]]
    cname<-as.character(subdata$Country[1])
    png(filename =paste0("figs/",cname,"/",phrase,"/",namp,".png"),width = 1200,height = 1000)
    Domain_trend_analyses(tmptrend)
   dev.off()
  }
  lapply(x,iterate_year_trends)
  
  
}
