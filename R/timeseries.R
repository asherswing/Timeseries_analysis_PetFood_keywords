ts_fillData<-function(cname,obs=10){
  s1<-paste0("output/",cname,"/related.csv")
  s2<-paste0("output/",cname,"/phrase.csv")
  
  dataset1<-read.csv(s1)
  dataset2<-read.csv(s2)
  
  tmpf<-function(dataset,arg1,cname){
    tmp<-dataset[order(dataset$CPC,decreasing = T),]
    t1<-tmp$Keyword[!duplicated(tmp$Keyword)]
    t2<-t1[1:obs]
    t2<-t2[!is.na(t2)]
    gglist1<-list()
    gglist2<-list()
    
    for(t in t2){
      newtmp<-subset(dataset,dataset$Keyword==t ,select =c(CPC,Date))
      newtmp<-subset(newtmp,as.Date(newtmp$Date) > as.Date("2011-01-01"),select =c(CPC,Date))
      
      newtmp<-newtmp[!duplicated(newtmp),]
      newtmp<-newtmp[complete.cases(newtmp), ]
      nats<-fill_missing(newtmp)
      if(nrow(nats)>=3){
        nats$CPC<-na.ma(nats$CPC)
        
      #  ggsave(filename = paste0("figs/",cname,"/",arg1,"/",t,"-ts.png"),timeseries_cpc(nats,t,cname))
        print(t)
        ggsave(filename = paste0("figs/",cname,"/",arg1,"/",t,"-fs.png"),timeseries_calc(nats,t,cname))
      } else print("not enough data")
      #timeseries_cpc(nats,fg)
    }
  }
  
  tmpf(dataset1,"related",cname)
  tmpf(dataset2,"phrase",cname)
}



timeseries_cpc<-function(arg1,arg2){
  
  TimeDesc<-arg1[order(as.Date(arg1$Date,format="%b-%Y")),,drop=FALSE]#Chronological order of the observations
  arg1k<-TimeDesc #Time series analysis in the first 100 observations.
  CostPerclick<-subset(arg1k,select=c(CPC,Date)) #Select the variables we need
  if(length(arg1$Date)>=25){
    mbrk<-"4 month"
  } else if(length(arg1$Date)>=20){mbrk<-"3 month"}
  else if(length(arg1$Date)>=13){mbrk<-"2 month"}
  else mbrk<-"1 month"
  zz<-ggplot(CostPerclick, aes(Date, CPC)) + geom_line() +
    scale_x_date(date_labels = "%b-%Y",date_breaks = mbrk) +
    xlab("TimeSeries") + ylab("CPC distribution")+labs(title=arg2)+
    theme(axis.text.x=element_text(angle=90, hjust=1))
  zz
}




fill_missing<-function(raw.data){
  raw.data$Date <- as.Date(raw.data$Date)
  sorted.data <- raw.data[order(raw.data$Date),]
  data.length <- length(sorted.data$Date)
  Date.min <- sorted.data$Date[1]
  Date.max <- sorted.data$Date[data.length]
  all.dates <- seq(Date.min, Date.max, by="month")
  all.dates.frame <- data.frame(list(Date=all.dates))
  merged.data <- merge(all.dates.frame, sorted.data, all=T)
  merged.data
}



timeseries_calc<-function(doc,arg2,pm=5){
  f<-doc$Date[order(doc$Date)]
  if(length(f)>=25){
    mbrk<-"4 month"
  } else if(length(f)>=20){mbrk<-"3 month"}
  else if(length(f)>=13){mbrk<-"2 month"}
  else mbrk<-"1 month"
  date1 <- as.yearmon(f, "%Y-%m-%d")
  y<-(format(date1[1],"%Y"))
  m<-(format(date1[1],"%m"))
  c<-ts(doc$CPC,frequency=12,start = as.numeric(c(y,m)))
  arima.model<-auto.arima(c) #ARIMA model
  forec.20<-forecast.Arima(arima.model,h = pm,bootstrap = TRUE)
  ps<-plot_timeseries(forec.20,format.date = T)+
    scale_x_date(date_labels = "%b-%Y",date_breaks = mbrk)+ylab("CPC Distribution")+
    xlab(title=paste0("Keyword :- ",arg2),subtitle = paste0("Region:-",cname),x="TimeSeries")+
    ggtitle(bquote(atop(.(paste0("Keyword :- ",arg2)), atop(italic(paste0("Region:-",cname), ""))))) +
    theme(axis.text.x=element_text(angle=90, hjust=1))
  ps
}


plot_timeseries<-function(forec.obj, data.color = 'blue', fit.color = 'red', forec.color = 'black',
                          lower.fill = 'darkgrey', upper.fill = 'grey', format.date = F)
{
  serie.orig = forec.obj$x
  serie.fit = forec.obj$fitted
  pi.strings = paste(forec.obj$level, '%', sep = '')
  
  if(format.date)
    dates = as.Date(time(serie.orig))
  else
    dates = time(serie.orig)
  
  serie.df = data.frame(date = dates, serie.orig = serie.orig, serie.fit = serie.fit)
  
  forec.M = cbind(forec.obj$mean, forec.obj$lower[, 1:2], forec.obj$upper[, 1:2])
  forec.df = as.data.frame(forec.M)
  colnames(forec.df) = c('forec.val', 'l0', 'l1', 'u0', 'u1')
  
  if(format.date)
    forec.df$date = as.Date(time(forec.obj$mean))
  else
    forec.df$date = time(forec.obj$mean)
  
  p = ggplot() + 
    geom_line(aes(date, serie.orig, colour = 'data'), data = serie.df) + 
    geom_line(aes(date, serie.fit, colour = 'fit'), data = serie.df) + 
    scale_y_continuous() +
    geom_ribbon(aes(x = date, ymin = l0, ymax = u0, fill = 'lower'), data = forec.df, alpha = I(0.4)) + 
    geom_ribbon(aes(x = date, ymin = l1, ymax = u1, fill = 'upper'), data = forec.df, alpha = I(0.3)) + 
    geom_line(aes(date, forec.val, colour = 'forecast'), data = forec.df) + 
    scale_color_manual('Series', values=c('data' = data.color, 'fit' = fit.color, 'forecast' = forec.color)) + 
    scale_fill_manual('P.I.', values=c('lower' = lower.fill, 'upper' = upper.fill))
  
  if (format.date)
    p = p + scale_x_date()
  
  p
}


