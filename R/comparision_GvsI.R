international_comp<-function(dataset,dataset1){
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  myColors = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  myColors<-myColors[52:70]
  names(myColors)<-levels(factor(c(country_name,"GLOBAL")))
  tmpf<-function(dataset,dataset1){
    tmp<-dataset[order(dataset$Search.Volume,decreasing = T),]
    tmp<-subset(tmp,tmp$Search.Volume >1000 )
    t1<-tmp$Keyword[!duplicated(tmp$Keyword)]
    t2<-t1
    t2<-t2[!is.na(t2)]
    pos<-list()
    neg<-list()
    objlist<-list()
    for(t in t2){
      objlist<-list()
      newtmp<-subset(dataset,dataset$Keyword==t ,select =c(Keyword,Search.Volume,Date))
     newtmp1<-subset(dataset1,dataset1$Keyword==t,select =c(Keyword,Search.Volume,Date,Country))
     newtmp<-subset(newtmp,as.Date(newtmp$Date) > as.Date("2011-01-01"),select =c(Keyword,Search.Volume,Date))
     newtmp1<-subset(newtmp1,as.Date(newtmp1$Date) > as.Date("2011-01-01"))
     
      newtmp<-newtmp[!duplicated(newtmp),]
      newtmp<-newtmp[complete.cases(newtmp), ]
      newtmp1<-newtmp1[!duplicated(newtmp1),]
      newtmp1<-newtmp1[complete.cases(newtmp1), ]
      newtmp$Country<-"GLOBAL"
      sortdate <- newtmp[order(as.Date(newtmp$Date)),]
        track<-((sortdate$Search.Volume[nrow(sortdate)]- sortdate$Search.Volume[1])/sortdate$Search.Volume[1] *100 )
      hvol<-sortdate$Search.Volume[nrow(sortdate)]
        if(track >= 100 |track <= -50) 
      {
      newtmp<-rbind(newtmp,newtmp1)
      
      tc<-newtmp$Country[!duplicated(newtmp$Country)]
      
     for(a in tc){
       ntmp<-subset(newtmp,newtmp$Country==a,select =c(Keyword,Search.Volume,Date,Country))
       
      if(nrow(ntmp)>2){
        nats<-fill_missing(ntmp)
        nats$Search.Volume<-na.ma(nats$Search.Volume)
        
            objlist<-rbind(objlist,nats)
          
     }}} else print("not fil criteria")
        
      if(!length(objlist)==0){
      objlist$Date<-as.Date(objlist$Date)
      ggsave(filename = paste0("figs/compare/",round(hvol),".",t,".png"),timeseries_Search.Volume(objlist,t))
      #timeseries_Search.Volume(nats,fg)
    }}
    
  }
  
  
  
  
  
  timeseries_Search.Volume<-function(arg1,arg2){
    
    TimeDesc<-arg1[order(as.Date(arg1$Date,format="%b-%Y")),,drop=FALSE]#Chronological order of the observations
    arg1k<-TimeDesc #Time series analysis in the first 100 observations.
    CostPerclick<-subset(arg1k,select=c(Search.Volume,Date,Country)) #Select the variables we need
    if(length(arg1$Date)>=25){
      mbrk<-"4 month"
    } else if(length(arg1$Date)>=20){mbrk<-"3 month"}
    else if(length(arg1$Date)>=13){mbrk<-"2 month"}
    else mbrk<-"1 month"
    
    zz<-ggplot(CostPerclick, aes(Date, Search.Volume,colour=Country)) + 
       geom_smooth()+
      scale_colour_manual(name = "Country",values = myColors)+
      scale_x_date(date_labels = "%b-%Y",date_breaks = mbrk) +
      xlab("TimeSeries") + ylab("Search.Volume distribution")+labs(title=paste0("Keyword:-",arg2))+
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
    merged.data$Keyword<-raw.data$Keyword[1]
    merged.data$Country<-raw.data$Country[1]
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
    c<-ts(doc$Search.Volume,frequency=12,start = as.numeric(c(y,m)))
    arima.model<-auto.arima(c) #ARIMA model
    forec.20<-forecast.Arima(arima.model,h = pm,bootstrap = TRUE)
   forec.20
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
  
  
  tmpf(dataset,dataset1)
}
