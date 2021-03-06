international<-function(dataset,arg1){
  
    tmpf<-function(dataset,arg1){
      tmp<-dataset[order(dataset$Search.Volume,decreasing = T),]
      tmp<-subset(tmp,tmp$Search.Volume >1000 | tmp$Search.Volume >= 500)
      t1<-tmp$Keyword[!duplicated(tmp$Keyword)]
      t2<-t1
      t2<-t2[!is.na(t2)]
      pos<-list()
      neg<-list()
      
      for(t in t2){
        newtmp<-subset(dataset,dataset$Keyword==t ,select =c(Search.Volume,Date))
        newtmp<-subset(newtmp,as.Date(newtmp$Date) > as.Date("2011-01-01"),select =c(Search.Volume,Date))
        
        newtmp<-newtmp[!duplicated(newtmp),]
        newtmp<-newtmp[complete.cases(newtmp), ]
        if(nrow(newtmp)>1){
          nats<-fill_missing(newtmp)
          if(nrow(nats)>=5 & any(nats$Date > as.Date("2015-12-01"))){
            nats$Search.Volume<-na.ma(nats$Search.Volume)
            track<-((nats$Search.Volume[nrow(nats)]- nats$Search.Volume[1])/nats$Search.Volume[1] *100 )
            if(track >= 50) { 
              pos<-c(pos,t)  ##country.searchvol.positive.keyword
              ggsave(filename = paste0("figs/search.vol/global/","global",".",round(track),".","positive",".",t,".png"),timeseries_calc(nats,t))}
            else if(track<= -50) {
              neg<-c(neg,t)
              ggsave(filename = paste0("figs/search.vol/global/","global",".",round(track),".","negative",".",t,".png"),timeseries_calc(nats,t))}
            
          } else print("not enough data") }
        #timeseries_Search.Volume(nats,fg)
      }
      
    }
    
 
  
  
  
  timeseries_Search.Volume<-function(arg1,arg2){
    
    TimeDesc<-arg1[order(as.Date(arg1$Date,format="%b-%Y")),,drop=FALSE]#Chronological order of the observations
    arg1k<-TimeDesc #Time series analysis in the first 100 observations.
    CostPerclick<-subset(arg1k,select=c(Search.Volume,Date)) #Select the variables we need
    if(length(arg1$Date)>=25){
      mbrk<-"4 month"
    } else if(length(arg1$Date)>=20){mbrk<-"3 month"}
    else if(length(arg1$Date)>=13){mbrk<-"2 month"}
    else mbrk<-"1 month"
    zz<-ggplot(CostPerclick, aes(Date, Search.Volume)) + geom_line() +
      scale_x_date(date_labels = "%b-%Y",date_breaks = mbrk) +
      xlab("TimeSeries") + ylab("Search.Volume distribution")+labs(title=arg2)+
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
    c<-ts(doc$Search.Volume,frequency=12,start = as.numeric(c(y,m)))
    arima.model<-auto.arima(c) #ARIMA model
    forec.20<-forecast.Arima(arima.model,h = pm,bootstrap = TRUE)
    g<-paste0("Keyword :- ",arg2)
    ps<-plot_timeseries(forec.20,format.date = T)+
      scale_x_date(date_labels = "%b-%Y",date_breaks = mbrk)+ylab("Search.Volume Distribution")+
      xlab("TimeSeries")+
      ggtitle(bquote(atop(.(paste0("Keyword :- ",arg2)), atop(italic("Region:-Global"), ""))))+
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
  
  
  tmpf(dataset,arg1)
}