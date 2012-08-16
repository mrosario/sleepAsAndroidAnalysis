#main
analyzeSleepData<-function(){
  sleepData<-scan(file="~/Dropbox/Apps/Sleep as Android/Graph/sleep-export.csv",what="character",sep="\n")
  sleepData<-lapply(X=seq(1,length(sleepData),2),parseSleepData,sleepData)
  
  print(c("(r)ating","(raw) data","(d)uration","(s)tacked raw data"))
  answer<-readline("What analysis would you like to perform: ")
  

    
    if(answer=="d")
      plotSleepInfo(sleepData,x="Duration",y="Rating")
    if(answer=="s")
      plotSleepInfo(sleepData,x="From",y="Rating")
    if(answer=="e")
      plotSleepInfo(sleepData,x="To",y="Rating")
    answer="complete"
  }
  if(answer=="raw"){
    plotSleepData(data=sleepData)
  }
  if(answer=="d"){
    durationBoxPlot(data=sleepData)
  }
  if(answer=="s"){
    print(c("(r)aw","(o)rdered"))
    answer<-readline("What analysis would you like to perform: ")
    if(answer=="r")
      plotSleepData(data=sleepData,stack=T)
    if(answer=="o")
      plotSleepData(data=sleepData,stack=T,ordered=T)
    answer="complete"
  }
  sleepData
}


#functions
parseSleepData<-function(lineNumber,data){
  tempString<-data[lineNumber]
  tempString<-gsub(pattern="\\\"",replacement="",tempString)
  tempString2<-data[lineNumber+1]
  tempString2<-gsub(pattern="\\\"",replacement="",tempString2)
  
  
  labels<-unlist(strsplit(x=tempString,split=","))
  values<-unlist(strsplit(x=tempString2,split=","))
  
  #work on first data frame of sleep details
  details<-data.frame(ID=values[1],Timezone=values[2],From=strptime(values[3],format="%d. %m. %Y %H:%M"),To=strptime(values[4],format="%d. %m. %Y %H:%M"),Duration=as.numeric(values[6]),Rating=as.numeric(values[7]),Comment=values[8],Framerate=as.numeric(values[9]))
  
  #time<-strptime(labels[10:length(values)],format("%H:%M"))
  time<-seq(from=details$From,to=details$To,length.out=length(values)-9)
  peaks<-as.numeric(values[10:length(values)])
  
  sleepData<-data.frame(time=time,peaks=peaks)
  
  list(info=details,sleepData=sleepData)
}

plotSleepInfo<-function(data=sleepData,x,y){
  #tempY=vector(mode="numeric",length=length(data))
  #tempX=vector(mode="numeric",length=length(data))
  tempY=NULL
  tempX=NULL
  
  
  for(i in 1:length(data)){
    tempY[i]=data[[i]]$info[,y]
    tempX[i]=data[[i]]$info[,x]
  }
  
  #tempX<<-tempX
  #unclass(as.POSIXlt(tempX,origin="1970-01-01",tz="America/New_York"))
  if(x=="From"|x=="To"){
    tempUnclass<-unclass(as.POSIXlt(tempX,origin="1970-01-01",tz="America/New_York"))
    tempX<-strptime(paste(tempUnclass$hour,tempUnclass$min,sep=":"),format="%H:%M")
  }
  plot(x=tempX,y=tempY,xlab=x,ylab=y,pch=20,main=paste(y,"vs",x,sep=" "))
  
}

normalizeTime<-function(date){
  current<-unclass(as.POSIXlt(Sys.time(),origin="1970-01-01",tz="America/New_York"))
  toFix<-unclass(as.POSIXlt(date[1],origin="1970-01-01",tz="America/New_York"))
  toFixEnd<-unclass(as.POSIXlt(date[length(date)],origin="1970-01-01",tz="America/New_York"))
  if(toFix$hour<toFixEnd$hour)
    current<-paste(current$mon+1,current$mday,toFix$hour,toFix$min,sep="-")
  else
    current<-paste(current$mon+1,current$mday-1,toFix$hour,toFix$min,sep="-")
  current<-strptime(current,format="%m-%d-%H-%M")
  
  difference<-current-date[1]
  date+difference
}

plotSleepData<-function(data,stack=F,ordered=F){
  
  tempTime<-normalizeTime(date=data[[1]]$sleepData$time)
  timeMin=min(tempTime)
  timeMax=max(tempTime)
  peakMax=max(data[[1]]$sleepData$peaks)
  
  #find min and max time
  for(i in 2:length(data)){
    tempTime<-normalizeTime(data[[i]]$sleepData$time)
    if(min(tempTime)<timeMin)
      timeMin<-min(tempTime)
    if(max(tempTime>timeMax))
      timeMax<-max(tempTime)
    if(max(data[[i]]$sleepData$peaks)>peakMax)
      peakMax<-(max(data[[i]]$sleepData$peaks))
  }
  
  if(stack){
    #empty plot
    plot(x=c(timeMin,timeMax),y=c(0,length(data)+1),type="n",xlab="Time",ylab="Acceleration by Night")
    sequence=NULL
    
    if(ordered){
      cat("ordered stack begin!\n")
      fourabove=NULL
      threeabove=NULL
      rest=1:length(data)
      for(i in 1:length(data)){
        if(data[[i]]$info$Rating>=4)
          fourabove<-c(fourabove,i)
        if(data[[i]]$info$Rating>=3)
          threeabove<-c(threeabove,i)
      }
      sequence<-unique(c(fourabove,threeabove,rest))
    }
    else
    {
      sequence=1:length(data)
    }
    k=0
    for(i in sequence){
      points(x=normalizeTime(data[[i]]$sleepData$time),y=(data[[i]]$sleepData$peaks/max(data[[i]]$sleepData$peaks))+k,col=rgb(red=0,green=ifelse(data[[i]]$info$Rating>=3&data[[i]]$info$Rating<4,1,0),blue=ifelse(data[[i]]$info$Rating>=3,1,0),alpha=0.4),lwd=2,pch=20,cex=ifelse(data[[i]]$info$Rating>=4,1,0.1),type="l")
      k=k+1
    }
    
  }
  else {
    #empty plot
    plot(x=c(timeMin,timeMax),y=c(0,peakMax),type="n",xlab="Time",ylab="Acceleration")
    #plot curves
    for(i in 1:length(data)){
      points(x=normalizeTime(data[[i]]$sleepData$time[-1]),y=data[[i]]$sleepData$peaks[-1],col=rgb(red=0,green=ifelse(data[[i]]$info$Rating>=3&data[[i]]$info$Rating<4,1,0),blue=ifelse(data[[i]]$info$Rating>=3,1,0),alpha=0.3),lwd=ifelse(data[[i]]$info$Rating>=4,1,0.5),pch=20,cex=ifelse(data[[i]]$info$Rating>=4,1.5,0.5),type="l")
    }
  }
  
  
}

durationBoxPlot<-function(data){
  duration<-vector(mode="numeric",length=length(data))
  rating<-vector(mode="character",length=length(data))
  
  for(i in 1:length(data)){
    duration[i]<-data[[i]]$info$Duration
    rating[i]<-as.character(data[[i]]$info$Rating)
  }
  
  rating<-as.factor(rating)
  boxplot(duration~rating,xlab="Rating",ylab="Duration")
  
}

plotSleepData1<-function(data){
  
  tempTime<-normalizeTime(date=data[[1]]$sleepData$time)
  timeMin=min(tempTime)
  timeMax=max(tempTime)
  peakMax=max(data[[1]]$sleepData$peaks)
  
  #find min and max time
  for(i in 2:length(data)){
    tempTime<-normalizeTime(data[[i]]$sleepData$time)
    if(min(tempTime)<timeMin)
      timeMin<-min(tempTime)
    if(max(tempTime>timeMax))
      timeMax<-max(tempTime)
    if(max(data[[i]]$sleepData$peaks)>peakMax)
      peakMax<-(max(data[[i]]$sleepData$peaks))
  }
  
  #empty plot
  plot(x=c(timeMin,timeMax),y=c(0,peakMax),type="n",xlab="Time",ylab="Acceleration")
  
  #plot curves
  for(i in 1:length(data)){
    points(x=normalizeTime(data[[i]]$sleepData$time[-1]),y=data[[i]]$sleepData$peaks[-1],col=rgb(red=0,green=ifelse(data[[i]]$info$Rating>=3&data[[i]]$info$Rating<4,1,0),blue=ifelse(data[[i]]$info$Rating>=3,1,0),alpha=0.3),lwd=ifelse(data[[i]]$info$Rating>=4,1,0.5),pch=20,cex=ifelse(data[[i]]$info$Rating>=4,1.5,0.5),type="l")
  }
  
}

durationBoxPlot<-function(data){
  duration<-vector(mode="numeric",length=length(data))
  rating<-vector(mode="character",length=length(data))
  
  for(i in 1:length(data)){
    duration[i]<-data[[i]]$info$Duration
    rating[i]<-as.character(data[[i]]$info$Rating)
  }
  
  rating<-as.factor(rating)
  boxplot(duration~rating,xlab="Rating",ylab="Duration")
  
}

x="test"
