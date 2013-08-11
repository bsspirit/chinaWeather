#' load China Map data only once.
#'
#' @return SpatialPolygonsDataFrame china map
#' @keywords map 
#' @export
#' @examples
#' loadChinaMap()
loadChinaMap<-function(){
  #library(maps)
  #library(mapdata)
  #library(maptools)
  #chinaMap<<-readShapePoly('../mapdata/bou2_4p.shp')
  data(chinaMap)
}

getColors<-function(temp,breaks){
  f=function(x,y) ifelse(x %in% y,which(y==x),0)
  colIndex=sapply(chinaMap$ADCODE99,f,WOEID$adcode99)
  
  arr <- findInterval(temp, breaks)
  arr[which(is.na(arr))]=19
  return(arr[colIndex])
}

drawBackground<-function(title,date,lang='zh'){
  text(100,58,title,cex=2)
  text(105,54,format(date,"%Y-%m-%d"))
  #text(98,65,paste('chinaweatherapp','http://apps.weibo.com/chinaweatherapp'))
  #text(120,-8,paste('provided by The Weather Channel',format(date, "%Y-%m-%d %H:%M")),cex=0.8)
}

drawDescription<-function(data,temp,lang='zh'){
  rows<-1:nrow(data)
  x<-ceiling(rows/7)*11+68
  y<-17-ifelse(rows%%7==0,7,rows%%7)*3
  fontCols<-c("#08306B","#000000","#800026")[findInterval(temp,c(0,30))+1]
  if(lang=='zh'){
    text(x,y,paste(data$zh,temp),col=fontCols)
  }else{
    text(x,y,paste(data$en,temp),col=fontCols)  
  }
  #text(x,y,bquote(paste(.(data$en),.(temp),degree,C)),col=fontCols)
}

drawLegend<-function(breaks,colors){
  breaks2 <- breaks[-length(breaks)]
  par(mar = c(5, 0, 15, 10))
  image(x=1, y=0:length(breaks2),z=t(matrix(breaks2)),col=colors[1:length(breaks)-1],axes=FALSE,breaks=breaks,xlab="",ylab="",xaxt="n")
  axis(4, at = 0:(length(breaks2)), labels = breaks, col = "white", las = 1)
  abline(h = c(1:length(breaks2)), col = "white", lwd = 2, xpd = FALSE)
}

drawTemperature<-function(data,lang='zh',type='high',date=Sys.time(),output=FALSE,path=''){
  if(length(which(ls()=="chinaMap"))!=1){
    loadChinaMap()
  }
  
  library("RColorBrewer")
  colors <- c(rev(brewer.pal(9,"Blues")),"#ffffef",brewer.pal(9,"YlOrRd"),"#500000")
  breaks=seq(-36,44,4)
  
  if(type=='high') {
    temp<-data$high
    title<-ifelse(lang=='zh', "中国各省白天气温", "Daytime Temperature")
    ofile<-paste(format(date,"%Y%m%d"),"_day.png",sep="")
  }else{
    temp<-data$low 
    title<-ifelse(lang=='zh', "中国各省夜间气温", "Nighttime Temperature")
    ofile<-paste(format(date,"%Y%m%d"),"_night.png",sep="")
  }
  
  if(output)png(file=paste(path,ofile,sep=''),width=600,height=600)
  
  layout(matrix(data=c(1,2),nrow=1,ncol=2),widths=c(8,1),heights=c(1,2))
  par(mar=c(0,0,3,10),oma=c(0.2,0.2,0.2,0.2),mex=0.3)
  plot(chinaMap,border="white",col=colors[getColors(temp,breaks)])
  points(data$long,data$lat,pch=19,col=rgb(0,0,0,0.3),cex=0.8)
  
  drawBackground(title,date,lang)
  drawDescription(data,temp,lang)
  drawLegend(breaks,colors)
}