#' Get Weather Data from Yahoo openAPI.
#'
#' @param woeid input a yahoo woeid
#' @return data.frame weather data
#' @keywords weather 
#' @export
#' @examples
#' getWeatherYahoo()
#' getWeatherYahoo(2151330)
getWeatherYahoo<-function(woeid=2151330){
  library(RCurl)
  library(XML)
  
  url<-paste('http://weather.yahooapis.com/forecastrss?w=',x,'&u=c',sep="")
  doc = xmlTreeParse(getURL(url),useInternal = TRUE)
  
  ans<-getNodeSet(doc, "//yweather:atmosphere")
  humidity<-as.numeric(sapply(ans, xmlGetAttr, "humidity"))
  visibility<-as.numeric(sapply(ans, xmlGetAttr, "visibility"))
  pressure<-as.numeric(sapply(ans, xmlGetAttr, "pressure"))
  rising<-as.numeric(sapply(ans, xmlGetAttr, "rising"))
  
  ans<-getNodeSet(doc, "//item/yweather:condition")
  code<-sapply(ans, xmlGetAttr, "code")
  
  ans<-getNodeSet(doc, "//item/yweather:forecast[1]")
  low<-as.numeric(sapply(ans, xmlGetAttr, "low"))
  high<-as.numeric(sapply(ans, xmlGetAttr, "high"))
  
  print(paste(x,'==>',low,high,code,humidity,visibility,pressure,rising))
  cbind(low,high,code,humidity,visibility,pressure,rising)
}