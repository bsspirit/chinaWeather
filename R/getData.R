#' Get weather data from Yahoo openAPI.
#'
#' @param woeid input a yahoo woeid
#' @return data.frame weather data
#' @keywords weather 
#' @export
#' @examples
#' getWeatherYahoo()
#' getWeatherYahoo(2151330)
getWeatherFromYahoo<-function(woeid=2151330){
  library(RCurl)
  library(XML)
  
  url<-paste('http://weather.yahooapis.com/forecastrss?w=',woeid,'&u=c',sep="")
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
  
  print(paste(woeid,'==>',low,high,code,humidity,visibility,pressure,rising))
  return(cbind(low,high,code,humidity,visibility,pressure,rising))
}

#' Get one city weather Data.
#'
#' @param en input a English city name
#' @param zh input a Chinese city name
#' @param src input data source
#' @return data.frame weather data
#' @keywords weather 
#' @export
#' @examples
#' getWeatherByCity()
#' getWeatherByCity(en="beijing")
#' getWeatherByCity(zh="上海")
getWeatherByCity<-function(en="beijing",zh=NULL,src="yahoo"){
  woeid<-getWOEIDByCity(en)
  if(src=="yahoo"){
    return(getWeatherFromYahoo(woeid))
  }else{
    return(NULL)
  }
}

#' Get all of city weather Data.
#'
#' @param src input data source
#' @return data.frame weather data
#' @keywords weather 
#' @export
#' @examples
#' getWeather()
getWeather<-function(src="yahoo"){
  cities<-getCityInfo()
  wdata<-do.call(rbind, lapply(cities$woeid,getWeatherFromYahoo))
  return(cbind(cities,wdata))
}

