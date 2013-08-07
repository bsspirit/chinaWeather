#' Get WOEID of Yahoo By City Name
#'
#' @param en input a English city name
#' @param zh input a Chinese city name
#' @return integer WOEID
#' @keywords WOEID 
#' @export
#' @examples
#' getWOEIDByCity()
#' getWOEIDByCity(en="beijing")
#' getWOEIDByCity(zh='上海')
getWOEIDByCity<-function(en="beijing",zh=NULL){
  if(!is.null(zh) && !is.na(zh) ){
    return(WOEID$woeid[which(WOEID$zh==zh)])
  }
  return(WOEID$woeid[which(WOEID$en==en)])
}
#' Get all of city info
#'
#' @param lang input a language
#' @return data.frame city info
#' @keywords language 
#' @export
#' @examples
#' getWOEIDByCity()
#' getWOEIDByCity(lang="en")
#' getWOEIDByCity(lang="zh")
getCityInfo<-function(lang="en"){
  if(lang=="en")return(WOEID[-c(3,4)])
  if(lang=="zh")return(WOEID[-c(4)])
}