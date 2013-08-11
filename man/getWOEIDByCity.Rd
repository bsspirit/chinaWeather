\name{getWOEIDByCity}
\alias{getWOEIDByCity}
\title{Get WOEID of Yahoo By City Name}
\usage{
  getWOEIDByCity(en = "beijing", zh = NULL)
}
\arguments{
  \item{en}{input a English city name}

  \item{zh}{input a Chinese city name}
}
\value{
  integer WOEID
}
\description{
  Get WOEID of Yahoo By City Name
}
\examples{
getWOEIDByCity()
getWOEIDByCity(en="beijing")
getWOEIDByCity(zh='上海')
}
\keyword{WOEID}

