\name{getWeatherByCity}
\alias{getWeatherByCity}
\title{Get one city weather Data.}
\usage{
  getWeatherByCity(en = "beijing", zh = NULL,
    src = "yahoo")
}
\arguments{
  \item{en}{input a English city name}

  \item{zh}{input a Chinese city name}

  \item{src}{input data source}
}
\value{
  data.frame weather data
}
\description{
  Get one city weather Data.
}
\examples{
getWeatherByCity()
getWeatherByCity(en="beijing")
getWeatherByCity(zh="上海")
}
\keyword{weather}

