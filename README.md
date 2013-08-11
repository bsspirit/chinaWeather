China Weather Project for R
========================================================

## install package

    library(devtools)
    install_github("chinaWeather","bsspirit")
    
## Run a demo

    library(chinaWeather)
    drawTemperature(weather20130810)
    drawTemperature(weather20130810,lang='en')