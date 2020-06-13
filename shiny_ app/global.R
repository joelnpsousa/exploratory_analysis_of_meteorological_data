# requiredPackages <- c("shiny", "shinythemes", "dygraphs", "shinyjs", "shinycssloaders","xts",
#                       "dplyr","tidyverse","lubridate","ggplot2","DT","shinyWidgets","plotly",
#                       "htmlwidgets","ggiraph","ggpubr","webshot")
# for(pkg in requiredPackages){
#   library(pkg, character.only = TRUE)
# }

library(shiny)
library(shinythemes)
library(dygraphs)
library(xts)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(DT)
library(shinycssloaders)
library(plotly)
library(shinyWidgets)
library(shinyjs)
library(ggiraph)
library(ggpubr)
library(htmlwidgets)
library(webshot)

load("data/df.RData")
load("data/df_transformed.RData")

options(spinner.color="black")

library(ggplot2); theme_set(theme_minimal() +    
                              theme(axis.text.x = element_text(vjust=3,size = 10),
                                    axis.title = element_text(color="black", size=13, face="bold"),
                                    plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold")))
                                    #panel.background = element_rect(fill = "#FFDEB3")))
                                    #plot.background = element_rect(fill = "#434343ff")))

parChoices4 <-c("Air Temperature Minimum" = "tmin",
                "Air Temperature Maximum" = "tmax",
                "Air Temperature Average"= "tmed",
                "Air Temperature Shade 9h" = "shadeAirTemperature9h",
                "Air Temperature Shade 12h" = "shadeAirTemperature12h",
                "Air Temperature Shade 15h" = "shadeAirTemperature15h",
                "Air Temperature Exposure 9h" = "exposureAirTemperature9h",
                "Air Temperature Exposure 12h" = "exposureAirTemperature12h",
                "Air Temperature Exposure 15h" = "exposureAirTemperature15h",
                "Precipitation" = "prec",
                "Atmospheric Pressure 9h" = "pressure9h",
                "Atmospheric Pressure 12h" = "pressure12h",
                "Atmospheric Pressure 15h" = "pressure15h",
                "Atmospheric Pressure Average" = "pressureAve",
                "Vapor Pressure 9h" = "vaporPressure9h",
                "Vapor Pressure 12h" = "vaporPressure12h",
                "Vapor Pressure 15h" = "vaporPressure15h",
                "Humidity 9h" = "humidity9h",
                "Humidity 12h" = "humidity12h",
                "Humidity 15h" = "humidity15h",
                "Ozone" = "ozone",
                "Wind speed Absolute" = "absoluteWindSpeed",
                "Wind speed 15h" = "hourlyWindSpeed")

parChoices2 <- c("Minimum Air Temperature" = "temp.minima",
                "Maximum Air Temperature" = "temp.maxima",
                "Average Air Temperature" = "temp.media",
                "Shade Air Temperature at 9h" = "temp.9h.som",
                "Shade Air Temperature at 12h" = "temp.12h.som",
                "Shade Air Temperature at 15h" = "temp.15h.som",
                "Exposure Air Temperature at 9h" = "temp.9h.exp",
                "Exposure Air Temperature at 12h" = "temp.12h.exp",
                "Exposure Air Temperature at 15h" = "temp.15h.exp",
                "Precipitation" = "precipitacao",
                "Atmospheric Pressure 9h" = "pressao9h",
                "Atmospheric Pressure 12h" = "pressao12h",
                "Atmospheric Pressure 15h" = "pressao15h",
                "Average Atmospheric Pressure" = "pressao.media",
                "Vapor Pressure 9h" = "tensao9h",
                "Vapor Pressure 12h" = "tensao12h",
                "Vapor Pressure 15h" = "tensao15h",
                "Humidity 9h" = "humidade9h",
                "Humidity 12h" = "humidade12h",
                "Humidity 15h" = "humidade15h",
                "Ozone" = "ozono",
                "Absolute wind speed" = "velocidade.absoluta",
                "Wind speed 15h" = "velocidade.horaria" )

parChoices <- c("Minimum Air Temperature" = "Minimum.Air.Temperature",
                 "Maximum Air Temperature" = "Maximum.Air.Temperature",
                 "Average Air Temperature" = "Average.Air.Temperature",
                 "Shade Air Temperature at 9h" = "Temperature.Shade.9h",
                 "Shade Air Temperature at 12h" = "Temperature.Shade.12h",
                 "Shade Air Temperature at 15h" = "Temperature.Shade.15h",
                 "Exposure Air Temperature at 9h" = "Temperature.Exposure.9h",
                 "Exposure Air Temperature at 12h" = "Temperature.Exposure.12h",
                 "Exposure Air Temperature at 15h" = "Temperature.Exposure.15h",
                 "Precipitation" = "Precipitation",
                 "Atmospheric Pressure 9h" = "AtmosphericPressure.9h",
                 "Atmospheric Pressure 12h" = "AtmosphericPressure.12h",
                 "Atmospheric Pressure 15h" = "AtmosphericPressure.15h",
                 "Average Atmospheric Pressure" = "AtmosphericPressure.Average",
                 "Vapor Pressure 9h" = "VaporPressure.9h",
                 "Vapor Pressure 12h" = "VaporPressure.12h",
                 "Vapor Pressure 15h" = "VaporPressure.15h",
                 "Humidity 9h" = "Humidity.9h",
                 "Humidity 12h" = "Humidity.12h",
                 "Humidity 15h" = "Humidity.15h",
                 "Ozone" = "Ozone",
                 "Absolute wind speed" = "Wind.Speed.Absolute",
                 "Wind speed 15h" = "Wind.Speed.Hourly" )

xLabs <-c("Minimum air temperature (\u00B0C)" = "tmin",
          "Maximum air temperature (\u00B0C)" = "tmax",
          "Average air temperature (\u00B0C)"= "tmed",
          "Shade air temperature 9h (\u00B0C)" = "shadeAirTemperature9h",
          "Shade air temperature 12h (\u00B0C)" = "shadeAirTemperature12h",
          "Shade air temperature 15h (\u00B0C)" = "shadeAirTemperature15h",
          "Exposure air temperature 9h (\u00B0C)" = "exposureAirTemperature9h",
          "Exposure air temperature 12h (\u00B0C)" = "exposureAirTemperature12h",
          "Exposure air temperature 15h (\u00B0C)" = "exposureAirTemperature15h",
          "Precipitation (mm)" = "prec",
          "Atmospheric pressure 9h (mmHg)" = "pressure9h",
          "Atmospheric pressure 12h (mmHg)" = "pressure12h",
          "Atmospheric pressure 15h (mmHg)" = "pressure15h",
          "Average atmospheric pressure (mmHg)" = "pressureAve",
          "Vapor pressure 9h (mmHg)" = "vaporPressure9h",
          "Vapor pressure 12h (mmHg)" = "vaporPressure12h",
          "Vapor pressure 15h (mmHg)" = "vaporPressure15h",
          "Humidity 9h (%)" = "humidity9h",
          "Humidity 12h (%)" = "humidity12h",
          "Humidity 15h (%)" = "humidity15h",
          "Ozone ('grãos med.')" = "ozone",
          "Absolute wind speed (km/day)" = "absoluteWindSpeed",
          "Wind speed 15h (km)" = "hourlyWindSpeed")

origem <- "1860-12-01"
fim <- "1898-03-31"
minimo <- "1860-12-01"
min.wind <- "1865-01-01"
maximo <- "1898-03-31"

minimo_ano <- 1861
maximo_ano <- 1897

appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
demo_users <- list(
  list(
    username = "demo-appsilon",
    password_sha256 = "A7574A42198B7D7EEE2C037703A0B95558F195457908D6975E681E2055FD5EB9",
    roles = list("basic", "admin")
  ),
  list(
    username = "john",
    password_sha256 = "C2F77349B4D0CDE5A1E865195A9E395E1DF8829BE9D31707BD12F44CEB384A60",
    roles = list("basic")
  )
)