datasetInput2 <- reactive({
  switch(input$dataset2,
         "Original" = df,
         "Changed" = changedx)
})

observeEvent(input$reset_input, {
  shinyjs::reset("dat") 
  shinyjs::reset("dat_wind") 
})

parameterInput <- reactive({
  dataset <- datasetInput2()
  switch(input$par,
         "tmax" = na.approx(dataset$temp.maxima),
         "tmin" = na.approx(dataset$temp.minima),
         "tmed" = na.approx(dataset$temp.media),
         "prec" = na.approx(dataset$precipitacao),
         "pressure9h" = na.approx(dataset$pressao9h),
         "pressure12h" = na.approx(dataset$pressao12h),
         "pressure15h" = na.approx(dataset$pressao15h),
         "pressureAve" = na.approx(dataset$pressao.media),
         "shadeAirTemperature9h" = na.approx(dataset$temp.9h.som),
         "shadeAirTemperature12h" = na.approx(dataset$temp.12h.som),
         "shadeAirTemperature15h" = na.approx(dataset$temp.15h.som),
         "exposureAirTemperature9h" = na.approx(dataset$temp.9h.exp),
         "exposureAirTemperature12h" = na.approx(dataset$temp.12h.exp),
         "exposureAirTemperature15h" = na.approx(dataset$temp.15h.exp),
         "vaporPressure9h" = na.approx(dataset$tensao9h),
         "vaporPressure12h" = na.approx(dataset$tensao12h),
         "vaporPressure15h" = na.approx(dataset$tensao15h),
         "humidity9h"= na.approx(dataset$humidade9h),
         "humidity12h"= na.approx(dataset$humidade12h),
         "humidity15h"= na.approx(dataset$humidade15h),
         "ozone" = na.approx(dataset$ozono),
         "absoluteWindSpeed"= dataset$velocidade.absoluta, #na.approx(dataset$velocidade.absoluta[-(1:1492), , drop = FALSE]),
         "hourlyWindSpeed"=dataset$velocidade.horaria
  )
})

parameterInput2 <- reactive({
  dataset <- datasetInput2()
  #dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  if(input$par == "absoluteWindSpeed" || input$par == "hourlyWindSpeed")
    dsrange<- subset(dataset, data >= as.Date(input$dat_wind[1]) & data <= as.Date(input$dat_wind[2]))
  else
    dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  switch(input$par,
         tmax = na.approx(dsrange$temp.maxima),
         tmin = na.approx(dsrange$temp.minima),
         tmed = na.approx(dsrange$temp.media),
         prec = na.approx(dsrange$precipitacao),
         "pressure9h" = na.approx(dsrange$pressao9h),
         "pressure12h" = na.approx(dsrange$pressao12h),
         "pressure15h" = na.approx(dsrange$pressao15h),
         "pressureAve" = na.approx(dsrange$pressao.media),
         "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
         "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
         "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
         "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
         "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
         "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
         "vaporPressure9h" = na.approx(dsrange$tensao9h),
         "vaporPressure12h" = na.approx(dsrange$tensao12h),
         "vaporPressure15h" = na.approx(dsrange$tensao15h),
         "humidity9h"= na.approx(dsrange$humidade9h),
         "humidity12h"= na.approx(dsrange$humidade12h),
         "humidity15h"= na.approx(dsrange$humidade15h),
         "ozone" = na.approx(dsrange$ozono),
         "absoluteWindSpeed"=dsrange$velocidade.absoluta,
         "hourlyWindSpeed"=dsrange$velocidade.horaria)
})
parameterInput3 <- reactive({  #used in statistics per year
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dataset, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  switch(input$par,
         tmax = na.approx(dsrange$temp.maxima),
         tmin = na.approx(dsrange$temp.minima),
         tmed = na.approx(dsrange$temp.media),
         prec = na.approx(dsrange$precipitacao),
         "pressure9h" = na.approx(dsrange$pressao9h),
         "pressure12h" = na.approx(dsrange$pressao12h),
         "pressure15h" = na.approx(dsrange$pressao15h),
         "pressureAve" = na.approx(dsrange$pressao.media),
         "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
         "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
         "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
         "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
         "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
         "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
         "vaporPressure9h" = na.approx(dsrange$tensao9h),
         "vaporPressure12h" = na.approx(dsrange$tensao12h),
         "vaporPressure15h" = na.approx(dsrange$tensao15h),
         "humidity9h"= na.approx(dsrange$humidade9h),
         "humidity12h"= na.approx(dsrange$humidade12h),
         "humidity15h"= na.approx(dsrange$humidade15h),
         "ozone" = na.approx(dsrange$ozono),
         "absoluteWindSpeed"=dsrange$velocidade.absoluta,
         "hourlyWindSpeed"=dsrange$velocidade.horaria)
})

dsfiltered <- reactive({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  dsrange
})


#### Chart ####
output$from <- renderText({
  if (!is.null(input$dygraph_date_window))
    strftime(input$dygraph_date_window[[1]], " %m/%Y")      
})
output$to <- renderText({
  if (!is.null(input$dygraph_date_window))
    strftime(input$dygraph_date_window[[2]], " %m/%Y")
})

output$dygraph <- renderDygraph({
  dataset <- datasetInput2()
  z <- xts(parameterInput(), as.Date(dataset$data, format = "%Y")) %>%
    dygraph(ylab=(names(xLabs[xLabs==input$par]))) %>%
    dyRangeSelector(dateWindow = input$dat) %>%
    dyOptions(stackedGraph = TRUE, 
              animatedZooms = TRUE, 
              colors="#D8AE5A",
              #colors = RColorBrewer::brewer.pal(4, "YlOrRd"),
              drawPoints = TRUE,
              #rightGap = 45,
              fillAlpha = 0.0,
              pointSize = 1.4,
              drawGapEdgePoints=TRUE
    ) %>%
    dySeries(, label = "Value") %>%
    dyAxis("x", drawGrid = TRUE) %>%
    dyLegend(show = "onmouseover") %>%
    dyRoller(showRoller = TRUE, rollPeriod = 0) %>%
    dyUnzoom()
})

output$help <-renderUI({
  helpText("Click and drag to zoom in (double-click to zoom out).\n",
           "The number inside the box in the lower left corner corresponds to the value used in the calculation of the moving average.")
})


#### Statistics ----
output$from3 <- renderText({
  (!is.null(input$daterangein))
  strftime(input$dat[1], "%d/%m/%Y")
})
output$to3 <- renderText({
  if (!is.null(input$daterangein))
    strftime(input$dat[2], "%d/%m/%Y")
})

#line <- reactive({as.numeric(input$temp_in)})

statistics <- function(){
  dataset <- datasetInput2()
  if(input$par == "absoluteWindSpeed" || input$par == "hourlyWindSpeed")
    dsrange<- subset(dataset, data >= as.Date(input$dat_wind[1]) & data <= as.Date(input$dat_wind[2]))
  else
    dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  p <- ggplot(dsrange, aes_string(x = dsrange$data, y=parameterInput2())) +#parameterInput2()
    geom_point(color="#D8AE5A")+
    scale_x_date(date_breaks = "year" , date_labels = "%Y")+
    xlab("Date") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1,size = 10),
          axis.title = element_text(color="black", size=13, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=14, face="bold"))
  
  if(input$par == "tmin")
    p+ggtitle ("Daily minimum air temperature")+ylab ("Minimum temperature (\u00B0C)")
  else if(input$par == "tmax")
    p+ggtitle ("Daily maximum air temperature")+ylab ("Maximum temperature (\u00B0C)") 
  else if(input$par == "tmed")
    p+ggtitle ("Daily average air temperature")+ylab ("Average temperature (\u00B0C)")
  else if(input$par == "prec")
    p+ggtitle("Daily precipitation")+ylab ("Precipitation (mm)")
  else if(input$par == "pressure9h")
    p+ggtitle("Daily atmospheric pressure at 9h")+ylab("Atmospheric pressure at 9h (mmHg)")
  else if(input$par == "pressure12h")
    p+ggtitle("Daily atmospheric pressure at 12h")+ylab("Atmospheric pressure at 12h (mmHg)")
  else if(input$par == "pressure15h")
    p+ggtitle("Daily atmospheric pressure at 15h")+ylab("Atmospheric pressure at 15h (mmHg)")
  else if(input$par == "pressureAve")
    p+ggtitle("Daily average atmospheric pressure")+ylab("Atmospheric pressure average (mmHg)")
  else if(input$par == "shadeAirTemperature9h")
    p+ggtitle("Daily shade air temperature at 9h")+ylab("Shade air temperature at 9h (\u00B0C)")
  else if(input$par == "shadeAirTemperature12h")
    p+ggtitle("Daily shade air temperature at 12h")+ylab("Shade air temperature at 12h (\u00B0C)")
  else if(input$par == "shadeAirTemperature15h")
    p+ggtitle("Daily shade air temperature at 15h")+ylab("Shade air temperature at 15h (\u00B0C)")
  else if(input$par == "exposureAirTemperature9h")
    p+ggtitle("Daily exposure air temperature at 9h")+ylab("Exposure air temperature at 9h (\u00B0C)")
  else if(input$par == "exposureAirTemperature12h")
    p+ggtitle("Daily exposure air temperature at 12h")+ylab("Exposure air temperature at 12h (\u00B0C)")
  else if(input$par == "exposureAirTemperature15h")
    p+ggtitle("Daily shade air temperature at 15h")+ylab("Exposure air temperature at 15h (\u00B0C)")
  else if(input$par == "vaporPressure9h")
    p+ggtitle("Daily vapor pressure at 9h")+ylab("Vapor pressure 9h (mmHg)")
  else if(input$par == "vaporPressure12h")
    p+ggtitle("Daily vapor pressure at 12h")+ylab("Vapor pressure 12h (mmHg)")
  else if(input$par == "vaporPressure15h")
    p+ggtitle("Daily vapor pressure at 15h")+ylab("Vapor pressure 15h (mmHg)")
  else if(input$par == "humidity9h")
    p+ggtitle("Daily humidity at 9h")+ylab("Humidity (%)")
  else if(input$par == "humidity12h")
    p+ggtitle("Daily humidity at 12h")+ylab("Humidity (%)")
  else if(input$par == "humidity15h")
    p+ggtitle("Daily humidity at 15h")+ylab("Humidity (%)")
  else if(input$par == "ozone")
    p+ggtitle("Daily ozone")+ylab("Ozone ('gr?os med.')")
  else if(input$par == "absoluteWindSpeed")
    p+ggtitle("Daily absolute wind speed")+ylab("Absolute wind speed (km/day)")
  else if(input$par == "hourlyWindSpeed")
    p+ggtitle("Daily hourly wind speed")+ylab("Hourly wind speed (km/day)")
}
statistics2 <- function(){
  #if(input$quergrafico == TRUE){
    if(input$querlinha == TRUE && input$quersmooth == TRUE)
      statistics()+
      geom_point(aes(colour = parameterInput2() > input$temp_in)) + #,shape = "."
      scale_colour_manual(name = paste('>',input$temp_in), values = setNames(c('#D8AE5A','red'),c(F, T)),guide=FALSE)+
      geom_hline(yintercept=input$temp_in, colour="red")+
      geom_text(data=data.frame(x=input$dat[1]+200,y=input$temp_in), aes(x, y), label="Reference Value", vjust=-1, colour="firebrick")+
      geom_smooth(size=1)
    else if(input$querlinha == TRUE && input$quersmooth == FALSE)
      statistics()+
      geom_point(aes(colour = parameterInput2() > input$temp_in)) + #,shape = "."
      scale_colour_manual(name = paste('>',input$temp_in), values = setNames(c('#D8AE5A','red'),c(F, T)),guide=FALSE)+
      geom_hline(yintercept=input$temp_in, colour="red")+
      geom_text(data=data.frame(x=input$dat[1]+200,y=input$temp_in), aes(x, y), label="Reference Value", vjust=-1, colour="firebrick")
    else if(input$querlinha == FALSE && input$quersmooth == TRUE)
      statistics()+ geom_smooth(size=1)
    else if(input$querlinha == FALSE && input$quersmooth == FALSE)
      statistics()
  }

output$statistics_plot <- renderPlot({
  statistics2()
})

output$estatisticas <- downloadHandler(
  filename =  function() {
    paste("Plot",input$par, input$type, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$type == "png")
      png(file,width = 1600) # open the png device
    else
      pdf(file,width = 14) # open the pdf device
    print(statistics2()) # draw the plot
    dev.off()  # turn the device off
  })




#### Statistics tables ####
output$statistics_table <- renderTable({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  par <- parameterInput2()
  
  valor5<- summarise(dsrange,sum(par > input$temp_in,na.rm = TRUE))
  newV5 <- as.vector(valor5[1,])
  valor1<- summarise(dsrange,max(par,na.rm = TRUE))
  newV1 <- as.vector(valor1[1,])
  valor3<- summarise(dsrange,min(par,na.rm = TRUE))
  newV3 <- as.vector(valor3[1,])
  valor2<- summarise(dsrange,round(mean(par,na.rm = TRUE),2))
  newV2 <- as.vector(valor2[1,])
  valores <- c(newV5,newV1,newV3,newV2)
  nomes <- c(paste0("Number of days with recordings greater than the reference value (",input$temp_in,")"),
             "Highest value",
             "Lowest value",
             "Average value")
  m <- cbind(nomes, valores)
  colnames(m) <- c(" "," ")
  m
})

output$statistics_max <- renderTable({
  dataset <- datasetInput2()
  h <-  dataset %>%
    select(data, temp.maxima,temp.minima) %>%
    filter(data >= input$dat[1] & data <= input$dat[2])
  dataset <- datasetInput2()
  
  valor0<- summarise(h,min(temp.maxima,na.rm = TRUE))
  valor1<- summarise(h,max(temp.maxima,na.rm = TRUE))
  newV1 <- as.vector(valor1[1,])
  valor2<- summarise(h,round(mean(temp.maxima,na.rm = TRUE),1))
  newV2 <- as.vector(valor2[1,])
  valor3<- summarise(h,sum(temp.maxima > 25,na.rm = TRUE))
  newV3 <- as.vector(valor3[1,])
  valor4<- summarise(h,sum(temp.maxima > 35,na.rm = TRUE))
  newV4 <- as.vector(valor4[1,])
  valor5<- summarise(h,sum(temp.maxima > input$temp_in,na.rm = TRUE))
  newV5 <- as.vector(valor5[1,])
  valor7<- summarise(h,max(temp.maxima,na.rm = TRUE)-min(temp.maxima,na.rm = TRUE))
  newV7 <- as.vector(valor7[1,])
  valor8<- summarise(h,max(temp.maxima,na.rm = TRUE)-min(temp.minima,na.rm = TRUE))
  newV8 <- as.vector(valor8[1,])
  
  h$daymonth <- format(as.Date(h$data), "%m-%d")
  h$mean <- with(h, ave(temp.maxima, daymonth))
  h$diff <- with(h, temp.maxima - (mean+5))
  newV6 <-  sum(with(rle(h$diff > 0), lengths >= 6))
  
  valores <- c(newV6,newV3,newV4,newV7,newV8)
  nomes <- c(
    "Number of heat waves",
    "Number of days with maximum temperature > 25 \u00B0C (Summer days)",
    "Number of days with maximum temperature> 35 \u00B0C (Extremely hot days)",
    "Extreme maximum temperature range",
    "Extreme temperature range"
  )
  m <- cbind(nomes, valores)
  colnames(m) <- c(" "," ")
  m
})


output$statistics_min <- renderTable({
  dataset <- datasetInput2()
  h <-  dataset %>%
    select(data, temp.minima, temp.maxima) %>%
    filter(data >= input$dat[1] & data <= input$dat[2])
  dataset <- datasetInput2()
  
  valor1<- summarise(h,min(temp.minima,na.rm = TRUE))
  newV1 <- as.vector(valor1[1,])
  valor2<- summarise(h,round(mean(temp.minima,na.rm = TRUE),1))
  newV2 <- as.vector(valor2[1,])
  valor3<- summarise(h,sum(temp.minima < 0,na.rm = TRUE))
  newV3 <- as.vector(valor3[1,])
  valor4<- summarise(h,sum(temp.minima > 20,na.rm = TRUE))
  newV4 <- as.vector(valor4[1,])
  valor5<- summarise(h,sum(temp.minima < input$temp_in,na.rm = TRUE))
  newV5 <- as.vector(valor5[1,])
  valor7<- summarise(h,max(temp.minima,na.rm = TRUE)-min(temp.minima,na.rm = TRUE))
  newV7 <- as.vector(valor7[1,])
  valor8<- summarise(h,max(temp.maxima,na.rm = TRUE)-min(temp.minima,na.rm = TRUE))
  newV8 <- as.vector(valor8[1,])

  h$daymonth <- format(as.Date(h$data), "%m-%d")
  h$mean <- with(h, ave(temp.minima, daymonth))
  h$diff <- with(h, temp.minima - (mean-5))
  newV6 <-  sum(with(rle(h$diff < 0), lengths >= 6))
  
  valores <- c(newV6,newV3,newV4,newV7,newV8)

  nomes <- c(
    "Number of cold waves",
    "Number of days with minimum temperature < 0 \u00B0C (Cold nights)",
    "Number of days with minimum temperature > 20 \u00B0C (Tropical nights)",
    "Extreme minimum temperature range",
    "Extreme temperature range"
  )

  m <- cbind(nomes, valores)
  colnames(m) <- c(" "," ")
  m
})  

output$statistics_prec <- renderTable({
  dataset <- datasetInput2()
  h <-  dataset %>%
    select(data, precipitacao) %>%
    filter(data >= input$dat[1] & data <= input$dat[2])
  valor0 <- summarise(h,sum(precipitacao > 0,na.rm = TRUE))
  newV0 <- as.vector(valor0[1,])
  valor1<- summarise(h,sum(precipitacao > 10,na.rm = TRUE))
  newV1 <- as.vector(valor1[1,])
  valor2<- summarise(h,sum(precipitacao > 20,na.rm = TRUE))
  newV2 <- as.vector(valor2[1,])
  valor3<- summarise(h,sum(precipitacao > 25,na.rm = TRUE))
  newV3 <- as.vector(valor3[1,])
  rain<-(h$precipitacao)
  rain5 <- summarise(h,max(rollapply(rain,5,sum),na.rm = TRUE))
  rain5 <- as.vector(rain5[1,])
  
  data.table::setDT(h)
  h[, dryday := +(precipitacao < 1)] [, hw.length := with(rle(dryday), rep(lengths,lengths))][dryday == 0, hw.length := 0]
  newV6 <- max(h$hw.length)
  
  h[, wetday := +(precipitacao >= 1)] [, hw.length2 := with(rle(wetday), rep(lengths,lengths))][wetday == 0, hw.length2 := 0]
  newV7 <- max(h$hw.length2)
  
  valores <- c(newV0,newV1,newV2,newV3,newV6,newV7,rain5)
  nomes <- c(#paste0("Number of days with precipitation greater than the reference value (",input$temp_in,"mm)"),
    "Number of days with precipitation > 0mm (wet days)",
    "Number of days with precipitation > 10mm (heavy precipitation days)",
    "Number of days with precipitation > 20mm (very heavy precipitation days)",
    "Number of days with precipitation > 25mm (extremely heavy precipitation days)",
    "Maximum consecutive dry days",
    "Maximum consecutive wet days",
    "Greatest 5-day precipitation total (Maximum 5-day precipitation sums) ")
  m <- cbind(nomes, valores)
  colnames(m) <- c(" "," ")
  m
})


#### Statistics per year ####

avg_year <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  str(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  #ano <- unique(dsrange$data)
  #str(dsrange$ano)
  #dsrange$data <- as.Date(dsrange$data, format="%Y")
  str(dsrange$data)
  par <- switch(input$par,
                tmax = na.approx(dsrange$temp.maxima),
                tmin = na.approx(dsrange$temp.minima),
                tmed = na.approx(dsrange$temp.media),
                prec = na.approx(dsrange$precipitacao),
                "pressure9h" = na.approx(dsrange$pressao9h),
                "pressure12h" = na.approx(dsrange$pressao12h),
                "pressure15h" = na.approx(dsrange$pressao15h),
                "pressureAve" = na.approx(dsrange$pressao.media),
                "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                "vaporPressure9h" = na.approx(dsrange$tensao9h),
                "vaporPressure12h" = na.approx(dsrange$tensao12h),
                "vaporPressure15h" = na.approx(dsrange$tensao15h),
                "humidity9h"= na.approx(dsrange$humidade9h),
                "humidity12h"= na.approx(dsrange$humidade12h),
                "humidity15h"= na.approx(dsrange$humidade15h),
                "ozone" = na.approx(dsrange$ozono),
                "absoluteWindSpeed"=dsrange$velocidade.absoluta,
                "hourlyWindSpeed"=dsrange$velocidade.horaria
  )

  p <- ggplot(dsrange, aes(x=data, y= par)) + 
    stat_summary(fun.y="mean", geom="bar", color="black",fill="#D8AE5A")+
    stat_summary(aes(label=round(..y..,1)), fun.y=mean, geom="text", size=4,vjust = -0.5)+
    geom_smooth(se=FALSE)+
    labs(y=(names(xLabs[xLabs==input$par])), x=("Year"), title=(paste("Annual average",names(parChoices4[parChoices4==input$par]))),colour = "red") +
    theme(axis.text.x = element_text(vjust=5, size = 15),
          axis.title = element_text(color="black", size=17, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=20, face="bold"))
  if(input$par == "pressure9h"){
    p + coord_cartesian(ylim = c(700,900))}#ylim(700, 1000)
  p
  #girafe(code = print(g))
}
#output$medmaxano_plot <-renderggiraph({
output$avg_year_plot <-renderPlot({
  avg_year()
})
output$avg_year_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$avg_year_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$avg_year_table <-renderDataTable({
  avg_year
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


max_year <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$year <- year(dsrange$data)
  if(input$par == "absoluteWindSpeed" || input$par == "hourlyWindSpeed")
    dsrange<- subset(dsrange, year >= as.Date(input$date.wind[1]) & year <= as.Date(input$date.wind[2]))
  else
    dsrange<- subset(dsrange, year >= as.Date(input$date[1]) & year <= as.Date(input$date[2]))
  
  #ano <- unique(dsrange$data)
  #str(dsrange$ano)
  
  dsrange$par <- switch(input$par,
                tmax = na.approx(dsrange$temp.maxima),
                tmin = na.approx(dsrange$temp.minima),
                tmed = na.approx(dsrange$temp.media),
                prec = na.approx(dsrange$precipitacao),
                "pressure9h" = na.approx(dsrange$pressao9h),
                "pressure12h" = na.approx(dsrange$pressao12h),
                "pressure15h" = na.approx(dsrange$pressao15h),
                "pressureAve" = na.approx(dsrange$pressao.media),
                "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                "vaporPressure9h" = na.approx(dsrange$tensao9h),
                "vaporPressure12h" = na.approx(dsrange$tensao12h),
                "vaporPressure15h" = na.approx(dsrange$tensao15h),
                "humidity9h"= na.approx(dsrange$humidade9h),
                "humidity12h"= na.approx(dsrange$humidade12h),
                "humidity15h"= na.approx(dsrange$humidade15h),
                "ozone" = na.approx(dsrange$ozono),
                "absoluteWindSpeed"=dsrange$velocidade.absoluta,
                "hourlyWindSpeed"=dsrange$velocidade.horaria
  )
  
  p <- ggplot(dsrange %>% group_by(year) %>% summarise(value=max(par, na.rm=TRUE)),
         aes(year, value)) +
    geom_bar(stat ="identity",fill="#D8AE5A",outlier.colour = "#D8AE5A",color="black") +
    geom_smooth(se=FALSE)+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
    labs(y=(names(xLabs[xLabs==input$par])), x=("Year"), title=(paste("Annual highest",names(parChoices4[parChoices4==input$par]))),colour = "red")+
    theme(axis.text.x = element_text(vjust=5, size = 15),
          axis.title = element_text(color="black", size=17, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=20, face="bold"))
  
  #stat_summary_bin(aes(label=..y..),fun.y=sum,geom="text",color="white",data=subset(dataset, Value >1))

  if(input$par == "pressure9h"){
    p + coord_cartesian(ylim = c(700, 800))}#ylim(700, 1000)
  p
}
output$max_year_plot <-renderPlot({ 
  max_year()
})
output$max_year_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste(xy_str(input$max_year_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$max_year_table <-renderDataTable({
  max_year
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


min_year <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  #dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  if(input$par == "absoluteWindSpeed" || input$par == "hourlyWindSpeed")
    dsrange<- subset(dsrange, data >= as.Date(input$date.wind[1]) & data <= as.Date(input$date.wind[2]))
  else
    dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  
  dsrange$par <- switch(input$par,
                        tmax = na.approx(dsrange$temp.maxima),
                        tmin = na.approx(dsrange$temp.minima),
                        tmed = na.approx(dsrange$temp.media),
                        prec = na.approx(dsrange$precipitacao),
                        "pressure9h" = na.approx(dsrange$pressao9h),
                        "pressure12h" = na.approx(dsrange$pressao12h),
                        "pressure15h" = na.approx(dsrange$pressao15h),
                        "pressureAve" = na.approx(dsrange$pressao.media),
                        "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                        "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                        "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                        "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                        "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                        "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                        "vaporPressure9h" = na.approx(dsrange$tensao9h),
                        "vaporPressure12h" = na.approx(dsrange$tensao12h),
                        "vaporPressure15h" = na.approx(dsrange$tensao15h),
                        "humidity9h"= na.approx(dsrange$humidade9h),
                        "humidity12h"= na.approx(dsrange$humidade12h),
                        "humidity15h"= na.approx(dsrange$humidade15h),
                        "ozone" = na.approx(dsrange$ozono),
                        "absoluteWindSpeed"= dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  #ggplot(dsrange, aes(x=data, y= par)) + 
  ggplot(dsrange %>% group_by(data) %>% summarise(value=min(par, na.rm=TRUE)), aes(data, value)) +
    geom_bar(stat ="identity",fill="#D8AE5A",outlier.colour = "#D8AE5A",color="black") +
    geom_smooth(se=FALSE)+
    geom_text(aes(label=value), position=position_dodge(width=0.9), vjust=-0.25)+
    labs(y=(names(xLabs[xLabs==input$par])), x=("Year"), title=(paste("Annual lowest",names(parChoices4[parChoices4==input$par]))),colour = "red") +
    theme(axis.text.x = element_text(vjust=5, size = 15),
          axis.title = element_text(color="black", size=17, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=20, face="bold"))
}
output$min_year_plot <-renderPlot({ 
  min_year()
})
output$min_year_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$min_year_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$min_year_table <-renderDataTable({
  min_year
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


year_box <- function() {
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  
  dsrange$par <- switch(input$par,
                tmax = na.approx(dsrange$temp.maxima),
                tmin = na.approx(dsrange$temp.minima),
                tmed = na.approx(dsrange$temp.media),
                prec = na.approx(dsrange$precipitacao),
                "pressure9h" = na.approx(dsrange$pressao9h),
                "pressure12h" = na.approx(dsrange$pressao12h),
                "pressure15h" = na.approx(dsrange$pressao15h),
                "pressureAve" = na.approx(dsrange$pressao.media),
                "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                "vaporPressure9h" = na.approx(dsrange$tensao9h),
                "vaporPressure12h" = na.approx(dsrange$tensao12h),
                "vaporPressure15h" = na.approx(dsrange$tensao15h),
                "humidity9h"= na.approx(dsrange$humidade9h),
                "humidity12h"= na.approx(dsrange$humidade12h),
                "humidity15h"= na.approx(dsrange$humidade15h),
                "ozone" = na.approx(dsrange$ozono),
                "absoluteWindSpeed"= dsrange$velocidade.absoluta,
                "hourlyWindSpeed"= dsrange$velocidade.horaria)

  p <- ggplot(dsrange, aes(x=data,y=par)) +
    geom_boxplot(aes(group = data),fill="#D8AE5A",outlier.colour = "#D8AE5A")+#position="stack",stat = "identity",show.legend = TRUE,fill="#D8AE5A")+  #position="dodge"
    #geom_text(aes(label=Valor), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    #scale_y_continuous(limits = c(-1,80))+
    ylab(names(xLabs[xLabs==input$par])) + xlab("Year") + ggtitle(paste("Annual",names(parChoices4[parChoices4==input$par]),"(boxplots)"))+
    theme(axis.text.x = element_text(vjust=5, size = 15),
          axis.title = element_text(color="black", size=17, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=20, face="bold"))
    #labs(colour = "")+
  p
}
output$year_box_plot <-renderPlot({
  year_box()
})
output$year_box_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$year_box_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$year_box_table <-renderDataTable({
  year_box
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


#### Temp maxima ####
heatwaves <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$daymonth <- format(as.Date(dsrange$data), "%m-%d")
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))


  dsrange$mean <- with(dsrange, ave(temp.maxima, daymonth))
  dsrange$diff <- with(dsrange, temp.maxima - mean)
  #newV6 <-  sum(with(rle(dsrange$diff < 0), lengths >= 6))
  
  ondacalor <- dsrange %>%
    group_by(data) %>%
    #summarise(count.heat.waves(dsrange, media, 6)) %>%
    summarise(sum(with(rle(dsrange$diff > 0), lengths >= 6))) %>% 
    setNames(c("Year", "Value")) %>%
    ungroup()
  #strptime(as.character(ondacalor$Ano), "%Y")
  p <- ggplot(ondacalor, aes(x=ondacalor$Year,y=ondacalor$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Heat waves (#)") + xlab("Year") + ggtitle("Number of heat waves per year") +
    labs(colour = "")
  p
}
output$heatwaves_plot <-renderPlot({
  heatwaves()
})
output$heatwaves_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$heatwaves_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$maior25_table <-renderDataTable({
  maior25
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

maior25_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  maior25 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(temp.maxima >25, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  strptime(as.character(maior25$Ano), "%Y")
  p <- ggplot(maior25, aes(x=maior25$Year,y=maior25$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with maximum temperatures higher than 25 ?C (#)") + xlab("Year") + ggtitle("Number of days with maximum temperatures higher than 25 ?C per year") +
    labs(colour = "")
  p
}
output$maior25_plot <-renderPlot({
  maior25_plot()
})
output$maior25_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$maior25_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$maior25_table <-renderDataTable({
  maior25
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

maior35_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  maior35 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(temp.maxima > 35, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(maior35, aes(x=maior35$Year,y=maior35$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with maximum temperatures higher than 35 ?C (#)") + xlab("Year") + ggtitle("Number of days with maximum temperatures higher than 35 ?C per year") +
    labs(colour = "")
  p
}
output$maior35_plot <-renderPlot({ 
  maior35_plot()
})
output$maior35_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$maior35_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$maior35_table <-renderDataTable({
  maior35
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})
extreme_max_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  extreme_max <- dsrange %>%
    group_by(data) %>%
    summarize(max(temp.maxima,na.rm = TRUE)-min(temp.maxima,na.rm = TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(extreme_max, aes(x=extreme_max$Year,y=extreme_max$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Maximum air temperature range") + xlab("Year") + ggtitle("Extreme maximum air temperature range per year") +
    labs(colour = "")
  p
}
output$extreme_max_plot <-renderPlot({ 
  extreme_max_plot()
})
output$extreme_max_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$extreme_max_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$extreme_max_table <-renderDataTable({
  extreme_max
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

#### Temp minima ####  
extreme_min_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  extreme_min <- dsrange %>%
    group_by(data) %>%
    summarize(max(temp.minima,na.rm = TRUE)-min(temp.minima,na.rm = TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(extreme_min, aes(x=extreme_min$Year,y=extreme_min$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Minimum air temperature range") + xlab("Year") + ggtitle("Extreme minimum air temperature range per year") +
    labs(colour = "")
  p
}
output$extreme_min_plot <-renderPlot({ 
  extreme_min_plot()
})
output$extreme_min_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$extreme_min_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$extreme_min_table <-renderDataTable({
  extreme_min
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

extreme2_min_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  extreme2_min <- dsrange %>%
    group_by(data) %>%
    summarize(max(temp.maxima,na.rm = TRUE)-min(temp.minima,na.rm = TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(extreme2_min, aes(x=extreme2_min$Year,y=extreme2_min$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Biggest difference between max and min air temperatures") + xlab("Year") + ggtitle("Extreme air temperature range per year") +
    labs(colour = "")
  p
}
output$extreme2_min_plot <-renderPlot({ 
  extreme2_min_plot()
})
output$extreme2_min_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$extreme2_min_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$extreme2_min_table <-renderDataTable({
  extreme_min2
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

min0_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  min0 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(temp.minima < 0, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  strptime(as.character(min0$Year), "%Y")
  p <- ggplot(min0, aes(x=min0$Year,y=min0$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Cold nights (#)") + xlab("Year") + ggtitle("Number of cold nights per year") +
    labs(colour = "")
  p
}
output$min0_plot <-renderPlot({ 
  min0_plot()
})
output$min0_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$min0_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$min0_table <-renderDataTable({
  min0
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

min20_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  min20 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(temp.minima > 20, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  strptime(as.character(min20$Year), "%Y")
  p <- ggplot(min20, aes(x=min20$Year,y=min20$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Tropical nights (#)") + xlab("Year") + ggtitle("Number of tropical nights per year") +
    labs(colour = "")
  p
}

output$min20_plot <-renderPlot({    
  min20_plot()
})
output$min20_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$min20_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$min20_table <-renderDataTable({
  min20
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

#### Precipitacao #### 
hidrologico <- function(){
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  
  dsrange$wy <- lfstat::water_year(dsrange$data, origin = input$hidro.year) 
  #wtr_yr(datah, 2)
  ggplot(dsrange, aes(x=wy, y=precipitacao)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") +
    ylab("Precipitation (mm)") + xlab("Hydrological year") + ggtitle(paste0("Precipitation by hydrological year (starting at month ",input$hidro.year,")")) +
    #geom_smooth(se=FALSE)+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1,size = 10),
          axis.title = element_text(color="black", size=13, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
}
output$hidrologico <- renderPlot({
  hidrologico()
})
output$hidrologico_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$hidrologico_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$hidrologico_table <-renderDataTable({
  hidrologico
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

pre0_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  pre0 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(precipitacao > 0, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(pre0, aes(x=pre0$Year,y=pre0$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with precipitation greater than 0 mm (#)") + xlab("Year") + ggtitle("Number of days with precipitation greater than 0 mm per year") +
    labs(colour = "")
  p
}
output$pre0_plot <-renderPlot({ 
  pre0_plot()
})
output$pre0_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$pre0_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$pre0_table <-renderDataTable({
  pre0
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

pre10_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  pre10 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(precipitacao > 10, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(pre10, aes(x=pre10$Year,y=pre10$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with precipitation greater than 10 mm (#)") + xlab("Year") + ggtitle("Number of days with precipitation greater than 10 mm per year") +
    labs(colour = "")
  p
}
output$pre10_plot <-renderPlot({ 
  pre10_plot()
})
output$pre10_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$pre10_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$pre10_table <-renderDataTable({
  pre10
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


pre20_plot <- function() {
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  pre20 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(precipitacao > 20, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(pre20, aes(x=pre20$Year,y=pre20$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with precipitation greater than 20 mm (#)") + xlab("Year") + ggtitle("Number of days with precipitation greater than 20 mm per year") +
    labs(colour = "")
  p
}
output$pre20_plot <-renderPlot({ 
  pre20_plot()
})
output$pre20_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$pre20_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$pre20_table <-renderDataTable({
  pre20
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


pre25_plot <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  pre25 <- dsrange %>%
    group_by(data) %>%
    summarize(sum(precipitacao > 25, na.rm=TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(pre25, aes(x=pre25$Year,y=pre25$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Days with precipitation greater than 25 mm (#)") + xlab("Year") + ggtitle("Number of days with precipitation greater than 25 mm per year") +
    labs(colour = "")
  p
}
output$pre25_plot <-renderPlot({ 
  pre25_plot()
})
output$pre25_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$pre25_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$pre25_table <-renderDataTable({
  pre25
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

pre_ano <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  precano <- dsrange %>%
    group_by(data) %>%
    summarize(round(sum(precipitacao, na.rm=TRUE),0)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(precano, aes(x=precano$Year,y=precano$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A")+  #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Total precipitation (mm)") + xlab("Year") + ggtitle("Total precipitation per year") +
    labs(colour = "") +
    theme(axis.text.x = element_text(vjust=4, size = 20),
          axis.title = element_text(color="black", size=20, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=23, face="bold"))
  p
}
output$pre_ano <-renderPlot({ 
  pre_ano()
})
output$preano_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$preano_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$preano_table <-renderDataTable({
  precano
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


dry_day_ano <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  
  data.table::setDT(dsrange)
  dsrange[, dryday := +(precipitacao < 1)] [, hw.length := with(rle(dryday), rep(lengths,lengths))][dryday == 0, hw.length := 0]
  newV6 <- max(dsrange$hw.length)
  
  drydayano <- dsrange %>%
    group_by(data) %>%
    summarize(round(max(hw.length, na.rm=TRUE),0)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(drydayano, aes(x=drydayano$Year,y=drydayano$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A")+  #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Consecutive dry days (#)") + xlab("Year") + ggtitle("Maximum consecutive dry days") +
    labs(colour = "")
  p
}
output$dry_day_ano <-renderPlot({ 
  dry_day_ano()
})
output$dry_day_ano_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$dry_day_ano_plot_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$dry_day_ano_table <-renderDataTable({
  drydayano
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


wet_day_ano <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  
  data.table::setDT(dsrange)
  dsrange[, wetday := +(precipitacao >= 1)] [, hw.length := with(rle(wetday), rep(lengths,lengths))][wetday == 0, hw.length := 0]
  newV6 <- max(dsrange$hw.length)
  
  wetdayano <- dsrange %>%
    group_by(data) %>%
    summarize(round(max(hw.length, na.rm=TRUE),0)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(wetdayano, aes(x=wetdayano$Year,y=wetdayano$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A")+  #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Consecutive wet days (#)") + xlab("Year") + ggtitle("Maximum consecutive wet days") +
    labs(colour = "")
  p
}
output$wet_day_ano <-renderPlot({ 
  wet_day_ano()
})
output$wet_day_ano_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$wet_day_ano_plot_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$wet_day_ano_table <-renderDataTable({
  wetdayano
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})

rain5days <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  dsrange$data <- year(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$date[1]) & data <= as.Date(input$date[2]))
  
  rain<-dsrange$precipitacao
  rain5 <- dsrange %>%
    group_by(data) %>%
    summarize(max(rollapply(precipitacao,5,sum),na.rm = TRUE)) %>%
    setNames(c("Year", "Value")) %>% 
    ungroup() 
  p <- ggplot(rain5, aes(x=rain5$Year,y=rain5$Value)) +
    geom_col(position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A")+  #position="dodge"
    geom_text(aes(label=Value), position=position_dodge(width=0.9), vjust=-0.25)+
    geom_smooth(se=FALSE)+
    ylab("Maximum greatest 5-days precipitation") + xlab("Year") + ggtitle("Annual greatest 5-day precipitation maximum") +
    labs(colour = "")
  p
}
output$rain5days <-renderPlot({ 
  rain5days()
})
output$rain5days_plot_info <- renderText({
  xy_str <- function(e) {
    if(is.null(e)) return("\n")
    paste("Year =", round(e$x, 0))#, " y=", round(e$y, 1), "\n")
  }
  paste0(xy_str(input$rain5days_plot_hover))
  #paste0("Valor=", input$plot_click$x, "\ny=", input$plot_click$y)
})
output$rain5days_table <-renderDataTable({
  rain5
  #DT::datatable(maxmaxano[, input$mostrar, drop = FALSE])
})


output$estatisticasano <- downloadHandler(
  filename =  function() {
    #paste("Plot", input$var8, sep=".")
    paste("Plots per year", input$par,".pdf")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    pdf(file) # open the pdf device
    print(avg_year())
    print(max_year())
    print(min_year())
    print(year_box())
    
    if(input$par == 'tmax'){
      print(maior25_plot())
      print(maior35_plot())
      print(extreme_max_plot())
    }
    else if(input$par == 'tmin'){
      print(min0_plot())
      print(min20_plot())
      print(extreme_min_plot())
    }
    else if(input$par == 'prec'){
      print(pre10_plot())
      print(pre20_plot())
      print(pre25_plot())
      print(pre_ano())
      print(hidrologico())
      print(wet_day_ano())
      print(dry_day_ano())
      print(rain5days())
    }
    # draw the plot
    dev.off()  # turn the device off
  })


#### Month ----
output$temp.prec <-renderPlotly({
  dataset <- datasetInput2()
  dsrange <- dataset
  #dsrange$month  <- months(as.Date(dsrange$data), abbreviate=TRUE)
  dsrange$month <- month(dsrange$data)
  #dsrange$month <- as.numeric(format(as.Date(dsrange$data),"%m"))
  dsrange<- subset(dsrange, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  tempprec <- dsrange %>%
    group_by(month) %>%
    summarize(round(mean(precipitacao, na.rm=TRUE),2),
              round(mean(temp.maxima, na.rm=TRUE),2),
              round(mean(temp.minima, na.rm=TRUE),2)) %>%
    setNames(c("Month", "Precipitation","Max.Temperature","Min.Temperature")) %>% 
    ungroup() 
  p <- ggplot(tempprec, aes(x=Month)) +
    geom_line(aes(y=Max.Temperature),color="red")+
    geom_line(aes(y=Min.Temperature),color="blue")+
    geom_col(aes(y=Precipitation),position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    #geom_text(aes(label=Precipitation), position=position_dodge(width=0.9), vjust=-0.25)+
    ylab("") + xlab("Month") + ggtitle("Maximum and minimum air temperatures and Precipitation") +
    #scale_y_continuous(sec.axis = sec_axis(~./max(tempprec$Precipitation)))
    labs(colour = "") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1,size = 10),
          axis.title = element_text(color="black", size=13, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
  p
  ggplotly(p, tooltip = c("all"),dynamicTicks = TRUE)
})

output$temp.prec2 <-renderPlotly({
  dataset <- datasetInput2()
  dsrange <- dataset
  #dsrange$month  <- months(as.Date(dsrange$data), abbreviate=TRUE)
  dsrange$year <- year(dsrange$data)
  #dsrange$month <- as.numeric(format(as.Date(dsrange$data),"%m"))
  dsrange<- subset(dsrange, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  tempprec <- dsrange %>%
    group_by(year) %>%
    summarize(round(mean(precipitacao, na.rm=TRUE),2),
              round(mean(temp.maxima, na.rm=TRUE),2),
              round(mean(temp.minima, na.rm=TRUE),2)) %>%
    setNames(c("Year", "Precipitation","Max.Temperature","Min.Temperature")) %>% 
    ungroup() 
  p <- ggplot(tempprec, aes(x=Year)) +
    geom_line(aes(y=Max.Temperature),color="red")+
    geom_line(aes(y=Min.Temperature),color="blue")+
    geom_col(aes(y=Precipitation),position="stack",stat = "identity",show.legend = TRUE,color="black",fill="#D8AE5A") + #position="dodge"
    #geom_text(aes(label=Precipitation), position=position_dodge(width=0.9), vjust=-0.25)+
    ylab("") + xlab("Year") + ggtitle("Maximum and minimum air temperatures and Precipitation") +
    #scale_y_continuous(sec.axis = sec_axis(~./max(tempprec$Precipitation)))
    labs(colour = "") +
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1,size = 10),
          axis.title = element_text(color="black", size=13, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
  p
  ggplotly(p, tooltip = c("all"),dynamicTicks = TRUE)
})

output$maximames <-renderPlotly({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  
  dsrange$MonthN <- as.numeric(format(as.Date(dsrange$data),"%m")) # Month's number
  dsrange$Year <- as.numeric(format(as.Date(dsrange$data),"%Y"))
  dsrange$Month  <- months(as.Date(dsrange$data), abbreviate=TRUE) # Month's abbr.
  
  dsrange$par <- switch(input$par,
                        tmax = dsrange$temp.maxima,
                        tmin = dsrange$temp.minima,
                        tmed = dsrange$temp.media,
                        prec = dsrange$precipitacao,
                        "pressure9h" = dsrange$pressao9h,
                        "pressure12h" = dsrange$pressao12h,
                        "pressure15h" = dsrange$pressao15h,
                        "pressureAve" = dsrange$pressao.media,
                        "shadeAirTemperature9h" = dsrange$temp.9h.som,
                        "shadeAirTemperature12h" = dsrange$temp.12h.som,
                        "shadeAirTemperature15h" = dsrange$temp.15h.som,
                        "exposureAirTemperature9h" = dsrange$temp.9h.exp,
                        "exposureAirTemperature12h" = dsrange$temp.12h.exp,
                        "exposureAirTemperature15h" = dsrange$temp.15h.exp,
                        "vaporPressure9h" = dsrange$tensao9h,
                        "vaporPressure12h" = dsrange$tensao12h,
                        "vaporPressure15h" = dsrange$tensao15h,
                        "humidity9h"= dsrange$humidade9h,
                        "humidity12h"= dsrange$humidade12h,
                        "humidity15h"= dsrange$humidade15h,
                        "ozone" = dsrange$ozono,
                        "absoluteWindSpeed"= dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  str(dsrange)
  Ref_Data2 <- dsrange %>%
    group_by(MonthN, Year, Month) %>%
    summarize(Value = round(mean(par,na.rm = TRUE),1)) %>%
    ungroup() %>%
    # Convert the Month column to factor variable with levels from Jan to Dec
    # Convert the YearN column to factor
    mutate(Month = factor(Month, levels = unique(Month)),
           Year = as.factor(Year))
  #print(Ref_Data2)
  g <- ggplot(data = Ref_Data2,
              aes(x = Month , y = Value, group = Year, colour = Year)) +
    geom_line()+
    labs(title=(paste("Monthly average",names(parChoices4[parChoices4==input$par]),"by years")),y=names(xLabs[xLabs==input$par]))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1),
          axis.title = element_text(color="black", face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
  ggplotly(g, tooltip = c("colour","y","x"),dynamicTicks = TRUE)
})

output$maximames2 <-renderPlotly({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  
  dsrange$MonthN <- as.numeric(format(as.Date(dsrange$data),"%m")) # Month's number
  dsrange$Year <- as.numeric(format(as.Date(dsrange$data),"%Y"))
  dsrange$Month  <- months(as.Date(dsrange$data), abbreviate=TRUE) # Month's abbr.
  
  dsrange$par <- switch(input$par,
                        tmax = dsrange$temp.maxima,
                        tmin = dsrange$temp.minima,
                        tmed = dsrange$temp.media,
                        prec = dsrange$precipitacao,
                        "pressure9h" = dsrange$pressao9h,
                        "pressure12h" = dsrange$pressao12h,
                        "pressure15h" = dsrange$pressao15h,
                        "pressureAve" = dsrange$pressao.media,
                        "shadeAirTemperature9h" = dsrange$temp.9h.som,
                        "shadeAirTemperature12h" = dsrange$temp.12h.som,
                        "shadeAirTemperature15h" = dsrange$temp.15h.som,
                        "exposureAirTemperature9h" = dsrange$temp.9h.exp,
                        "exposureAirTemperature12h" = dsrange$temp.12h.exp,
                        "exposureAirTemperature15h" = dsrange$temp.15h.exp,
                        "vaporPressure9h" = dsrange$tensao9h,
                        "vaporPressure12h" = dsrange$tensao12h,
                        "vaporPressure15h" = dsrange$tensao15h,
                        "humidity9h"= dsrange$humidade9h,
                        "humidity12h"= dsrange$humidade12h,
                        "humidity15h"= dsrange$humidade15h,
                        "ozone" = dsrange$ozono,
                        "absoluteWindSpeed"= dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  Ref_Data2 <- dsrange %>%
    group_by(MonthN, Year, Month) %>%
    summarize(Value = round(mean(par,na.rm = TRUE),1)) %>%
    ungroup() %>%
    # Convert the Month column to factor variable with levels from Jan to Dec
    # Convert the YearN column to factor
    mutate(Month = factor(Month, levels = unique(Month)),
           Year = as.factor(Year))
  
  g <- ggplot(data = Ref_Data2, aes(x = Year , y = Value, group = Month, colour = Month)) +
    geom_line()+
    labs(title=(paste("Annual average",names(parChoices4[parChoices4==input$par]),"by months")),y=names(xLabs[xLabs==input$par]))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1),
          axis.title = element_text(color="black", face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1),
  #       plot.title = element_text(hjust = 0.5,color="#D8AE5A", size=14, face="bold"))
  ggplotly(g, tooltip = c("colour","y","x"),dynamicTicks = TRUE)
  
})

output$maximames3 <-renderPlotly({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  #dsrange <- mutate(data = ymd(data))
  dsrange$par <- switch(input$par,
                        tmax = dsrange$temp.maxima,
                        tmin = dsrange$temp.minima,
                        tmed = dsrange$temp.media,
                        prec = dsrange$precipitacao,
                        "pressure9h" = dsrange$pressao9h,
                        "pressure12h" = dsrange$pressao12h,
                        "pressure15h" = dsrange$pressao15h,
                        "pressureAve" = dsrange$pressao.media,
                        "shadeAirTemperature9h" = dsrange$temp.9h.som,
                        "shadeAirTemperature12h" = dsrange$temp.12h.som,
                        "shadeAirTemperature15h" = dsrange$temp.15h.som,
                        "exposureAirTemperature9h" = dsrange$temp.9h.exp,
                        "exposureAirTemperature12h" = dsrange$temp.12h.exp,
                        "exposureAirTemperature15h" = dsrange$temp.15h.exp,
                        "vaporPressure9h" = dsrange$tensao9h,
                        "vaporPressure12h" = dsrange$tensao12h,
                        "vaporPressure15h" = dsrange$tensao15h,
                        "humidity9h"= dsrange$humidade9h,
                        "humidity12h"= dsrange$humidade12h,
                        "humidity15h"= dsrange$humidade15h,
                        "ozone" = dsrange$ozono,
                        "absoluteWindSpeed"= dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  p <- dsrange %>%
    mutate(
      Year = factor(year(data)),     # use year to define separate curves
      Month = update(data, year = 1)  # use a constant year for the x-axis
    ) %>%
    ggplot(aes(Month, par, color = Year)) 
  #labs(fill = "Dose (mg)")+
  #scale_x_date(date_breaks = "1 month", date_labels = "%b")
  
  # Raw daily data
  p +geom_line(aes(group = Year), color = "black", alpha = 0.1)+
    geom_smooth(se = F)+
    labs(title="Maximum monthly temperature per year (Trend)",y="Temperature")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45,hjust = 1, vjust=1),
          axis.title = element_text(color="black", face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=17, face="bold"))
  ggplotly(p, tooltip = c("color"),dynamicTicks = TRUE)
})



avg_month <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  #dsrange$month <- gsub("0*", " ", format(dsrange$data,"%m"))
  dsrange$month <- format(dsrange$data, "%m")
  #dsrange$month <- month(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  str(dsrange$month)
  par <- switch(input$par,
                tmax = na.approx(dsrange$temp.maxima),
                tmin = na.approx(dsrange$temp.minima),
                tmed = na.approx(dsrange$temp.media),
                prec = na.approx(dsrange$precipitacao),
                "pressure9h" = na.approx(dsrange$pressao9h),
                "pressure12h" = na.approx(dsrange$pressao12h),
                "pressure15h" = na.approx(dsrange$pressao15h),
                "pressureAve" = na.approx(dsrange$pressao.media),
                "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                "vaporPressure9h" = na.approx(dsrange$tensao9h),
                "vaporPressure12h" = na.approx(dsrange$tensao12h),
                "vaporPressure15h" = na.approx(dsrange$tensao15h),
                "humidity9h"= na.approx(dsrange$humidade9h),
                "humidity12h"= na.approx(dsrange$humidade12h),
                "humidity15h"= na.approx(dsrange$humidade15h),
                "ozone" = na.approx(dsrange$ozono),
                "absoluteWindSpeed"=dsrange$velocidade.absoluta,
                "hourlyWindSpeed"=dsrange$velocidade.horaria
  )
  
  ggplot(dsrange, aes(x=month, y= par)) + 
    stat_summary(fun.y="mean", geom="bar", color="black",fill="#D8AE5A")+
    stat_summary(aes(label=round(..y..,1)), fun.y=mean, geom="text", size=8,vjust = 1.3)+
    geom_smooth(se=FALSE)+
    labs(y=(names(xLabs[xLabs==input$par])), x=("Month"), title=(paste("Monthly average",names(parChoices4[parChoices4==input$par]))),colour = "red") +
    theme_minimal()+
    theme(axis.text.x = element_text(vjust=4, size = 20),
          axis.title = element_text(color="black", size=20, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=23, face="bold"))
}

output$avg_month_plot <-renderPlot({
  avg_month()
})


total_month <- function(){
  dataset <- datasetInput2()
  dsrange <- dataset
  #dsrange$month <- gsub("0*", " ", format(dsrange$data,"%m"))
  dsrange$month <- format(dsrange$data, "%m")
  #dsrange$month <- month(dsrange$data)
  dsrange<- subset(dsrange, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  
  ggplot(dsrange, aes(x=month, y= precipitacao)) + 
    stat_summary(fun.y="sum", geom="bar", color="black",fill="#D8AE5A")+
    stat_summary(aes(label=round(..y..,0)), fun.y=sum, geom="text", size=8,vjust = 1.3)+
    geom_smooth(se=FALSE)+
    labs(y=("Total precipitation (mm)"), x=("Month"), title=("Total precipitation by month"),colour = "red") +
    theme_minimal()+
    theme(axis.text.x = element_text(vjust=4, size = 20),
          axis.title = element_text(color="black", size=20, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=23, face="bold"))
}

output$total_month_plot <-renderPlot({
  total_month()
})


#### Seasons ####
output$seasons <- renderPlotly({
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  dsrange$date <- dsrange$data
  
  mydata <- openair::cutData(fortify(dsrange), type = "season")
  mydata$Year <- as.numeric(format(as.Date(mydata$data),"%Y"))
  #ggplot(mydata, mapping=aes(x=date, y = mydata[,input$ycol1],group = season))+geom_boxplot()
  
  mydata$par <- switch(input$par,
                       tmax = mydata$temp.maxima,
                       tmin = mydata$temp.minima,
                       tmed = mydata$temp.media,
                       prec = mydata$precipitacao,
                       "pressure9h" = mydata$pressao9h,
                       "pressure12h" = mydata$pressao12h,
                       "pressure15h" = mydata$pressao15h,
                       "pressureAve" = mydata$pressao.media,
                       "shadeAirTemperature9h" = mydata$temp.9h.som,
                       "shadeAirTemperature12h" = mydata$temp.12h.som,
                       "shadeAirTemperature15h" = mydata$temp.15h.som,
                       "exposureAirTemperature9h" = mydata$temp.9h.exp,
                       "exposureAirTemperature12h" = mydata$temp.12h.exp,
                       "exposureAirTemperature15h" = mydata$temp.15h.exp,
                       "vaporPressure9h" = mydata$tensao9h,
                       "vaporPressure12h" = mydata$tensao12h,
                       "vaporPressure15h" = mydata$tensao15h,
                       "humidity9h"= mydata$humidade9h,
                       "humidity12h"= mydata$humidade12h,
                       "humidity15h"= mydata$humidade15h,
                       "ozone" = mydata$ozono,
                       "absoluteWindSpeed"= mydata$velocidade.absoluta,
                       "hourlyWindSpeed"= mydata$velocidade.horaria)
  
  Ref_Data2 <- mydata %>%
    group_by(Year,season) %>%
    summarise(Average = round(mean(par,na.rm = TRUE),1)) %>% #summarize
    ungroup() %>%
    mutate(Year = as.factor(Year))
  
  g <- ggplot(data = Ref_Data2, aes(x = Year , y = Average, group = season, colour = season)) +
    geom_line()+
    labs(title=paste("Annual average",names(parChoices4[parChoices4==input$par]), "by season"),y=(names(xLabs[xLabs==input$par])))+
    theme_minimal()+
    theme(axis.text.x = element_text(angle=45, hjust = 1, vjust=1),
          axis.title = element_text(color="black", face="bold"),
          plot.title = element_text(hjust = 0.5, color="black", size=17, face="bold"),
          legend.key.width=unit(1,"cm"))
  # theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust=1),
  ggplotly(g, tooltip = c("x","y"),dynamicTicks = TRUE)
})

avg_season <- function(){
  dataset <- datasetInput2()
  dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  
  par <- switch(input$par,
                tmax = na.approx(dsrange$temp.maxima),
                tmin = na.approx(dsrange$temp.minima),
                tmed = na.approx(dsrange$temp.media),
                prec = na.approx(dsrange$precipitacao),
                "pressure9h" = na.approx(dsrange$pressao9h),
                "pressure12h" = na.approx(dsrange$pressao12h),
                "pressure15h" = na.approx(dsrange$pressao15h),
                "pressureAve" = na.approx(dsrange$pressao.media),
                "shadeAirTemperature9h" = na.approx(dsrange$temp.9h.som),
                "shadeAirTemperature12h" = na.approx(dsrange$temp.12h.som),
                "shadeAirTemperature15h" = na.approx(dsrange$temp.15h.som),
                "exposureAirTemperature9h" = na.approx(dsrange$temp.9h.exp),
                "exposureAirTemperature12h" = na.approx(dsrange$temp.12h.exp),
                "exposureAirTemperature15h" = na.approx(dsrange$temp.15h.exp),
                "vaporPressure9h" = na.approx(dsrange$tensao9h),
                "vaporPressure12h" = na.approx(dsrange$tensao12h),
                "vaporPressure15h" = na.approx(dsrange$tensao15h),
                "humidity9h"= na.approx(dsrange$humidade9h),
                "humidity12h"= na.approx(dsrange$humidade12h),
                "humidity15h"= na.approx(dsrange$humidade15h),
                "ozone" = na.approx(dsrange$ozono),
                "absoluteWindSpeed"=dsrange$velocidade.absoluta,
                "hourlyWindSpeed"=dsrange$velocidade.horaria
  )
  
  getSeason <- function(DATES) {
    WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
    SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
    SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
    FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
    # Convert dates from any year to 2012 dates
    d <- as.Date(strftime(DATES, format="2012-%m-%d"))
    ifelse (d >= WS | d < SE, "Winter",
            ifelse (d >= SE & d < SS, "Spring",
                    ifelse (d >= SS & d < FE, "Summer", "Fall")))
  }

  dsrange$season <- getSeason(dsrange$data)
  str(dsrange)
  
  ggplot(dsrange, aes(x=season, y= par)) +
    stat_summary(fun.y="mean", geom="bar", color="black",fill="#D8AE5A")+
    stat_summary(aes(label=round(..y..,1)), fun.y=mean, geom="text", size=8,vjust = 1.3)+
    geom_smooth(se=FALSE)+
    labs(y=(names(xLabs[xLabs==input$par])), x=("Season"), title=(paste("Seasonal average",names(parChoices4[parChoices4==input$par]))),colour = "red") +
    theme_minimal()+
    scale_x_discrete(limits=c("Spring", "Summer", "Fall", "Winter"))+
    theme(axis.text.x = element_text(vjust=4, size = 20),
          axis.title = element_text(color="black", size=20, face="bold"),
          plot.title = element_text(hjust = 0.5,color="black", size=23, face="bold"))
}

output$avg_season_plot <-renderPlot({
  avg_season()
})


#### Decomposition ----
output$decomposed <-renderPlot({
  dataset <- datasetInput2()
  if(input$par == "absoluteWindSpeed" || input$par == "hourlyWindSpeed")
    dsrange<- subset(dataset, data >= as.Date(input$dat_wind[1]) & data <= as.Date(input$dat_wind[2]))
  else
    dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  #dsrange<- subset(dataset, data >= as.Date(input$dat[1]) & data <= as.Date(input$dat[2]))
  dsrange$ano <- as.numeric(format(as.Date(dsrange$data),"%Y"))
  par2 <- parameterInput2()
  z <- xts(par2,dsrange$data,frequency = 365)
  z1 <- na.locf(z)
  str(dsrange$data)
  str(input$dat[1])
  print(as.Date(input$dat[1],format = "%y-%m-%d"))
  z2 <- ts(as.numeric(z1),frequency = 365, start = c(dsrange[1,"ano"]))
  print(z2)
  plot(stl(z2,s.window = "periodic"), #labels=c("g","cs","fe","h"), 
       main=paste(names(xLabs[xLabs==input$par]),"decomposition"),cex.main=5, col = "#D8AE5A")
})



#### Data table ####

output$tabela <- DT::renderDataTable(server = FALSE,{#renderDT
  dataset <- datasetInput2()
  dataset$par2 <- parameterInput()
  j <- dataset %>%
    select(data,par2) %>%
    filter(data >= input$dat[1] & data <= input$dat[2]) %>%
    mutate(data=as.Date(data))

  j[,-1] <-round(j[,-1],2) 
  DT::datatable(j,
                rownames = FALSE, extensions = c('Buttons','Scroller','FixedColumns'),
                colnames = c('Date',gsub(" ", "", names(parChoices4[parChoices4==input$par]), fixed = TRUE)),
                filter = 'top',
                options = list(
                  autoWidth=TRUE,
                  columnDefs = list(list(className = 'dt-center', targets = '_all')),
                  #columnDefs=list(list(targets='_all',visible=FALSE, targets=0,visible=TRUE)),
                  lengthChange = TRUE,autoWidth = TRUE,scroller = TRUE,
                  sPaginationType= "full_numbers",iDisplayLength=50,sScrollY= "400px",
                  #dom='Brtip',#
                  sDom='<"top">Brt<"bottom">p',scrollX = TRUE,
                  #columnFilter = I('[{columnDefs: ["targets": [0,1], type: "checkbox"]}]'),
                  buttons = list(
                    #list(extend = 'colvis', text='Show/Hide Columns'),# collectionLayout='fixed one-column'
                    list(extend='csv',exportOptions = list(columns = ':visible'),
                         filename = 'Tabela de Dados'),
                    list(extend='excel',exportOptions = list(columns = ':visible'),
                         filename = 'Tabela de Dados'),
                    list(extend='pdf',orientation = 'landscape',exportOptions=list(columns = ':visible'),
                         filename = 'Tabela de Dados'),
                    list(extend='copy',exportOptions = list(columns = ':visible'),
                         filename = 'Tabela de Dados'),
                    list(extend='print', bShowAll= FALSE,exportOptions = list(columns = ':visible'),
                         filename = 'Tabela de Dados')
                  )
                )
  )
})