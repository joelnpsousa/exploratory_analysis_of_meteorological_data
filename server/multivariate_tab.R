datasetInput <- reactive({
  switch(input$dataset,
         "Original" = df,
         "Changed" = changedx)
})

observeEvent(input$reset_input2, {
  shinyjs::reset("daterangein") 
  shinyjs::reset("xcol")
  shinyjs::reset("ycol")
})


output$from2 <- renderText({
  (!is.null(input$daterangein))
  strftime(input$daterangein[1], "%d/%m/%Y")
})
output$to2 <- renderText({
  if (!is.null(input$daterangein))
    strftime(input$daterangein[2], "%d/%m/%Y")
})

datafiltrada <- reactive({
  dataset <- datasetInput()
  filter(dataset, between(data ,input$daterangein[1], input$daterangein[2]))
})

correlacoes <- function(){
  dataset <- datasetInput()
  dsrange<- subset(dataset, data >= as.Date(input$daterangein[1]) & data <= as.Date(input$daterangein[2]))
  dsrange$par <- switch(input$xcol,
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
                        "absoluteWindSpeed" = dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  dsrange$par2 <- switch(input$ycol,
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
                         "absoluteWindSpeed" = dsrange$velocidade.absoluta,
                         "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  # colnames(j) = c('Date',
  #                 'Atmospheric_Pressure_9h','Atmospheric_Pressure_12h','Atmospheric_Pressure_15h','Atmospheric_Pressure_Average',
  #                 'Air_Temperature_Exposure_9h','Air_Temperature_Shade_9h','Air_Temperature_Exposure_12h',
  #                 'Air_Temperature_Shade_12h','Air_Temperature_Exposure_15h','Air_Temperature_Shade_15h',
  #                 'Air_Temperature_Maximum','Air_Temperature_Minimum','Air_Temperature_Average',
  #                 'Vapor_Pressure_9h','Vapor_Pressure_12h','Vapor_Pressure_15h',
  #                 'Humidity_9h','Humidity_12h','Humidity_15h',
  #                 'Precipitation','Ozone','Wind_Direction_9h','Wind_Direction_12h','Wind_Direction_15h',
  #                 "Wind_Speed_Absolute", "Wind_Speed_Hourly")
  # library("GGally")
  # a<- ggcorr(j, nbreaks = 5, palette = "RdYlBu", label=TRUE, hjust = 1, layout.exp=6)
  # a
  # cor_5 <- rcorr(as.matrix(changed[,c(2:22,26,27)]))
  # M <- cor_5$r
  # p_mat <- cor_5$P
  # corrplot(M, type = "lower", order = "hclust", method = "number",
  #          p.mat = p_mat, insig = "blank")
  
  ggscatter(dsrange, x = "par", y= "par2", color="#D8AE5A", shape=21, size = 3,
            add = "loess",  add.params = list(color = "blue", fill = "gray"),
            conf.int = TRUE,mean.point=TRUE,rug=TRUE,
            cor.coef = TRUE, cor.method = "pearson",
            xlab = names(xLabs[xLabs==input$xcol]), 
            ylab =names(xLabs[xLabs==input$ycol]))
  
}
output$correlacoes <-renderPlot({     
  print(correlacoes())
})
correlacoes2 <- function(){
  dataset <- datasetInput()
  dsrange<- subset(dataset, data >= as.Date(input$daterangein[1]) & data <= as.Date(input$daterangein[2]))
  dsrange$par <- switch(input$xcol,
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
                        "absoluteWindSpeed" = dsrange$velocidade.absoluta,
                        "hourlyWindSpeed"= dsrange$velocidade.horaria)
  
  dsrange$par2 <- switch(input$ycol,
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
                         "absoluteWindSpeed" = dsrange$velocidade.absoluta,
                         "hourlyWindSpeed"= dsrange$velocidade.horaria)
  #filter(dataset, between(data ,input$daterangein[1], input$daterangein[2]))
  #library("GGally")
  #ggcorr(dsrange, geom = "text", nbreaks = 5, palette = "RdYlBu", hjust = 1)
  #ggcorr(o, label=TRUE, hjust = 0.95,layout.exp = 5)
  ggscatter(dsrange, x = "par", y= "par2", color="#D8AE5A", shape=21, size = 3,
            add = "reg.line",  add.params = list(color = "blue", fill = "gray"),
            conf.int = TRUE,mean.point=TRUE,rug=TRUE,
            cor.coef = TRUE, cor.method = "pearson",
            xlab = names(xLabs[xLabs==input$xcol]), 
            ylab =names(xLabs[xLabs==input$ycol]))
}
output$correlacoes2 <-renderPlot({     
  print(correlacoes2())
})
output$correlations <- downloadHandler(
  filename =  function() {
    paste("Plot", input$var5, sep=".")
  },
  # content is a function with argument file. content writes the plot to the device
  content = function(file) {
    if(input$var5 == "png")
      png(file) # open the png device
    else
      pdf(file) # open the pdf device
    if(input$var6 == "Local regression fitting"){
      print(correlacoes()) # draw the plot
      dev.off()  # turn the device off
    }
    else{
      print(correlacoes2()) # draw the plot
      dev.off()  # turn the device off
    }
  })

parameterInput22 <- reactive({
  dataset <- datasetInput()
  dsrange<- subset(dataset, data >= as.Date(input$daterangein[1]) & data <= as.Date(input$daterangein[2]))
  switch(input$xcol,
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

parameterInput33 <- reactive({
  dataset <- datasetInput()
  dsrange<- subset(dataset, data >= as.Date(input$daterangein[1]) & data <= as.Date(input$daterangein[2]))
  switch(input$ycol,
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

output$decomposed2 <-renderPlot({
  dataset <- datasetInput()
  dsrange<- subset(dataset, data >= as.Date(input$daterangein[1]) & data <= as.Date(input$daterangein[2]))
  dsrange$ano <- as.numeric(format(as.Date(dsrange$data),"%Y"))
  #axis.POSIXct(1, at=seq(daterange[1], daterange[2], by="month"), format="%b")
  par <- parameterInput22()
  par2 <- parameterInput33()
  z1 <- xts(par,dsrange$data,frequency = 365)
  z2 <- xts(par2,dsrange$data,frequency = 365)
  #z5 <- xts.obj[paste(as.Date(input$dat[1]),as.Date(input$dat[2]),sep="::")]
  z11 <- na.locf(z1)
  z22 <- na.locf(z2)
  #str(dsrange$data)
  #str(input$dat[1])
  #print(as.Date(input$daterangein[1],format = "%y-%m-%d"))
  #z2 <- ts(as.numeric(z1),frequency = 365, start = c(as.Date(input$dat[1],format = "%y-%m-%d")))
  z111 <- ts(as.numeric(z11),frequency = 365, start = c(dsrange[1,"ano"]))
  z222 <- ts(as.numeric(z22),frequency = 365, start = c(dsrange[1,"ano"]))
  #print(z111)
  #z2 <- filter(ds$data >= input$dat[1])
  a <- stl(z111,s.window = "periodic")
  b <- stl(z222,s.window = "periodic")
  print(length(a))
  print(length(b))
  ts.plot(z111,z222, 
       plot.type="single",#labels=c("g","cs","fe","h"), 
       main=paste(names(parChoices4[parChoices4==input$par]),"decomposition"))
  #range.bars=TRUE, col.range="red")
  #seasonplot(ts.sa, 12, col=rainbow(12), year.labels=TRUE, main="Seasonal plot") # seasonal frequency set as 12 for monthly data.
})
