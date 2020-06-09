datasetInput3 <- reactive({
  switch(input$dataset3,
         "Original" = df,
         "Changed" = changedx)
})

observeEvent(input$reset_input3, {
  shinyjs::reset("daterangeinput") 
  shinyjs::reset("ycol1")
})

output$from4 <- renderText({
  (!is.null(input$daterangeinput))
  strftime(input$daterangeinput[1], "%d/%m/%Y")
  
})
output$to4 <- renderText({
  if (!is.null(input$daterangeinput))
    strftime(input$daterangeinput[2], "%d/%m/%Y")
})

output$help2 <-renderUI({
  helpText("Click and drag to zoom in (double-click to zoom out).\n",
           "The number of the box in the lower left corner corresponds to the value used in the calculation of the moving average.")
})

output$visualizations <- renderDygraph({
  dataset <- datasetInput3()
  dsrange<- subset(dataset, data >= as.Date(input$daterangeinput[1]) & data <= as.Date(input$daterangeinput[2]))
  dsrange$date <- dsrange$data
  str(dsrange)
  colnames(dsrange) = c('data',
               'AtmosphericPressure.9h','AtmosphericPressure.12h','AtmosphericPressure.15h','AtmosphericPressure.Average',
               'Temperature.Exposure.9h','Temperature.Shade.9h','Temperature.Exposure.12h',
               'Temperature.Shade.12h','Temperature.Exposure.15h','Temperature.Shade.15h',
               'Maximum.Air.Temperature','Minimum.Air.Temperature','Average.Air.Temperature',
               'VaporPressure.9h','VaporPressure.12h','VaporPressure.15h',
               'Humidity.9h','Humidity.12h','Humidity.15h',
               'Precipitation','Ozone','Wind.Direction.9h','Wind.Direction.12h','Wind.Direction.15h',
               "Wind.Speed.Absolute", "Wind.Speed.Hourly", "date")
  str(dsrange)
  
  data <- gather(select(dsrange, "date", input$ycol1), variable, value, -date)
  z <- xts(dsrange[,input$ycol1], as.Date(dsrange$data, format = "%Y"))
  
  dygraph(z, ylab="") %>% 
    dyRangeSelector(dateWindow = input$daterangeinput) %>% 
    dyOptions(colors = RColorBrewer::brewer.pal(8, "Set2"),
              drawPoints = TRUE, pointSize = 1.2,
              retainDateWindow=TRUE,
              animatedZooms = TRUE
    ) %>% 
    dyLegend(show="onmouseover",labelsSeparateLines=TRUE) %>% 
    dyRoller(rollPeriod = 1) %>% 
    dyUnzoom()
})