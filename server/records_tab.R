output$dados <- DT::renderDataTable(server=FALSE,{#renderDT
  dataset <- datasetInput2()
  j <- dataset %>%
    filter(data >= input$dr[1] & data <= input$dr[2]) %>%
    mutate(data=as.Date(data))
  
  j[,2:22] <-round(j[,2:22],2) 
  
  DT::datatable(j, rownames = FALSE, 
                extensions = c('Buttons','Scroller','ColReorder','FixedColumns','FixedHeader','KeyTable'),
                colnames = c('Date',
                             'AtmosphericPressure.9h','AtmosphericPressure.12h','AtmosphericPressure.15h','Atmospheric.pressure.average',
                             'Temperature.Exposure.9h','Temperature.Shade.9h','Temperature.Exposure.12h',
                             'Temperature.Shade.12h','Temperature.exposure.15h','Temperature.shade.15h',
                             'Maximum.temp.','Minimum.temp.','Average.temp.',
                             'VaporPressure.9h','VaporPressure.12h','VaporPressure.15h',
                             'Humidity.9h','Humidity.12h','Humidity.15h',
                             'Precipitation','Ozone','Wind.Direction.9h','Wind.Direction.12h','Wind.Direction.15h',
                             "Wind.Speed.Absolute", "Wind.Speed.Hourly"),
                filter = list(position = "top", clear = FALSE),
                options = list(lengthChange = TRUE,
                               autoWidth = TRUE,
                               sPaginationType= "full_numbers",
                               scroller = TRUE,
                               sScrollY= "400px",
                               fixedColumns = list(leftColumns = 1),
                               fixedHeader = TRUE,
                               keys = TRUE,
                               scrollX=TRUE, 
                               colReorder = list(realtime = FALSE),
                               sDom='<"top">Brt<"bottom">p',
                               sRowSelect='multi',
                               columnDefs = list(list(className = 'dt-center', targets = '_all')),
                               buttons = list(list(extend='csv',exportOptions = list(columns = ':visible'),
                                                   filename = 'Tabela de Dados'),
                                              list(extend='excel',exportOptions = list(columns = ':visible'),
                                                   filename = 'Tabela de Dados'),
                                              list(extend='pdf',orientation = 'landscape',exportOptions=list(columns = ':visible'),
                                                   filename = 'Tabela de Dados'),
                                              list(extend='copy',exportOptions = list(columns = ':visible'),
                                                   filename = 'Tabela de Dados'),
                                              list(extend='print', bShowAll= FALSE,exportOptions = list(columns = ':visible'),
                                                   filename = 'Tabela de Dados'),
                                              list(extend = 'colvis', text='Show/Hide Columns', collectionLayout='fixed four-column')
                               )
                )
  )
})