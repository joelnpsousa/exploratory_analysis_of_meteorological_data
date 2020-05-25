tabPanel("Records",value = "tab5_val",width = 12,icon = icon("table fa-2x"),  
         fluidPage(
           dateRangeInput("dr", 
                          "Choose time period to view", 
                          start     = origem,
                          end       = fim,
                          min       = minimo,
                          max       = maximo,
                          startview = "decade", 
                          separator = " - "),align="center",
           DT::dataTableOutput("dados") %>% withSpinner())
)