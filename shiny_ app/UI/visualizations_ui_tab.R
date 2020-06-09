tabPanel("Visualizations",value = "tab4_val", icon=icon("bar-chart fa-2x"),
         sidebarLayout(
           sidebarPanel(width = 3,
                        div(wellPanel(selectInput(inputId = "dataset3",
                                                  label = "Choose dataset",
                                                  choices = c("Changed","Original")),align="center"),
                            div(selectInput('ycol1', 'Choose one or more parameters', choices=parChoices, selected=parChoices[1], multiple = TRUE),align="center"),
                            br(),
                            div(dateRangeInput("daterangeinput", 
                                               "Choose time period to view", 
                                               start     = origem,
                                               end       = fim,
                                               min       = minimo,
                                               max       = maximo,
                                               startview = "decade", 
                                               separator = " - "),align="center"),
                            
                            div(actionButton("reset_input3", "Reset choices",style='padding:4px; font-size:70%'),align="center"),align="center",
                            br(),
                            tags$head(
                              tags$script(src = "dygraph-extra.js"))
                        )
           ),
           mainPanel(width = 9,                     
                     wellPanel(strong("Since: "),textOutput("from4", inline = TRUE),
                               strong("To: "), textOutput("to4", inline = TRUE),align = "center"),
                     dygraphOutput("visualizations")%>% withSpinner(),
                     htmlOutput("help2"),align="center",
                     conditionalPanel(
                       condition = "input.ycol1=='Minimum.Air.Temperature'||input.ycol1 =='Maximum.Air.Temperature'||input.ycol1 =='Precipitation'",
                     plotlyOutput("temp.prec")%>% withSpinner(),
                     plotlyOutput("temp.prec2")%>% withSpinner())
           )
         )
)