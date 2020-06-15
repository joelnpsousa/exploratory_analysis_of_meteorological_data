#### Univariate analysis ----
tabPanel("Univariate analysis", value = "tab2_val", 
         sidebarLayout(
           sidebarPanel(width=3,#tags$style(".well {background-color:#f4f4f4;}"),#style = "position:fixed ; width:inherit;"
                        div(selectInput(inputId = "dataset2",
                                        label = "Choose dataset",
                                        choices = c("Changed","Original")),
                            br(),
                            selectInput("par", "Choose parameter to view",choices=parChoices4),
                            tags$head(tags$style(HTML(".selectize-dropdown-content {max-height: 360px;}"))),align="center"),
                        #hr(),
                        br(),
                        conditionalPanel(
                          condition = "input.tabselected==1 || input.tabselected==2 || input.tabselected==4 ||
                                        input.tabselected==5||input.tabselected==6",
                          conditionalPanel(
                            condition = "input.par == 'absoluteWindSpeed' || input.par =='hourlyWindSpeed'",
                            dateRangeInput("dat_wind", "Choose time period to view", 
                                           start     = min.wind, end = fim,
                                           min       = min.wind, max = maximo,
                                           startview = "decade", 
                                           separator = " - "),align="center"),
                          conditionalPanel(
                            condition = "input.par=='tmin'||input.par =='tmax'||input.par=='tmed'||input.par=='prec'||
                                           input.par=='shadeAirTemperature9h'||input.par=='shadeAirTemperature12h'||input.par=='shadeAirTemperature15h'||
                            input.par=='exposureAirTemperature9h'||input.par=='exposureAirTemperature12h'||input.par=='exposureAirTemperature15h'||input.par=='pressure9h'||input.par=='pressure12h'||
                            input.par=='pressure15h'||input.par=='pressureAve'||input.par=='vaporPressure9h'||input.par=='vaporPressure12h'||input.par=='vaporPressure15h'||
                            input.par=='humidity9h'||input.par=='humidity12h'||input.par=='humidity15h'||input.par=='ozone'"
                            ,
                            dateRangeInput("dat", "Choose time period to view", 
                                           start = origem, end = fim,
                                           min = minimo, max = maximo,
                                           startview = "decade", 
                                           separator = " - ")),
                          div(actionButton("reset_input", "Reset date",style='padding:4px; font-size:70%'),align="center"),align="center"),
                        
                        conditionalPanel(condition="input.tabselected==2",
                                         hr(),
                                         div(helpText("Choose a value to serve as reference value in the statistics and in the graph."),align="center"),
                                         div(numericInput("temp_in", "Reference value:", 20, min = -10, max = 200,width='150px'),align="center"),
                                         #checkboxInput("quergrafico", tags$b("View graph"), TRUE),
                                         checkboxInput("querlinha", tags$b("View horizontal line with reference value in the chart"), FALSE),
                                         checkboxInput("quersmooth", tags$b("View trend of the chart"), TRUE),
                                         hr(),
                                         div(helpText("If you want to save the chart, choose the file type and click the download button."),align="center"),
                                         div(radioButtons(inputId = "type", label = "Select the file type", choices = list("png", "pdf")),align="center"),
                                         div(downloadButton(outputId = "estatisticas", label = "Download the plot",style='padding:4px; font-size:70%'),align="center")
                        ),
                        conditionalPanel(condition="input.tabselected==3 ",
                                         conditionalPanel(
                                           condition = "input.par == 'absoluteWindSpeed' || input.par =='hourlyWindSpeed'",
                                           sliderInput("date.wind", "Years to view:", min=1865, max=maximo_ano,
                                                       value=c(minimo_ano,maximo_ano), dragRange = TRUE, sep=""),align="center"),
                                         conditionalPanel(
                                           condition = "input.par=='tmin'||input.par =='tmax'||input.par=='tmed'||input.par=='prec'||
                                           input.par=='shadeAirTemperature9h'||input.par=='shadeAirTemperature12h'||input.par=='shadeAirTemperature15h'||
input.par=='exposureAirTemperature9h'||input.par=='exposureAirTemperature12h'||input.par=='exposureAirTemperature15h'||input.par=='pressure9h'||input.par=='pressure12h'||
                                           input.par=='pressure15h'||input.par=='pressureAve'||input.par=='vaporPressure9h'||input.par=='vaporPressure12h'||input.par=='vaporPressure15h'||
                                           input.par=='humidity9h'||input.par=='humidity12h'||input.par=='humidity15h'||input.par=='ozone'"
                                           ,
                                           sliderInput("date", "Years to view:", min=minimo_ano, max=maximo_ano,
                                                       value=c(minimo_ano,maximo_ano), dragRange = TRUE, sep=""),align="center"),
                                         br(),
                                         downloadButton(outputId = "estatisticasano", label = "Download plots",style='padding:4px; font-size:70%'),align="center")
                        
           ),
           mainPanel(width=9,
                     tabsetPanel(#id='seccao',
                       type = "tabs",
                       tabPanel("Chart", value=1,
                                wellPanel(div(strong("Since: "), textOutput("from", inline = TRUE),"\t",
                                              strong("To: "), textOutput("to", inline = TRUE)),align = "center"),
                                dygraphOutput("dygraph")%>% withSpinner(),
                                htmlOutput("help"),align="center"),
                       
                       tabPanel("Statistics", value=2,
                                br(),
                                plotOutput("statistics_plot")%>% withSpinner(),
                                tableOutput("statistics_table"),align="center",
                                
                                conditionalPanel(
                                  condition = "input.par == 'tmax'",
                                  tableOutput("statistics_max"),align="center"
                                  
                                ),
                                conditionalPanel(
                                  condition = "input.par == 'tmin'",
                                  tableOutput("statistics_min"),align="center"
                                ),
                                conditionalPanel(
                                  condition = "input.par == 'prec'",
                                  tableOutput("statistics_prec"),align="center"
                                )
                       ),
                       
                       #### Stats per year -----------
                       tabPanel("Statistics per year", value=3,
                                plotOutput("avg_year_plot", hover = "avg_year_hover")
                                %>% withSpinner(),
                                verbatimTextOutput("avg_year_info"), 
                                
                                plotOutput("max_year_plot", hover = "max_year_hover")%>% withSpinner(),#click="plot_click",
                                verbatimTextOutput("max_year_info"), 
                                
                                plotOutput("min_year_plot", hover = "min_year_hover")%>% withSpinner(),
                                
                                verbatimTextOutput("min_year_info"),
                                
                                plotOutput("year_box_plot", hover = "year_box_hover")%>% withSpinner(),
                                verbatimTextOutput("year_box_info"),
                                
                                conditionalPanel(
                                  condition = "input.par == 'tmin'",
                                  plotOutput("min0_plot", hover = "min0_hover")%>% withSpinner(),
                                  verbatimTextOutput("min0_plot_info"),
                                  
                                  plotOutput("min20_plot", hover = "min20_hover")%>% withSpinner(),
                                  verbatimTextOutput("min20_plot_info"),
                                  
                                  plotOutput("extreme_min_plot", hover = "extreme_min_hover")%>% withSpinner(),
                                  verbatimTextOutput("extreme_min_plot_info"),
                                  
                                  plotOutput("extreme2_min_plot", hover = "extreme2_min_hover")%>% withSpinner(),
                                  verbatimTextOutput("extreme2_min_plot_info")
                                ),
                                conditionalPanel(
                                  condition = "input.par == 'tmax'",
                                  plotOutput("heatwaves_plot", hover = "heatwaves_hover")%>% withSpinner(),
                                  verbatimTextOutput("heatwaves_plot_info"),
                                  plotOutput("maior25_plot", hover = "maior25_hover")%>% withSpinner(),
                                  verbatimTextOutput("maior25_plot_info"),
                                  
                                  plotOutput("maior35_plot", hover = "maior35_hover")%>% withSpinner(),
                                  verbatimTextOutput("maior35_plot_info"),
                                  
                                  plotOutput("extreme_max_plot", hover = "extreme_max_hover")%>% withSpinner(),
                                  verbatimTextOutput("extreme_max_plot_info")
                                ),
                                conditionalPanel(
                                  condition = "input.par == 'prec'",
                                  plotOutput("pre_ano",hover = "preano_hover")%>% withSpinner(),
                                  verbatimTextOutput("preano_plot_info"), 
                                  
                                  div(numericInput("hidro.year", "Month to start hydrological year:", 9, min = 1, max = 12,width='150px'),align="center"),
                                  plotOutput("hidrologico",hover = "hidrologico_hover")%>% withSpinner(),
                                  verbatimTextOutput("hidrologico_info"), 
                                  
                                  plotOutput("pre0_plot", hover = "pre0_hover")%>% withSpinner(),
                                  verbatimTextOutput("pre0_plot_info"), 
                                  
                                  plotOutput("pre10_plot", hover = "pre10_hover")%>% withSpinner(),
                                  verbatimTextOutput("pre10_plot_info"), 
                                  
                                  plotOutput("pre20_plot", hover = "pre20_hover")%>% withSpinner(),
                                  verbatimTextOutput("pre20_plot_info"), 
                                  
                                  plotOutput("pre25_plot", hover = "pre25_hover")%>% withSpinner(),
                                  verbatimTextOutput("pre25_plot_info"),
                                  
                                  plotOutput("dry_day_ano", hover = "dry_day_ano_plot_hover")%>% withSpinner(),
                                  verbatimTextOutput("dry_day_ano_plot_info"),
                                  
                                  plotOutput("wet_day_ano", hover = "wet_day_ano_plot_hover")%>% withSpinner(),
                                  verbatimTextOutput("wet_day_ano_plot_info"),
                                  
                                  plotOutput("rain5days", hover = "rain5days_plot_hover")%>% withSpinner(),
                                  verbatimTextOutput("rain5days_plot_info")
                                )
                       ),
                       #### Stats per month ------------
                       tabPanel("Statistics per month", value=4,
                                br(),
                                plotlyOutput("maximames2")%>% withSpinner(),
                                br(),
                                plotlyOutput("maximames")%>% withSpinner(),
                                plotOutput("avg_month_plot")%>% withSpinner(),
                                br(),
                                conditionalPanel(
                                  condition = "input.par == 'prec'",
                                  plotOutput("total_month_plot")%>% withSpinner()
                                )
                       ),
                       #### Stats per season ------------
                       tabPanel("Statistics per season", value=4,
                                plotlyOutput("seasons")%>% withSpinner(),
                                br(),br(),
                                plotOutput("avg_season_plot")%>% withSpinner()
                       ),
                       tabPanel("Decomposition", value=5,
                                wellPanel(plotOutput("decomposed")%>% withSpinner())),
                       tabPanel("Data Table", value=6, icon = icon("table"),
                                
                                DT::dataTableOutput("tabela")%>% withSpinner()),
                       id = "tabselected"
                     )
           )
         )
)