tabPanel("Multivariate Analysis", value = "tab3_val",icon=icon("bar-chart fa-2x"),
         sidebarLayout(
           sidebarPanel(width = 3,
                        div(selectInput(inputId = "dataset",
                                        label = "Choose dataset:",
                                        choices = c("Changed", "Original")),
                            br(),
                            selectInput('xcol', 'Parameter x', choices = parChoices4,selected = parChoices4[1]),
                            selectInput('ycol', 'Parameter y', choices = parChoices4,selected = parChoices4[2]),
                            br(),
                            dateRangeInput("daterangein", 
                                           "Choose time period to view", 
                                           start     = origem,
                                           end       = fim,
                                           min       = minimo,
                                           max       = maximo,
                                           startview = "decade", 
                                           separator = " - "),
                            actionButton("reset_input2", "Reset choices",style='padding:4px; font-size:80%'),
                            br(),br(),
                            radioButtons(inputId = "var6", label = "Select the regression line", choices = list("Local regression fitting", "Linear regression line")),
                            radioButtons(inputId = "var5", label = "Select the file type", choices = list("png", "pdf")),
                            downloadButton(outputId = "correlations", label = "Download the plot",style='padding:4px; font-size:80%'),align="center")
           ),
           mainPanel(width = 9,
                     wellPanel(strong("Since: "),textOutput("from2", inline = TRUE),
                               strong("To: "), textOutput("to2", inline = TRUE),align = "center"),
                     conditionalPanel(condition="input.var6=='Local regression fitting'",
                                      plotOutput('correlacoes') %>% withSpinner()),
                     conditionalPanel(condition="input.var6=='Linear regression line'",
                                      plotOutput('correlacoes2') %>% withSpinner())
                     #plotOutput("decomposed2")%>% withSpinner()
           )
         )
)