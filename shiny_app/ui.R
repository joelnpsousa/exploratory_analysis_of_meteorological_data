shinyUI(
  fluidPage(theme = shinytheme("sandstone"),
            
            useShinyjs(),
            # inlineCSS(appCSS),
            # div(id = "loading-content",
            #     h2("Loading...")
            # ),
            # # verbatimTextOutput("dataInfo"),
            
            #hidden(
              # tags$head(tags$style(HTML("#app-content li a[data-value = 'tab2_val'],
              #                    #app-content li a[data-value = 'tab3_val'],
              #                    #app-content li a[data-value = 'tab4_val'],
              #                    #app-content li a[data-value = 'tab5_val']{
              #                    display: none;}"))),
              # div(id = "app-content",
                  navbarPage(theme = shinytheme("sandstone"),
                             "Meteorological Data Analysis",  
                             # tabPanel("home",value = "tab1_val",
                             #          div(id = "intro",
                             #              p("This application was made as part of a dissertation and aims to make an interactive exploratory analysis of the meteorological data recorded by the medical-surgical school of Porto between December 1860 and March 1898.")),
                             #          tags$style(type="text/css", "#intro {font-size:20px;   text-align: center;position:absolute;top: 15%;left: 23%;right: 23%;}"),
                             #          # div(id = "login",
                             #          #     wellPanel(textInput('username', 'Username'),
                             #          #               passwordInput("pass", 'Password'),
                             #          #               actionButton("enter", "enter"))),
                             #          tags$style(type="text/css", "#login {font-size:20px; text-align: center;position:absolute;top: 43%;left: 40%;}"),#margin-top: -100px;margin-left: -150px;
                             #          fluidRow(#column(width=4, offset = 4,
                             #            div(id= "login",
                             #                wellPanel(#id = "login",
                             #                  textInput(".username", "Username:"),
                             #                  passwordInput(".password", "Password:"),
                             #                  div(actionButton(".login", "Log in"), style="text-align: center;")),
                             #                textOutput("message")
                             #                
                             #            )
                             #          )),
                             source(file.path("UI", "univariate_ui_tab.R"), local = TRUE)$value,
                             source(file.path("UI", "multivariate_ui_tab.R"), local = TRUE)$value,
                             source(file.path("UI", "visualizations_ui_tab.R"), local = TRUE)$value,
                             source(file.path("UI", "records_ui_tab.R"), local = TRUE)$value
                 # ) #### end of navbarPage
             # )
            )
  ) #### end of fluidPage
) #### end of shinyUI

