#a$decade <- 10*as.integer(as.numeric(substring(as.character(a$data), 3, 4)) / 10)

shinyServer(function(input, output) {
  source(file.path("server", "univariate_tab.R"),  local = TRUE)$value
  source(file.path("server", "multivariate_tab.R"),  local = TRUE)$value
  source(file.path("server", "visualizations_tab.R"),  local = TRUE)$value
  source(file.path("server", "records_tab.R"),  local = TRUE)$value
  
  Sys.sleep(1)
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")   
  show("app-content")

  observeEvent(input$.login, {
    if (input$.username == 'admin'& input$.password == '123'){
      showElement(selector = '#app-content li a[data-value="tab2_val"]')
      show(selector = '#app-content li a[data-value="tab3_val"]')
      show(selector = '#app-content li a[data-value="tab4_val"]')
      show(selector = '#app-content li a[data-value="tab5_val"]')
      hide(selector = '#app-content li a[data-value="tab1_val"]')
    }
    else {
      show("message")
      output$message = renderText("Invalid user name or password.")
      delay(2000, hide("message", anim = TRUE, animType = "fade"))
    }
  })
  
  # Logged = FALSE
  # my_username <- "test"
  # my_password <- "test"
  # values <- reactiveValues(authenticated = FALSE)
  # dataModal <- function(failed = FALSE) {
  #   modalDialog(
  #     textInput("username", "Username:"),
  #     passwordInput("password", "Password:"),
  #     footer = tagList(
  #       # modalButton("Cancel"),
  #       actionButton("ok", "OK")
  #     )
  #   )
  # }
  # 
  # # Show modal when button is clicked.  
  # # This `observe` is suspended only whith right user credential
  # 
  # obs1 <- observe({
  #   showModal(dataModal())
  # })
  # 
  # obs2 <- observe({
  #   req(input$ok)
  #   isolate({
  #     Username <- input$username
  #     Password <- input$password
  #   })
  #   Id.username <- which(my_username == Username)
  #   Id.password <- which(my_password == Password)
  #   if (length(Id.username) > 0 & length(Id.password) > 0) {
  #     if (Id.username == Id.password) {
  #       Logged <<- TRUE
  #       values$authenticated <- TRUE
  #       obs1$suspend()
  #       removeModal()
  #       
  #     } else {
  #       values$authenticated <- FALSE
  #     }     
  #   }
  # })
  # 
  # output$dataInfo <- renderPrint({
  #   if (values$authenticated) "OK!!!!!"
  #   else "You are NOT authenticated"
  # })
})
