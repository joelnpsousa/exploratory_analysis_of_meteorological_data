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

})
