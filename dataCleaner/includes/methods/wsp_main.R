observeEvent(input$wsp_action, {
  
  tryCatch({
    i <- get_input_vars(input, 'wsp')
    keyVar <- input$keyVariable
    data <- dataset$data.loaded
    
   
    future(
      test_apply(c(i$wsp_enabled, length(i$wsp_subset)),
                 cleanify,
                 data = data, keyVar = keyVar,
                 checks = c(if (i$wsp_whitespaces) 'whitespaces', if (i$wsp_doubleWSP) 'doubleWSP'),
                 options = opt(global(subset(!!i$wsp_subset)))
      )
    ) %>% 
      then(onFulfilled = function(res) chkRes$wsp_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'wsp')
      )
  }, error = 
    function(e) {
      
      print(e)
      # Do something here
    })
})

observeEvent(c(input$wsp_display, input$wsp_action, chkRes$wsp_result),{
  if (!is.null(chkRes$wsp_result)){
    output$wsp_logTable <- renderUI(renderLog(chkRes = chkRes$wsp_result, display = input$wsp_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'wsp')
  }
})


output$wsp_log <-
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "wsp_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('wsp_logTable')
    )
  )