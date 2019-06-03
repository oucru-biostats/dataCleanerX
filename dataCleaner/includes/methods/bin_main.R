observeEvent(input$bin_action, {

  tryCatch({
    i <- get_input_vars(input, 'bin')
    keyVar <- input$keyVariable
    data <- dataset$data.loaded
    
    
    future(
      test_apply(i$bin_enabled,
                 binary_scan,
                 data = data, keyVar = keyVar,
                 subset = i$bin_subset, upLimit = i$bin_upLimit
      )
    ) %>% 
      then(onFulfilled = function(res) chkRes$bin_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'bin')
      )
    
  }, error =
    function(e) {
      print(e)
      # Do something here
    })
  
  NULL
})

observeEvent(c(input$bin_display, input$bin_action, chkRes$bin_result),{
  if(!is.null(chkRes$bin_result)){
    output$bin_logTable <- renderUI(renderLog(chkRes = chkRes$bin_result, display = input$bin_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'bin')
  }
})

output$bin_log <- 
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "bin_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('bin_logTable')
    )
  )

