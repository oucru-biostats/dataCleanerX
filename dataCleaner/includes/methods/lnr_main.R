observeEvent(input$lnr_action, {
  tryCatch({
    i <- get_input_vars(input, 'lnr')
    keyVar <- input$keyVariable
    data <- dataset$data.loaded
    
    
    future(
      test_apply(c(i$lnr_enabled, length(i$lnr_subset)),
                 loners_scan,
                 data = data, keyVar = keyVar,
                 subset = i$lnr_subset, threshold = i$lnr_threshold,
                 upLimit = i$lnr_upLimit, accept.dateTime = i$lnr_dateAsFactor
      )
    ) %>% 
      then(onFulfilled = function(res) chkRes$lnr_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'lnr')
    )
  }, error = 
    function(e) {
      print(e)
      # Do something here
    })
  
  NULL
})

observeEvent(c(input$lnr_display, input$lnr_action, chkRes$lnr_result),{
  if(!is.null(chkRes$lnr_result)){
    output$lnr_logTable <- renderUI(renderLog(chkRes = chkRes$lnr_result, display = input$lnr_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'lnr')
  }
})

output$lnr_log <- 
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "lnr_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('lnr_logTable')
    )
  )


