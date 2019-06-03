observeEvent(input$msd_action, {
  tryCatch({
    i <- get_input_vars(input, 'msd')
    keyVar <- input$keyVariable
    data <- dataset$data.loaded
    
    future(
      test_apply(c(i$msd_enabled, length(i$msd_subset)),
                 missing_scan,
                 data = data, keyVar = keyVar,
                 subset = i$msd_subset, fix = i$msd_fix
      )
    ) %>% 
      then(onFulfilled = function(res) chkRes$msd_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'msd')
      )
    
  }, error =
    function(e) {
      print(e)
  })
  
  NULL
})

observeEvent(c(input$msd_display, input$msd_action, chkRes$msd_result), {
  if(!is.null(chkRes$msd_result)){
    output$msd_logTable <- renderUI(renderLog(chkRes = chkRes$msd_result, display = input$msd_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'msd')
    session$sendCustomMessage('res', list(test = 'msd', val = chkRes$msd_result$problemIndexes))
    # future({
    #   renderLog(chkRes = msd_result, display = msd_display, keys = keys)
    # }
    # ) %...>% 
    #   (function (res) {
    #     msd_logTable(res)
    #     session$sendCustomMessage('logOn', 'msd')
    #   })
  }
})

output$msd_log <- 
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "msd_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('msd_logTable')
    )
  )

