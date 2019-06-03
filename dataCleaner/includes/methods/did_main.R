observeEvent(input$did_action, {
  
  tryCatch({
    i <- get_input_vars(input, 'did')
    v <- dataset$data.loaded[[i$did_v]]
    data <- dataset$data.loaded
    keyVar <- input$keyVariable
    
    future({
      test_apply(i$did_enabled,
                 redundancy_check,
                 v = v, repNo = i$did_repNo,
                 upLimit = i$did_upLimit
      )
      
    }) %>% 
      then(onFulfilled = function(res) chkRes$did_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'did')
      )
    
    
    future(
      test_apply(c(i$did_enabled, i$did_dateTime_enabled),
                 pair_check,
                 data = data, x = i$did_pair_x, y = i$did_pair_y, 
                 group_by = if (!is.null(i$did_v) & i$did_dateTime_enabled)i$did_v else NULL, 
                 time_consistency,
                 format = i$did_pair_format,
                 keyVar = keyVar)
      
    ) %>%
      then(onFulfilled = function(res) chkRes$did_dateTime_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'did'))
    
  }, error = 
    function(e) {
      print(e)
      # Do something here
    })
  
  NULL
})

observeEvent(c(input$did_display, input$did_action, chkRes$did_result, chkRes$did_dateTime_result),{
  if(!is.null(chkRes$did_result)){
    output$did_logTable <- renderUI(renderLog(chkRes = chkRes$did_result, vars = input$did_v,
                                              display = input$did_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'did')
  }
  if(!is.null(chkRes$did_dateTime_result)){
    output$did_dateTime_logTable <- renderUI(renderLog(chkRes = chkRes$did_dateTime_result, vars = paste(input$did_pair_x, input$did_pair_y, sep = ' - '),
                                                       display = input$did_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'did')
  }
})

output$did_log <- 
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "did_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes'
                  )),
      p('Redundancy Check Result'),
      uiOutput('did_logTable'),
      if (isTRUE(input$did_dateTime_enabled)) 
        list(
          p('Date-Time Misalignment Result'),
          uiOutput('did_dateTime_logTable'))
    )
  )

