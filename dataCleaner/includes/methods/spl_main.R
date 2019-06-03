observeEvent(input$spl_action, {
  
  tryCatch({
    i <- get_input_vars(input, 'spl')
    keyVar <- input$keyVariable
    data <- dataset$data.loaded
    
   
    future(
      test_apply(c(i$spl_enabled, length(i$spl_subset)),
                 cleanify,
                 data = data, keyVar = keyVar,
                 checks = c(if (i$spl_case) 'case', if (i$spl_spelling) 'spelling'),
                 options = opt(global(subset(!!i$spl_subset)), spelling(upLimit(!!i$spl_upLimit)))
      )
    ) %>% 
      then(onFulfilled = function(res) chkRes$spl_result <- res,
           onRejected = function() session$sendCustomMessage('logOn', 'spl')
      )
  }, error = 
    function(e) {
      print(e)
      # Do something here
    })
  
  NULL
})

observeEvent(c(input$spl_display, input$spl_action, chkRes$spl_result),{
  if(!is.null(chkRes$spl_result)){
    output$spl_logTable <- renderUI(renderLog(chkRes = chkRes$spl_result, display = input$spl_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'spl')
  }
})

output$spl_log <-
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "spl_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('spl_logTable')
    )
  )