observeEvent(input$outl_action, {
  
  if (length(input$outl_subset))
    tryCatch({
      i <- get_input_vars(input, 'outl')
      keyVar <- input$keyVariable
      data <- dataset$data.loaded
      
      
      future(
        test_apply(i$outl_enabled,
                   outliers_scan,
                   data = data, keyVar = keyVar,
                   subset = i$outl_subset,skewParam = list(a = i$outl_skewA, b = i$outl_skewB),
                   customFn = setCustomFn(fn = paste(i$outl_fnLower, i$outl_fnUpper, sep = ';'),
                                          param = i$outl_params),
                   accept.negative = i$outl_acceptNegative, accept.zero = i$outl_acceptZero
        )
      ) %>% 
        then(onFulfilled= function(res) chkRes$outl_result <- res,
             onRejected = function() session$sendCustomMessage('logOn', 'outl')
        )
      
    }, error = 
      function(e) {
        # Do something here
      })
  
  NULL
  
})

observeEvent(c(input$outl_display, input$outl_action, chkRes$outl_result), {
  if(!is.null(chkRes$outl_result)){
    output$outl_logTable <- renderUI(renderLog(chkRes = chkRes$outl_result, display = input$outl_display, keys = data.keys()))
    session$sendCustomMessage('logOn', 'outl')
  }
})

output$outl_log <-
  renderUI(
    div(
      class = 'log-inner',
      pickerInput(inputId = "outl_display",
                  label = "View mode",
                  inline = TRUE,
                  width = '100%',
                  choices = list(
                    'Value' = 'values',
                    'Real index' = 'indexes',
                    'ID (base on Key)' = 'keys'
                  )),
      uiOutput('outl_logTable')
    )
  )

