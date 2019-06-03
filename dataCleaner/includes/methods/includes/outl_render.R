# chkRes$outl_result <- outliers_scan(data = dataset$data.loaded, keyVar = input$keyVariable, subset = input$outl_subset,
#                                     model = input$outl_model, skewParam = list(a = input$outl_skewA, b = input$outl_skewB),
#                                     customFn = setCustomFn(fn = paste(input$outl_fnLower, input$outl_fnUpper, sep = ';'),
#                                                            param = input$outl_params),
#                                     accept.negative = input$outl_acceptNegative, accept.zero = input$outl_acceptZero)
# 
# output$outl_log <- 
#   renderUI(
#     div(
#       class = 'log-inner',
#       pickerInput(inputId = "outl_display",
#                   label = "View mode",
#                   inline = TRUE,
#                   width = '100%',
#                   choices = list(
#                     'Value' = 'values',
#                     'Real index' = 'indexes',
#                     'ID (base on Key)' = 'keys'
#                   )),
#       uiOutput('outl_logTable')
#     )
#   )
# 
# output$outl_logTable <- renderUI(renderLog(chkRes$outl_result, display = input$outl_display, keys = data.keys()))
# session$sendCustomMessage('logOn', 'outl')

i <- get_input_vars(input, 'outl')
keyVar <- input$keyVariable
data <- dataset$data.loaded

chkRes$outl_result <- 
  future(
    test_apply(i$outl_enabled,
               outliers_scan,
               data = data, keyVar = keyVar,
               subset = i$outl_subset,skewParam = list(a = i$outl_skewA, b = i$outl_skewB),
               customFn = setCustomFn(fn = paste(i$outl_fnLower, i$outl_fnUpper, sep = ';'),
                                      param = i$outl_params),
               accept.negative = i$outl_acceptNegative, accept.zero = i$outl_acceptZero
    )
  ) 

