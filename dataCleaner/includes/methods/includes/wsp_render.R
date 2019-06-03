# chkRes$wsp_result <- cleanify(data = dataset$data.loaded, keyVar = input$keyVariable,
#                               checks = c(if (input$wsp_whitespaces) 'whitespaces', if (input$wsp_doubleWSP) 'doubleWSP'), 
#                               options = opt(global(subset(!!input$wsp_subset))))
# 
# output$wsp_log <- 
#   renderUI(
#     div(
#       class = 'log-inner',
#       pickerInput(inputId = "wsp_display",
#                   label = "View mode",
#                   inline = TRUE,
#                   width = '100%',
#                   choices = list(
#                     'Value' = 'values',
#                     'Real index' = 'indexes',
#                     'ID (base on Key)' = 'keys'
#                   )),
#       uiOutput('wsp_logTable')
#     )
#   )
# 
# output$wsp_logTable <- renderUI(renderLog(chkRes$wsp_result, display = input$wsp_display, keys = data.keys()))
# 
# session$sendCustomMessage('logOn', 'wsp')

i <- get_input_vars(input, 'wsp')
keyVar <- input$keyVariable
data <- dataset$data.loaded

chkRes$wsp_result <- 
  future(
    test_apply(c(i$wsp_enabled, length(i$wsp_subset)),
               cleanify,
               data = data, keyVar = i$keyVariable,
               checks = c(if (i$wsp_whitespaces) 'whitespaces', if (i$wsp_doubleWSP) 'doubleWSP'),
               options = opt(global(subset(!!i$wsp_subset)))
    )
  ) 




