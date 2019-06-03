# chkRes$lnr_result <- loners_scan(data = dataset$data.loaded, keyVar = input$keyVariable,
#                                  subset = input$lnr_subset, threshold = input$lnr_threshold,
#                                  upLimit = input$lnr_upLimit, accept.dateTime = input$lnr_dateAsFactor)
# 
# output$lnr_log <- 
#   renderUI(
#     div(
#       class = 'log-inner',
#       pickerInput(inputId = "lnr_display",
#                   label = "View mode",
#                   inline = TRUE,
#                   width = '100%',
#                   choices = list(
#                     'Value' = 'values',
#                     'Real index' = 'indexes',
#                     'ID (base on Key)' = 'keys'
#                   )),
#       uiOutput('lnr_logTable')
#     )
#   )
# 
# output$lnr_logTable <- renderUI(renderLog(chkRes$lnr_result, display = input$lnr_display, keys = data.keys()))
# 
# session$sendCustomMessage('logOn', 'lnr')

i <- get_input_vars(input, 'lnr')
keyVar <- input$keyVariable
data <- dataset$data.loaded

chkRes$lnr_result <- 
  future(
    test_apply(c(i$lnr_enabled, length(i$lnr_subset)),
               loners_scan,
               data = data, keyVar = i$keyVariable,
               subset = i$lnr_subset, threshold = i$lnr_threshold,
               upLimit = i$lnr_upLimit, accept.dateTime = i$lnr_dateAsFactor
    )
  ) 




