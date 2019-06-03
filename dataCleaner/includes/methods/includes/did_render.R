# chkRes$did_result <-
#   test_apply(input$did_enabled,
#              redundancy_check,
#              v = dataset$data.loaded[[input$did_v]], repNo = input$did_repNo,
#              upLimit = input$did_upLimit)
# 
# 
# output$did_logTable <- renderUI(renderLog(chkRes$did_result, vars = input$did_v, display = input$did_display, keys = data.keys()))
# session$sendCustomMessage('logOn', 'did')
# 
# did_enabled <- input$did_enabled
# did_upLimit <- input$did_upLimit
# v <- dataset$data.loaded[[input$did_v]]
# did_repNo <- input$did_repNo


i <- get_input_vars(input, 'did')
v <- dataset$data.loaded[[i$did_v]]

chkRes$did_result <- 
  future(
    test_apply(i$did_enabled,
               redundancy_check,
               v = v, repNo = i$did_repNo,
               upLimit = i$did_upLimit
    )
  ) 



