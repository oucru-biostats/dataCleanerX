# i <- get_input_vars(input, 'msd')
# keyVar <- input$keyVariable
# data <- dataset$data.loaded
# 
# chkRes$msd_result <- 
#   future(
#     test_apply(c(i$msd_enabled, length(i$msd_subset)),
#                missing_scan,
#                data = data, keyVar = i$keyVariable,
#                subset = i$msd_subset, fix = i$msd_fix
#     )
#   ) 