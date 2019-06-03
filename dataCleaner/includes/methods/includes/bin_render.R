i <- get_input_vars(input, 'bin')
data <- dataset$data.loaded

chkRes$bin_result <- 
  future(
    test_apply(i$bin_enabled,
               binary_scan,
               data = data, keyVar = i$keyVariable,
               subset = i$bin_subset, upLimit = i$bin_upLimit
    )
  )




