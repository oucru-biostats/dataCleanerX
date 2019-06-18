list(
  # awesomeCheckboxGroup(inputId = "msd_subset", 
  #                         label = "Select variables to check", 
  #                         choices = data$colnames[data$colnames %in% names(which(cleanify$intelliCompatible(isolate(data$data.filtered), "missing")))], 
  #                         selected = data$colnames[data$colnames %in% names(which(cleanify$intelliCompatible(isolate(data$data.filtered), "missing")))], 
  #                         inline = TRUE, status = "info"),
     materialSwitch(inputId = "msd_fix", 
                    label = "Auto replace suspects with NA", 
                    status = "danger",
                    value = FALSE,
                    right = TRUE)
)




