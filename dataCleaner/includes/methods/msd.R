list(awesomeCheckboxGroup(inputId = "msd_subset", 
                          label = "Select variables to check", 
                          choices = dataset$colnames[dataset$colnames %in% names(which(intelliCompatible(isolate(dataset$data.table), "missing")))], 
                          selected = dataset$colnames[dataset$colnames %in% names(which(intelliCompatible(isolate(dataset$data.table), "missing")))], 
                          inline = TRUE, status = "info"),
     materialSwitch(inputId = "msd_fix", 
                    label = "Auto replace suspect with NA", 
                    status = "danger",
                    value = FALSE,
                    right = TRUE)
)

