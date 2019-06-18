list(
  # awesomeCheckboxGroup(inputId = "lnr_subset", 
  #                        label = "Select variables to check", 
  #                        choices = dataset$colnames[dataset$colnames %in% names(which(cleanify$intelliCompatible(isolate(dataset$data.filtered), "loners", accept.dateTime = isolate(input$lnr_dateAsFactor))))], 
  #                        selected = dataset$colnames[dataset$colnames %in% names(which(cleanify$intelliCompatible(isolate(dataset$data.filtered), "loners", accept.dateTime = isolate(input$lnr_dateAsFactor))))], 
  #                        inline = TRUE, status = "info"),
    fluidRow(
      column(4,
             knobInput(
               inputId = "lnr_upLimit",
               label = "Upper Limit",
               value = 70,
               thickness = 0.1,
               min = 20,
               max = 100,
               step = 5,
               displayPrevious = TRUE, 
               width = 100,
               height = 100,
               lineCap = "round",
               fgColor = "#428BCA",
               inputColor = "#428BCA"
             )),
      column(8,
             numericInput(
               inputId = 'lnr_threshold',
               label = 'Max number of observation for loners',
               value = 5,
               min = 1,
               step = 1,
               width = '100%'
             ),
             materialSwitch(
               inputId = "lnr_dateAsFactor", 
               label = "Check date-time variables", 
               status = "info",
               inline = TRUE,
               value = FALSE,
               right = TRUE)
      )
      
    )
)