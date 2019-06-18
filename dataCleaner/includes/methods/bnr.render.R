list(
  # awesomeCheckboxGroup(inputId = "bin_subset", 
  #                        label = "Select variables to check", 
  #                        choices = data.cols(), 
  #                        selected = data.cols()[data.cols() %in% names(which(intelliCompatible(isolate(dataset$data.loaded), "binary")))], 
  #                        inline = TRUE, status = "info"),
    fluidRow(
      column(4),
      column(4,
             knobInput(
               inputId = "bin_upLimit",
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
      column(4)
    )
)