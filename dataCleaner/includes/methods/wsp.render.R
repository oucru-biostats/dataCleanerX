list(
  # awesomeCheckboxGroup(inputId = "wsp_subset", 
  #                        label = "Select variables to check", 
  #                        choices = data.cols(), 
  #                        selected = data.cols(), 
  #                        inline = TRUE, status = "info"),
    materialSwitch(
      inputId = "wsp_whitespaces", 
      label = "Leading & trailing spaces", 
      status = "info",
      inline = TRUE,
      value = TRUE,
      right = TRUE),
    materialSwitch(
      inputId = "wsp_doubleWSP", 
      label = "Double whitespaces", 
      status = "info",
      inline = TRUE,
      value = TRUE,
      right = TRUE)
)