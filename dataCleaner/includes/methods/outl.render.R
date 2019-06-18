source('includes/methods/includes/outlierFn.R', local=TRUE)

list(
  # awesomeCheckboxGroup(
  #   inputId = "outl_subset", 
  #   label = "Select variables to check", 
  #   choices = data$colnames[data$colnames %in% names(which(cleanify$intelliCompatible(isolate(dataset$data.filtered), "outliers")))], 
  #   selected = data$colnames[data$colnames %in% names(which(cleanify$intelliCompatible(isolate(dataset$data.filtered), "outliers")))], 
  #   inline = TRUE, status = "info"
  # ),
  pickerInput(
    inputId = 'outl_model',
    label = 'Outlier model (default: Adjusted)',
    choices = c('Adjusted model' = 'adjusted', 'Tukey Boxplot model' = 'boxplot', 'Custom model' = 'custom'),
    selected = 'adjusted',
    width = '100%'
  ),
  fluidRow(
    column(6, 
           textInput(inputId = 'outl_fnLower',
                     label = 'Lower Bound Function',
                     width = '100%',
                     value = getOutlValue(type = 'upper', model = isolate(input$outl_model)))
    ),
    column(6, 
           textInput(inputId = 'outl_fnUpper',
                     label = 'Upper Bound Function',
                     width = '100%',
                     value = getOutlValue(type = 'lower', model = isolate(input$outl_model)))
    )
  ),
  conditionalPanel(
    condition = 'input.outl_model == "adjusted"',
    fluidRow(
      column(6,
             numericInput(inputId = 'outl_skewA',
                          label = 'Skew Param a',
                          value = -4,
                          width = '100%')),
      column(6,
             numericInput(inputId = 'outl_skewB',
                          label = 'Skew Param b',
                          value = 3,
                          width = '100%'))
    )
  ),
  conditionalPanel(
    condition = 'input.outl_model == "custom"',
    textInput(inputId = 'outl_params',
              label = 'Additional params definition',
              placeholder = 'var1 = value1, var2 = value2',
              width = '100%')
  ),
  fluidRow(
    column(6,
           materialSwitch(inputId = "outl_acceptNegative", 
                          label = "Accept negative value", 
                          status = "info",
                          value = FALSE,
                          right = TRUE)
    ),
    column(6,
           materialSwitch(inputId = "outl_acceptZero", 
                          label = "Accept zero value", 
                          status = "info",
                          value = FALSE,
                          right = TRUE)
    )
  )
)