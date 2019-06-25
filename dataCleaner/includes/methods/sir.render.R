{
  
  observeEvent(input$idVariable, {
    js$cloneValue(list(val = input$idVariable, target = '#id-var-report'))
  })
  
  list(
    fluidRow(
      column(4,
             knobInput(
               inputId = "sir_upLimit",
               label = "Upper Limit",
               value = 50,
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
             p(span('Current Subject ID variable is: '), span(id='id-var-report', style='font-weight: 700;'), 
               tags$script("$('#id-var-report').html($('#idVariable').data('shinyjs-resettable-value'))")
               ),
             numericInput(inputId = 'sir_repNo',
                          label = 'Number of observation per ID',
                          value = 1,
                          min = 1,
                          step = 1,
                          width = '100%')
      )
    ),
    fluidRow(
      column(6,
             awesomeCheckbox(inputId = 'sir_dateTime_enabled',
                             label = 'Date-Time Alignment Check', 
                             value = FALSE, status = 'info', width = '100%')
      ),
      conditionalPanel(
        condition = 'input.sir_dateTime_enabled',
        column(6,
               awesomeCheckbox(inputId = 'sir_group_by',
                               label = 'Group by subject IDs', 
                               value = FALSE, status = 'info', width = '100%'))
      )
    ),
    conditionalPanel(
      condition = 'input.sir_dateTime_enabled',
      fluidRow(
        column(4,
               pickerInput(inputId = 'sir_pair_x',
                           label = 'Relative Time',
                           choices = dataset$colnames,
                           options = pickerOptions(dropupAuto = TRUE, 
                                                   liveSearch = TRUE, liveSearchNormalize = TRUE,
                                                   width = '100%',
                                                   size = 6))
        ),
        column(4,
               pickerInput(inputId = 'sir_pair_y',
                           label = 'Absolute Time',
                           choices = dataset$colnames,
                           options = pickerOptions(dropupAuto = TRUE, 
                                                   liveSearch = TRUE, liveSearchNormalize = TRUE,
                                                   width = '100%',
                                                   size = 6))
        ),
        column(4,
               textInput(inputId = 'sir_pair_format',
                         label = 'Date Format',
                         width = '100%',
                         placeholder = '(y=year, m=month, d=day, H=hour, M=min, S=sec)'))
      )
    )
  )
}