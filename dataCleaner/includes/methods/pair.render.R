list(
  fluidRow(
    column(4,
           knobInput(
             inputId = "pair_upLimit",
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
           pickerInput(inputId = 'pair_v',
                       label = 'Subject IDs',
                       choices = dataset$colnames,
                       options = pickerOptions(dropupAuto = FALSE, 
                                               liveSearch = TRUE, liveSearchNormalize = TRUE,
                                               width = '100%',
                                               size = 6),
                       width = '100%'),
           numericInput(inputId = 'pair_repNo',
                        label = 'Number of observation per ID',
                        value = 1,
                        min = 1,
                        step = 1,
                        width = '100%')
    )
  ),
  fluidRow(
    column(6,
           awesomeCheckbox(inputId = 'pair_dateTime_enabled',
                           label = 'Date-Time Alignment Check', 
                           value = FALSE, status = 'info', width = '100%')
    ),
    conditionalPanel(
      condition = 'input.pair_dateTime_enabled',
      column(6,
             awesomeCheckbox(inputId = 'pair_group_by',
                             label = 'Group by subject IDs', 
                             value = FALSE, status = 'info', width = '100%'))
    )
  ),
  conditionalPanel(
    condition = 'input.pair_dateTime_enabled',
    fluidRow(
      column(4,
             pickerInput(inputId = 'pair_pair_x',
                         label = 'Relative Time',
                         choices = dataset$colnames,
                         options = pickerOptions(dropupAuto = TRUE, 
                                                 liveSearch = TRUE, liveSearchNormalize = TRUE,
                                                 width = '100%',
                                                 size = 6))
      ),
      column(4,
             pickerInput(inputId = 'pair_pair_y',
                         label = 'Absolute Time',
                         choices = dataset$colnames,
                         options = pickerOptions(dropupAuto = TRUE, 
                                                 liveSearch = TRUE, liveSearchNormalize = TRUE,
                                                 width = '100%',
                                                 size = 6))
      ),
      column(4,
             textInput(inputId = 'pair_pair_format',
                       label = 'Date Format',
                       width = '100%',
                       placeholder = '(y=year, m=month, d=day, H=hour, M=min, S=sec)'))
    )
  )
)