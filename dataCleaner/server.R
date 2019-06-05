shinyServer(function(session, input, output) {
  
  # Definition of necessary variables ####
  dataset <- reactiveValues() #save info of loaded dataset
  checkOptions <- reactiveValues() #save info of check options
  dataset$ready <- FALSE #save the ready state of dataset
  show_fullUI <- reactiveVal(FALSE)
  misc <- new.env()
  source('includes/misc.R', local=misc)
  ui_build <- new.env()
  source('includes/ui_build.R', local=ui_build)
  cleanify <- new.env()
  source('includes/cleanify.main.R', local=cleanify)
  source('includes/cleanify.ruleChk.R', local=cleanify)
  methods <- list(meta = read_json('includes/methods/meta.json'))
  methods$names <- names(methods$meta)
  # if (dev) {
  #   dev <- new.env()
  #   source('dev.R', local=dev)
  # }

  #A Temp block for hidden element
   output$tmp <-
    renderUI(
      list(
        uiOutput('inputBox'),
        DTOutput('DT')
      )
    )
  

  #Check if excel inputted
  observeEvent(dataset$fileExt, {
    if (dataset$fileExt %in% c('xls', 'xlsx')){
      session$sendCustomMessage('isExcel', TRUE)
      dataset$sheetsList <- excel_sheets(dataset$filePath)
      names(dataset$sheetsList) <- dataset$sheetsList
      output$sheetPicker <- 
        renderUI(
          pickerInput(
            inputId = "sheetPick",
            label = "Please select one table",
            choices = dataset$sheetsList,
            options = pickerOptions(
              title = '(Select one)',
              dropdownAlignRight = TRUE,
              liveSearch = TRUE,
              mobile = FALSE
              )
          )         
        )
    } else {
      session$sendCustomMessage('isExcel', FALSE)
      dataset$sheetsList <- NULL
      output$sheetPicker <- NULL
    }
  })

  # Read the data into program
  observeEvent(c(input$datasource, input$sheetPick),{
    fileInfo <- misc$getFileInfo(input$datasource)

    # Exception handling
    if (misc$is_support(fileInfo$fileExt)) {
      dataset <- misc$reactive_append(dataset, fileInfo)
      sheet <- if (dataset$fileExt %in% c('xls', 'xlsx')) req(input$sheetPick) else NULL
      data.table <- misc$read_data(dataset, sheet)
      is.valid <- misc$is_valid_data(data.table)

      if (is.valid$valid) {
        if (is.valid$not_blank){
          dataset$data.table <- misc$text_parse(data.table)
          if (dataset$fileExt %in% c('xls', 'xlsx'))
            dataset$sheet <- input$sheetPick else {
              dataset$sheet <- NULL
              updatePickerInput(session,
                inputId = 'sheetPick',
                selected = NULL)
            }
        } else {
          sendSweetAlert(session, title = 'Blank Data',
              text = 'This is a blank data. You will be revert to the old one if available.', type = 'error')
          if (req(dataset$fileExt) %in% c('xls', 'xlsx') & !is.null(req(dataset$sheet))) {
            updatePickerInput(session,
              inputId = 'sheetPick',
              selected = dataset$sheet)
          }  
        } 
      } else {
          sendSweetAlert(session, title = 'Invalid data frame',
            text = 'Invalid data frame. You will be reset to the old one if available.', type = 'error')
          if (req(dataset$fileExt) %in% c('xls', 'xlsx') & !is.null(req(dataset$sheet))) {
            updatePickerInput(session,
              inputId = 'sheetPick',
              selected = dataset$sheet)
          }
        }
      } else {
        sendSweetAlert(session, title = 'Unsupported file format',
            text = 'Unsupported file format. You will be reset to the old one if available.', type = 'error')
        if (req(dataset$fileExt) %in% c('xls', 'xlsx') & !is.null(req(dataset$sheet))) {
            updatePickerInput(session,
              inputId = 'sheetPick',
              selected = dataset$sheet)
        } 
      }
  })

  observeEvent(dataset$data.table, {
    session$sendCustomMessage('dokidoki', TRUE)
    dataset$colnames <- colnames(dataset$data.table)
    data.table <-
      cbind("<span style='color:grey; font-style:italic; font-weight:light'>(index)</span>" = 1:nrow(req(dataset$data.table)), dataset$data.table)
    output$DT <-
      renderDT(
        expr = data.table,
        filter = list(position = 'top', caseInsensitive = FALSE),
        escape = FALSE, 
        server = TRUE,
        selection = 'single',
        rownames = FALSE,
        options = list(
          id = 'data-table',
          search = list(caseInsensitive = FALSE),
          pageLength = 5,
          scrollY = 'auto',
          scrollX = TRUE,
          autoWidth = TRUE,
          scroller = TRUE,
          # drawCallback = JS("function() {_DT_callback(this);}"),
          initComplete = JS("function() {
            let dt = this;
            import('/etc/lib.js').then((lib) => {
              lib._DT_initComplete(dt);
            });
            show_full_ui()}")
          )
        )
  })

  output$DT <- NULL
  # observeEvent(dataset$DT, {output$DT <- dataset$DT})

  observeEvent(input$dataReady, {
      show_fullUI(TRUE)
  })

    
  observeEvent(show_fullUI(), {
    session$sendCustomMessage('dataReady', TRUE)
    Sys.sleep(0.5)
    session$sendCustomMessage('to_tmp', '#inputBox')
    output$mainUI <- misc$render_case(show_fullUI(), uiOutput('fullProgram'), uiOutput('inputDialog'))
  })

  observeEvent(dataset$fileExt, {
    if (dataset$fileExt %in% c('xls','xlsx')) session$sendCustomMessage('sheetPicker_on', TRUE) else session$sendCustomMessage('sheetPicker_on', FALSE)
  })

  output$inputDialog <-
    renderUI(
      div(
        class = 'dialog shadowSurge',
        uiOutput('title'),
        uiOutput('meta'),
        tags$script('$(\'#inputBox\').insertAfter(\'#inputDialog #title\');')
      )
    )

  output$inputBox <-
    renderUI(
      list(
        uiOutput('fileInput'),
        uiOutput('sheetPicker')
      )
    )
  
  output$title <- 
    renderUI(
      div(
        id = 'header',
        h1('🤖 Data Clean Robot')
      )
    )

  output$meta <-
    renderUI(
      a(
        sprintf("Version %s, build %d (%s), %s", ui_build$AppInfo$version, ui_build$AppInfo$build, ui_build$AppInfo$buildDate, ui_build$AppInfo$owner),
        href = ui_build$AppInfo$contact
        )
    )
  
  output$fileInput <-
    renderUI(
      fileInput(inputId = 'datasource',
                label = 'Open your data file', 
                accept = c("text/csv", 
                           "text/comma-separated-values,text/plain",
                           ".csv", ".xls", ".xlsx")
      )
    )
  


  output$fullProgram <-
    renderUI(
      list(
        navbarPage(
          title = '🤖 Data-Clean Robot',
          tabPanel('Quick Check',
                   tags$script('$(".tab-content .tab-pane[data-value=\'Quick Check\']").append($(\'#DT\'));'),
                   class = 'grand-tab'
          ),
          tabPanel('Custom Plan',
                   uiOutput('defTableHolder'),
                   # rHandsontableOutput('defTable'),
                   class = 'grand-tab'
          ),
          tabPanel('Result',
                   uiOutput('result'),
                   class = 'grand-tab'
          ),
          header = div(
            id = 'full-program',
            class = 'shadowSurge',
            tags$script(src='etc/nav-bar.js')
          )
        ),
        div(
          id = 'checkUI-holder',
          class = 'options float',
          uiOutput('checkUI')
        )
      )  
    )

  output$defTableHolder <-
    renderUI(
      rHandsontableOutput('defTable')
    )

  observeEvent(dataset$data.table,{
    data <- dataset$data.table
    dataset$intelliType <- cleanify$intelliType(data, threshold = 0.8)
    type <- sapply(seq_along(dataset$intelliType),
                    function(i) {
                      iType <- dataset$intelliType[[i]]
                      cType <- sapply(dataset$colnames, function(col) class(data[[col]]), USE.NAMES = FALSE)
                      out <- 
                        if (is.null(iType)) ''
                        else if ('key' %in% iType) {
                        if ('numeric' %in% iType) 'numeric' else 'character'
                        }
                        else switch(iType[length(iType)],
                                    'lang' = 
                                      if (!any(grepl(',', data[[i]])) &
                                          sum(grepl('\\s',  data[[i]], perl = TRUE)) < 2 &
                                          length(unique(cleanify$na.blank.omit(data[[i]]))) < 20) 'factor' else 'character',
                                    'other' = 
                                      if (!any(grepl(',', data[[i]])) &
                                          sum(grepl('\\s',  data[[i]], perl = TRUE)) < 2 &
                                          length(unique(cleanify$na.blank.omit(data[[i]]))) < 20) 'factor' else 'character',
                                    'numeric' = 
                                      if (length(unique(na.omit(data[[i]]))) <= 2 & 
                                          all(as.numeric(na.omit(data[[i]])) == floor(as.numeric(na.omit(data[[i]])))) &
                                          all(as.numeric(na.omit(data[[i]])) <= 10))
                                        'factor' 
                                      else 'numeric',
                                    'dateTime' = 'dateTime',
                                    'binary' = 'factor')
                      
                    }, USE.NAMES = FALSE)
    
    values <- sapply(seq_along(type),
                              function(i) {
                                type = type[i]
                                
                                if (type == 'numeric'){
                                  min = min(as.numeric(na.omit(data[[i]])))
                                  max = max(as.numeric(na.omit(data[[i]])))
                                  if (isTRUE(min != max)) paste0('[', min, ', ', max, ']')
                                  else min
                                } 
                                else {
                                  if (type == 'factor'){
                                    val = sort(unique(as.character(cleanify$na.blank.omit(as.character(data[[i]])))))
                                    if (length(val) >  1) paste0('{', toString(val), '}')
                                    else val
                                  } 
                                  else ''
                                }
                              }, USE.NAMES = FALSE)
    missing <- rep(NA, length(type))
    dataset$availTestNames <- c('Missing Data', 'Outliers/Loners', 'Binary', 'Whitespaces', 'Spelling', 'Serial Data')
    availTests <- matrix(rep(FALSE, length(methods$names)*length(dataset$colnames)), ncol = length(methods$names))
    colnames(availTests) <- methods$names
    dataset$defTable <- 
      cbind(varName = dataset$colnames, type = type, values = values, rules = "", availTests)
    row.names(dataset$defTable) <- NULL
  })

  #Handle reload request
  observeEvent(input$reloadRequest, {
    if (input$reloadRequest)
      confirmSweetAlert(session, 
        inputId = 'reloadConfirm', 
        title = 'Load new dataset', 
        text = 'Unsaved plot will be lost when loading new dataset.',
        btn_labels = c("Cancel", "Continue"))
  })

  observeEvent(input$reloadConfirm, {
    if (input$reloadConfirm) session$sendCustomMessage('reloadRequest-ans', TRUE) 
    else session$sendCustomMessage('reloadRequest-ans', FALSE)
  })

  output$defTable <- renderRHandsontable(
    rhandsontable(req(dataset$defTable), stretchH = "all", search = TRUE) %>%
      hot_cols(colWidths = c('','', '120px', '100px', '')) %>%
      hot_rows(rowHeights = rep('24px', 5)) %>%
      hot_col(col = 'varName', readOnly = TRUE) %>%
      hot_col(col = 'type', type = 'dropdown', source = c('numeric', 'character', 'dateTime', 'factor'), allowInvalid = FALSE) %>%
      hot_col(col = 'values', placeholder = '{a, b, c} or [min, max]') %>%
      hot_col(col = 'rules', placeholder = '=') %>%
      (function(tab) {
        for (test in methods$names) {
          tab <- hot_col(tab, col = test, type='checkbox')
        }
        tab
      })
  )
  
  output$checkUI <- renderUI(ui_build$navlistBuild(methods$names)) 
  
   misc$set_always_on(
    c('inputBox', 'DT', 'fullProgram', 'defTable'),
    output = output)  
})


  # output$sidebar <-
  #   renderUI(
  #     div(
  #       navlistPanel(
  #         tabPanel("Missing Data", uiOutput('msd_all')),
  #         tabPanel("Numerical Outliers", uiOutput('outl_all')),
  #         tabPanel("Categorical Loners", uiOutput('lnr_all')),
  #         tabPanel("Binary", uiOutput('bin_all')),
  #         tabPanel("Whitespaces", uiOutput('wsp_all')),
  #         tabPanel("Spellings Issues", uiOutput('spl_all')),
  #         tabPanel("Serial Data Checks", uiOutput('did_all')),
  #         widths = c(11, 1),
  #         well = FALSE
  #       ),
  #       class = 'navlist-outer'
  #     )
  #   )