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
  methods$names <- structure(sapply(methods$meta, function(mtd) mtd$id), names = names(methods$meta))
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
    
    output$DT <- ui_build$DT_build(data.table, 
                                   initComplete = "function() {
                                      
                                      let dt = this;
                                      import('/etc/lib.js').then((lib) => {
                                        lib._DT_initComplete(dt);
                                      });
                                      
                                      show_full_ui();}",
                                   drawCallback = "function(){
                                    let $this=$(this);
                                    $this.addClass('animate');
                                    setTimeout(function(){$this.removeClass('animate')}, 500);
                                   }")
    
    output$doChecks_simple <-
      renderUI(
        awesomeCheckboxGroup(inputId = 'doChecks',
                             label = 'Do check for...',
                             choices = names(methods$names),
                             selected = names(methods$names),
                             status = 'info',
                             inline = TRUE)
      )
  })
  
  output$dataOptions <-
    renderUI(
      dropdownButton(
        inputId = 'datasetMenu',
        circle = FALSE,
        label = '',
        status = "default",
        icon = htmltools::browsable(tags$i(class = 'ms-Icon ms-Icon--CollapseMenu', 'aria-hidden'='true', '')),
        right = FALSE,
        size = 'sm',
        width = 'auto',
        tags$ul(id = 'dataset-menu',
                tags$li(class = if (!length(dataset$data.keys)) 'disabled',
                        div('Set key variable', id = 'set-key-var'),
                        tags$ul(
                          tags$li(
                            id = 'key-var-chooser-holder',
                            div(
                              if (length(dataset$data.keys))
                                awesomeRadio(inputId = 'keyVariable',
                                             label = NULL,
                                             status = 'primary',
                                             choices = dataset$data.keys,
                                             selected = dataset$data.keys[1],
                                             width = 'auto'
                                ) else p('(Empty)'),
                              id = 'key-var-chooser',
                              class = 'awesome-checkbox-chooser'
                            )
                          )
                        )
                ),
                
                tags$li(div('Columns filter', id = 'show-hide-columns'),
                        tags$ul(
                          tags$li(
                            id = 'columns-chooser-holder',
                            div(
                              awesomeCheckboxGroup(inputId = 'shownColumns',
                                                   label = NULL,
                                                   status = 'columns-list',
                                                   choices = dataset$colnames,
                                                   selected = dataset$colnames,
                                                   width = 'auto'
                              ),
                              id = 'columns-chooser',
                              class = 'awesome-checkbox-chooser'
                            )
                          )
                        )
                )
        ),
        tags$script(src = 'etc/data-options.js')
      )
    )
  
  observeEvent(input[['DT_rows_all']], {
    DT_rows_all <- input$`DT_rows_all`
    
    dataset$data.filtered <- dataset$data.table[DT_rows_all,]
    
    output$DT.filtered <- ui_build$DT_build(dataset$data.filtered)
  })
  
  observeEvent(dataset$data.filtered, {
    ui_build$navlistBuild(methods, input, output, data = dataset)
  })
  
  output$DT <- NULL
  
  observeEvent(input$dataReady, {
      show_fullUI(TRUE)
  })
  
  observeEvent(input$sendHiddenMsg, {
    sendSweetAlert(session, title = '', type = 'success', text = h1('KT <3 KL', style='text-align:center; margin-top:5px;color:crimson'), html=TRUE)
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
        uiOutput('app-meta'),
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
        h1('Data Clean Robot')
      )
    )

  output$`app-meta` <-
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
          title = 'Data-Clean Robot',
          tabPanel('Quick Check',
                   tags$script('$(".tab-content .tab-pane[data-value=\'Quick Check\']").prepend($(\'#DT\'));'),
                   uiOutput('dataOptions'),
                   uiOutput('doChecks_simple'),
                   class = 'grand-tab'
          ),
          tabPanel('Custom Profiles',
                   uiOutput('defTable'),
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
          div(
            id='checkUI-toggler',
            class='options-toggler',
            tags$i(
              id='checkUI-btn-close',
              class="ms-Icon ms-Icon--ChevronUp btn-close",
              "aria-hidden"="true"
            )
          ),
          uiOutput('checkUI'),
          tags$script(src='etc/checkUI.js')
        )
      )  
    )
  
  
  defTable.generate <- actionButton('defTable.generate', ui_build$ms_icon('Generate'), title = 'Auto generate profiles')
  
  observeEvent(dataset$data.table, {
    data <- dataset$data.table
    dataset$intelliType <- cleanify$intelliType(data, threshold = 0.8)
    dataset$data.keys <- cleanify$intelliKey(data, showAll = TRUE)
    
    varName.holder <- factor(NULL, levels = sort(unique(dataset$colnames)))
    type.holder <- factor(NULL, levels = c('numeric', 'character', 'dateTime', 'factor', '*'))
    checks.holder <- factor(NULL, levels = names(methods$names))
    dataset$defTable <- 
      tibble(varName = varName.holder, 
             type = type.holder, values = character(), checks = character(), subset = character(), rules = character()) %>% as.data.frame
  })
  
  # defTable.workingDialog <- ui_build$workingDialog('Generating...')
  output$workingDialog <- ui_build$workingDialog('Generating...')
  
  
  observeEvent(input$defTable.generate,{
    session$sendCustomMessage('show', '#workingDialog .working-dialog')
    data <- dataset$data.table
    dataset$checks.compatible <- cleanify$intelliCompatible(data)
  
    type <- 
      sapply(seq_along(dataset$intelliType),
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
                           else if (all(as.numeric(na.omit(data[[i]])) == floor(as.numeric(na.omit(data[[i]]))))) 'integer'
                           else 'numeric',
                           'dateTime' = 'dateTime',
                           'binary' = 'factor')
               
             }, USE.NAMES = FALSE) %>% factor(., levels = c('numeric', 'integer', 'factor', 'character', 'dateTime', ''))
    values <- sapply(seq_along(type),
                     function(i) {
                       type = type[i]
                       
                       if (type %in% c('numeric', 'integer')){
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
    
    checks <-
      sapply(colnames(dataset$checks.compatible), 
             function(var){
               data.var <- dataset$checks.compatible[, var]
               names(data.var)[data.var & !names(data.var) %in% c('doubleWSP', 'case')] %>%
                 factor(x = .,
                        levels = methods$names,
                        labels = names(methods$names)
                 ) %>% as.character 
               # %>% append(c(sep=', ')) %>% as.list %>% do.call(paste, .)
             },
             simplify = FALSE
      )
    
    
    dataset$defTable <- 
      tibble(varName = as.factor(dataset$colnames), type = factor(type), values = values, checks = checks, subset = '', rules = '') %>% as.data.frame()
    
    row.names(dataset$defTable) <- NULL
      
    NULL
  })
  
  observeEvent(input$defTable.generate, {
    ui_build$updateData(dataset$defTable.proxy, dataset$defTable, 
                rownames = FALSE)
    dataset$dt.result$thedata <- dataset$defTable
    NULL
  })
  
  observeEvent(dataset$data.table, {
    #  # req(dataset$defTable)
    dataset$dt.result <- 
    ui_build$dtedit(input, output,
                    name = 'defTable',
                    thedata = dataset$defTable,
                    edit.cols = c('varName', 'type', 'values', 'checks', 'subset', 'rules'),
                    edit.label.cols = c('Variables', 'Data Type', 'Accepted Values', 'Do checks for...', '...where', 'Other rules'),
                    input.types = list(
                      values = 'textInput',
                      checks = 'selectInputMultiple', 
                      type = 'selectInput',
                      subset = 'textInput',
                      rules = 'textInput'),
                    input.choices = list(
                      checks = names(methods$names), 
                      type = c('numeric', 'integer', 'character', 'dateTime', 'factor')), 
                    title.add = 'New check profile', label.add = ui_build$ms_icon('Add'),
                    title.edit = 'Edit check profile', class.edit = 'btn-disabled', label.edit = ui_build$ms_icon('Edit'),
                    title.delete = 'Delete check profile', class.delete = 'btn-disabled', label.delete = ui_build$ms_icon('Remove'),
                    title.copy = 'Clone this profile', class.copy = 'btn-disabled', label.copy = ui_build$ms_icon('Copy'),
                    additional.control = defTable.generate,
                    callback.update = defTable.update.callback,
                    callback.insert = defTable.insert.callback,
                    callback.delete = defTable.delete.callback,
                    datatable.options = list(
                      id = 'def-table',
                      class = 'bq-data-table',
                      paging = FALSE,
                      scrollY = 'fit-content',
                      scrollX = TRUE,
                      scroller = TRUE,
                      autoWidth = FALSE,
                      columnDefs = list(list(width = '30%', targets = c(3, 4))),
                      fixedHeader = TRUE,
                      drawCallback = JS(
                        'function(){
                          /*$(\'<div id = "defTable-edit-control"></div>\')
                          .insertBefore(\'#defTabledt\')
                          .append($("#defTable_add"))
                          .append($("#defTable_edit"))
                          .append($("#defTable_remove"))
                          .append($("#defTable_copy"));
                          
                          $(\'#defTable #defTable_edit\').addClass(\'btn-disabled\');
                          $(\'#defTable #defTable_remove\').addClass(\'btn-disabled\');
                          $(\'#defTable #defTable_copy\').addClass(\'btn-disabled\');*/
                          
                          
                          $("#workingDialog .working-dialog").slideUp();
                        }')
                    ),
                    colnames = c('Variables' = 'varName', 'Type' = 'type', 'Accepted Values' = 'values', 'Do check for...' = 'checks', '...where' = 'subset', 'Additional rules' = 'rules')
    )
  })
  
  # observeEvent(dataset$defTable, {
  #   dataset$defTable.edited <- dataset$defTable
  # })
  
  dataset$defTable.proxy <- DT::dataTableProxy('defTabledt')
  
  defTable.insert.callback <- function(data, row) {
    # dataset$defTable <- rbind(data, dataset$defTable)
    dataset$defTable <- data
    return(dataset$defTable)
  }
  
  defTable.update.callback <- function(data, olddata, row) {
    dataset$defTable[row,] <- data[row,]
    return(dataset$defTable)
  }
  
  defTable.delete.callback <- function(data, row) {
    dataset$defTable <- dataset$defTable[-row,]
    return(dataset$defTable)
  }

  #Handle reload request
  observeEvent(input$reloadRequest, {
    # print(input$reloadRequest)
    if (input$reloadRequest)
      confirmSweetAlert(session = session, 
                        inputId = 'reloadConfirm', 
                        title = 'Load new dataset?', 
                        text = 'Current profile will be resetted when data is unloaded.',
                        btn_labels = c("Cancel", "Continue"))
  })
  
 observeEvent(input$reloadConfirm, {
    if (input$reloadConfirm) session$sendCustomMessage('reloadRequest-ans', TRUE) 
    else session$sendCustomMessage('reloadRequest-ans', FALSE)
  })

  
  # output$checkUI <- renderUI(ui_build$navlistBuild(methods)) 
  
  
  misc$set_always_on(
    c('inputBox', 'DT', 'fullProgram'),
    output = output)  
})