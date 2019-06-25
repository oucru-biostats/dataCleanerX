navlistBuild <- function(methods, input, output, data = dataset, well=FALSE, widths=c(2, 10)){

    navlistPanel.custom <- function(...) navlistPanel(..., well=well, widths=widths)
    sapply(names(methods$names), function(method){
        method.id <- methods$names[[method]]
        
        tryCatch({
            options.ui <- source(paste0('includes/methods/',methods$meta[[method]]$render), local=TRUE)[[1]]
            intr.ui <- methods$meta[[method]]$instruction
            output[[method.id]] <- renderUI(list(div(options.ui, class='arg-holder'), 
                                                 div(HTML(intr.ui), class='instr-holder')))
        },error = function(e){
            output[[method.id]] <- renderUI(p(toString(e)))
        })
    })
    
    misc$set_always_on(methods$names, output)
    
    output[['checkUI']] <- renderUI(do.call(navlistPanel.custom, 
                                            lapply(names(methods$names), 
                                                   function(method){
                                                       tabPanel(method, uiOutput(methods$names[[method]]))}
                                                   )
                                            )
                                    )
    return(NULL)
}

AppInfo <- read_json(path='AppInfo.json')
methods <- list(meta = read_json('includes/methods/meta.json'))
methods$names <- names(methods$meta)

checkOption_build <- function(check){
    out <- source(paste0('includes/methods/',check,'.render.R'))
    return(renderUI(div(out, id=paste(check,'-args-holder'))))
}

checkInstr_build <- function(check){
    out <- source(paste0('includes/methods/', check, '.intr.RS'))
    return(renderUI(div(out, id=paste(check, '-instr-holder'))))
}

check.result_build <- function(check){
    out <- source(paste0())
}

DT_build <- function(expr, initComplete = NULL, drawCallback = NULL, ...){
    
    renderDT(
        expr = expr,
        filter = list(position = 'top', caseInsensitive = FALSE),
        escape = FALSE, 
        server = TRUE,
        selection = 'single',
        width = '100%',
        rownames = FALSE,
        options = list(
            id = 'data-table',
            search = list(caseInsensitive = FALSE),
            pageLength = 15,
            scrollY = 'fit-content',
            scrollX = TRUE,
            # autoWidth = TRUE,
            scroller = TRUE,
            drawCallback = if(!is.null(drawCallback)) JS(drawCallback),
            initComplete = if (!is.null(initComplete)) JS(initComplete)
        )
    )
}

dtedit <- 
    function (input, output, name, thedata, view.cols = names(thedata), 
              edit.cols = names(thedata), edit.label.cols = edit.cols, 
              input.types, input.choices = NULL, selectize = TRUE, modal.size = "m", 
              text.width = "100%", textarea.width = "570px", textarea.height = "200px", 
              date.width = "100px", numeric.width = "100px", select.width = "100%", 
              defaultPageLength = 10, title.delete = "Delete", title.edit = "Edit", title.copy = "Copy",
              title.add = "New", label.delete = "Delete", label.edit = "Edit", 
              label.add = "New", label.copy = "Copy", show.delete = TRUE, 
              show.update = TRUE, show.insert = TRUE, show.copy = TRUE, 
              class.add = NULL, class.delete = NULL, class.edit = NULL, class.copy = NULL,
              additional.control= NULL, additional.control.endPos = TRUE,
              callback.delete = function(data, row) {
              }, callback.update = function(data, olddata, row) {
              }, callback.insert = function(data, row) {
              }, click.time.threshold = 2, datatable.options = list(pageLength = defaultPageLength), ...) 
    {
        if (!is.data.frame(thedata) | ncol(thedata) < 1) {
            stop("Must provide a data frame with at least one column.")
        }
        else if (length(edit.cols) != length(edit.label.cols)) {
            stop("edit.cols and edit.label.cols must be the same length.")
        }
        else if (!all(view.cols %in% names(thedata))) {
            stop("Not all view.cols are in the data.")
        }
        else if (!all(edit.cols %in% names(thedata))) {
            stop("Not all edit.cols are in the data.")
        }
        DataTableName <- paste0(name, "dt")
        dt.result <- shiny::reactiveValues()
        dt.result$thedata <- thedata
        dt.result$view.cols <- view.cols
        dt.result$edit.cols <- edit.cols
        dt.proxy <- DT::dataTableProxy(DataTableName)
        selectInputMultiple <- function(...) {
            shiny::selectInput(multiple = TRUE, selectize = selectize, 
                               ...)
        }
        valid.input.types <- c("dateInput", "selectInput", "numericInput", 
                               "textInput", "textAreaInput", "passwordInput", "selectInputMultiple")
        inputTypes <- sapply(thedata[, edit.cols], FUN = function(x) {
            switch(class(x), list = "selectInputMultiple", character = "textInput", 
                   Date = "dateInput", factor = "selectInput", integer = "numericInput", 
                   numeric = "numericInput")
        })
        if (!missing(input.types)) {
            if (!all(names(input.types) %in% edit.cols)) {
                stop("input.types column not a valid editting column: ", 
                     paste0(names(input.types)[!names(input.types) %in% 
                                                   edit.cols]))
            }
            if (!all(input.types %in% valid.input.types)) {
                stop(paste0("input.types must only contain values of: ", 
                            paste0(valid.input.types, collapse = ", ")))
            }
            inputTypes[names(input.types)] <- input.types
        }
        for (i in 1:ncol(thedata)) {
            if (nrow(thedata) == 0) {
                thedata[, i] <- character()
            }
            else if (is.list(thedata[, i])) {
                thedata[, i] <- sapply(thedata[, i], FUN = function(x) {
                    paste0(x, collapse = ", ")
                })
            }
        }
        output[[DataTableName]] <- DT::renderDT({
            thedata[, view.cols]
        },..., options = datatable.options, server = TRUE, selection = "single", 
        rownames = FALSE)
        getFields <- function(typeName, values) {
            fields <- list()
            for (i in seq_along(edit.cols)) {
                if (inputTypes[i] == "dateInput") {
                    value <- ifelse(missing(values), as.character(Sys.Date()), 
                                    as.character(values[, edit.cols[i]]))
                    fields[[i]] <- dateInput(paste0(name, typeName, 
                                                    edit.cols[i]), label = edit.label.cols[i], 
                                             value = value, width = date.width)
                }
                else if (inputTypes[i] == "selectInputMultiple") {
                    value <- ifelse(missing(values), "", values[, 
                                                                edit.cols[i]])
                    if (is.list(value)) {
                        value <- value[[1]]
                    }
                    choices <- ""
                    if (!missing(values)) {
                        choices <- unique(unlist(values[, edit.cols[i]]))
                    }
                    if (!is.null(input.choices)) {
                        if (edit.cols[i] %in% names(input.choices)) {
                            choices <- input.choices[[edit.cols[i]]]
                        }
                    }
                    if (length(choices) == 1) if (choices == "") {
                        warning(paste0("No choices available for ", 
                                       edit.cols[i], ". Specify them using the input.choices parameter"))
                    }
                    fields[[i]] <- selectInputMultiple(paste0(name, 
                                                              typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                       choices = choices, selected = value, width = select.width)
                }
                else if (inputTypes[i] == "selectInput") {
                    value <- ifelse(missing(values), "", as.character(values[, 
                                                                             edit.cols[i]]))
                    fields[[i]] <- shiny::selectInput(paste0(name, 
                                                             typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                      choices = if (!edit.cols[i] %in% names(input.choices)) levels(dt.result$thedata[, edit.cols[i]]) else input.choices[[edit.cols[i]]], 
                                                      selected = value, width = select.width)
                }
                else if (inputTypes[i] == "numericInput") {
                    value <- ifelse(missing(values), 0, values[, 
                                                               edit.cols[i]])
                    fields[[i]] <- shiny::numericInput(paste0(name, 
                                                              typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                       value = value, width = numeric.width)
                }
                else if (inputTypes[i] == "textAreaInput") {
                    value <- ifelse(missing(values), "", values[, 
                                                                edit.cols[i]])
                    fields[[i]] <- shiny::textAreaInput(paste0(name, 
                                                               typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                        value = value, width = textarea.width, height = textarea.height)
                }
                else if (inputTypes[i] == "textInput") {
                    value <- ifelse(missing(values), "", values[, 
                                                                edit.cols[i]])
                    fields[[i]] <- shiny::textInput(paste0(name, 
                                                           typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                    value = value, width = text.width)
                }
                else if (inputTypes[i] == "passwordInput") {
                    value <- ifelse(missing(values), "", values[, 
                                                                edit.cols[i]])
                    fields[[i]] <- shiny::passwordInput(paste0(name, 
                                                               typeName, edit.cols[i]), label = edit.label.cols[i], 
                                                        value = value, width = text.width)
                }
                else {
                    stop("Invalid input type!")
                }
            }
            return(fields)
        }
        output[[paste0(name, "_message")]] <- shiny::renderText("")
        updateData <- function(proxy, data, ...) {
            for (i in 1:ncol(data)) {
                if (is.list(data[, i])) {
                    data[, i] <- sapply(data[, i], FUN = function(x) {
                        paste0(x, collapse = ", ")
                    })
                }
            }
            DT::replaceData(proxy, data, ...)
        }
        observeEvent(input[[paste0(name, "_add")]], {
            if (!is.null(row)) {
                shiny::showModal(addModal())
            }
        })
        insert.click <- NA
        observeEvent(input[[paste0(name, "_insert")]], {
            if (!is.na(insert.click)) {
                lastclick <- as.numeric(Sys.time() - insert.click, 
                                        units = "secs")
                if (lastclick < click.time.threshold) {
                    warning(paste0("Double click detected. Ignoring insert call for ", 
                                   name, "."))
                    return()
                }
            }
            insert.click <<- Sys.time()
            newdata <- dt.result$thedata
            row <- nrow(newdata) + 1
            newdata[row, ] <- NA
            for (i in edit.cols) {
                if (inputTypes[i] %in% c("selectInputMultiple")) {
                    newdata[[i]][row] <- list(input[[paste0(name, 
                                                            "_add_", i)]])
                }
                else {
                    newdata[row, i] <- input[[paste0(name, "_add_", 
                                                     i)]]
                }
            }
            tryCatch({
                callback.data <- callback.insert(data = newdata, 
                                                 row = row)
                if (!is.null(callback.data) & is.data.frame(callback.data)) {
                    dt.result$thedata <- callback.data
                }
                else {
                    dt.result$thedata <- newdata
                }
                updateData(dt.proxy, dt.result$thedata[, view.cols], 
                           rownames = FALSE)
                shiny::removeModal()
                return(TRUE)
            }, error = function(e) {
                output[[paste0(name, "_message")]] <<- shiny::renderText(geterrmessage())
                return(FALSE)
            })
        })
        addModal <- function(row, values) {
            output[[paste0(name, "_message")]] <- shiny::renderText("")
            fields <- getFields("_add_", values)
            shiny::modalDialog(title = title.add, shiny::div(shiny::textOutput(paste0(name, 
                                                                                      "_message")), style = "color:red"), fields, footer = shiny::column(shiny::modalButton("Cancel"), 
                                                                                                                                                         shiny::actionButton(paste0(name, "_insert"), "Save"), 
                                                                                                                                                         width = 12), size = modal.size)
        }
        observeEvent(input[[paste0(name, "_copy")]], {
            row <- input[[paste0(name, "dt_rows_selected")]]
            if (!is.null(row)) {
                if (row > 0) {
                    shiny::showModal(addModal(values = dt.result$thedata[row, 
                                                                      ]))
                }
            }
        })
        observeEvent(input[[paste0(name, "_edit")]], {
            row <- input[[paste0(name, "dt_rows_selected")]]
            if (!is.null(row)) {
                if (row > 0) {
                    shiny::showModal(editModal(row))
                }
            }
        })
        update.click <- NA
        observeEvent(input[[paste0(name, "_update")]], {
            if (!is.na(update.click)) {
                lastclick <- as.numeric(Sys.time() - update.click, 
                                        units = "secs")
                if (lastclick < click.time.threshold) {
                    warning(paste0("Double click detected. Ignoring update call for ", 
                                   name, "."))
                    return()
                }
            }
            update.click <- Sys.time()
            row <- input[[paste0(name, "dt_rows_selected")]]
            if (!is.null(row)) {
                if (row > 0) {
                    newdata <- dt.result$thedata
                    for (i in edit.cols) {
                        if (inputTypes[i] %in% c("selectInputMultiple")) {
                            newdata[[i]][row] <- list(input[[paste0(name, 
                                                                    "_edit_", i)]])
                        }
                        else {
                            newdata[row, i] <- input[[paste0(name, "_edit_", 
                                                             i)]]
                        }
                    }
                    tryCatch({
                        callback.data <- callback.update(data = newdata, 
                                                         olddata = dt.result$thedata, row = row)
                        if (!is.null(callback.data) & is.data.frame(callback.data)) {
                            dt.result$thedata <- callback.data
                        }
                        else {
                            dt.result$thedata <- newdata
                        }
                        updateData(dt.proxy, dt.result$thedata[, view.cols], 
                                   rownames = FALSE)
                        shiny::removeModal()
                        return(TRUE)
                    }, error = function(e) {
                        output[[paste0(name, "_message")]] <<- shiny::renderText(geterrmessage())
                        return(FALSE)
                    })
                }
            }
            return(FALSE)
        })
        editModal <- function(row) {
            output[[paste0(name, "_message")]] <- renderText("")
            fields <- getFields("_edit_", values = dt.result$thedata[row, 
                                                                  ])
            shiny::modalDialog(title = title.edit, shiny::div(shiny::textOutput(paste0(name, 
                                                                                       "_message")), style = "color:red"), fields, footer = column(shiny::modalButton("Cancel"), 
                                                                                                                                                   shiny::actionButton(paste0(name, "_update"), "Save"), 
                                                                                                                                                   width = 12), size = modal.size)
        }
        observeEvent(input[[paste0(name, "_remove")]], {
            row <- input[[paste0(name, "dt_rows_selected")]]
            if (!is.null(row)) {
                if (row > 0) {
                    shiny::showModal(deleteModal(row))
                }
            }
        })
        observeEvent(input[[paste0(name, "_delete")]], {
            row <- input[[paste0(name, "dt_rows_selected")]]
            if (!is.null(row)) {
                if (row > 0) {
                    newdata <- callback.delete(data = dt.result$thedata, 
                                               row = row)
                    if (!is.null(newdata) & is.data.frame(newdata)) {
                        dt.result$thedata <- newdata
                    }
                    else {
                        dt.result$thedata <- dt.result$thedata[-row, ]
                    }
                    updateData(dt.proxy, dt.result$thedata[, view.cols], 
                               rownames = FALSE)
                    shiny::removeModal()
                    return(TRUE)
                }
            }
            return(FALSE)
        })
        deleteModal <- function(row) {
            fields <- list()
            for (i in view.cols) {
                fields[[i]] <- div(paste0(i, " = ", dt.result$thedata[row, 
                                                                   i]))
            }
            shiny::modalDialog(title = title.delete, shiny::p("Are you sure you want to delete this record?"), 
                               fields, footer = shiny::column(modalButton("Cancel"), 
                                                              shiny::actionButton(paste0(name, "_delete"), 
                                                                                  "Delete"), width = 12), size = modal.size)
        }
        output[[name]] <- shiny::renderUI({
            shiny::div(
                shiny::div(class='control-buttons',
                           if (!additional.control.endPos) additional.control,
                           if (show.insert) {
                               shiny::actionButton(paste0(name, "_add"), label.add, class = class.add, title = title.add)
                           }, if (show.update) {
                               shiny::actionButton(paste0(name, "_edit"), label.edit, class = class.edit, title = title.edit)
                           }, if (show.delete) {
                               shiny::actionButton(paste0(name, "_remove"), label.delete, class = class.delete, title = title.delete)
                           }, if (show.copy) {
                               shiny::actionButton(paste0(name, "_copy"), label.copy, class = class.copy, title = title.copy)
                           }, if (additional.control.endPos) additional.control
                )
                , DT::DTOutput(DataTableName)
            )
        })
        return(dt.result)
    }

ms_icon <- function(icon.id){
    class = paste0('ms-Icon ms-Icon--', icon.id)
    shiny::tags$i(class=class, 'aria-hidden'="true")
}

updateData <- function(proxy, data, ...) {
    for (i in 1:ncol(data)) {
        if (is.list(data[, i])) {
            data[, i] <- sapply(data[, i], FUN = function(x) {
                paste0(x, collapse = ", ")
            })
        }
    }
    DT::replaceData(proxy, data, ...)
}

workingDialog <- function(message){
    renderUI(
        div(class = 'float working-dialog',
            p(message),
            div(class = 'marquee-background',
                div(class = 'marquee')))
    )
}