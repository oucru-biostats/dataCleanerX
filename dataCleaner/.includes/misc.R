# This file contains necessary functions for UI Generation
# Writtten by Trinh Dong
# Ver 0.1

# These functions return different UIs in different cases.
# A suffix 's' indicates a vectorized version.
.ui_case <- function(case, if.true, if.false, callback = shiny::renderUI){
    if (missing(callback)) callback <- identity
    return(callback(if (isTRUE(case)) if.true else if.false))
}

.ui_cases <- function(cases, if.true, if.false, callback = shiny::renderUI){
    if (missing(callback)) callback <- identity
    return(callback({
        if (length(if.true) != length(cases)) {
            if (length(if.true) == 1) if.true <- rep(if.true, length(cases)) else stop('Length of if.true must be either 1 or length of `cases`')
        }
        if (length(if.false) != length(cases)) {
            if (length(if.false) == 1) if.false <- rep(if.false, length(cases)) else stop('Length of if.false must be either 1 or length of `cases`')
        }
        
        lapply(1:seq_along(cases), function(i){
            .ui_case(case[[i]], if.true[[i]], if.false[[i]], callback = identity)
        })
    }))
}

# Shorthand for .ui_case and .ui_cases (which are hidden)
render_cases <- function(cases, if.true, if.false) .ui_case(cases, if.true, if.false, callback=shiny::renderUI)
render_case <- function(case, if.true, if.false) .ui_case(case, if.true, if.false, callback=shiny::renderUI)

# Pre-processing the files

getFileInfo <- function(filePath){
    fileExt <- tools::file_ext(filePath$datapath)
    name.split <- strsplit(filePath$name, '\\.')[[1]]
    fileName <- do.call(paste0, as.list(name.split[-length(name.split)]))
    folderPath <- gsub(filePath$name, '', filePath$datapath)
    fullPath <- filePath$datapath
    
    return(list(fileExt = fileExt, fileName = fileName, folderPath = folderPath, filePath = fullPath))
}

is_support <- function(fileExt){
    if (fileExt %in% c('csv', 'xls', 'xlsx')) return(TRUE) else return(FALSE)
}

# Parse text to avoid problem with special characters
text_parse <- function(data){
  data <- lapply(data,
                 function(col){
                   col.res <- iconv(col, to  = 'UTF-8//TRANSLITE')
                   as.is <- if (any(length(grepl("\\s", col.res, perl = TRUE)) >= 2)) TRUE else FALSE
                   col.res <- type.convert(col.res, as.is = as.is)
                 })
  data <- as_tibble(data)
}

# Read data and return error if neeeded
read_excel_shiny <- function(dataset, sheet){
    req(dataset$sheetsList)
    if (!is.null(sheet) & isTRUE(sheet %in% dataset$sheetsList))
      read_excel(dataset$filePath, sheet = sheet, trim_ws = FALSE)
    else print(dataset$sheetsList)
}
  
read_data <- function(dataset, sheet){
    
    switch(dataset$fileExt,
        "xls" = read_excel_shiny(dataset, sheet),
        "xlsx" = read_excel_shiny(dataset, sheet),
        "csv" = vroom(dataset$filePath, delim=',', na = character(), trim_ws = FALSE,  escape_double = FALSE),
        'sas' = read_sas(dataset$filePath),
        default = cat('Unsupported')
    )
}

is_valid_data <- function(data){
    if (is_tibble(data)) {
      if (nrow(data) > 0 && ncol(data) >0) {
        return(list(valid = TRUE, not_blank = TRUE))
      } else return(list(valid = TRUE, not_blank = FALSE))
    } else return(list(valid = FALSE))
}

reactive_append <- function(reactiveValues, list){
    if (!length(names(list))) stop('Appending list must have a name.')
    for (i in names(list)) reactiveValues[[i]] <- list[[i]]
    return(reactiveValues)
}

set_always_on <- function(objs, output = output){
  for (obj in objs)
    outputOptions(output, obj, suspendWhenHidden = FALSE)
}

