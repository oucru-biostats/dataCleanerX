navlistBuild <- function(methods, well=FALSE, widths=c(3, 9)){
    navlistPanel.custom <- function(...) navlistPanel(..., well=well, widths=widths)
    do.call(navlistPanel.custom, lapply(methods, function(method){
        tabPanel(method, uiOutput(method))
    }))
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

checkResult_build <- function(check){
    out <- source(paste0())
}

checkPanel <-
    navlistPanel()
