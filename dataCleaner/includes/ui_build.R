navlistBuild <- function(names, uis, render){
    if (length(names) != length(uis)) stop('Length mismatched!')
    
    lapply(1:length(uis), function(i){
        navlistPanel(names[[i]], render(uis[[i]]))
    })
}

uice