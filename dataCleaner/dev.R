library(jsonlite)
setwd('C:\\Users\\trinhdhk\\OneDrive\\Works\\Git\\dataCleanerX\\dataCleaner\\.includes\\methods')

methods <- list.files(pattern = 'method.R$')
method <- 
    list('Missing Data'= list(options='msd.R', result='msd.result.R'),
    'Outliers'= list(options='outl.R', result='outl.result.R'),
    'Loners'= list(options='lnr.R', result='lnr.result.R'),
    'Binaries'= list(options='bin.R', result='bin.result.R'),
    'White spaces'= list(options='wsp.R', result='wsp.result.R'),
    'Spelling issues'= list(options='spl.R', result='spl.result.R'),
    'Serial Data'= list(options='srl.R', result='srl.result.R')
    )

write_json(method, 'meta.json', auto_unbox=TRUE, pretty=TRUE)