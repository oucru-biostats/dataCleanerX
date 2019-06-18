library(jsonlite)

methodList_build <- 
    function(methodList, methodPath = 'includes/methods'){
        methodPath.full <- paste(methodPath, 'meta.json', sep = '/')
        write_json(methodList, methodPath.full, auto_unbox=TRUE, pretty=TRUE)
    }


add.method <-
    function(method, name, id, alias, 
             render=paste0(alias, '.render.R'), 
             main=paste0(alias,'.main.R'), 
             result=paste0(alias,'.result.R'),
             instruction=''){
        method[[name]] <- list(id=id, alias=alias, render=render, main=main, result=result, instruction=instruction)
        return(method)
    }


method <- NULL
method <- add.method(method, "Missing Data", 'missing', 'msd', instruction = msd_instruction)
method <- add.method(method, "Outliers", 'outliers', 'outl', instruction = outl_instruction)
method <- add.method(method, "Loners", 'loners', 'lnr', instruction = lnr_instruction)
method <- add.method(method, "Binaries", 'binary', 'bnr', instruction = bnr_instruction)
method <- add.method(method, "White spaces", 'whitespaces', 'wsp', instruction = wsp_instruction)
method <- add.method(method, "Spelling issues", 'spelling', 'spl', instruction = spl_instruction)
method <- add.method(method, "Pair Incorrespondence", 'pair', 'pair')


# method <- 
#     list('Missing Data'= list(id='missing', alias = 'msd', options='msd.render.R', process = 'msd.main.R', result='msd.result.R'),
#     'Outliers'= list(id='outliers',options='outl.render.R', process = 'outl.main.R', result='outl.result.R'),
#     'Loners'= list(id='loners',options='lnr.render.R', process = 'lnr.main.R', result='lnr.result.R'),
#     'Binaries'= list(id='binary',options='bin.render.R', process = 'bin.main.R', result='bin.result.R'),
#     'White spaces'= list(id='whitespaces',options='wsp.render.R', process = 'wsp.main.R', result='wsp.result.R'),
#     'Spelling issues'= list(id='spelling',options='spl.render.R', process = 'spl.main.R', result='spl.result.R'),
#     'Serial Data'= list(id='serial',options='srl.render.R', process = 'srl.main.R', result='srl.result.R')
#     )

# write_json(method, 'meta.json', auto_unbox=TRUE, pretty=TRUE)

methodList_build(method)

AppInfo_build <-
    function(version = '3.0', build, buildDate = Sys.Date()) {
        AppInfo <-
            list(
                version = version,
                build = build,
                buildDate = buildDate,
                developer = 'Trinh-Dong@OUCRU-VN',
                license = 'MIT',
                owner = 'Biostatistics-Group@OUCRU-VN',
                contact = 'mailto:trinhdhk@oucru.org'
            )
        write_json(AppInfo, 'AppInfo.json', auto_unbox=TRUE, pretty=TRUE)
    }
