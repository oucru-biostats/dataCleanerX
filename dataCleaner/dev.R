library(jsonlite)

methodList_build <- 
    function(methodList, methodPath = 'includes/methods'){
        methodPath.full <- paste(methodPath, 'meta.json', sep = '/')
        write_json(methodList, methodPath.full, auto_unbox=TRUE, pretty=TRUE)
    }

method <- 
    list('Missing Data'= list(options='msd.render.R', process = 'msd.main.R', result='msd.result.R'),
    'Outliers'= list(options='outl.render.R', process = 'outl.main.R', result='outl.result.R'),
    'Loners'= list(options='lnr.render.R', process = 'lnr.main.R', result='lnr.result.R'),
    'Binaries'= list(options='bin.render.R', process = 'bin.main.R', result='bin.result.R'),
    'White spaces'= list(options='wsp.render.R', process = 'wsp.main.R', result='wsp.result.R'),
    'Spelling issues'= list(options='spl.render.R', process = 'spl.main.R', result='spl.result.R'),
    'Serial Data'= list(options='srl.render.R', process = 'srl.main.R', result='srl.result.R')
    )

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
