# Useful functions ######

## Vectorize version of isTRUE and isFALSE
is_true <- function(x)
  if (length(x)) Vectorize(isTRUE, 'x')(x) else FALSE
is_false <- function(x)
  if (length(x)) Vectorize(isFALSE, 'x')(x) else FALSE
iVectorize <- function(fun, ...){
  args <- list(...)
  if(length(names(args))) do.call(Vectorize(fun, names(args)), args)
  else do.call(Vectorize(fun), args)
}
if.else <- function(expr, yes, no){
  has.no <- !missing(no)  
  has.yes <- !missing(yes)
  len <- length(expr)
  if (has.yes) if (length(yes) != len) yes <- rep(yes, len %/% length(yes) + 1)[1:len]
  if (has.no) if (length(no) != len) no <- rep(no, len %/% length(no) + 1)[1:len]
  
  sapply(seq_along(expr), function(i){
    if (expr[[i]] & has.yes) return(yes[[i]]) else invisible(NULL)
    if (has.no) return(no[[i]]) else invisible(NULL)
  })
}
  

## Generate a list of names in list x
names_list <- function(x){
  names <- lapply(seq_along(x), function(i){
    x.1 <- x[i]
    if (is.list(x.1[[1]])) {
      out <- list()
      out[[1]] <-  names(x.1)
      out[[2]] <-  names_list(x.1[[1]])
    } else out <-  names(x.1)
    return(out)
  })
  class(names) <- c('list', 'namesList')
  return(names)
}

## Get the value list of specified level in list x
get_level <- function(x, lv = 1, simplify = TRUE){
  UseMethod('get_level')
}
get_level.list <- function(list, lv = 1, simplify = TRUE){
  out <- sapply(list, function(this){
    
    if (lv == 1) this[[1]]
    else if(is.list(this)) {
      get_level.list(this[-1][[1]], lv-1, simplify = simplify)
    } 
  })
  if (simplify) out <- unlist(out)
  return(out)
}
get_level.namesList <- function(namesList, lv, simplify = TRUE){
  get_level.list(namesList, lv =1, simplify)
}

## Convert a string to expression
str_to_expr <- function(str){
  pre_parsed <- strsplit(str, split = "\\s?[;,]\\s?", perl = TRUE)
  parsed <- sapply(pre_parsed, function(text) parse(text = text))
  return(parsed)
}

## Get levels number of nest in a list
n_levels <- function(x){
  n <- 1
  if(any(sapply(x, is.list))){
    n <- n + max(sapply(x, n_levels))
  }
  return(n)
}

## Generate a list using nesting syntax
quo_to_list <- function(..., eval = TRUE){
  if (!requireNamespace('rlang')) stop('rlang is needed.')
  l.quos <- rlang::enquos(...)
  l <- lapply(l.quos, function(l.quo){
    l.quo <- rlang::quo_squash(l.quo)
    l.names <- names(l.quo)[-1]
    out <- lapply(l.names, function(l.name){
      if (eval) eval(l.quo[[l.name]]) else l.quo[[l.name]]
    })
    names(out) <- l.names
    return(out)
  })
  names(l) <- sapply(l.quos, function(l.quo) quo_squash(l.quo)[[1]])
  return(l)
} #old and deprecated

quo_to_list <- function(..., eval = TRUE){
  if (!require(rlang)) stop('rlang is needed')
  x.quos <- rlang::enquos(...)
  x <- lapply(x.quos, function(x.quo){
    x.quo <- rlang::quo_squash(x.quo)
    out <- sapply(x.quo[-1], function(q){
      if (is.language(q) & !is.symbol(q)) quo_to_list(UQ(q), eval = eval)
      else {
        o <- if (eval) eval(q) else q
        names(o) <- names(q)
        return(o)
      }
    })
    return(out)
  })
  
  names(x) <- sapply(x.quos, function(x.quo) rlang::quo_squash(x.quo)[[1]])
  return(x)
}
qlist <- function(..., eval = TRUE){
  quo_to_list(..., eval = eval)
} # A mere shortcut

## Update a list
update.list <- function(x, y, add = TRUE, null.omit = TRUE){
  
  #### Get name list ####
  y.namesList <- names_list(y)
  
  #### Do the update ####
  for(y.namesList.this in y.namesList){
    if (is.list(y.namesList.this)){
      if (all(is.null(x[y.namesList.this[[1]]][[1]]), add)){
        if (!all(is.null(y[[y.namesList.this[[1]]]])) | !null.omit)
          x[y.namesList.this[[1]]] <-  y[y.namesList.this[[1]]]
      } else { 
        x[[y.namesList.this[[1]]]] <- update.list(x[[y.namesList.this[[1]]]], y[[y.namesList.this[[1]]]], add = add, null.omit = null.omit)
      }
    } else if (add | !is.null(x[[y.namesList.this[[1]]]])) {
      if(!null.omit | !is.null(y[[y.namesList.this]])) x[y.namesList.this] <- y[y.namesList.this] 
    }
  }
  return(x)
}
update_list <- function(x, y, accept.na = FALSE, silent = TRUE, simplify = FALSE){
  x.names <- names(x)
  y.names <- names(y)
  if (!setequal(x, y) & !silent) warning('Two sets don\'t have identical name list. Only matched names will be updates')
  
  z <- lapply(seq_along(x), function(i){
    x <- x[i]
    x.name <- names(x)
    x <- x[1][[1]]
    y <- y[[x.name]]
    if (!is.null(y)) if (!is.na(y) | accept.na) x <- y
    return(x)
  })
  
  names(z) <- x.names
  return(z)
} # old and depricated

## A combination mapping function
expand_map2 <- function(.x, .y, fn){
  if (any(missing(.x), missing(.y), !length(.x), !length(.y))) stop('x, y cannot be empty.')
  fn <- gsub('.x', 'each.x', fn, fixed = TRUE)
  fn <- gsub('.y', 'each.y', fn, fixed = TRUE)
  
  out <- sapply(.x, function(each.x) sapply(.y, function(each.y) eval(parse(text = fn))))
  return(out)
}
expand_reduce2 <- function(.x, fn){
  if (missing(fn)) stop('fn cannot be empty')
  if (!length(.x)) stop('x cannot be empty')
  if (!requireNamespace('purrr')) stop('purrr is required.')
  out <- purrr::reduce(.x = .x, function(x, y) expand_map(x, y, fn = fn))
  return(out)
}

expand_map <- function(.x, .y, fn, dimnames = list(names(.y), names(.x))){
  if (any(missing(.x), missing(.y), !length(.x), !length(.y))) stop('x, y cannot be empty.')
  if (!requireNamespace('purrr')) stop('purrr is required.')
  expandGrid <- expand.grid(.x, .y, stringsAsFactors = FALSE)
  x <- expandGrid$Var1
  y <- expandGrid$Var2
  
  outV <- purrr::map2(x, y, fn)
  outM <- matrix(outV,
                 nrow = length(.y),
                 ncol = length(.x),
                 byrow = TRUE,
                 dimnames = dimnames)
  return(outM)
}

expand_reduce <- function(.x, fn){
  if (missing(fn)) stop('fn cannot be empty')
  if (!length(.x)) stop('x cannot be empty')
  if (!requireNamespace('purrr')) stop('purrr is required.')
  .x = lapply(.x, function(x) .x)
  out <- purrr::reduce(.x = .x, function(x, y) expand_map(x, y, fn = fn))
  return(out)
}

## A quick sniper to remove observation
qSniper <- function(x, check){
  return(x[eval(parse(text = check))])
}

## A quick sniper to remove NA and blank observation
na.blank.omit <- function(v){
  # return(v[!is.na(v) & v != 'NA' & v != ''])
  return(qSniper(x = v, check = '!is.na(x) & !x %in% c("", "nan", "NaN", "NAN", "na", "NA", "Na", "-", " ")'))
}

# Helper functions ######

messageGenerator.306 <- function(message, displayName = c('keys', 'indexes', 'values'), displayValues, nMax){
  if (!is.numeric(nMax)) stop('nMax must be integer')
  nMax <- abs(nMax) 
  displayName <- match.arg(displayName)
  
  message <- sub('$display$', displayName, message, fixed = TRUE)
  message <- sub('$behave$', if (displayName == 'values') 'be' else 'have', message, fixed = TRUE)
  suffix <- '\n'
  if (nMax < length(displayValues)) {
    deltaL <- length(displayValues) - nMax
    displayValues <- displayValues[1:nMax]
    suffix <- sprintf(' (and %d more)\n', deltaL)
  }
  displayValues.string <- toString(ifelse(displayName == 'values' & !is.na(displayValues), paste0('"', displayValues, '"'), displayValues))
  out <- sprintf('%s %s%s', message, displayValues.string, suffix)
  return(out)
}

getTestList <- function(data){
  names <- purrr::reduce(sapply(data, function(v) names(v)), union)
  return(names[names != 'res'])
}

get_res <- function(x){
  UseMethod('get_res',x)
}

get_res.checkResult.306 <- function(x){
  return(x$res)
}

get_res.checkResult.306.cleanify <- function(x){
  if (!'checkResult.306.cleanify' %in% class(x)) stop('x must be a cleanify object!')
  outMatrix <- data.frame(lapply(x, function(var) var$res))
  return(outMatrix)
}

printAll <- function(x){
  UseMethod('printAll', x)
}

printAll.checkResult.306 <- function(x, table = TRUE, display = c('values', 'keys', 'indexes')){
  print.checkResult.306(x = x, nMax = Inf, table = table, display = display)
}

print.checkResult.306 <- function(x, display = c('values', 'keys', 'indexes'), table = TRUE, nMax = 5,  res = FALSE){
  
  #### Argument check ####
  if (!requireNamespace('pander', quietly = TRUE)) {
    warning('pander is required to draw table, set table to false.')
    table <- FALSE
  }
  if (any(is.na(nMax), is.null(nMax), nMax < 0)) nMax <- 5
  
  display <- match.arg(display)
  displayValues <- switch(display,
                          'values' = lapply(x$problemValues, function(this.problemValues) return(unique(this.problemValues))),
                          'keys' = as.character(x$problemKeys),
                          'indexes' = x$problemIndexes)
  
  names(displayValues) <- names(x$problemValues)
  
  if (!length(x$problem)) return(cat('Empty check result!\n'))
  
  #### Printing methods ####
  if (res) return(x$res)
  varList <- names(x$problem)
  if (is.null(varList)) {
    if (length(x$problem) > 1) varList <- 1:length(x$problem) else {
      mes <- 
        if (x$problem) messageGenerator.306(message = x$message, displayName = display, displayValues = displayValues, nMax = nMax)
        else x$message
      cat(mes)
      invisible(x)
    }
  }
  if (!is.null(varList)) {
    if (table) {
      message <- sapply(varList, function(var) {
        testResult <-
          if (x$problem[[var]]){
            if (nMax < length(displayValues[[var]]))
              paste(toString(ifelse(display == 'values' & !is.na(displayValues[[var]][1:nMax]), paste0('"', displayValues[[var]][1:nMax], '"'), displayValues[[var]][1:nMax])), '(and', length(displayValues[[var]]) - nMax, 'more)')
            else 
              paste(toString(ifelse(display == 'values' & !is.na(displayValues[[var]]), paste0('"', displayValues[[var]], '"'), displayValues[[var]])))
          } else x$message[[var]]
        return(c(var, testResult))
      })
      
      outMatrix <- matrix(nrow = length(varList),
                          ncol = 2,
                          byrow = TRUE,
                          data = message)
      colnames(outMatrix) <- c('Variables', paste('Suspected', x$testName))
      pander::pandoc.table(outMatrix, justify = 'left')
      invisible(x)
    } else {
      for (var in varList) {
        mes <- 
          if (!x$problem[[var]]) x$message[[var]]
        else messageGenerator.306(message = x$message[[var]], displayName = display, displayValues = displayValues[[var]], nMax = nMax)
        
        cat('-', var, ':', mes,'\n')
      }
      invisible(x)
    }
  }
}

print.checkResult.306.all_check <- function(x, table = TRUE, nMax = 5, display = c('values', 'indexes')){
  
  #### Argument check ####
  if (!requireNamespace('pander', quietly = TRUE)) {
    warning('pander is required to draw table, set table to false.')
    table <- FALSE
  }
  
  if (any(is.na(nMax), is.null(nMax), nMax < 0)) nMax <- 5
  
  #### Printing method ####
  testList <- names(x)
  display <- match.arg(display)
  displayValues <- switch(display,
                          'values' = sapply(testList, function(test) unique(x[[test]]$problemValues)),
                          'indexes' = sapply(testList, function(test) x[[test]]$problemIndexes)
                          )
  if (table){
    message <- sapply(testList,
                      function(test){
                        this <- x[[test]]
                        if (!length(this$problem)) '-'
                        else if (this$problem) {
                          if (nMax < length(displayValues[[test]]))
                              sprintf('%s (and %d more)',
                                      toString(ifelse(!is.na(displayValues[[test]][1:nMax]), 
                                                      if (display == 'values') 
                                                        paste0('"', displayValues[[test]][1:nMax], '"')
                                                      else
                                                        displayValues[[test]][1:nMax])),
                                      length(displayValues[[test]]) - nMax)
                          else toString(displayValues[[test]])
                        } else this$message
                      })
    message <- cbind(testList, message)
    outMatrix <- matrix(nrow = length(testList),
                        ncol = 2,
                        byrow = FALSE,
                        data = message)
    colnames(outMatrix) <- c('Test', 'Result')
    pander::pandoc.table(outMatrix, justify = 'left')
  } else {
    for (test in testList) {
      this <- x[[test]]
      mes <-
        if (length(this$problem)){
          if (this$problem) messageGenerator.306(message = this$message, displayName = display, displayValues = displayValues[[test]], nMax = nMax)
          else this$message
      }
      cat('-', test, ':\t', mes, '\n\n')
    }
  }
  
  invisible(x)
}

print.checkResult.306.cleanify <- function(x, tests = getTestList(x), table = TRUE, nMax = 5, display = c('values', 'keys', 'indexes'), plain.ascii = TRUE, res = FALSE,...){
  
  #### Argument check ####
  if (!requireNamespace('pander', quietly = TRUE)) {
    warning('pander is required to draw table, set table to false.')
    table <- FALSE
  }
  if (any(is.na(nMax), is.null(nMax), nMax < 0)) nMax <- 5
  varList <- names(x)
  testList <- tests
  display <- match.arg(display)
  displayValues <- switch(display,
                          'values' = expand_map(.x = varList, .y = testList, function(v,t, u=x) unique(u[[v]][t][[1]]$problemValues), dimnames = list(testList, varList)),
                          'keys' = expand_map(.x = varList, .y = testList, function(v, t, u=x) u[[v]][t][[1]]$problemKeys, dimnames = list(testList, varList)),
                          'indexes' = expand_map(.x = varList, .y = testList, function(v, t, u=x) u[[v]][t][[1]]$problemIndexes, dimnames = list(testList, varList)))
   #### Printing methods ####
  if (res) {
    return(get_res(x))
  } else{
    if (table){
      outMatrix <- expand_map(varList, testList, function(var, test, u = x){
        this <- u[[var]][test][[1]]
        if (!length(this$problem)) '-'
        else if (this$problem) {
          if (nMax < length(displayValues[[test, var]]))
            sprintf('%s (and %d more)',
                    toString(ifelse(!is.na(displayValues[[test,var]][1:nMax]), 
                                    if (display == 'values') 
                                      paste0('"', displayValues[[test,var]][1:nMax], '"')
                                    else
                                      displayValues[[test,var]][1:nMax],
                                    NA)),
                    length(displayValues[[test,var]]) - nMax)
          else toString(displayValues[[test,var]])
        } else this$message
      })
      pander::pandoc.table(outMatrix, justify = 'left', row.names = testList, col.names = varList, plain.ascii = plain.ascii, ...)
    } else {
      for (var in varList){
        cat('+', var, '\n')
        for (test in testList){
          this <- x[[var]][test][[1]]
          mes <-
            if (length(this$problem)){
              if (this$problem) messageGenerator.306(message = this$message, displayName = display, displayValues = displayValues[[test, var]], nMax = nMax)
              else this$message
            } 
          if (!is.null(mes)) cat('\t -', test, ':', mes, '\n')
        }
        cat('\n')
      }
    }
    invisible(x)
  }
}

getDefaultOptions <- function(v.type){
  defaultOpt <- list(whitespaces = list(),
                     doubleWSP = list(),
                     outliers = list(v.type = v.type,
                                     model = 'adjusted', skewParam = list(a = -4, b = 3),
                                     customFn = NULL,
                                     accept.negative = FALSE, accept.zero = FALSE
                     ),
                     loners = list(v.type = v.type,
                                   accept.dateTime = FALSE, threshold = 5, upLimit = 0.7
                     ),
                     binary = list(upLimit = 0.5
                     ),
                     missing =  list(),
                     spelling = list(v.type = v.type,
                                     upLimit = 0.5
                     )
  )
  return(defaultOpt)
}

getDefaultFn <- function(v, model = c('adjusted', 'boxplot', 'custom')){
  requireNamespace('robustbase')
  MC <- robustbase::mc(v)
  mean <- mean(v)
  quartiles <- stats::quantile(v)
  min <- quartiles[[1]]
  Q1 <- quartiles[[2]]
  med <- quartiles[[3]]
  Q3 <- quartiles[[4]]
  max <- quartiles[[5]]
  IQR <- Q3 - Q1
  SIQRl <- med - Q1
  SIQRu <- Q3 - med
  QS <- ((SIQRu - SIQRl))/IQR
  paramList <- c(MC = MC, mean = mean, med = med, 
             min = min, Q1 = Q1, Q3 = Q3, max = max, 
             IQR = IQR, SIQRl = SIQRl, SIQRu = SIQRu, QS = QS)
  
  model <- match.arg(model)
  fnList <- switch(model,
                   'adjusted' = c(parse(text = 'Q1 - 1.5*exp(a*MC)*IQR'), parse(text = 'Q3 + 1.5*exp(b*MC)*IQR')),
                   'boxplot' = c(parse(text = 'Q1 - 1.5*IQR'), parse(text = 'Q3 + 1.5*IQR')),
                   'custom' = NULL)
  
  out <- structure(list(fnList = fnList, paramList = paramList), class = 'robustFn')
  return(out)
}

opt <- function(...){
  # if ('all_check' %in% as.character(sys.call(1)) | 'cleanify' %in% as.character(sys.call(1))) quo_to_list(...)
  # else stop('This helper function can only be used inside either "cleanify" or "all_check"')
  quo_to_list(...)
}

setCustomFn <- function(fn = NULL, param = NULL){
  fnList <- if(!is.null(fn)) str_to_expr(fn) else NULL
  paramList <- if(!is.null(param)) str_to_expr(param) else NULL
  
  out <- structure(list(fnList = fnList, paramList = paramList), class = 'robustFn')
  return(out)
}

intelliRep <- function(v, simplify = TRUE){
  if (!is.atomic(v)) stop('v should be an atomic vector.')
  v.unique <- unique(na.omit(v))
  nRep <- sapply(v.unique, function(v.this) sum(na.omit(v) == v.this))
  nRep.unique <- unique(nRep)
  if (simplify) return(nRep.unique)
  else return(nRep)
}

intelliIsKey <- function(v, threshold = 1, repNo = 1){
  if (!is.atomic(v)) stop('v should be an atomic vector.')
  vector.rep <- intelliRep(v)
  is.key <- length(vector.rep) == 1 && (if (!is.null(repNo)) vector.rep[[1]] == repNo else TRUE)
  deltaL <- length(v) %% length(unique(v))
  is.key <- is.key & deltaL < threshold
  return(list(is.key = is.key, deltaL = deltaL))
}

intelliKey <- function(df, threshold = 1, repNo = 1, showAll = FALSE){
  df <- as.data.frame(df)
  if (ncol(df) < 2) return(NULL)
  vars <- colnames(df)
  
  are.Keys <- lapply(as.list(vars), function(var) return(intelliIsKey(df[[var]], threshold = threshold, repNo = repNo)))
  are.keys <- sapply(are.Keys, function(is.Key) return(is.Key$is.key))
  deltaL <- sapply(are.Keys, function(is.Key) return(is.Key$deltaL))
  
  if (any(are.keys)){
    if (showAll) {
      if (any(deltaL[are.keys] > 0)) 
        warning(paste(vars[are.keys][deltaL[are.keys] > 0], 'is treated as a key but has', deltaL[are.keys][deltaL[are.keys] > 0], 'repeated observations \n'))
      return(vars[are.keys]) 
    } else {
      if (deltaL[are.keys][1] > 0)
        warning(paste(vars[are.keys][1], 'is treated as a key but has', deltaL[are.keys][1], 'repeated observations \n'))  
      return(vars[are.keys][[1]])
    }
  } else return(NULL)
}

intelliType <- function(df, threshold = 1){
  ##### Take var names #####
  df <- as.data.frame(df)
  vars <- colnames(df)
  
  ##### Set regex patterns #####
  pattern.numeric <- '(^-?|^)((\\d+\\.?$)|(\\d*\\.\\d+$))'
  pattern.lang <- '^[A-Za-z\\s(),.!?\\\\/&\\_\\-]+$'
  pattern.dateTime <- '((\\s|^)((([12]\\d{3}|\\d{2})[-\\/.](0?[1-9]|[12]\\d|3[01])[-\\/.](0?[1-9]|[12]\\d|3[01]))|((0?[1-9]|[12]\\d|3[01])[-\\/.](0?[1-9]|[12]\\d|3[01])[-\\/.]([12]\\d{3}|\\d{2}))))|((\\s|^)([0-2]?\\d)\\s?[:]\\s?\\d{1,2})'
  
  ##### Check whether variables match any pattern #####
  res <- lapply(vars, function(var){
    this <- df[[var]]
    this.noNA <- na.blank.omit(this)
    if (length(this.noNA)){
      is.num <- grepl(pattern.numeric, this.noNA, perl = TRUE)
      is.num <- sum(is.num)/length(is.num) >= threshold
      is.lang <- grepl(pattern.lang, this.noNA, perl = TRUE)
      is.lang <- sum(is.lang)/length(is.lang) >= threshold
      is.dateTime <- grepl(pattern.dateTime, this.noNA, perl = TRUE)
      is.dateTime <- sum(is.dateTime)/length(is.dateTime) >= threshold
      out <- c('numeric', 'lang', 'dateTime')[c(is.num, is.lang, is.dateTime)]
      if (!length(out)) out <- 'other'
    } else out <- NULL
    return(out)
  })
  
  
  ###### Detect key Var by calling intelliKey #####
  if (ncol(df) > 1) {
    names(res) <- vars
    keyName <- intelliKey(df)
    if (length(keyName)) res[[keyName]] <- append(res[[keyName]], 'key', after = 0)
  }
  
  if (length(res) < 2) res <- unlist(res)

  return(res)
}

intelliIsCompatible <- function(v = NULL, v.type = NULL, test = c('whitespaces', 'doubleWSP', 'outliers', 'loners', 'binary', 'missing', 'spelling', 'case'), accept.dateTime = FALSE){
  test <- match.arg(test)
  if (is.null(v.type)) v.type <- c(intelliType(v), 'key'[intelliIsKey(v)$is.key])
  if (is.null(v.type) && is.null(v)) stop('Either v or v.type should be defined.')
  
  is.compatible <- switch(test, 
                          'whitespaces' = TRUE,
                          'doubleWSP' =  TRUE,
                          'outliers' = 'numeric' %in% v.type,
                          'loners' = (!('dateTime' %in% v.type) | accept.dateTime) & !('key' %in% v.type),
                          'binary' = !'key' %in% v.type & if (!missing(v)) length(unique(v)) < 5 else TRUE,
                          'missing' = TRUE,
                          'spelling' = 'lang' %in% v.type,
                          'case' = 'lang' %in% v.type
                          )
  return(is.compatible)
}

intelliCompatible <- function(data, tests = c('whitespaces', 'doubleWSP', 'outliers', 'loners', 'binary', 'missing', 'spelling', 'case'), accept.dateTime = FALSE){
  if (missing(accept.dateTime)) accept.dateTime <- FALSE 
  else {
    accept.dateTime <- as.logical(accept.dateTime)
    if (any(!length(accept.dateTime), is.na(accept.dateTime))) accept.dateTime <- FALSE
  }
  vars <- names(data)
  if (!is.null(vars)){
    out <- sapply(vars, function(var) {
      is.compatible <- sapply(tests, function(test) {
        intelliIsCompatible(v = data[[var]], test = test, accept.dateTime = accept.dateTime) 
      }, USE.NAMES = if(length(tests)<2) FALSE else TRUE)
    })
  }
  else out <- sapply(tests, function(test) {
    intelliIsCompatible(v = data, test = test) | if (test == 'loners') accept.dateTime else FALSE
  })
  return(out)
}

cR306_init <- function(testName, problem, problemValues, problemIndexes, problemKeys, message, res = NULL, outClass = c('checkResult.306', 'checkResult')){
  outClass <- match.arg(outClass)
  if (outClass == 'checkResult.306'){
    out <- list(testName = testName, 
                problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
    class(out) <- 'checkResult.306'
  } else {
    message <- messageGenerator.306(message = message, displayName = 'values', displayValues = problemValues, nMax = Inf)
    out <- list(problem = problem, problemValues = problemValues, message = message)
    class(out) <- 'checkResult'
  }
  return(out)
}

outliers_plot <- function(v, problemValues = outliers_check(v)$problemValues, plot.color = 'gray78', outliers.color = 'firebrick3', theme = theme_light(), gg.options = NULL, interactive = FALSE){
  if (missing(v)) stop('v and problemValues not provided')
  if (!require('ggplot2')) stop('ggplot2 is required.')
  if (interactive & !requireNamespace('plotly')) interactive <- FALSE
  x.label <- deparse(substitute(v))
  
  v.plot <- ggplot(mapping = aes(x = v)) + geom_density(color = plot.color, fill = plot.color) + theme + gg.options + xlab(label = x.label) + ylab(label = 'Count')
  if (all(!is.null(problemValues))) v.plot <- v.plot + geom_vline(xintercept = problemValues, color = outliers.color, alpha = 0.7)
  return(if (interactive) plotly::ggplotly(v.plot) else v.plot)
}

intelli_guessTime <- function(v, full = FALSE){
  requireNamespace('lubridate')
  models <- unlist(expand_map(c('ymd','mdy','dmy','ym','my','md','dm','y','m','d','','ydm','myd','dym'),
                              c('HMS','HM','MS','H','M','S'),
                              fn = paste0))
  
  fitted.model <- NULL
  for (model in models) {
    v.parse <- suppressWarnings(lubridate::parse_date_time(v, orders = model))
    if (length(na.omit(v)) == length(na.omit(v.parse))) {
      if (full) fitted.model <- append(fitted.model, model) else return(model) 
    }
  }
  
  if (length(fitted.model)) return(fitted.model)
  cat('No fitted order found')
}

time_consistency <- function(relativeTime, realTime, format = intelli_guessTime(realTime), window = 0){
  if (length(realTime) != length(relativeTime)) stop('Two length must agree!')
  requireNamespace('lubridate')
  realTime.Date <- as.Date(lubridate::parse_date_time(realTime, orders = format))
  delta.relativeTime <- sapply(relativeTime, function(time) time - relativeTime[[1]])
  delta.realTime <- sapply(realTime.Date, function(time) time - realTime.Date[[1]])
  delta.diff <- abs(delta.realTime - delta.relativeTime)
  test <- delta.diff > window 
  problem <- any(test)
  realVal <- realTime[test]
  relVal <- relativeTime[test]
  idx <- seq_along(relativeTime)[test]
  return(list(problem = problem, x = relVal, y = realVal, idx = idx))
}

# Check functions for 1 vector #####

whitespaces_check <- function(v, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the whitespaces_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Pre-test Notice ####
  
  #### Apply the check on specific variable####
  v <- as.character(v)
  test <- dataMaid::identifyWhitespace(v, nMax = Inf)
  
  if (test$problem) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ leading/trailing whitespaces:'
    problemValues <- test[[3]]
    problemIndexes <- which(v %in% test[[3]])
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'whitespaces', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

doubleWSP_check <- function(v, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use doubleWSP_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Pre-test Notice ####
  
  #### Apply the check on specific variable####
  v <- as.character(v)
  
  hasDoubleWSP <- grepl('\\s{2,}', v, perl = TRUE)
  
  if (any(hasDoubleWSP)) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ double whitespaces:'
    problemValues <- v[hasDoubleWSP]
    problemIndexes <- which(hasDoubleWSP)
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'doublespaces', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

outliers_check <- function(v, v.type = intelliType(v), model = c('adjusted', 'boxplot', 'custom'), skewParam = list(a = -4, b = 3), customFn = setCustomFn(), accept.negative = FALSE, accept.zero = FALSE, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the outliers_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  if (!is.character(v.type)) v.type <- intelliType(v.type)
  accept.negative <- as.logical(accept.negative)
  if (any(is.na(accept.negative), is.null(accept.negative))) accept.negative <- FALSE
  accept.zero = as.logical(accept.zero) 
  if (any(is.na(accept.zero), is.null(accept.zero))) accept.zero <- FALSE
  if (any(is.na(skewParam), is.null(skewParam))) skewParam <- list(a = -4, b = 3)
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  model <- match.arg(model)
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Check the input for compatibility ####
  is.supported <- intelliIsCompatible(v.type = v.type, test = 'outliers')
  if (!is.supported) stop('This type of variable is not supported by this check.')
  
  ##### Simple check for Model custom #####
  if (model == 'custom') {
    if (all(is.na(customFn))) customFn <- NULL
    if (is.null(customFn)) {
      message$notice <- append(message$notice, 'No custom function set. Switch back to "adjusted" model.')
      if (!silent) warning('No custom function set. Switch back to "adjusted" model.')
      model <- 'adjusted'
    }
    
    if (length(customFn$fnList) > 2) {
      message$notice <- append(message$notice, 'More than 2 functions provided. Only the first and second will be used.')
      if (!silent) warning('More than 2 functions provided. Only the first and second will be used.')
    }
  }
  
  ##### Model skew parameters preparation #####
  if (model == 'adjusted') {
    a <- 
      tryCatch(skewParam$a, error = function(e){
        if (!silent) warning('Parameter a is not well-defined. Switch back to default a = -4')
        message$notice <- append(message$notice, 'Parameter a is not well-defined. Switch back to default a = -4')
        return(-4)
      })
    
    b <- 
      tryCatch(skewParam$b, error = function(e){
        warning('Parameter b is not well-defined. Switch back to default b = 3')
        message$notice <- append(message$notice, 'Parameter b is not well-defined. Switch back to default b = 3')
        return(3)
      })
  }
  
  #### Apply the check on specific variable ####
  ##### Get default functions and parameters #####
  v <- suppressWarnings(as.numeric(v))
  v.noNA <- na.blank.omit(v)
  defaultFn <- getDefaultFn(v = v.noNA, model = model)
  
  if (model != 'custom') {
    fn.lower <- defaultFn$fnList[1]
    fn.upper <- defaultFn$fnList[2]
  }
  defaultParams <- defaultFn$paramList
  
  Q1 <- defaultParams['Q1']
  Q3 <- defaultParams['Q3']
  min <- defaultParams['min']
  max <- defaultParams['max']
  mean <- defaultParams['mean']
  med <- defaultParams['med']
  IQR <- defaultParams['IQR']
  SIQRl <- defaultParams['SIQRl']
  SIQRu <- defaultParams['SIQRu']
  QS <- defaultParams['QS']
  MC <- defaultParams['MC']
  
  ##### Get custom functions and parameters #####
  if (model == 'custom') {
    fn.lower <- customFn$fnList[1]
    fn.upper <- customFn$fnList[2]
    customParams <- customFn$paramList
    
    if (length(customParams))
      for (param in customParams) 
        tryCatch(eval(customParams),
                 error = function(e) stop('Parameters defining seems faulty. Stop.'))
  }
  
  tryCatch({
    upperLimit <- eval(fn.upper)
    lowerLimit <- eval(fn.lower)
  }, error = function(e) {
    stop('Cannot apply limit determinating functions. Please check if your have provided all custom parameters. Stop.\n Original error: ', e$message)
  })
  
  outlierPlaces <- v.noNA < lowerLimit | v.noNA > upperLimit
  if (!accept.negative) outlierPlaces <- outlierPlaces | (v.noNA < 0)
  if (!accept.zero) outlierPlaces <- outlierPlaces | (v.noNA == 0)
  
  if (any(outlierPlaces)) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ potential outliers'
    problemValues <- v.noNA[outlierPlaces]
    problemIndexes <- which(v %in% problemValues)
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'outliers', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

loners_check <- function(v, v.type = intelliType(v), accept.dateTime = FALSE, threshold = 5, upLimit = 0.7, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  if (!is.character(v.type)) v.type <- intelliType(v.type)
  accept.dateTime <- as.logical(accept.dateTime)
  if (any(is.null(accept.dateTime), is.na(accept.dateTime))) accept.dateTime <- FALSE
  threshold <- suppressWarnings(as.numeric(threshold))
  if (any(is.na(threshold), is.null(threshold))) threshold <- 5
  if (any(is.na(upLimit), is.null(upLimit))) upLimit <- 0.7
  if (upLimit > 1) while (upLimit > 1) upLimit <- upLimit/10
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  res <- v
  
  #### Check the input for compatibility ####
  is.supported <- intelliIsCompatible(v.type = v.type, test = 'loners') | accept.dateTime
  if (!is.supported) stop('Date/time variable is not supported by this check by default. \n You can override this behavior by setting accept.dateTime to TRUE')
  
  #### Apply the check on specific variable####
  v.noNA <- na.blank.omit(v)
  v.noNA <- as.factor(v.noNA)
  v.levels <- levels(v.noNA)
  is.loners <- table(v.noNA) < threshold
  
  if (any(is.loners)) {
    if (sum(is.loners)/length(is.loners) <= upLimit){
      problem <- TRUE
      message <- 'These $display$ might $behave$ potential loners:'
      problemValues <- v.levels[is.loners]
      problemIndexes <- which(v %in% problemValues)
    } else {
      problem <- FALSE
      message <- sprintf('We notice loners within your variable. However, this might be a wrong conclusion due to their overly high frequency (~%d%%).\n>>> Set upLimit to a higher value to override this behavior.', floor(upLimit * 100))
    }
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'loners', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

binary_check <- function(v, upLimit = 0.5, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  if (is.numeric(upLimit)) while (upLimit > 1) upLimit <- upLimit/10
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  res <- v
  
  #### Check the input for compatibility ####
  is.supported <- intelliIsCompatible(v, test = 'binary')
  if (!is.supported) stop('This is potentialy a key variable and should not be checked.')
  
  #### Apply the check on specific variable####
  v.noNA <- na.blank.omit(v)
  v.noNA <- factor(v.noNA, levels = sort(unique(v.noNA)))
  v.levels <- levels(v.noNA)
  is.binary <- length(v.levels) == 2
  
  if (!is.binary) {
    if (length(v.levels) < 2) {
      problem <- FALSE
      message <- paste('One-category variable:', v.levels)
    } else {
      table_tmp <- table(v.noNA)
      table_tmp.order <- order(table(v.noNA), decreasing = TRUE)
      is.suspected <- table_tmp.order > 2
      suspectedAmount <- sum(table_tmp[is.suspected])
      suspectedValues <- v.levels[is.suspected]
      if (upLimit == 'auto') upLimit <- sum(table_tmp[!is.suspected])/sum(table_tmp)
       
      if (suspectedAmount/length(v.noNA) <= upLimit){
        problem <- TRUE
        message <- 'This variable is not binary. These are suspected problematic $display$:'
        problemValues <- suspectedValues
        problemIndexes <- which(v %in% problemValues)
      } else {
        problem <- FALSE
        message <- sprintf('The number of problematic values is higher than expected (%d%%). This variable might be key or free-text.', floor(upLimit * 100))
      }
    }
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'binaries', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

missing_check <- function(v, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  res <- v
  
  #### Check the input for compatibility ####
  
  #### Apply the check on specific variable####
  test1 <- dataMaid::identifyMissing(as.character(v), nMax = Inf)
  test2 <- is.na(v)
  is.missing <- any(test1$problem, test2)
  
  if (is.missing) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ missing values:'
    problemValues <- unique(c(test1$problemValues, if (any(test2)) NA))
    problemIndexes <- c(which(is.na(v) | v %in% test1$problemValues))
    
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'missing data', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  
  return(out)
}

case_check <- function(v, v.type = intelliType(v), silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  if (!requireNamespace('hunspell')) stop('Please install hunspell package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  res <- NULL
  
  #### Check the input for compatibility ####
  is.supported <- intelliIsCompatible(v.type = v.type, test = 'case')
  if (!is.supported) stop('This is not a lingustic variable and is not compatible with the check.')
  
  #### Apply the check on variable ####
  v <- as.character(v)
  
  test <- dataMaid::identifyCaseIssues(v, nMax = Inf)
  
  if (test$problem){
    problem <- TRUE
    message <- 'These $display$ might $behave$ case issue:'
    problemValues <- test[[3]]
    problemIndexes <- which(v %in% test[[3]])
    res <- sapply(test[[3]], function(val) {
      val.lower <- unique(tolower(val))
      v.lower <- tolower(v)
      v.pos <- which(v.lower %in% val.lower)
      return(do.call(paste, as.list(append(v.pos, sep = '-'))))
    })
  } else {
    problem <- FALSE
    message <- 'No problems found'
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'case issues', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message, res = res)
  return(out)
}

spelling_check <- function(v, v.type = intelliType(v), upLimit = 0.5, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  if (!requireNamespace('hunspell')) stop('Please install hunspell package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(upLimit), is.null(upLimit), !is.numeric(upLimit))) upLimit <- 0.5
  else while (upLimit > 1) upLimit <- upLimit/10
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Check the input for compatibility ####
  is.supported <- intelliIsCompatible(v.type = v.type, test = 'spelling')
  if (!is.supported) stop('This is not a lingustic variable and is not compatible with the check.')
  
  #### Apply the check on variable ####
  v.noNA <- na.blank.omit(v)
  v.noNA <- as.character(v.noNA)
  
  strings <- hunspell::hunspell_parse(v.noNA)
  is.Wrong <- sapply(strings, function(string){
    !hunspell::hunspell_check(string)
  })
  is.Wrong.wholeWord <- sapply(is.Wrong, any)
  wrongWords.wholeWord <- v.noNA[is.Wrong.wholeWord]
  
  if (any(is.Wrong.wholeWord)){
    if (sum(is.Wrong.wholeWord)/length(v.noNA) <= upLimit) {
      problem <- TRUE
      message <- 'These $display$ have potential spelling issues:'
      problemValues <- wrongWords.wholeWord
      problemIndexes <- which(v %in% problemValues)
    } else {
      problem <- FALSE
      message <- sprintf('The amount of problematic values has surpass the upper limit (%d%%). We aren\'t sure that this is English. You can adjust this limit by changing upLimit.', floor(upLimit * 100))
    }
  } else {
    problem <- FALSE
    message <- 'No problems found'
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'spelling issues', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message, res = is.Wrong)
  return(out)
}



pair_check <- function(data, keyVar, x, y, group_by = NULL, FUN, ..., silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if(missing(FUN)) stop('Check FUNction should be defined!')
  if (missing(group_by)) group_by <- NULL
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  hasData <- !missing(data)
  if (missing(keyVar)) keyVar <- NULL
  if (hasData & is.null(keyVar)) {
    keyVar <- intelliKey(data)
    if (is.null(keyVar)) {
      keyVar <- 'index'
      data <- cbind(index = 1:nrow(data), data)
    } 
  }
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  problemKeys <- NULL
  message <- list()
  
  #### Apply the check on variables ####
  if (!is.null(group_by)){
    if (hasData) {
      if (!is.character(group_by)) groups.full <- data[[rlang::quo_name(rlang::enquo(group_by))]]
      else groups.full <- data[[group_by]]
      groups <- unique(groups.full)
      if (!is.character(x)) x <- data[[rlang::quo_name(rlang::enquo(x))]] else x <- data[[x]]
      if (!is.character(y)) y <- data[[rlang::quo_name(rlang::enquo(y))]] else y <- data[[y]]
    } else {
      groups.full <- group_by
      groups <- unique(group_by)
    }
    
    checkRes <- lapply(groups,
                       function(group){
                         x.group <- x[groups.full == group]
                         y.group <- y[groups.full == group]
                         
                         FUN(x.group, y.group, ...)
                       })
    
    problem <- any(sapply(checkRes, function(eachRes) eachRes$problem))
    if (problem) {
      problemValues <- unlist(sapply(checkRes, function(eachRes) paste(eachRes$x, eachRes$y, sep = ' - ')))
      problemIndexes <- unlist(sapply(seq_along(checkRes), 
                                      function(i){
                                        eachRes <- checkRes[[i]]
                                        if (eachRes$problem){
                                          res.name <- groups[i]
                                          res.x <- eachRes$x
                                          res.y <- eachRes$y
                                          which(groups.full == res.name & x %in% res.x & y %in% res.y)
                                        }
                                      }
                               ))
      if (hasData) problemKeys <- data[[keyVar]][problemIndexes]
      message <- 'These $display$ have potential pairing issue:'
    } else message <- 'No problem found'
  } else {
    if (hasData){
      x <- data[[rlang::quo_name(rlang::enquo(x))]]
      y <- data[[rlang::quo_name(rlang::enquo(y))]]
    }
    checkRes <- FUN(x, y, ...)
    problem <- checkRes$problem
    if (problem){
      problemValues <- paste(checkRes$x, checkRes$y, sep = ' - ')
      problemIndexes <- checkRes$idx
      if (hasData) problemKeys <- data[[keyVar]][problemIndexes]
      message <- 'These $display$ have potential pairing issue:'
    } else message <- 'No problem found'
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'pairing issues', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = if (hasData & !is.null(keyVar)) problemKeys else problemIndexes, message = message)
  return(out)
}

redundancy_check <- function(v, repNo = 2, upLimit = 0.5, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the loners_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  repNo <- suppressWarnings(as.numeric(repNo))
  if (is.na(repNo) | is.null(repNo)) repNo <- 2 
  if (any(is.na(upLimit), is.null(upLimit), !is.numeric(upLimit))) upLimit <- 0.5
  else while (upLimit > 1) upLimit <- upLimit/10
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Preparation of Outputs ####
  problem <- logical(0)
  problemValues <- NULL
  problemIndexes <- NULL
  res <- NULL
  message <- list()
  
  #### Apply the check on variable ####
  if (any(intelliRep(v) != repNo)) {
    # diff <- setdiff(intelliRep(v), repNo)
    rep.full <- intelliRep(v, simplify = FALSE)
    diff <- setdiff(rep.full, repNo)
    rep.pos <- which(rep.full %in% diff)
    v.val <- unique(v)[rep.pos]
    v.pos <- sapply(v.val, function(val){
      pos <- which(v == val)
      pos <- append(pos, c(sep = '-'))
      return(do.call(paste, as.list(pos)))
    })

    # rep.pos <- which(rep.full %in% diff)
    # v.pos <- sapply(rep.pos, function(pos) sum(rep.full[1:pos-1]) + 1)
    if (length(v.pos)/length(v) <= upLimit){
      problem <- TRUE
      problemValues <- v.val
      problemIndexes <- v.pos
      message <-  'These $display$ might $behave$ potential redundancy:'
      res <- structure(rep.full[rep.pos], names = v.val)
    } else {
      problem <- FALSE
      message <- sprintf('We notice redundancy within your variable. However, this might be a wrong conclusion due to their overly high frequency (~%d%%).\n>>> Set upLimit to a higher value to override this behavior.', floor(upLimit * 100))
    }
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'redundancy issues', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message, res = res)
  return(out)
}

all_check <- function(v, v.type = intelliType(v), checks = c('missing', 'whitespaces', 'doubleWSP', 'spelling', 'outliers', 'binary', 'loners', 'case'), options = opt(), silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
    
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use cleanify instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  if (!requireNamespace('hunspell')) stop('Please install hunspell package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (missing(options)) options <- list()
  
  #### Preparation of the output ####
  res <- list()
  
  #### Get default options ####
  defaultOpt <- getDefaultOptions(v.type)
  
  #### Update options ####
  options <- update.list(defaultOpt, options, add = TRUE)
  
  #### Check the input for compatibilty ####
  is.supported <- intelliCompatible(data = v, tests = checks, accept.dateTime = options$loners$accept.dateTime)
  checks <- checks[is.supported]
  
  #### Run the check ####
  for (i in 1:length(checks)) {
    res[i] <- as.list(res[i])
    res[[i]] <- switch(checks[[i]],
                       missing = missing_check(v, silent = silent, outClass = outClass),
                       whitespaces = whitespaces_check(v, silent = silent, outClass = outClass),
                       doubleWSP = doubleWSP_check(v, silent = silent, outClass = outClass),
                       outliers = outliers_check(v, v.type = v.type,
                                                 model = options$outliers$model, skewParam = options$outliers$skewParam,
                                                 customFn = option$outliers$customFn,
                                                 accept.negative = options$outliers$accept.negative, accept.zero = options$outliers$accept.zero,
                                                 silent = silent, outClass = outClass),
                       loners = loners_check(v, v.type = v.type,
                                             accept.dateTime = options$loners$accept.dateTime,
                                             threshold = options$loners$threshold, upLimit = options$loners$upLimit,
                                             silent = silent, outClass = outClass),
                       binary = binary_check(v, upLimit = options$binary$upLimit,
                                             silent = silent, outClass = outClass),
                       spelling = spelling_check(v, v.type,
                                                 upLimit = options$spelling$upLimit,
                                                 silent = silent, outClass = outClass),
                       case = case_check(v, v.type,
                                         silent = silent, outClass = outClass))
  }
  names(res) <- checks
  class(res) <- c('checkResult.306.all_check')
  return(res)
}

# Fix functions for 1 vector ####

whitespaces_fix <- function(v, checkResult = whitespaces_check(v), silent = FALSE){
  
  #### Arguments check ####
  if (any(!'checkResult.306' %in% class(checkResult), is.null(checkResult), is.na(checkResult))) stop('checkResult must be a 306 checkResult.')
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the whitespaces_scan instead.')
  silent <- as.logical(silent)
  if (any(is.na(silent), !length(silent))) silent <- FALSE
  res <- v
  
  #### Do the fix ####
  problem <- checkResult$problem
  if (problem) {
    if (!requireNamespace('stringr')) stop('Need "stringr" package')
    res <- stringr::str_trim(res, side = "both")
  } else {
    if (!silent) cat('No problems found')
  }
  return(res)
}

doubleWSP_fix <- function(v, checkResult = doubleWSP_check(v), silent = FALSE){
  
  #### Arguments check ####
  if (any(!'checkResult.306' %in% class(checkResult), is.null(checkResult), is.na(checkResult))) stop('checkResult must be a 306 checkResult.')
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the doubleWSP_scan instead.')
  silent <- as.logical(silent)
  if (any(is.na(silent), !length(silent))) silent <- FALSE
  res <- v
  
  #### Do the fix ####
  problem <- checkResult$problem
  if (problem) {
    if (!requireNamespace('stringr')) stop('Need "stringr" package')
    res <- stringr::str_squish(res)
  } else {
    if (!silent) cat('No problems found')
  }
  return(res)
}

missing_fix <- function(v, checkResult = missing_check(v), silent = FALSE){
  
  #### Arguments check ####
  if (any(!'checkResult.306' %in% class(checkResult), is.null(checkResult), is.na(checkResult))) stop('checkResult must be a 306 checkResult.')
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the missing_scan instead.')
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  res <- v
  
  #### Do the fix ####
  problem <- checkResult$problem
  if (problem) {
    problemValues <- checkResult$problemValues
    res <- ifelse(v %in% problemValues, NA, v) 
  } else {
    if (!silent) cat('No problems found')
  }
  return(res)
}

spelling_fix <- function(v, checkResult = spelling_check(v), outType = c('suggest_list', 'auto_fixed'), silent = FALSE){
  
  #### Arguments check ####
  if (any(!'checkResult.306' %in% class(checkResult), is.null(checkResult), is.na(checkResult))) stop('checkResult must be a 306 checkResult.')
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use spelling_scan instead.')
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  outType <- match.arg(outType)
  res <- v
  
  #### Do the fix ####
  problem <- checkResult$problem
  if (problem) {
    v <- as.character(v)
    v <- hunspell::hunspell_parse(v)
    problemIndexes <- checkResult$problemIndexes
    issueMatrix <- checkResult$res
    suggestList <- lapply(1:length(v), function(i){
      lapply(1:length(v[[i]]), function(j) {
        if (issueMatrix[[i]][[j]]) return(unlist(hunspell::hunspell_suggest(v[[i]][[j]]))) else return(v[[i]][[j]])
      })
    })
    
    if (outType == 'suggest_list') res <- suggestList
    else res <- sapply(1:length(v), function(i){
      do.call(paste, 
              lapply(suggestList[[i]], function(w) return(w[[1]])))
    })
  } else {
    if (!silent) cat('No problems found')
  }
  return(res)
}

# Scan functions for whole data frame ######

whitespaces_scan <- function(data, keyVar = intelliKey(data), subset = names(data), fix = FALSE, silent = FALSE, ...) {

  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  fix <- as.logical(fix)
  if (any(is.na(fix), !length(fix))) fix <- TRUE
  silent <- as.logical(silent)
  if (any(is.na(silent), !length(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  
  #### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Quick check on the data provided ####
  
  #### Pre-test Notice ####
  m.temp <- 'Keep in mine that only character-pattern columns will be checked.'
  message$notice <- append(message$notice, m.temp)
  if (!silent) message(m.temp)
  
  #### Itterate through variables and apply the check ####
  for (c in subset){
    data[[c]] <- as.character(data[[c]])
    test <- whitespaces_check(v = data[[c]], silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    
    if (fix) res[[c]] <- whitespaces_fix(res[[c]], checkResult = test, silent = TRUE)
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'whitespaces',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

doubleWSP_scan <- function(data, keyVar = intelliKey(data), subset = names(data), fix = FALSE, silent = FALSE, ...){
  
  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  fix <- as.logical(fix)
  if (any(is.na(fix), is.null(fix))) fix <- TRUE
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  #### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  
  #### Illiterate through variables and apply the check ####
  for (c in subset){
    test <- doubleWSP_check(v = data[[c]], silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    
    if (fix) res[[c]] <- doubleWSP_fix(res[[c]], checkResult = test, silent = TRUE)
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'doublespaces',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

outliers_scan <- function(data, keyVar = intelliKey(data), subset = names(data), model = c('adjusted', 'boxplot', 'custom'), skewParam = list(a = -4, b = 3), customFn = setCustomFn(), accept.negative = FALSE, accept.zero = FALSE, silent = FALSE, ...){
  
  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) subset <- names(data)
  accept.negative <- as.logical(accept.negative)
  if (any(is.na(accept.negative), is.null(accept.negative))) accept.negative <- FALSE
  accept.zero = as.logical(accept.zero) 
  if (any(is.na(accept.zero), is.null(accept.zero))) accept.zero <- FALSE
  if (any(is.na(skewParam), is.null(skewParam))) skewParam <- list(a = -4, b = 3)
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  model <- match.arg(model)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  ##### Simple check for Model custom #####
  if (model == 'custom') {
    if (all(is.na(customFn))) customFn <- NULL
    if (is.null(customFn)) {
      message$notice <- append(message$notice, 'No custom function set. Switch back to "adjusted" model.')
      if (!silent) warning('No custom function set. Switch back to "adjusted" model.')
      model <- 'adjusted'
    }
    
    if (length(customFn$fnList) > 2) {
      message$notice <- append(message$notice, 'More than 2 functions provided. Only the first and second will be used.')
      if (!silent) warning('More than 2 functions provided. Only the first and second will be used.')
    }
  }
  
  ##### Model skew parameters preparation #####
  if (model == 'adjusted') {
    a <- 
      tryCatch(skewParam$a, error = function(e){
        if (!silent) warning('Parameter a is not well-defined. Switch back to default a = -4')
        message$notice <- append(message$notice, 'Parameter a is not well-defined. Switch back to default a = -4')
        return(-4)
      })
    
    b <- 
      tryCatch(skewParam$b, error = function(e){
        warning('Parameter b is not well-defined. Switch back to default b = 3')
        message$notice <- append(message$notice, 'Parameter b is not well-defined. Switch back to default b = 3')
        return(3)
      })
  }
  
  #### Pre-test Notice ####
  message$notice <- append(message$notice, 'Keep in mine that only numeric-pattern columns will be checked.')
  if (!silent) message('Keep in mine that only numeric-pattern columns will be checked.')
  
  #### Quick check on the data provided ####
  vars <- names(data)
  are.compatible <- intelliCompatible(data, test = 'outliers')
  vars.supported <- vars[are.compatible]
  subset.supported <- intersect(subset, vars.supported)
  if (!setequal(subset.supported, subset)) {
    w.temp <- sprintf('Some variables are not supported and will be omitted from this check: %s', toString(setdiff(subset, subset.supported)))
    message$notice <- append(message$notice, w.temp)
    if (!silent) message(w.temp)
    subset <- subset.supported
  }
  
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    data[[c]] <- suppressWarnings(as.numeric(data[[c]]))
    
    test <- outliers_check(v = data[[c]], v.type = 'numeric', model = model, skewParam = skewParam, customFn = customFn, accept.negative = accept.negative, accept.zero = accept.zero, silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    res[[c]] <- test$res
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'outliers',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

loners_scan <- function(data, keyVar = intelliKey(data), subset = names(data), accept.dateTime = FALSE, threshold = 5, upLimit = 0.75, silent = FALSE, ...){
  
  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  
  accept.dateTime <- as.logical(accept.dateTime)
  if (any(is.null(accept.dateTime), is.na(accept.dateTime))) accept.dateTime <- FALSE
  threshold <- suppressWarnings(as.numeric(threshold))
  if (any(is.na(threshold), is.null(threshold))) threshold <- 5
  if (any(is.na(upLimit), is.null(upLimit))) upLimit <- 0.75
  if (upLimit > 1) while (upLimit > 1) upLimit <- upLimit/10
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  message$notice <- append(message$notice, 'Keep in mine that date-time-pattern columns will NOT be checked.')
  if (!silent) message('Keep in mine that date-time-pattern columns will NOT be checked.')
  
  #### Quick check on the data provided ####
  dataTypes <- intelliType(data)
  vars <- names(data)
  vars.supported <- vars[vars %in% names(dataTypes[sapply(dataTypes, function(dataType) return(!'dateTime' %in% dataType & !'key' %in% dataType))])]
  subset.supported <- intersect(subset, vars.supported)
  if (!setequal(subset.supported, subset)) {
    w.temp <- sprintf('Some variables are not supported and will be omitted from this check: %s. You can partialy override this behavior by changing accept.dateTime to TRUE.', toString(setdiff(subset, subset.supported)))
    message$notice <- append(message$notice, w.temp)
    if (!silent) message(w.temp)
    subset <- subset.supported
  }
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    data[[c]] <- suppressWarnings(as.factor(data[[c]]))
    
    test <- loners_check(v = data[[c]],accept.dateTime = accept.dateTime, silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'loners',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

binary_scan <- function(data, keyVar = intelliKey(data), subset = names(data), upLimit = 'auto', silent = FALSE, ...){
  
  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  
  if (is.numeric(upLimit)) {if (upLimit > 1) while (upLimit > 1) upLimit <- upLimit/10}
  else if (any(is.na(upLimit), is.null(upLimit), upLimit != 'auto')) upLimit <- 0.5
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  
  #### Quick check on the data provided ####
  dataTypes <- intelliType(data)
  vars <- names(data)
  vars.supported <- vars[!vars %in% intelliKey(data, showAll = TRUE)]
  subset.supported <- intersect(subset, vars.supported)
  if (!setequal(subset.supported, subset)) {
    w.temp <- sprintf('Some variables are not supported and will be omitted from this check: %s.', toString(setdiff(subset, subset.supported)))
    message$notice <- append(message$notice, w.temp)
    if (!silent) message(w.temp)
    subset <- subset.supported
  }
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    data[[c]] <- suppressWarnings(as.factor(data[[c]]))
    
    test <- binary_check(v = data[[c]], upLimit = upLimit, silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'binaries',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

missing_scan <- function(data, keyVar = intelliKey(data), subset = names(data), fix = FALSE, silent = FALSE, ...){
  
  #### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  
  fix <- as.logical(fix)
  if (any(is.na(fix), is.null(fix))) fix <- TRUE
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  
  #### Quick check on the data provided ####
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    test <- missing_check(v = data[[c]], silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    
    if (fix) res[[c]] <- missing_fix(res[[c]], checkResult = test, silent = TRUE)
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'missing data',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

spelling_scan <- function(data, keyVar = intelliKey(data), subset = names(data), upLimit = 0.5, fix = FALSE, silent = FALSE, ...){
  
  ### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  
  if (any(is.na(upLimit), is.null(upLimit))) upLimit <- 0.5
  if (upLimit > 1) while (upLimit > 1) upLimit <- upLimit/10
  fix <- as.logical(fix)
  if (any(is.na(fix), is.null(fix))) fix <- FALSE
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  message$notice <- append(message$notice, 'Keep in mine that only potential meaningful text columns will be checked.')
  if (!silent) message('Keep in mine that only potential meaningful text columns will be checked.')
  
  #### Quick check on the data provided ####
  dataTypes <- intelliType(data)
  vars <- names(data)
  vars.supported <- vars[vars %in% names(dataTypes[sapply(dataTypes, function(dataType) return('lang' %in% dataType))])]
  subset.supported <- intersect(subset, vars.supported)
  if (!setequal(subset.supported, subset)) {
    w.temp <- sprintf('Some variables are not supported and will be omitted from this check: %s', toString(setdiff(subset, subset.supported)))
    message$notice <- append(message$notice, w.temp)
    if (!silent) message(w.temp)
    subset <- subset.supported
  }
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    data[[c]] <- suppressWarnings(as.character(data[[c]]))
    
    test <- spelling_check(v = data[[c]], v.type = 'lang', upLimit = upLimit, silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    
    if (fix) res[[c]] <- spelling_fix(v = data[[c]], checkResult = test, outType = 'auto_fixed', silent = TRUE)
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'spelling issues',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

case_scan <- function(data, keyVar = intelliKey(data), subset = names(data), silent = FALSE, ...){
  ### Argument check ####
  if (missing(data)) stop('`data` cannot be empty.') 
  else if (is.atomic(data)) {
    name <- deparse(substitute(data))
    data <- as.data.frame(data)
    colnames(data) <- name
  }
  if (any(is.na(subset), is.null(subset))) {
    if (is.null(names(data))) subset <- 1:ncol(data) else subset = names(data)
  }
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- cbind(index = 1:nrow(data), data)
  
  ### Preparation of Output ####
  problem <- list()
  problemValues <- list()
  problemIndexes <- list()
  problemKeys <- list()
  message <- list()
  res <- data
  
  #### Pre-test Notice ####
  message$notice <- append(message$notice, 'Keep in mine that only potential meaningful text columns will be checked.')
  if (!silent) message('Keep in mine that only potential meaningful text columns will be checked.')
  
  #### Quick check on the data provided ####
  dataTypes <- intelliType(data)
  vars <- names(data)
  vars.supported <- vars[vars %in% names(dataTypes[sapply(dataTypes, function(dataType) return('lang' %in% dataType))])]
  subset.supported <- intersect(subset, vars.supported)
  if (!setequal(subset.supported, subset)) {
    w.temp <- sprintf('Some variables are not supported and will be omitted from this check: %s', toString(setdiff(subset, subset.supported)))
    message$notice <- append(message$notice, w.temp)
    if (!silent) message(w.temp)
    subset <- subset.supported
  }
  
  #### Itterate over variables and apply the check ####
  for (c in subset){
    data[[c]] <- suppressWarnings(as.character(data[[c]]))
    
    test <- case_check(v = data[[c]], v.type = 'lang', silent = silent)
    
    problem[[c]] <- test$problem
    message[[c]] <- test$message
    problemValues[[c]] <- test$problemValues
    problemIndexes[[c]] <- test$problemIndexes
    problemKeys[[c]] <- data[[keyVar]][test$problemIndexes]
    res[[c]] <- test$res
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'case issues',
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemKeys, message = message, res = res)
  if (!silent) return(print(out, ...))
  return(out)
}

# Do-every-thing function ######

cleanify <- function(data, keyVar = intelliKey(data), checks = c('missing', 'whitespaces', 'doubleWSP', 'spelling', 'outliers', 'binary', 'loners', 'case'), options = opt(), silent = FALSE, ...){
  
  #### Arguments check ####  
  if (missing(data)) stop('`data` cannot be empty.') else {
    data <- as.data.frame(data)
    if (ncol(data) < 1) stop('Empty dataset')
    if (ncol(data) == 1) {
      return(cleanify_check(data))
    }
  }
  
  if (any(!length(checks), is.na(checks))) checks <- c('missing', 'whitespaces', 'doubleWSP', 'spelling', 'outliers', 'binary', 'loners', 'case')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  if (is.null(keyVar)) keyVar <- 'index'
  data <- mutate(data, index = 1:nrow(data))
  
  #### Get ouput ####
  out <- list()
  res <- data
  
  #### Get options ####
  
  options <- lapply(checks, function(this){
    append(options$global, options[[this]])
  })
  
  names(options) <- checks
  
  #### Get subset ####
  subset <- lapply(checks, function(check){
    subset.this <- options[[check]]$subset
    if (!length(subset.this)) subset.this <- names(data)
    return(subset.this)
  })
  combinedSubset <- purrr::reduce(subset, union)
  
  #### Pre-test notice ####
  if (!silent) message('Only compatible variables will be checked.\n')
  
  #### Itterate through data and do the check ####
  for (c in combinedSubset) {
    is.checked <- sapply(checks, function(check) c %in% if(is.null(options[[check]]$subset)) combinedSubset[] else options[[check]]$subset)
    this.checks <- checks[is.checked]
    out[[c]] <- all_check(v = data[[c]], checks = this.checks, options = options, silent = silent)
    
    if (all('spelling' %in% names(out[[c]]) & !is.null(options$spelling$fix))) if(options$spelling$fix) res[[c]] <- spelling_fix(v = res[[c]], checkResult = out[[c]]$spelling, outType = if (!is.null(options$spelling$outType)) outType else 'auto_fixed', silent = TRUE)
    if (all('whitespaces' %in% names(out[[c]]) & !is.null(options$whitespaces$fix))) if(options$whitespaces$fix) res[[c]] <- whitespaces_fix(v = res[[c]], checkResult = out[[c]]$whitespaces, silent = TRUE)
    if (all('doubleWSP' %in% names(out[[c]]) & !is.null(options$doubleWSP$fix))) if(options$doubleWSP$fix) res[[c]] <- doubleWSP_fix(v = res[[c]], checkResult = out[[c]]$doubleWSP, silent = TRUE)
    if (all('missing' %in% names(out[[c]]) & !is.null(options$missing$fix))) if(options$missing$fix) res[[c]] <- missing_fix(v = res[[c]], checkResult = out[[c]]$missing, silent = TRUE)
    
    out[[c]]$res <- res[[c]] 
  }
  
  #### Return the output ####
  class(out) <- 'checkResult.306.cleanify'
  if (!silent) return(print(out, ...))
  return(out)
}

# EOF 
# Writing the report function

