parse_range <- function(range){
  requireNamespace('stringr')
  rangeSign.1 <- substr(range,1,1)
  rangeSign.2 <- substr(range,stringr::str_length(range),stringr::str_length(range))
  range.full <- strsplit(substr(range, 2, stringr::str_length(range)-1), '\\s*[,;.]\\s*', perl = TRUE)[[1]]
  
  out <- 
    if (rangeSign.1 == '{' & rangeSign.2 == '}') .parse_discrete(range.full)
  else if (rangeSign.1 == '[' & rangeSign.2 == ']') .parse_cont(range.full)
  else {
    warning('Parsing failed')
    NULL
  }
  
  return(out)
}

.parse_cont <- function(range){
  range.num <- suppressWarnings(as.numeric(range))
  if (all(!is.na(range.num), length(range.num) == 2)) structure(range.num, class=c('range', 'contRange'))
  else {
    warning('Parse failed')
    NULL
  }
}

.parse_discrete <- function(range){
  structure(range, class=c('range', 'discRange'))
}

range_check <- function(v, range, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  #### Arguments check ####
  if (ncol(as.data.frame(v)) > 1) stop('This function is for vector only. Use the whitespaces_scan instead.')
  if (!requireNamespace('dataMaid')) stop('Please install dataMaid package before continuing.')
  
  silent <- as.logical(silent)
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
  
  #### Output preparation ####
  problem <- logical()
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Parse the range ####
  range <- parse_range(range)
  
  #### Check variable for value out of range ####
  if ('discRange' %in% range) rangeChk <- v %in% range
  if ('contRange' %in% range) rangeChk <- v >= range[[1]] & x <= range[[2]]
  
  if (any(rangeChk)) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ out-of-range values:'
    problemValues <- v[!rangeChk]
    problemIndexes <- which(!rangeChk)
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'out-of-range', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out)
}

rule_check <- function(data, var, rule, silent = FALSE, outClass = c('checkResult.306', 'checkResult')){
  #### Arguments check ####
  if (!is.data.frame(data)) stop('Data must be data frame')
  silent <- as.logical(silent)
  if (missing(var)) stop ('variable names should be provided') else if(!var %in% names(data)) stop('Variable should be in dataset')
  if (any(is.na(silent), is.null(silent))) silent <- FALSE
  if (any(is.na(outClass), is.null(outClass))) outClass <- 'checkResult.306' else outClass <- match.arg(outClass)
  if (exists('shinyOn')) if (shinyOn == TRUE) silent <- TRUE
 
  #### Creating the full rule ####
  rule.full <- paste(var, rule)
  
  #### Output preparation ####
  problem <- logical()
  problemValues <- NULL
  problemIndexes <- NULL
  message <- list()
  
  #### Applying the check ####
  ruleChk <- with(data, eval(parse(text = rule.full)))
                  
  if (any(ruleChk)) {
    problem <- TRUE
    message <- 'These $display$ might $behave$ misaligned issue:'
    problemValues <- v[!ruleChk]
    problemIndexes <- which(!ruleChk)
  } else {
    problem <- FALSE
    message <- 'No problems found' 
  }
  
  #### Return the output ####
  out <- cR306_init(testName = 'misruling', outClass = outClass,
                    problem = problem, problemValues = problemValues, problemIndexes = problemIndexes, problemKeys = problemIndexes, message = message)
  return(out) 
}
