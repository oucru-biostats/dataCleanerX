getOutlValue <- function(type=c('upper', 'lower'), model = c('adjusted', 'boxplot', 'custom')){
  type = match.arg(type)
  model = match.arg(model)
  if (type=='upper')
    out <-
    if (model == 'adjusted') 'Q3 + 1.5*exp(b*MC)*IQR'
    else if (model == 'boxplot') 'Q3 + 1.5*IQR'
    else NULL
  else
    out <-
    if (model == 'adjusted') 'Q1 - 1.5*exp(a*MC)*IQR'
    else if (model == 'boxplot')'Q1 - 1.5*IQR'
    else NULL
  
  return(out)
}