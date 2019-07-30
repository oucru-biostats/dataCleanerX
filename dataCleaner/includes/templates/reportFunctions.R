resultTable <- function(chkRes,...){
  UseMethod('resultTable')
}

resultTable.checkResult.306 <- function(chkRes, varname = names(chkRes$problem), chkName = chkRes$testName, dataset, output){
  display <- match.arg(display)
  if (is.null(varname)){
    varname <- deparse(substitute(chkRes))
  }
  
  # problem <- chkRes$problem
  # problemValues <- chkRes$problemValues
  problemIndexes <- chkRes$problemIndexes
  # message <- chkRes$message[!is_true(names(chkRes$message) == 'notice')]
  # hasKey <- length(keys)
  # if (hasKey) problemKeys <- chkRes$problemKeys
  
  # optionsWidget <-
  #   function(id, label, choices, selected = choices[1],...){
  #     choice.names <- ifelse(names(choices), names(choices), choices)
  #     
  #     label.block <- sprintf('<label for="%s">%s</label>',id,label)
  #     option.block <- 
  #       do.call(paste0,
  #         lapply(seq_along(choices), function(i){
  #           sprintf('<option value="%s" %s>%s</option>',choices[i], if (choices[i] == selected) "selected" else "", names(choices)[i])
  #         })
  #       )
  #     open.block <- sprintf('<select id="%s">',id)
  #     close.block <- '</select>'
  #     script.block <-
  #       sprintf('<script>$("#%s").change(function(){console.log($(this).find("option:selected").attr("value"))})</script>',id)
  #     
  #     return(paste0(label.block, open.block, option.block, close.block, script.block))
  #   }
  
  
  
  data.filtered <- dataset[problemIndexes,]
  return(BSTable(data = data.filtered, id = varname))
}

cdnHook <- function(link, tag = c('script', 'style'), ...){
  threedt=function(...){
    .threedt = list(...)
    
    do.call(paste,
            sapply(seq_along(.threedt), function(i) {
              if ((names(.threedt)[[i]]) != '') paste(names(.threedt)[[i]], .threedt[[i]], sep = ' = ')
              else (.threedt[[i]])
            }, simplify = FALSE, USE.NAMES = TRUE))
  }
  
  lines <- do.call(paste0, as.list(readLines(link, warn=FALSE)))
  tag <- match.arg(tag)
  three.dots <- threedt(...)
  if (!length(three.dots)) three.dots <- ''
  
  out <- sprintf('<%s async defer %s>%s</%s>',tag,three.dots,lines,tag)
  return(htmltools::HTML(out))
}

log_create <- function(output, allRes){
  file.copy('includes/templates/reportTemplate.Rmd', output)
  log_append <- function(output, chkRes) {
    write('```{r}', file=output, append=TRUE)
    write('')
  }
  for (chkRes in allRes){
    
  }
  
    
}

BSTable <- function(data, id = deparse(substitute(data))){
  if (!is.data.frame(data)) {
    warning('Data must be data.frame')
    return(NULL)
  }
  
  el <- htmltools::HTML(
    paste0('  <div id="', id, '-bootstrap-table"><bootstrap-table :columns="columns" :data="data" :options="options" ></bootstrap-table></div>')
  )
  data.json <- jsonlite::toJSON(data)
  def.script <- htmltools::tags$script(paste0('let ', id,' = ', data.json))
  data.columns <- jsonlite::toJSON(
    lapply(colnames(data), function(name){
      return(list(title = paste0(toupper(substring(name, 1, 1)), substring(name, 2, nchar(name))),
                  field = name,
                  sortable = TRUE,
                  filterControl = 'input',
                  filterStrictSearch = FALSE
      ))
    }), auto_unbox = TRUE
  )
  vue.script <- htmltools::tags$script(
    sprintf("
            new Vue({
            el: '#%s',
            components: {
              'BootstrapTable': BootstrapTable
            },
            data: {
              columns: %s,
              data: %s,
              options: {
                search: false,
                showButtonText: true,
                showColumns: true,
                showColumnsToggleAll: true,
                smartDisplay: true,
                pagination: true,
                resizable: true,
                virtualScroll: true,
                sortable: true,
                sortOrder: 'asc',
                filterControl: true,
                filterShowClear: true,
                showExport: true,
                exportDataType: 'all',
                exportTypes: ['json', 'xml', 'csv', 'txt', 'sql', 'excel', 'pdf']
              }
            }
        });
      ",
            paste0(id, '-bootstrap-table'),
            data.columns,
            id
    ))
  
  return(list(el = el, def = def.script, vue = vue.script))
}

#   <label class="control-label" for="text">Choose animal</label>
#   <div><select id="text"><option value="dog">dog</option><option value="cat">cat</option><option value="trump" selected>trump</option></select><script type="application/json" data-for="text" data-nonempty="">{}</script></div>
# <div id="result"></div>
# <script>document.querySelector("#text").onchange=function(){$("#result").html($("#text option:selected").text())}</script>')