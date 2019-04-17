include <- 
    list(
      tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
      tags$link(rel='stylesheet', href="styles.css"),
      tags$link(rel='stylesheet', href="styles-mobile.css"),
      tags$link(rel='stylesheet', href='jquery-mobile/jquery.mobile.custom.structure.min.css'),
      tags$link(rel='stylesheet', href="jquery-ui/jquery-ui.css"),
      tags$link(rel='stylesheet', href='simplebar/simplebar.css'),
      tags$link(rel='stylesheet', href="tippy/themes/light-border.css"),
      tags$script(src = 'etc/css-global-variables.min.js'),
      tags$script(src = 'jquery-mobile/jquery.mobile.custom.min.js'),
      tags$script(src = 'jquery-ui/jquery-ui.js'),
      tags$script(src = 'simplebar/simplebar.js'),
      tags$script(src = 'main.js')
    )


shinyUI(
  fluidPage(
    tags$title('Data Cleaner'),
    include,
    
    uiOutput('header'),
    uiOutput('inputDialog'),
    uiOutput('sidebar'),
    uiOutput('body'),
    uiOutput('floatbar'),
    uiOutput('floatwindow')
  )
  
)
