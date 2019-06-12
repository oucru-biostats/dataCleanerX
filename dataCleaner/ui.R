shinyUI(
  fluidPage(

    tags$title('Data-Clean Robot'),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel="icon",type="img/ico",href="img/favicon-robot.png"),
    tags$link(rel='stylesheet', href="styles.css"),
    # tags$link(rel='stylesheet', href="dialogs.css"),
    tags$link(rel='stylesheet', href="sidebar.css"),
    tags$link(rel='stylesheet', href="styles-mobile.css"),
    tags$link(rel='stylesheet', href='jquery-mobile/jquery.mobile.custom.structure.min.css'),
    tags$link(rel='stylesheet', href="jquery-ui/jquery-ui.css"),
    tags$link(rel='stylesheet', href='simplebar/simplebar.css'),
    tags$link(rel='stylesheet', href="tippy/themes/light-border.css"),
    tags$script(src = 'etc/css-global-variables.min.js'),
    tags$script(src = 'jquery-mobile/jquery.mobile.custom.min.js'),
    tags$script(src = 'jquery-ui/jquery-ui.js'),
    tags$script(src = 'simplebar/simplebar.js'),
    tags$script(type = 'module', src = 'main.js'),
    tags$script(src = 'scripts.js'),
    tags$script(src = 'etc/dt-modifiers.js'),
    useShinyjs(),
    
    uiOutput('mainUI'),
    div(id='header'),
    uiOutput('sidebar'),
    uiOutput('body'),
    uiOutput('floatbar'),
    uiOutput('floatwindow'),
    uiOutput('tmp')
  )
  
)
