shinyUI(
  fluidPage(

    tags$title('Data-Clean Robot'),
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$link(rel="icon",type="img/ico",href="img/favicon-robot.png"),
    tags$link(rel='stylesheet', href="styles.css"),
    tags$link(rel='stylesheet', href="sidebar.css"),
    tags$link(rel='stylesheet', href="styles-mobile.css"),
    tags$link(rel='stylesheet', href='jquery-mobile/jquery.mobile.custom.structure.min.css'),
    tags$link(rel='stylesheet', href="jquery-ui/jquery-ui.css"),
    tags$link(rel='stylesheet', href='simplebar/simplebar.min.css'),
    tags$link(rel='stylesheet', href="tippy/themes/light-border.css"),
    tags$script(type="text/javascript", src = 'jquery-mobile/jquery.mobile.custom.min.js'),
    tags$script(type="text/javascript", src = 'darkMode/darkmode-js.min.js'),
    tags$script(type="text/javascript", src = 'jquery-ui/jquery-ui.js'),
    tags$script(type="text/javascript", src = 'simplebar/simplebar.min.js'),
    tags$script(type = 'module', src = 'main.js'),
    tags$script(type="text/javascript", src = 'scripts.js'),
    tags$script(type="text/javascript", src = 'etc/dt-modifiers.js'),
    shinyjs::useShinyjs(),
    shinyjs::extendShinyjs(text="shinyjs.cloneValue = function(input){input = input[0]; $(input.target).html(input.val);}"),
    
    uiOutput('mainUI'),
    div(id='header'),
    uiOutput('sidebar'),
    uiOutput('body'),
    uiOutput('workingDialog'),
    uiOutput('floatwindow'),
    uiOutput('tmp')
  )
  
)
