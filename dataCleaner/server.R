shinyServer(function(input, output) {
   
  output$header <-
    renderUI(
      div(
        id = 'header',
        h1('Data Clean Robot')
      )
    )
   
  
  output$inputDialog <-
    renderUI(
      div(
        class = c('on-top', 'dialog'),
        uiOutput('header')
      )
    )
})
