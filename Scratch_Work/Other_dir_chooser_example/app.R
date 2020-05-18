library(shiny)
library(shinyFiles)
library('shinyDirectoryInput')

ui <- fluidPage(
  
  titlePanel("Example App"),
  
  
  mainPanel(
    directoryInput('directory', label = 'select a directory'),
    verbatimTextOutput("sheets_dir")
  )
)


server <- function(input, output, session) {
  
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch
        
        # launch the directory selection dialog with initial path read from the widget
        path <- choose.dir(default = readDirectoryInput(session, 'directory'))
        
        # update the widget value
        updateDirectoryInput(session, 'directory', value = path)
        
      }
    }
  )
  
  output$sheets_dir <- renderPrint({
    path <- choose.dir(default = readDirectoryInput(session, 'directory'))
    path
  })
  
}



shinyApp(ui, server)



