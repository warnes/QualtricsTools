library(shiny)
library(shinyFiles)

ui <- fluidPage(
  titlePanel("Example App"),
  mainPanel(
    textInput("root", "Please enter your project folder root:"),
    shinyFiles::shinyDirButton(id = 'sheets_dir', label = "Folder select", title = "Sheets Folder Selector"),
    verbatimTextOutput("sheets_dir")
  )
)

server <- function(input, output, session) {
  Theroots <- reactive({
    root <- input$root
    req(root, dir.exists(root))

    if(length(root) == 0 || root == ""){
      volumes <- getVolumes()()
      c(volumes)
      print(volumes)
    } else{
      c(project_root = root)
    }
  })

  Thesheets_dir <- reactive({
    shinyDirChoose(input, 'sheets_dir', roots = Theroots(), session = session)
    parseDirPath(roots = Theroots(), input$sheets_dir)
  })

  output$sheets_dir <- renderPrint({
    Thesheets_dir()
  })
}

shinyApp(ui, server)
