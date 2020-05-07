library(shiny)
library(shinyFiles)

ui <- fluidPage(

  titlePanel("Example App"),


  mainPanel(
    textInput("root", "Please enter your project folder root:"),
    h5(strong("Choose QSF Survey File:")),
    shinyFilesButton('file1', "Browse...", "Choose QSF Survey File", multiple = FALSE),
    verbatimTextOutput("qsf_path"),
    h5(strong("Sheets Folder Selector:")),
    shinyFiles::shinyDirButton(id = 'sheets_dir', label = "Folder select", title = "Sheets Folder Selector"),
    verbatimTextOutput("sheets_dir")
  )
)


server <- function(input, output, session) {

  Theroots <- reactive({
    if(input$root == ""){
      volumes <- getVolumes()()
      Theroots <- c(volumes)
    } else{
      Theroots <- c(project_root = input$root)
    }
    return(Theroots)
  })



  Theqsf_path <- reactive({
    shinyFileChoose(input, 'file1', roots = Theroots(), filetypes=c('qsf'), session = session)
    qsf_path <- parseFilePaths(roots = Theroots(), input$file1)
    return(qsf_path)
  })

  output$qsf_path <- renderPrint({
    Theqsf_path()
  })


  Thesheets_dir <- reactive({
    shinyDirChoose(input = input, 'sheets_dir',
                               roots = Theroots(), session = session)
    sheets_dir <- parseDirPath(roots = Theroots(), input$sheets_dir)
    print(sheets_dir)
    return(sheets_dir)
  })

  output$sheets_dir <- renderPrint({
    Thesheets_dir()
  })

}



shinyApp(ui, server)



