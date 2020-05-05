library(shinydashboard)

sidebar <- dashboardSidebar(
  sidebarMenu(id="tabs",
    menuItem("File Uploading",
             icon=icon("upload"),
           selected=TRUE,
             textInput("root", "Please enter your project folder root:"),
             h5(strong("Choose QSF Survey File:")),
             shinyFiles::shinyFilesButton('file1', "Browse...", "Choose QSF Survey File", multiple = FALSE),
             h5(strong("Choose CSV Response Set File:")),
             shinyFiles::shinyFilesButton('file2', "Browse...", "Choose CSV Response Set File", multiple = FALSE),

             div(class="sidebar-text",
                 HTML("QualtricsTools requires data be exported with the <a href='https://github.com/emmamorgan-tufts/QualtricsTools/wiki/Appendix-of-Qualtrics-Terms#legacy-and-insights-data', target='_blank'>Legacy Exporter</a>.")),
             checkboxInput("insights_or_not", "Unchecked Legacy View Results (3 header rows & QIDs)?", value = TRUE, width = NULL)
             ),
    menuItem("Processed Results", tabName="report", icon=icon("leanpub")),
    menuItem("Include/Exclude Responses", tabName="include_exclude", icon=icon("toggle-on")),
    menuItem("More Options", tabName="more_options", icon=icon("dashboard")),

    # empty h5 headers below are for spacing
    h5(""),
    downloadButton('downloadZip', 'Download Zip', class="btn-primary"),
    h5(""),
    actionButton("quit", "Stop App")
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$script(src = "custom.js")
  ),

  tabItems(
  tabItem(tabName = "report",
    fluidRow(
    column(width = 12,
      tabBox( width = NULL,
              tabPanel(h5("results tables"),
                       uiOutput("uncodeable_message"),
                       verbatimTextOutput("test"),
                       uiOutput("results_tables")
                      ),
              tabPanel(h5("question dictionary"),
                       checkboxInput("uncodeable-only", "Only Uncodeable Questions", value = FALSE, width = NULL),
                       dataTableOutput("question_dictionary")
                       ),
              tabPanel(h5("text appendices"),
                       uiOutput("text_appendices")
                       ),
              tabPanel(h5("display logic"),
                       uiOutput("display_logic")
                       ),
              tabPanel(h5("coded comment appendices"),
                       uiOutput("coded_comments")
                       )
              )
      )
    )
  ),
  tabItem(tabName = "include_exclude",
          fluidRow(
            column(width=12,
            h2('Include or Exclude Specific Questions'),
            actionButton("selectAll", "Unselect/Select All"),
            actionButton("submit", "Apply"),
            HTML("<br><br>"),
              dataTableOutput("select_qdict")
              )
  )),
  tabItem(tabName = "more_options",
          fluidPage(
            sidebarLayout(
              sidebarPanel(
                h1('Downloads'),
                # This is HTML for creating an invisible table for a clean layout
                # of the download buttons for each of the frequency results tables,
                # question dictionary, text appendices, and display logic.
                textInput("file_name", "Enter the name of your file:"),
                HTML("<br><table style='width: 100%;'> <tr> <td>"),
                selectInput("rt_format", "Format for Results Tables:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadResultsTables', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("qd_format", "Format for Question Dictionary:",
                            choices = c("csv"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadQuestionDictionary', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("ta_format", "Format for Text Appendices:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadTextAppendices', '', class="btn-primary"),

                HTML("</td> </tr> <tr> <td>"),
                selectInput("dl_format", "Format for Display Logic:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadDisplayLogic', '', class="btn-primary"),
                HTML("</td> </tr> <tr> <td>"),
                selectInput("cc_format", "Format for Comment Coded:",
                            choices = c("docx", "html", "md", "pdf", "xls"),
                            width='90%'),
                HTML("</td> <td>"),
                downloadButton('downloadCodedCommentAppendices', '', class="btn-primary"),
                HTML("</td> </tr> </table>")
              ),

              sidebarPanel(width=8,
                h1('Splitting Respondents'),
                HTML("Select the columns for which you'd like to split the respondents
                     into unique respondent groups"),
                uiOutput("select_response_columns"),
                HTML("Select for which answer (combinations) you'd like to restrict the respondents to"),
                uiOutput('select_respondent_group'),
                tableOutput('table_respondent_groups'),
                downloadButton('downloadSplit', 'Download All Split Reports and Appendices', class="btn-primary")
              )
            ),
            sidebarPanel(
                h3('Ignore Survey Flow'),
                checkboxInput("ignoreflow", "Check this box if you would like the report to render without reordering
                              the questions according to the survey's ordering.", FALSE)
            ),
            sidebarPanel(
              h3("Comment Coding Options"),
              radioButtons("comment_choices", "Generate Coded Comments?",
                           c("No",
                             "Yes, Generate Coded Comment Appendices" = "Yes"),
                           selected = "No"),
              radioButtons("code_type", "Code Type?",
                           c("NVivo format" = "nvivo",
                             "FileMaker Pro Format" = "fmp")),
              numericInput("n_threshold", "N Threshold", 15, min = 1, max = 100),
              radioButtons("include_verbatim", "Include Verbatim Coments In Appendices?",
                           c("Yes", "No"),
                           selected = "Yes"),
              h5(strong("Sheets Folder Selector:")),
              shinyFiles::shinyDirButton(id = "sheets_dir", label = "Folder select", title = "Sheets Folder Selector"),
            )
          )
   ))
)

dashboardPage(
  dashboardHeader(title = "QualtricsTools"),
  sidebar,
  body
)
