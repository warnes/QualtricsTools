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
    menuItem("Tableau Reshaping", tabName="reshape_tableau", icon=icon("chart-line")),
    menuItem("More Options", tabName="more_options", icon=icon("dashboard")),
    menuItem("FAQ", tabName="faq", icon=icon("question-circle")),

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
                       # verbatimTextOutput("test"),
                       uiOutput("results_tables")
                      ),
              tabPanel(h5("verbatim comment appendices"),
                       uiOutput("text_appendices")
              ),

              tabPanel(h5("coded comment appendices"),
                       uiOutput("coded_comments")
              ),

              tabPanel(h5("question dictionary"),
                       checkboxInput("uncodeable-only", "Only Uncodeable Questions", value = FALSE, width = NULL),
                       DT::dataTableOutput("question_dictionary")
                       ),

              tabPanel(h5("display logic"),
                       uiOutput("display_logic")
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
  tabItem(tabName = "reshape_tableau",
          fluidRow(
            column(width = 12,
                   tabBox( width = NULL,
                           tabPanel(h5("Tableau Controls"),
                                    radioButtons("gen_tableau", "Generate Tableau Data?",
                                                 c("No",
                                                   "Yes, Generate Tableau Data" = "Yes"),
                                                 selected = "No"),
                                    uiOutput("col_input"),
                                    uiOutput("tableau_filename"),
                                    uiOutput("download_tab_data")
                           ),
                           tabPanel(h5("Lean Responses"),
                                    DT::dataTableOutput("lean_responses")
                           ),

                           tabPanel(h5("Response Column Dictionary"),
                                    DT::dataTableOutput("qdict")
                           ),

                           tabPanel(h5("Panel Data"),
                                    DT::dataTableOutput("panel_df")
                           )

                   )
            )
          )
          ),
  tabItem(tabName = "more_options",
          fluidRow(
            column(width = 3,
                   box(
                     status = "warning",
                     width = NULL,
                     h2('Downloads'),
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
                     selectInput("ta_format", "Format for Verbatim Comment Appendices:",
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
                     selectInput("cc_format", "Format for Coded Comment Appendices:",
                                 choices = c("docx", "html", "md", "pdf", "xls"),
                                 width='90%'),
                     HTML("</td> <td>"),
                     downloadButton('downloadCodedCommentAppendices', '', class="btn-primary"),
                     HTML("</td> </tr> </table>")
                   )
                   ),
            column(width = 5,
                   box(
                     status = "info",
                     width = NULL,
                     h2('Splitting Respondents'),
                     HTML("Select the columns for which you'd like to split the respondents
                     into unique respondent groups"),
                     uiOutput("select_response_columns"),
                     HTML("Select for which answer (combinations) you'd like to restrict the respondents to"),
                     uiOutput('select_respondent_group'),
                     tableOutput('table_respondent_groups'),
                     downloadButton('downloadSplit', 'Download All Split Reports and Appendices', class="btn-primary")
                   ),

                   box(
                     status = "danger",
                     width = NULL,
                     h2('Ignore Survey Flow'),
                     checkboxInput("ignoreflow", "Check this box if you would like the report to render without reordering
                              the questions according to the survey's ordering.", FALSE)

                   )

                   ),
            column(width = 4,
                   box(
                     width = NULL,
                     status = "info",
                     h2("Comment Coding Options"),

                     shinyDirectoryInput::directoryInput('sheets_dir', label = 'Sheets Folder Selector: '),
                     # shinyFiles::shinyDirButton(id = 'sheets_dir', label = "Folder select", title = "Sheets Folder Selector"),

                     radioButtons("comment_choices", "Generate Coded Comments?",
                                  c("No",
                                    "Yes, Generate Coded Comment Appendices" = "Yes"),
                                  selected = "No"),
                     radioButtons("code_type", "Code Type?",
                                  c("NVivo format" = "nvivo",
                                    "FileMaker Pro Format" = "fmp")),
                     numericInput("n_threshold", "N Threshold", 15, min = 1, max = 100),
                     radioButtons("include_verbatim", "Include Verbatim Comments In Coded Appendices?",
                                  c("Yes", "No"),
                                  selected = "Yes")
                   ))
          )
   ),
   tabItem(tabName = "faq",
           fluidRow(
             box(status = "info",
                 width = 12,
                 uiOutput('faqmarkdown')
                 )
           )
   )
  )
)

dashboardPage(
  skin = "green",
  dashboardHeader(title = "QualtricsTools"),
  sidebar,
  body
)
