navbarPage("FinnPRIO-Assessor",
            tabPanel("Assessments",
                     tabsetPanel(id = "all_assessments",
                        tabPanel(id = "all", value = "all",
                                title = tagList(icon("laptop-file", class = "fas"), "Assessments"),
                                fluidPage(
                                  br(),
                                  column(10,
                                         h3(strong("All Assessments"), style = "color:#7C6A56"),
                                         DTOutput("assessments")
                                         ),
                                  column(2,
                                         h4(strong("Create New Assessment"), style = "color:#7C6A56"),
                                         actionButton("new_ass", "Create Assessment"),
                                         br(),
                                         br(),
                                         h4(strong("Export All Assessments"), style = "color:#7C6A56"),
                                         downloadButton("export_wide", "Export"),
                                         checkboxInput("exp_all", 
                                                       p("Export all assessments", 
                                                          tags$span("Otherwise, only valid assessments are exported.",
                                                                 style = "color:black; font-size: 12px;"),
                                                       class = "bubble"),
                                                       value = FALSE)
                                     # h3(strong("Filters"), style = "color:#7C6A56"),
                                     # selectInput("filter_pest", "Select Pest Species", 
                                     #             choices = NULL),
                                     # selectInput("filter_assessor", "Select User", 
                                     #             choices = NULL),
                                     # checkboxGroupInput("filter_entry_path",
                                     #                    label = "Select entry pathways",
                                     #                    choices = NULL,
                                     #                    inline = FALSE),
                                     # br(),
                                  )
                                )
                              ), 
                        tabPanel(id = "selected", value = "sel",
                                 title = uiOutput("selectedAssName"),
                                 br(),
                                 fluidRow(
                                   tags$style(
                                   HTML("
                                       #save_answers {
                                       position: fixed;
                                       top: 315px;       /* Distance from top */
                                       right: 20px;      /* Distance from right */
                                       z-index: 1000;    /* Ensures it stays on top */
                                       }")
                                   ),
                                   actionButton("save_answers", "Save Answers") #,
                                   # Autosave status text
                                   # tags$div(
                                   #   style = "margin-left: 20px;",
                                   #   strong("Autosave Status: "),
                                   #   textOutput("save_status", inline = TRUE)
                                   # )
                                   
                                 ),
                                 uiOutput("questionarie")
                        )
                      )
                    # )
            ),
            tabPanel("Pest-species data",
                    fluidPage(
                      h3(strong("Pest Information"), style = "color:#7C6A56"),
                      column(10,
                             DTOutput("pests")
                      ),
                      column(2,
                             actionButton("new_pest", "+ Add Pest")
                      )
                    )
            ),
            tabPanel("Assessors",
                    h3(strong("Assessor information"), style = "color:#7C6A56"),
                        actionButton("new_assessor", "+ Add Assessor")
            ),
            tabPanel("Instructions",
                     fluidPage(
                       includeHTML("www/instructions.html")
                     )
            ),
           header = tagList(
             useShinyjs(), # Initialize shinyjs
             tags$head(
               tags$link(rel = "shortcut icon", href = "./img/bug-slash-solid-full-gray.svg"),
               # Include our custom CSS
               tags$link(rel = "stylesheet", href = "styles.css")
             ),
             
             fluidRow(
               style = "margin:20px",
                      uiOutput("file_input_ui"),
                      # uiOutput("file_path_ui") ## in case we want to work with uploading the file
               uiOutput("unload_db_ui")
              )
             ),
           # position = "fixed-top",
           theme = shinythemes::shinytheme("sandstone")
)
