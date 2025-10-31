navbarPage("FinnPRIO-Assessor",
            tabPanel("Assessments",
                     tabsetPanel(id = "all_assessments",
                        tabPanel(#id = "all_assessments", 
                                title = tagList(icon("laptop-file", class = "fas"), "Assessments"),
                                value = 1,
                                fluidPage(
                                  br(),
                                  column(8,
                                         h3(strong("All Assessments"), style = "color:#7C6A56"),
                                         DTOutput("assessments")
                                         ),
                                  column(4,
                                         h3(strong("Filters"), style = "color:#7C6A56"),
                                         selectInput("filter_pest", "Select Pest Species", 
                                                     choices = NULL),
                                         selectInput("filter_assessor", "Select User", 
                                                     choices = NULL),
                                         checkboxGroupInput("filter_entry_path",
                                                            label = "Select entry pathways",
                                                            choices = NULL,
                                                            inline = FALSE),
                                         br(),
                                         h4(strong("Create New Assessment"), style = "color:#7C6A56"),
                                         actionButton("new_ass", "Create Assessment")
                                  )
                                      
                                # sidebarLayout(
                                #     # Sidebar with filters: 
                                #     sidebarPanel(
                                    #   
                                    # ),
                                    # mainPanel(
                                      # uiOutput("modal_new_assessment"),
                                    # )
                                # )
                                )
                              ), 
                        tabPanel(#id = "selected_assessments",
                                 title = uiOutput("selectedAssName"),
                                 value = 2,
                                 br(),
                                 fluidRow(
                                   # # style = "margin:20px; padding:10px; border:1px solid #ccc;",
                                   # # style = "margin:20px",
                                   # column(width = 2, offset = 10,
                                   #        actionButton("save", "Save Assessment")
                                   # )
                                   tags$style(
                                   HTML("
                                       #save {
                                       position: fixed;
                                       top: 315px;       /* Distance from top */
                                       right: 20px;      /* Distance from right */
                                       z-index: 1000;    /* Ensures it stays on top */
                                       }")
                                   ),
                                   actionButton("save", "Save Assessment")
                                   
                                 ),
                                 uiOutput("questionarie")
                        )
                      )
                    # )
            ),
            # tabPanel("Simulation",
            #           fluidPage(
            #             br(),
            #             column(8,
            #                    h3(strong("All Simulations"), style = "color:#7C6A56")#,
            #                    # DTOutput("simulations")
            #             ),
            #             column(4,
            #                    h4(strong("Run New Simulation"), style = "color:#7C6A56"),
            #                    actionButton("all_sim", "Run All Simulation")
            #             )
            #           )
            # ),
            tabPanel("Pest-species data",
                    fluidPage(
                      h3(strong("Pest Information"), style = "color:#7C6A56"),
                      column(8,
                             tableOutput("pests")
                      ),
                      column(4,
                             actionButton("new_pest", "+ Add Pest")
                             # uiOutput("species_summary")
                      )
                    )
            ),
            tabPanel("Assessors",
                    h3(strong("Assessor information"), style = "color:#7C6A56"),
                        actionButton("new_assessor", "+ Add Assessor")
            ),
            tabPanel("Instructions",
                     fluidPage(
                       load_ui_content("ui/instructions.R"),
                     )
            ),
           header = tagList(
             # Initialize shinyjs
             useShinyjs(),
             tags$head(
               tags$link(rel = "shortcut icon", href = "./img/bug-slash-solid-full-gray.svg"),
             ),
             
             fluidRow(
               # style = "margin:20px; padding:10px; border:1px solid #ccc;",
               style = "margin:20px",
               # column(width = 4,
                      uiOutput("file_input_ui"),
                      # uiOutput("file_path_ui")
               #        ),
               # column(width = 2, #offset = 4,
               uiOutput("unload_db_ui")
               # actionButton("unload_db", "Unload database", 
               #                     style = "margin-top: 20px;")
                      # actionButton("save", "Save Assessment")
                      # )
              ) 
             ),
           # footer = tagList(
           #   fluidRow(
           #     style = "margin:20px",
           #     column(width = 4,
           #            
           #            )
           #     )
           #   ),
           theme = shinythemes::shinytheme("sandstone")
)
