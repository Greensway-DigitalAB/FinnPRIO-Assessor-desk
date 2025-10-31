server <- function(input, output, session) {
  # Reactive values ####
  load <- reactiveValues(status = FALSE, timestamp = NULL)
  con <- reactiveVal()
  assessors <- reactiveValues(data = NULL)
  threats <- reactiveValues(data = NULL)
  pests <- reactiveValues(data = NULL)
  taxa <- reactiveValues(data = NULL)
  quaran <- reactiveValues(data = NULL)
  pathways <- reactiveValues(data = NULL)
  assessments <- reactiveValues(data = NULL, questionarie = NULL, 
                                selected = NULL, entry = NULL,
                                selected_pathways = NULL,
                                threats = NULL)
  simulations <- reactiveValues(data = NULL, selected = NULL,
                                results = NULL)
  questions <- reactiveValues(main = NULL, entry = NULL)
  points <- reactiveValues(main = NULL, entry = NULL, table2 = NULL, table3 = NULL)
  answers <- reactiveValues(main = NULL, entry = NULL)
  frominput <- reactiveValues(main = NULL, entry = NULL)

  
  shinyDirChoose(input, "db_folder", roots = volumes, session = session,  
                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  shinyFileChoose(input, "db_file", roots = volumes, session = session)
  db_path <- reactiveVal(NULL)
  db_file <- reactiveVal(NULL)
  
  # ---- Database connection ----
  # output$file_path_ui <- renderUI({
  #   if (load$status == FALSE){
  #     tagList(
  #       shinyDirButton("db_folder", "Choose Folder", "Select folder containing SQLite DB"),
  #       br()
  #     )
  #   } else {
  #     tagList(
  #       h3("Working with", db_path(), load$timestamp),
  #       verbatimTextOutput("folder_path"),
  #       actionButton("disconnect_db", "Disconnect database")
  #     )
  #   }
  # })
  
  output$file_input_ui <- renderUI({
    if (load$status == FALSE){
      tagList(
        # fileInput("db_file", "Upload SQLite Database", accept = c(".sqlite", ".db")),
        shinyFilesButton("db_file", "Choose File", 
                         "Select file containing the SQLite database", 
                         multiple = FALSE, style = "margin-top: 20px;")
      )
    } else {
      tagList(
        # h3("Working with", db_path(), load$timestamp)#,
        h3("Working with", input$db_file$files$`0`[[3]]),
        h5(db_path())#,
        # actionButton("unload_db", "Unload database")
      )
    }
  })

  output$unload_db_ui <- renderUI({
    req(load$status)
    actionButton("unload_db", "Unload database", 
                 style = "margin-top: 20px;")
  })
  
  observeEvent(input$unload_db, {
    dbDisconnect(con())
    runjs("document.getElementById('db_file').value = ''")
    # con(NULL)
    assessments$data <- NULL
    assessments$selected <- NULL
    assessments$entry <- NULL
    assessments$threats <- NULL
    assessments$questionarie <- NULL
    assessments$selected <- NULL
    answers$main <- NULL
    answers$entry <- NULL
    load$status <- FALSE
    updateTabsetPanel(session, "all_assessments", selected = "1")
    # session$reload()
  })
  
  
  # observeEvent(input$disconnect_db, {
  #   runjs("document.getElementById('db_file').value = ''")
  #   dbDisconnect(con())
  #   con(NULL)
  #   session$reload()
  #   load$status <- FALSE
  # })
  
  observeEvent(input$db_file, {
    
    file_path <- parseFilePaths(volumes, input$db_file)$datapath
  
  # filepath<<-input$db_file
  # print(input$db_file$files$`0`[[3]])
    
    if (length(file_path) > 0 && file.exists(file_path)) {
      db_path(file_path)
    } else {
      db_path(NULL)
    }
    
  })
  
  # observeEvent(input$db_folder, {
  #   folder <- parseDirPath(volumes, input$db_folder)
  #   db_file <- list.files(folder, pattern = "\\.db$", full.names = TRUE)
  #   
  #   if (length(db_file) > 0) {
  #     db_path(db_file[1])  # Use the first .sqlite file found
  #   } else {
  #     db_path(NULL)
  #   }
  # })
  
  output$folder_path <- renderPrint({
    db_path()
  })
  
  # Load data from database ####
  observeEvent(db_path(), {
    # req(input$db_file)
    req(db_path())
    load$status <- TRUE
    load$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    
    # print(db_path())
    
    withProgress({
      setProgress(.1)
      consql <- dbConnect(RSQLite::SQLite(), db_path())
      # consql <- dbConnect(RSQLite::SQLite(), dbname = input$db_file$datapath)
                          # dbname = ifelse(production,
                          #                 'data/FinnPrio_DB_v01.db',
                          #                 'data/FinnPrio_DB_v01_test.db'))
      # consqlGlobal <<- consql
      con(consql)
      if (!is.null(con())) {
        message("Connection established")
        message("You are in Neo")
        assessors$data <- dbReadTable(con(), "assessors")
        assessors$data$fullName <- paste(assessors$data$firstName, assessors$data$lastName)
        threats$data <- dbReadTable(con(), "threatenedSectors")
        pests$data <- dbReadTable(con(), "pests")
        taxa$data <- dbReadTable(con(), "taxonomicGroups")
        quaran$data <- dbReadTable(con(), "quarantineStatus")
        pathways$data <- dbReadTable(con(), "pathways")
        
        assessments$data <- dbReadTable(con(), "assessments")
        simulations$data <- dbReadTable(con(), "simulations")
        questions$main <- dbReadTable(con(), "questions")
        questions$entry <- dbReadTable(con(), "pathwayQuestions")
        table2 <- dbReadTable(con(), "table2")
        table3 <- dbReadTable(con(), "table3")
        points$table2 <- table2 |> 
          pivot_longer(-ENT3.ENT2,
                       names_to = "ENT2", values_to = "Points") |> 
          mutate(ENT2 = gsub("\\.", " ", ENT2)) |> 
          rename(ENT3 = ENT3.ENT2)
# table2_exp <<- points$table2
        points$table3 <- table3 |> 
          pivot_longer(-EST3.EST2,
                       names_to = "EST2", values_to = "Points") |> 
          mutate(EST2 = gsub("\\.", " ", EST2)) |> 
          rename(EST3 = EST3.EST2)
# table3_exp <<- points$table3
        
        points$main <- get_points_as_table(questions$main)
# pointsmain_exp <<- points$main
        points$entry <- get_points_as_table(questions$entry)
# pointsentry_exp <<- points$entry
     } #else { stop() }
        setProgress(1)
    }, message = "Läser in bakgrund data")
  })
  
  # Load options and lists from DB
  observe({
    req(con())
    update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
  })
  
# Assessments ----
  # Show saved assessments
  output$assessments <- renderDT({
    req(assessments$data)
    tab <- assessments$data |>
      mutate(finished = as.logical(finished),
             valid = as.logical(valid),
             startDate = as.Date(startDate),
             endDate = as_datetime(endDate, tz = "CET")) |> 
      left_join(pests$data, by = "idPest") |>
      left_join(assessors$data, by = "idAssessor") |>
      select(idAssessment, scientificName, eppoCode, fullName, 
             startDate, endDate, finished,	valid,	notes,	version)

    datatable(tab, 
              class = 'row-border stripe compact hover',
              extensions = 'Buttons', 
              rownames = FALSE, selection = 'single', autoHideNavigation = FALSE,
              colnames = c("Pest", "Pest species", "EPPO code", "Assessor", "Started", 
                           "Last Edited", "Finished", "Valid", "Notes", "Version"),
              options = list(
                columnDefs = list(
                  list(targets = c(0), visible = FALSE) # hides 1st column 
                ),
                dom = 'lftpB', #pageLength = 6,
                stateSave = TRUE,
                # language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Swedish.json'),
                searching = TRUE, autoFill = FALSE, ordering = TRUE,
                lengthMenu = list(c(10, 25, 50, 100, -1),
                                  c('10', '25', '50', '100','All')),
                pageLength = 25,
                lengthChange = TRUE, scrollX = TRUE, scrollY = FALSE,
                paging = TRUE)
    ) |> 
      formatDate(columns = c('startDate'), method = 'toLocaleDateString') |> 
      formatDate(columns = c('endDate'), method = 'toLocaleString')
                     
  })
  
  proxyassessments <- dataTableProxy("assessments")
  
  
  ## when selecting assessment
  observeEvent(input$assessments_rows_selected, {
    if (is.null(input$assessments_rows_selected)) {
      assessments$selected <- NULL
      assessments$entry <- NULL
      assessments$threats <- NULL
      answers$main <- NULL
      answers$entry <- NULL
    } else {
  ## TODO watch here the selection process based on the filter options
      assessments$selected <- assessments$data[input$assessments_rows_selected, ]
      
      assessments$selected <- assessments$selected |> 
        left_join(pests$data, by = "idPest") |>
        left_join(assessors$data, by = "idAssessor") |> 
        mutate(label = paste(scientificName, eppoCode, 
                             paste(firstName, lastName), startDate, 
                             sep = "_"))
      # assessments$selected
      selected_entries <- dbGetQuery(con(), glue("SELECT * FROM entryPathways 
                                                 -- LEFT JOIN pathways ON entryPathways.idPathway = pathways.idPathway
                                                 WHERE idAssessment = {assessments$selected$idAssessment}"))

      if (nrow(selected_entries) > 0) {
        assessments$entry <- vector(mode = "list", length = nrow(selected_entries))
        names(assessments$entry) <- selected_entries$idPathway
      }
      
      assessments$threats <- dbGetQuery(con(), glue("SELECT idThreat, threatXassessment.idThrSect, threatGroup, name FROM threatXassessment
                                             LEFT JOIN threatenedSectors ON threatXassessment.idThrSect = threatenedSectors.idThrSect
                                             WHERE idAssessment = {as.integer(assessments$selected$idAssessment)}"))
      # Load previous answers
      answers$main <- dbGetQuery(con(), glue("SELECT * FROM answers WHERE idAssessment = {assessments$selected$idAssessment}"))
      answers$entry <- dbGetQuery(con(), glue("SELECT pa.*, ep.idAssessment, ep.idPathway
                                              FROM pathwayAnswers AS pa 
                                              LEFT JOIN entryPathways AS ep ON pa.idEntryPathway = ep.idEntryPathway
                                              WHERE pa.idEntryPathway IN ({paste(selected_entries$idEntryPathway, collapse = ', ')})"))
      
      updateTabsetPanel(session, "all_assessments", selected = "2")
        
    }

  })
  
  ## Assessments summary ----
  output$selectedAssName <- renderUI({
    if (is.null(input$assessments_rows_selected)) {
      tagList(icon("file", class = "fas"), "Selected Assessment")
    } else {
      tagList(icon("file-lines", class = "fas"), assessments$selected$label)
    }
  })
  
  output$assessment_summary <- renderUI({
    req(input$assessments_rows_selected)
    
    ass_info <- assessments$selected
    
    # Build card UI
    tagList(
      tags$div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
               fluidRow(
                 column(4, 
                        h3(em(ass_info$scientificName)),
                        h4(ass_info$fullName),
                        p("Created on", ass_info$startDate),
                        p("Last edited on", ass_info$endDate),
                        p("Questionary ver.", ass_info$version),
                        checkboxInput("ass_finish", label = "Is finished?", value = ass_info$finished),
                        checkboxInput("ass_valid", label = "Is valid?", value = ass_info$valid),
                        uiOutput("species_summary")
                        ),
                 column(4,
                        tags$div(class = "card", 
                                 style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                                 uiOutput("threat_checkboxes")
                                 )
                        ),
                  column(4,
                         tags$div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                                  uiOutput("entrypath_checkboxes")
                                  )
                  )
               )
      ),
      tags$div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
               fluidRow(
                 textAreaInput("ass_notes", label = "Notes", 
                               value = ifelse(is.na(ass_info$notes), "", ass_info$notes),
                               width = "auto", height = "500px", resize = "vertical")
                 )
      ),
      br()
    )
    
  })
  
  ### Pest summary ----
  # Once selected the species, show all you know about it
  output$species_summary <- renderUI({
    req(assessments$selected)
    ass_info <- assessments$selected

    quaran <- dbGetQuery(con(), glue("SELECT name FROM quarantineStatus
                                           WHERE idQuarantineStatus = {as.integer(ass_info$idQuarantineStatus)}"))
    taxa <- dbGetQuery(con(), glue("SELECT name FROM taxonomicGroups
                                           WHERE idTaxa = {as.integer(ass_info$idTaxa)}"))
    # Build card UI
    tagList(
      div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
          h4(strong("Pest species information"), style = "color:#7C6A56"),
          p(ass_info$vernacularName),
          p(strong("EPPO code: "), ass_info$eppoCode),
          p(strong("GBIF uuid: "), ass_info$gbifuuid),
          p(strong("Synonyms: "), em(ass_info$synonyms)),
          p(strong("Taxonomic Group: "), taxa),
          p(strong("Quarantine Status: "), quaran),
          p(strong("Precense in Europe: "), as.logical(ass_info$inEurope))
      )
    )
      
  })
  
  
  ### Potential entry checkboxes ----
  output$entrypath_checkboxes <- renderUI({
    req(pathways$data)
    req(assessments$selected)
    
    tagList(
      h4(strong("Entry Pathways"), style = "color:#7C6A56"),
      textAreaInput("ass_pot_entry_path_text",
                    label = "Potential entry pathways",
                    value = ifelse(is.na(assessments$selected$potentialEntryPathways), "", assessments$selected$potentialEntryPathways),
                    width = "auto", height = "200px",
                    resize = "vertical"),
      checkboxGroupInput("ass_pot_entry_path",
                         label = "Select potential entry pathways to assess",
                         choices = setNames(pathways$data$idPathway, pathways$data$name),
                         selected = if(!is.null(assessments$entry)) names(assessments$entry) else NULL,
                         inline = FALSE)
    )
  })
  
  
  observe({
    req(assessments$selected)
    if (is.null(input$ass_pot_entry_path) || length(input$ass_pot_entry_path) == 0) {
      assessments$selected_pathways <- NULL
    } else {
      assessments$selected_pathways <- input$ass_pot_entry_path
    }
  })
   

#   # observeEvent(assessments$selected_pathways,{
#     # req(assessments$selected)
# # print(input$ass_pot_entry_path)
# print("triggered")
#     # n_entry <- length(input$ass_pot_entry_path)
#     # print(input$pot_entry_path)
#     # assessments$entry <- vector(mode = "list", length = n_entry)
#     # names(assessments$entry) <- input$ass_pot_entry_path
#     
#     # removeUI("questionariePath", session)
#     # Insert associated entry pathways
#     selected_pathways <- assessments$selected_pathways #<-input$ass_pot_entry_path
#     current_pathways <- names(assessments$entry)
#     paths_to_add <- setdiff(selected_pathways, current_pathways)
#     paths_to_remove <- setdiff(current_pathways, selected_pathways)
#     # Add new pathways
#     if (length(paths_to_add) > 0) {
#       for (path_id in paths_to_add) {
#         dbExecute(con(), "INSERT INTO entryPathways(idAssessment, idPathway) VALUES(?, ?)",
#                   params = list(assessments$selected$idAssessment, path_id))
#       }
#     }
#     
#     # Remove unchecked pathways
#     if (length(paths_to_remove) > 0) {
#       for (path_id in paths_to_remove) {
#       ### CAUTION here we delete also the answers for this pathway in a cascade
#         dbExecute(con(), "DELETE FROM entryPathways WHERE idAssessment = ? AND idPathway = ?",
#                   params = list(assessments$selected$idAssessment, path_id))
#       }
#     }
#     
#     # selected_entries <- dbGetQuery(con(), glue("SELECT * FROM entryPathways 
#     #                                              -- LEFT JOIN pathways ON entryPathways.idPathway = pathways.idPathway
#     #                                              WHERE idAssessment = {assessments$selected$idAssessment}"))
#     # if (nrow(selected_entries) > 0) {
#     #   assessments$entry <- vector(mode = "list", length = nrow(selected_entries))
#     #   names(assessments$entry) <- selected_entries$idPathway
#     # } else {
#     #   assessments$entry <- NULL
#     # }
#     
#   })
  
  ### Threat checkboxes (pull from DB table Threatened Sectors) ----
  output$threat_checkboxes <- renderUI({
    req(assessments$selected)
    req(threats$data)
    threats <- threats$data
    
    # Assuming there's a column called 'group' to group threats
    threat_groups <- split(threats, threats$threatGroup)

    # Generate UI for each group
    group_ui <- lapply(names(threat_groups), function(group_name) {
      group_threats <- threat_groups[[group_name]]
      
      selected_threats <- if (!is.null(assessments$threats)) {
        assessments$threats$idThrSect
      } else {
        NULL
      }
      
      checkboxGroupInput(
        inputId = paste0("group_", group_name),
        label = group_name,
        choices = setNames(group_threats$idThrSect, group_threats$name),
        selected = selected_threats,
        inline = FALSE
      )
    })
    
    half <- ceiling(length(group_ui) / 2)
    
    ui <- tagList(
      h4(strong("Threatened Sectors"), style = "color:#7C6A56"),
      fluidRow(
          column(6, group_ui[1:half]),
          column(6, group_ui[(half + 1):length(group_ui)])
      )
    )
    return (ui)
  })
  
  ## Modal for new assessment ----
  observeEvent(input$new_ass, {
    assessments$selected <- NULL
    assessments$entry <- NULL
    answers$main <- NULL
    answers$entry <- NULL
    req(pests$data)
    req(assessors$data)
    req(pathways$data)
    showModal(modalDialog(
      title = "Add New Assessment",
      selectInput("pest", "Pest Species", 
                  choices = setNames(c("", pests$data$idPest), c("", pests$data$scientificName))),
      selectInput("assessor", "Assessor", 
                  choices = setNames(c("", assessors$data$idAssessor), c("", assessors$data$fullName))),
      # Entry pathways 
      h4(strong("Entry Pathways"), style = "color:#7C6A56"),
      textAreaInput("pot_entry_path_text",
                    label = "Potential entry pathways",
                    resize = "vertical"),
      checkboxGroupInput("pot_entry_path",
                         label = "Select potential entry pathways to assess",
                         choices = setNames(pathways$data$idPathway, pathways$data$name),
                         inline = FALSE),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_ass", "Save")
      ),
      size = "s",
      easyClose = TRUE
    ))
  })
  
  observeEvent(input$confirm_ass, {
    dbExecute(con(), "INSERT INTO assessments(idPest, idAssessor, potentialEntryPathways, startDate, endDate) VALUES(?,?,?,?,?)",
              params = list(input$pest, input$assessor, input$pot_entry_path_text, 
                            as.character(today("CET")), format(now("CET"), "%Y-%m-%d %H:%M:%S")  ))
    assessments$data <- dbReadTable(con(), "assessments")
    id_new_ass <- last(assessments$data$idAssessment) ## Alternatively highest if they return disordered
    
    if (length(input$pot_entry_path) > 0) {
      for (p in input$pot_entry_path) {
        dbExecute(con(), "INSERT INTO entryPathways(idAssessment, idPathway) VALUES(?,?)",
                  params = list(id_new_ass, p))
      }
    }
    # assessments$entry <- dbReadTable(con(), "entryPathways")
    removeModal()
    update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
  })

  ## Questionaries ----
  output$questionarie <- renderUI({
    req(questions$main)
    req(answers$main)
    req(assessments$selected)
  
    quesEnt <- questions$main |> filter(group == "ENT") |> arrange(number)
    quesEst <- questions$main |> filter(group == "EST") |> arrange(number)
    quesImp <- questions$main |> filter(group == "IMP") |> arrange(number)
    quesMan <- questions$main |> filter(group == "MAN") |> arrange(number)

    tagList(
      tabsetPanel(
        tabPanel(id = "info", 
                 title = "General Information",
                 uiOutput("assessment_summary")
        ),
        tabPanel(id = "entry", 
                 title = "Entry",
                 lapply(1:nrow(quesEnt), 
                        function(x){
                          question <- quesEnt$question[x]
                          options <- quesEnt$list[x]
                          id <- quesEnt$number[x]
                          just <- answers$main |> 
                            filter(idQuestion == quesEnt$idQuestion[x]) |> 
                            pull(justification)
                          # print(answers$main |> filter(idQuestion == quesEnt$idQuestion[x]))
                          tagList(
                            h4(glue("ENT {id}: {question}")),
                            fluidRow(
                              div(style = "margin: 20px;",
                                  # column(5,
                                  # för info
                                  # tags$p(icon("pencil", class = "fas"), 
                                  #        tags$span("Redigera projektet", style = "color:black;"), 
                                  #        class = "bubble", style = "color:#FEAB3B;"))),
                                  render_quest_tab("ENT", id, question,
                                                   fromJSON(options)$opt,
                                                   fromJSON(options)$text,
                                                   answers_2_logical(answers$main, questions$main)),
                                  #        # ),
                                  # column(7,
                                  # uiOutput("ENT1_warning"),
                                  br(),
                                  textAreaInput(glue("justEnt{id}"),
                                                label = "Justification",
                                                value = just,  
                                                width = 'auto',
                                                height = '150px',
                                                resize = "vertical")
                                  # )
                              )
                            ),
                            hr(style = "border-color: gray;")
                          )
                        }),
                 br(),
                 uiOutput("questionariePath")
        ),
        tabPanel(id = "est", 
                 title = "Establishment and Spread",
                 lapply(1:nrow(quesEst), 
                        function(x){
                          question <- quesEst$question[x]
                          options <- quesEst$list[x]
                          id <- quesEst$number[x]
                          just <- answers$main |> 
                            filter(idQuestion == quesEst$idQuestion[x]) |> 
                            pull(justification)
                          tagList(
                            h4(glue("EST {id}: {question}")),
                            fluidRow(
                              div(style = "margin: 20px;",
                                  # column(5,
                                  render_quest_tab("EST", id, question,
                                                   fromJSON(options)$opt,
                                                   fromJSON(options)$text,
                                                   answers_2_logical(answers$main, questions$main)),
                                  # ),
                                  # column(7,
                                  br(),
                                  textAreaInput(glue("justEst{id}"),
                                                label = "Justification",
                                                value = just,  
                                                width = 'auto',
                                                height = '150px',
                                                resize = "vertical")
                                  # )
                              )
                            ),
                            hr(style = "border-color: gray;")
                          )
                        })
        ),
        tabPanel(id = "imp", 
                 title = "Impact",
                 lapply(1:nrow(quesImp),
                        function(x){
                          question <- quesImp$question[x]
                          options <- quesImp$list[x]
                          id <- quesImp$number[x]
                          just <- answers$main |> 
                            filter(idQuestion == quesImp$idQuestion[x]) |> 
                            pull(justification)
                          type <- quesImp$type[x]
                          tagList(
                            h4(glue("IMP {id}: {question}")),
                            fluidRow(
                              div(style = "margin: 20px;",
                                  # column(5,
                                  render_quest_tab("IMP", id, question,
                                                   fromJSON(options)$opt,
                                                   fromJSON(options)$text,
                                                   answers_2_logical(answers$main, questions$main),
                                                   type),
                                  # ),
                                  # column(7,
                                  br(),
                                  textAreaInput(glue("justImp{id}"),
                                                label = "Justification",
                                                value = just,  
                                                width = 'auto',
                                                height = '150px',
                                                resize = "vertical")
                                  # )
                              )
                            ),
                            hr(style = "border-color: gray;")
                          )
                        })
        ),
        tabPanel(id = "man", 
                 title = "Management",
                 lapply(1:nrow(quesMan),
                        function(x){
                          question <- quesMan$question[x]
                          options <- quesMan$list[x]
                          id <- quesMan$number[x]
                          just <- answers$main |> 
                            filter(idQuestion == quesMan$idQuestion[x]) |> 
                            pull(justification)
                          sub <- quesMan$subgroup[x]
                          tagList(
                            h4(glue("MAN {id}: {question}")),
                            fluidRow(
                              div(style = "margin: 20px;",
                                  # column(5,
                                  render_quest_tab("MAN", id, question,
                                                   fromJSON(options)$opt,
                                                   fromJSON(options)$text,
                                                   answers_2_logical(answers$main, questions$main)),
                                  #        ),
                                  # column(7,
                                  br(),
                                  textAreaInput(glue("justMan{id}"),
                                                label = "Justification",
                                                value = just,  
                                                width = 'auto',
                                                height = '150px',
                                                resize = "horizontal")
                                  # )
                              )
                            ),
                            hr(style = "border-color: gray;")
                          )
                        })
        ),
        tabPanel(id = "ref", 
                 title = "References",
                 br(),
                 textAreaInput("ass_reftext",
                               label = "References",
                               value = ifelse(is.na(assessments$selected$reference), "", 
                                              assessments$selected$reference),  
                               width = 'auto',
                               height = '500px',
                               resize = "both")
        ),
        tabPanel(id = "sim", 
                 title = "Simulation",
                 br(),
                 tagList(
                   div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                       h3(strong("All Simulations for this Assessment"), style = "color:#7C6A56"),
                       DTOutput("simulations")
                       )
                   ),
                 br(),
                 h4(strong("Run New Simulation"), style = "color:#7C6A56"),
                 fluidRow(
                   column(8,
                          tagList(
                            div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                                h4(strong("Settings"), style = "color:#7C6A56"),
                                fluidRow(
                                  column(6,
                                         # numericInput("seed_sim", "Random Seed:", value = default_sim$seed, min = 1),
                                         numericInput("n_sim", "Iterations:", 
                                                      value = default_sim$n_sim, 
                                                      min = 1000, step = 100),
                                         numericInput("lambda_sim", "Lambda:", 
                                                      value = default_sim$lambda, 
                                                      min = 0, max = 100, step = 1)
                                  ),
                                  column(6,
                                         numericInput("w1_sim", "Weight 1\n(economic impact):", 
                                                      value = default_sim$w1, 
                                                      min = 0, max = 1, step = 0.01),
                                         numericInput("w2_sim", "Weight 2\n(environ. and social impacts):", 
                                                      value = default_sim$w2, 
                                                      min = 0, max = 1, step = 0.01)
                                  )
                                )
                            )
                          )
                   ),
                   column(4,
                          tagList(
                            div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                                actionButton("def_sim", "Default Settings"),
                                actionButton("new_sim", "Run Simulation"),
                                actionButton("save_sim", "Save Simulation")
                            )
                          )
                   )
                 ),
                 fluidRow(
                   div(style = "margin: 20px;",
                       tagList(
                         div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                             h4(strong("Results"), style = "color:#7C6A56"),
                             DTOutput("sim_results")
                         )
                       )
                   )
                 )
        )
      )
    )
  })
  
  ### Control all inputs dynamically ----
  observe({
    req(questions$main)
    # req(assessments$entry)
    answ_ent <- extract_answers(questions$main, groupTag = "ENT", input)
    answ_est <- extract_answers(questions$main, groupTag = "EST", input)
    answ_imp <- extract_answers(questions$main, groupTag = "IMP", input)
    answ_man <- extract_answers(questions$main, groupTag = "MAN", input)
    answ_all <- c(answ_ent, answ_est, answ_imp, answ_man)
# print(answ_all)    
    frominput$main <- get_inputs_as_df(answ_all, input) #, points$main
    
    if (!is.null(assessments$entry)) {
      answ_ent_path <- extract_answers_entry(questions$entry, groupTag = "ENT", 
                                        path = names(assessments$entry), 
                                        input)
      
      any_non <- any(!lapply(answ_ent_path, is.null) |> unlist())
      
      if(any_non){
        frominput$entry <- get_inputs_path_as_df(answ_ent_path, input) 
      } else {
        frominput$entry <- NULL
      }
    } else {
      frominput$entry <- NULL
    }
# print(frominput$entry)
  })
  
  # for (group in c("ENT1", "EST1", "EST2", "IMP1", "IMP2", "MAN1")) {
  #   local({
  #     g <- group
  #     answers <- extract_answers(questions$main, g, input)
  #     print(answers)
  #     # output[[paste0(g, "_warning")]] <- render_severity_warning(g, answers)
  #   })
  # }
  #### Error message for order of minimum likely maximum ----
  lapply(c("ENT1", "EST1", "EST2", "EST3", "EST4", "IMP1", "IMP3", 
           "MAN1", "MAN2", "MAN3", "MAN4", "MAN5"), function(tag){
    output[[paste0(tag, "_warning")]] <- renderUI({
      render_severity_warning(tag, frominput$main)
    })
  })
  
  lapply(c("IMP2.1", "IMP2.2", "IMP2.3", "IMP4.1", "IMP4.2", "IMP4.3"), function(tag){
    output[[paste0(tag, "_warning")]] <- renderUI({
      render_severity_boolean_warning(tag, frominput$main)
    })
  })
  
  #### Error message for entry pathways questions ----
  # this is hardcoded for now
  lapply(c(1:8), function(p){
    output[[paste0("ENT2A_", p, "_warning")]] <- renderUI({
      render_severity_warning("ENT2A", frominput$entry |> filter(path == p))
    })
  })
  lapply(c(1:8), function(p){
    output[[paste0("ENT2B_", p, "_warning")]] <- renderUI({
      render_severity_warning("ENT2B", frominput$entry |> filter(path == p))
    })
  })
  lapply(c(1:8), function(p){
    output[[paste0("ENT3_", p, "_warning")]] <- renderUI({
      render_severity_warning("ENT3", frominput$entry |> filter(path == p))
    })
  })
  lapply(c(1:8), function(p){
    output[[paste0("ENT4_", p, "_warning")]] <- renderUI({
      render_severity_warning("ENT4", frominput$entry |> filter(path == p))
    })
  })
  
  ## Questionaries pathways ----
  output$questionariePath <- renderUI({
    req(assessments$selected)
    req(assessments$entry)

    tabs <- lapply(names(assessments$entry), function(x){
      tabPanel(id = x, 
               title = pathways$data |>
                 filter(idPathway == x) |>
                 pull(name),
                div(style = "margin: 20px;",
                   h4(glue("ENT 2A: {questions$entry$question[1]}")),
                   render_quest_tab("ENT", paste0(questions$entry$number[1],"_", 
                                                  rep(x, length(questions$entry$number[1]))),
                                    questions$entry$question[1], 
                                    fromJSON(questions$entry$list[1])$opt,
                                    fromJSON(questions$entry$list[1])$text,
                                    answers_path_2_logical(answers$entry, questions$entry)),
                   br(),
                   textAreaInput(glue("justEnt2A_{x}"),
                                 label = "Justification",
                                 value = answers$entry |> 
                                   filter(idEntryPathway == x, idPathQuestion == 1) |> 
                                   pull(justification),
                                 width = 'auto',
                                 height = '150px',
                                 resize = "vertical"),
                   # ),
                   hr(style = "border-color: gray;"),
                   h4(glue("ENT 2B: {questions$entry$question[2]}")),
                    render_quest_tab("ENT", paste0(questions$entry$number[2],"_", 
                                                   rep(x, length(questions$entry$number[2]))),
                                     questions$entry$question[2], 
                                     fromJSON(questions$entry$list[2])$opt,
                                     fromJSON(questions$entry$list[2])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   br(),
                    textAreaInput(glue("justEnt2B_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idEntryPathway == x, idPathQuestion == 2) |> 
                                    pull(justification),
                                  width = 'auto',
                                  height = '150px',
                                  resize = "vertical"),
                   tags$hr(style = "border-color: gray;"),
                   h4(glue("ENT 3: {questions$entry$question[3]}")),
                    render_quest_tab("ENT", paste0(questions$entry$number[3],"_", 
                                                   rep(x, length(questions$entry$number[3]))),
                                     questions$entry$question[3],
                                     fromJSON(questions$entry$list[3])$opt,
                                     fromJSON(questions$entry$list[3])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   br(),
                    textAreaInput(glue("justEnt3_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idEntryPathway == x, idPathQuestion == 3) |> 
                                    pull(justification),
                                  width = 'auto',
                                  height = '150px',
                                  resize = "vertical"),
                   tags$hr(style = "border-color: gray;"),
                   h4(glue("ENT 4: {questions$entry$question[4]}")),
                    render_quest_tab("ENT", paste0(questions$entry$number[4],"_", 
                                                   rep(x, length(questions$entry$number[4]))),
                                     questions$entry$question[4], 
                                     fromJSON(questions$entry$list[4])$opt,
                                     fromJSON(questions$entry$list[4])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   br(),
                    textAreaInput(glue("justEnt4_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idEntryPathway == x, idPathQuestion == 4) |> 
                                    pull(justification),
                                  width = 'auto',
                                  height = '150px',
                                  resize = "vertical"),
                   tags$hr(style = "border-color: gray;")
                )
              )
    })
    
    ui <- do.call(tabsetPanel, tabs)
    return(ui)
  })
  
  ## Save Assessment ----
  # Mark as finished and valid
  observeEvent(input$ass_finish, {
    if (input$ass_finish == TRUE) {
      ## Check for the main questions
      answers_df <- answers$main |> 
        left_join(questions$main, by = "idQuestion")
      
      is_complete_main <- check_minmax_completeness(answers_df) 

      if (!is_complete_main) {
        shinyalert(
          title = "Incomplete Assessment",
          text = "Please answer all main assessment questions before saving.",
          type = "warning"
        )
        updateCheckboxInput(session, "ass_finish", value = FALSE)
        return()
      } 
      
      ## Check for the entry pathways questions
      if (length(assessments$entry) > 0) {
        answers_df <- answers$entry |> 
          left_join(questions$entry, by = "idPathQuestion")
        is_complete_entry <- check_minmax_completeness(answers_df, all = TRUE) 
        
        if (!is_complete_entry) {
          shinyalert(
            title = "Incomplete Pathway Assessment",
            text = "Please answer all pathway assessment questions before saving.",
            type = "warning"
          )
          return()
        }
        
      } 
      
      # If all checks pass
      dbExecute(con(), "UPDATE assessments SET finished = ?, endDate = ? WHERE idAssessment = ?",
                params = list(1, 
                              format(now("CET"), "%Y-%m-%d %H:%M:%S"), 
                              assessments$selected$idAssessment))
    } else {
      updateCheckboxInput(session, "ass_valid", value = FALSE)
      dbExecute(con(), "UPDATE assessments SET finished = ?, valid = ? WHERE idAssessment = ?",
                params = list(0, 0,
                              assessments$selected$idAssessment))
    }
    assessments$data <- dbReadTable(con(), "assessments")
    assessments$selected <- assessments$data[input$assessments_rows_selected, ]
    assessments$selected <- assessments$selected |> 
      left_join(pests$data, by = "idPest") |>
      left_join(assessors$data, by = "idAssessor") |> 
      mutate(label = paste(scientificName, eppoCode, 
                           paste(firstName, lastName), startDate, 
                           sep = "_"))
  })
  
  observeEvent(input$ass_valid, {
    if (assessments$selected$finished) {
### TODO is there another for the species also valid? ----
      # shinyalert(
      #   title = "Incomplete Pathway Assessment",
      #   text = "Please answer all pathway assessment questions before saving.",
      #   type = "warning"
      # )
      dbExecute(con(), "UPDATE assessments SET valid = ? WHERE idAssessment = ?",
                params = list(as.integer(input$ass_valid),
                              assessments$selected$idAssessment))
      
      assessments$data <- dbReadTable(con(), "assessments")
      assessments$selected <- assessments$data[input$assessments_rows_selected, ]
      assessments$selected <- assessments$selected |> 
        left_join(pests$data, by = "idPest") |>
        left_join(assessors$data, by = "idAssessor") |> 
        mutate(label = paste(scientificName, eppoCode, 
                             paste(firstName, lastName), startDate, 
                             sep = "_"))
    }
  })
  
  # Save Assessment
  observeEvent(input$save, {
    
    # Save assessment general info
    dbExecute(con(), "UPDATE assessments SET endDate = ?, 
                                              potentialEntryPathways = ?,
                                              reference = ?, 
                                              notes = ?
                      WHERE idAssessment = ?",
              params = list(format(now("CET"), "%Y-%m-%d %H:%M:%S"),
                            input$ass_pot_entry_path_text,
                            input$ass_reftext,
                            input$ass_notes,
                            assessments$selected$idAssessment))

    assessments$data <- dbReadTable(con(), "assessments")
    assessments$selected <- assessments$data[input$assessments_rows_selected, ]
    
    assessments$selected <- assessments$selected |> 
      left_join(pests$data, by = "idPest") |>
      left_join(assessors$data, by = "idAssessor") |> 
      mutate(label = paste(scientificName, eppoCode, 
                           paste(firstName, lastName), startDate, 
                           sep = "_"))
    
    # Insert associated threats
    threat_groups <- unique(threats$data$threatGroup)
    selected_threats <- sapply(threat_groups, function(group) {
      input[[paste0("group_", group)]]
      }, simplify = TRUE) |> unlist() |> as.integer()
    
    threats_to_add <- setdiff(selected_threats, assessments$threats$idThrSect)
    threats_to_remove <- setdiff(assessments$threats$idThrSect, selected_threats)

    # Add new threats
    if (length(threats_to_add) > 0) {
        for (threat_id in threats_to_add) {
          dbExecute(con(), "INSERT INTO threatXassessment(idAssessment, idThrSect) VALUES(?, ?)",
                    params = list(assessments$selected$idAssessment, threat_id))
        }
    }

    # Remove unchecked threats
    if (length(threats_to_remove) > 0) {
        for (threat_id in threats_to_remove) {
          dbExecute(con(), "DELETE FROM threatXassessment WHERE idAssessment = ? AND idThrSect = ?",
                    params = list(assessments$selected$idAssessment, threat_id))
        }
    }
  
    assessments$threats <- dbGetQuery(con(), glue("SELECT idThreat, threatXassessment.idThrSect, threatGroup, name FROM threatXassessment
                                             LEFT JOIN threatenedSectors ON threatXassessment.idThrSect = threatenedSectors.idThrSect
                                             WHERE idAssessment = {as.integer(assessments$selected$idAssessment)}"))
    

    # Proceed with saving the ANSWERS IN assessment
    # answ_ent <- extract_answers(questions$main, groupTag = "ENT", input)
    # answ_est <- extract_answers(questions$main, groupTag = "EST", input)
    # answ_imp <- extract_answers(questions$main, groupTag = "IMP", input)
    # answ_man <- extract_answers(questions$main, groupTag = "MAN", input)
    # answ_all <- c(answ_ent, answ_est, answ_imp, answ_man)
    # resmain <- get_inputs_as_df(answ_all, input) #, points$main
    resmain <- frominput$main 

    if (nrow(resmain) == 0) {
      # shinyalert(
      #   title = "No Answers to Save",
      #   text = "There are no answers to save for the main assessment questions.",
      #   type = "info"
      # )
      # return()
    } else {
      for (i in 1:nrow(resmain)) {
        # Check if the answer already exists
        idQue <- questions$main |> 
          filter(group == substr(resmain$question[i], 1, 3),
                 number == substr(resmain$question[i], 4, nchar(resmain$question[i]))) |> 
            pull(idQuestion)
        
        existing <- dbGetQuery(con(), "SELECT COUNT(*) as count FROM answers
                                      WHERE idAssessment = ? AND idQuestion = ?",
                               params = list(assessments$selected$idAssessment, idQue))
        
        ## Actually, the database controls for unique combinations of idAssessment and IdQuestion, so this count would never be higher than 1
        if (existing$count[1] > 0) { 
          # Update existing answer
          dbExecute(con(), "UPDATE answers SET min = ?, likely = ?, max = ?, justification = ?
                            WHERE idAssessment = ? AND idQuestion = ?",
                    params = list(
                      as.character(resmain$minimum[i]),
                      as.character(resmain$likely[i]),
                      as.character(resmain$maximum[i]),
                      as.character(resmain$justification[i]),
                      as.integer(assessments$selected$idAssessment),
                      as.integer(idQue)
                    )
          )
        } else {
          # Insert new answer
          dbExecute(con(), "INSERT INTO answers(idAssessment, idQuestion,
                            min, likely, max, justification)
                            VALUES(?, ?, ?, ?, ?, ?)",
                    params = list(
                      as.integer(assessments$selected$idAssessment),
                      as.integer(idQue),
                      as.character(resmain$minimum[i]),
                      as.character(resmain$likely[i]),
                      as.character(resmain$maximum[i]),
                      as.character(resmain$justification[i])
                    )
          )
        }
      } # end for main answers
      
    } # end if nrow resmain
    
    ## Save potential entry pathways text ----
    ### save entry answers
    selected_pathways <- assessments$selected_pathways
    current_pathways <- names(assessments$entry)
    paths_to_add <- setdiff(selected_pathways, current_pathways) |> as.integer()
    paths_to_remove <- setdiff(current_pathways, selected_pathways) |> as.integer()

    # Add new pathways
    if (length(paths_to_add) > 0) {
      for (path_id in paths_to_add) {
        dbExecute(con(), "INSERT INTO entryPathways(idAssessment, idPathway) VALUES(?, ?)",
                  params = list(assessments$selected$idAssessment, path_id))
      }
    }
    
    # Remove unchecked pathways
    if (length(paths_to_remove) > 0) {
      for (path_id in paths_to_remove) {
        idEntryPath <- dbGetQuery(con(), "SELECT idEntryPathway FROM entryPathways 
                                 WHERE idAssessment = ? AND idPathway = ?",
                                 params = list(assessments$selected$idAssessment, path_id)) |>
          pull(idEntryPathway)
        ### CAUTION here we delete also the answers for this pathway in a cascade
        dbExecute(con(), "DELETE FROM entryPathways WHERE idEntryPathway = ?",
                  params = list(idEntryPath))
        dbExecute(con(), "DELETE FROM pathwayAnswers WHERE idEntryPathway = ?",
                  params = list(idEntryPath))
      }
    }
    
    selected_entries <- dbGetQuery(con(), glue("SELECT * FROM entryPathways 
                                               WHERE idAssessment = {assessments$selected$idAssessment}"))

    if (nrow(selected_entries) > 0) {
      assessments$entry <- vector(mode = "list", length = nrow(selected_entries))
      names(assessments$entry) <- selected_entries$idPathway
    } else {
      assessments$entry <- NULL
    }
    
    if (!is.null(assessments$entry)) {
      answ_ent <- extract_answers_entry(questions$entry, groupTag = "ENT", 
                                         path = names(assessments$entry), 
                                         input)
      
      any_non <- any(!lapply(answ_ent, is.null) |> unlist())
      
      if(any_non){
        # resentry <- get_inputs_path_as_df(answ_ent, input) 
        resentry <- frominput$entry

        for (i in 1:nrow(resentry)) {
          # Check if the answer already exists
          idQue <- questions$entry |> 
            filter(group == substr(resentry$question[i], 1, 3),
                   number == substr(resentry$question[i], 4, nchar(resentry$question[i]))) |> 
            pull(idPathQuestion)
    
          idEntry <- selected_entries |> 
            filter(idPathway == resentry$path[i]) |> 
            pull(idEntryPathway)
    
          existing <- dbGetQuery(con(), "SELECT COUNT(*) as count FROM pathwayAnswers
                                        WHERE idEntryPathway = ? AND idPathQuestion = ?",
                                 params = list(idEntry, idQue))
          if (existing$count[1] > 0) { 
            # Update existing answer
            dbExecute(con(), "UPDATE pathwayAnswers SET min = ?, likely = ?, max = ?, justification = ?
                              WHERE idEntryPathway = ? AND idPathQuestion = ?",
                      params = list(
                        as.character(resentry$minimum[i]),
                        as.character(resentry$likely[i]),
                        as.character(resentry$maximum[i]),
                        as.character(resentry$justification[i]),
                        as.integer(idEntry),
                        as.integer(idQue)
                      )
            )
          } else {
            # Insert new answer
            dbExecute(con(), "INSERT INTO pathwayAnswers(idEntryPathway, idPathQuestion,
                              min, likely, max, justification)
                              VALUES(?, ?, ?, ?, ?, ?)",
                      params = list(
                        as.integer(idEntry),
                        as.integer(idQue),
                        as.character(resentry$minimum[i]),
                        as.character(resentry$likely[i]),
                        as.character(resentry$maximum[i]),
                        as.character(resentry$justification[i])
                      )
            )
          }
        }
      } # end if any non null
    } # end if entry not null
    
    # assessments$data <- dbReadTable(con(), "assessments")
    answers$main <- dbGetQuery(con(), glue("SELECT * FROM answers WHERE idAssessment = {assessments$selected$idAssessment}"))
    # answers$entry <- dbGetQuery(con(), glue("SELECT * FROM pathwayAnswers WHERE idEntryPathway IN ({paste(selected_entries$idEntryPathway, collapse = ', ')})"))
    answers$entry <- dbGetQuery(con(), glue("SELECT pa.*, ep.idAssessment, ep.idPathway
                                              FROM pathwayAnswers AS pa 
                                              LEFT JOIN entryPathways AS ep ON pa.idEntryPathway = ep.idEntryPathway
                                              WHERE pa.idEntryPathway IN ({paste(selected_entries$idEntryPathway, collapse = ', ')})"))
  })
  
  # Simulations ----
  output$simulations <- renderDT({
    req(simulations$data)
    req(assessments$selected)
    
    tab <- simulations$data |> 
      filter(idAssessment == assessments$selected$idAssessment) |>
      select(-c(idAssessment,idSimulation))
    
    datatable(tab,
              colnames = c("Iterarions (actually 'samples')", "Lambda", "Weigth 1", 
                           "Weight 2", "Date created"),
              selection = "single",
              rownames = FALSE,
              options = list(paging = FALSE,
                             searching = FALSE))
  })
  
  proxysimulations <- dataTableProxy("simulations")
  
  observeEvent(input$simulations_rows_selected, {
    # req(simulations$data)
    if (is.null(input$simulations_rows_selected)) {
      simulations$selected <- NULL
    } else {
      simulations$selected <- simulations$data[input$simulations_rows_selected, ]
      updateNumericInput(session, "n_sim", value = simulations$selected$iterations)
      updateNumericInput(session, "lambda_sim", value = simulations$selected$lambda)
      updateNumericInput(session, "w1_sim", value = simulations$selected$w1)
      updateNumericInput(session, "w2_sim", value = simulations$selected$w2)
      
      simulations$summary <- dbGetQuery(con(), glue("SELECT * FROM simulationSummaries 
                                               WHERE idSimulation = {simulations$selected$idSimulation}")) |> 
        select(-c(idSimSummary, idSimulation)) |> 
        as.data.frame()
    }
    
  })
  
  observeEvent(input$w1_sim, {
    # Update w2 to keep the sum at 1
    new_w2 <- round(1 - input$w1_sim, 2)
    updateNumericInput(session, "w2_sim", value = new_w2)
  })
  
  observeEvent(input$w2_sim, {
    # Update w1 to keep the sum at 1
    new_w1 <- round(1 - input$w2_sim, 2)
    updateNumericInput(session, "w1_sim", value = new_w1)
  })
  
  observeEvent(input$def_sim, {
    updateNumericInput(session, "n_sim", value = 5000)
    # updateNumericInput(session, "seed_sim", value = 12345)
    updateNumericInput(session, "lambda_sim", value = 1)
    updateNumericInput(session, "w1_sim", value = 0.5)
    updateNumericInput(session, "w2_sim", value = 0.5)
  })
  
  observeEvent(input$new_sim, {
    req(assessments$selected)
    if (as.logical(assessments$selected$finished)) {
      points_main <- points$main |> 
        rename_with(tolower) |> 
        select(-text)
      
      points_entry <- points$entry |> 
        rename_with(tolower) |> 
        select(-text)
      
      answers_df <- answers$main |> 
        left_join(questions$main, by = "idQuestion") |> 
        select(-list) |> 
        rename_with(tolower) |> 
        mutate(question = paste0(group, number)) |>
        left_join(points_main, by = c("question" = "question", "min" = "option")) |> 
        rename(min_points = points) |> 
        left_join(points_main, by = c("question" = "question", "likely" = "option")) |> 
        rename(likely_points = points) |> 
        left_join(points_main, by = c("question" = "question", "max" = "option")) |>
        rename(max_points = points) |>
        mutate(min_points = ifelse(is.na(min_points), 0, min_points),
               likely_points = ifelse(is.na(likely_points), 0, likely_points),
               max_points = ifelse(is.na(max_points), 0, max_points)) |> 
        mutate(
          question = case_when(
            question %in% c("IMP2.1", "IMP2.2", "IMP2.3") ~ "IMP2",
            question %in% c("IMP4.1", "IMP4.2", "IMP4.3") ~ "IMP4",
            TRUE ~ question
          )
        ) |>
        group_by(question) |>
        summarise(
          min_points = sum(as.numeric(min_points), na.rm = TRUE),
          likely_points = sum(as.numeric(likely_points), na.rm = TRUE),
          max_points = sum(as.numeric(max_points), na.rm = TRUE),
          .groups = "drop"
        )
      
  # answers_df_exp <<- answers_df
  
      answers_entry_df <- answers$entry |> 
        left_join(questions$entry, by = "idPathQuestion") |> 
        select(-list) |> 
        rename_with(tolower) |> 
        mutate(question = paste0(group, number)) |>
        left_join(points_entry, by = c("question" = "question", "min" = "option")) |> 
        rename(min_points = points) |> 
        left_join(points_entry, by = c("question" = "question", "likely" = "option")) |> 
        rename(likely_points = points) |> 
        left_join(points_entry, by = c("question" = "question", "max" = "option")) |>
        rename(max_points = points) |>
        mutate(min_points = ifelse(is.na(min_points), 0, min_points),
               likely_points = ifelse(is.na(likely_points), 0, likely_points),
               max_points = ifelse(is.na(max_points), 0, max_points)) 
  # answers_entry_df_exp <<- answers_entry_df
      
      # Run simulation function
      simulations$results <- simulation(answers_df, answers_entry_df, 
                                        iterations = input$n_sim, lambda = input$sim_lambda, 
                                        w1 = input$w1_sim , w2 = input$w2_sim)
      
      simulations$summary <- simulations$results |>
        as.data.frame() |> 
        reframe(across(everything(), list(
          min = ~min(.x, na.rm = TRUE) |> round(2),
          q5  = ~quantile(.x, 0.05, na.rm = TRUE) |> round(2),
          q25  = ~quantile(.x, 0.25, na.rm = TRUE) |> round(2),
          median  = ~quantile(.x, 0.50, na.rm = TRUE) |> round(2),
          q75  = ~quantile(.x, 0.75, na.rm = TRUE) |> round(2),
          q95  = ~quantile(.x, 0.95, na.rm = TRUE) |> round(2),
          max = ~max(.x, na.rm = TRUE) |> round(2),
          mean = ~mean(.x, na.rm = TRUE) |> round(2)
        ), .names = "{.col}_{.fn}")) |>
        pivot_longer(cols = everything(),
                     names_to = c("variable", "stat"),
                     names_sep = "_",
                     values_to = "value") |>
        pivot_wider(names_from = stat, values_from = value) |> 
        as.data.frame()
        
      message("simulation run")
      
    } else {
      shinyalert(
        title = "Assessment Not Finished",
        text = "Please finish the assessment before running simulations.",
        type = "warning"
      )
    }
    
  })
  
  output$sim_results <- renderDT({
    req(simulations$summary)
    
    datatable(simulations$summary |> 
                mutate(variable = c("Entry A*", "Entry B**", "Establishment", 
                                    "Invasion A*", "Invasion B**", "Impact", 
                                    "Preventavility", "Controlability", "Manageability")),
              rownames = FALSE,
              colnames = c("Variable", "Min", "5th Percentile", "25th Percentile", 
                            "Median", "75th Percentile", "95th Percentile", "Max", "Mean"),
              selection = "none",
              caption = HTML("* Not taking into account current entry management measures <br>** Taking into account current official entry management measures"),
              options = list(paging = FALSE, searching = FALSE)
              )
  })
  
  ## Save Simulation ----
  observeEvent(input$save_sim, {
    req(simulations$results)
    
    res <- dbExecute(conn = con(),
                     "INSERT INTO simulations(idAssessment, iterations, lambda, weight1, weight2, date)
                        VALUES(?,?,?,?,?,?);",
                     params = list(assessments$selected$idAssessment,
                                   input$n_sim,
                                   input$lambda_sim,
                                   input$w1_sim,
                                   input$w2_sim,
                                   format(today("CET"), "%Y-%m-%d"))
                     )
    
    # Insert simulation results
    sim_id <- dbGetQuery(con(), glue("SELECT MAX(idSimulation) AS idSimulation FROM simulations")) |> 
      as.integer()
    sim_sum <- simulations$summary |>
      as.data.frame() |>
      mutate(idSimulation = sim_id) |>
      select(idSimulation, everything())
    # sim_sum_exp <<- sim_sum

    dbWriteTable(con(), "simulationSummaries", sim_sum, append = TRUE, row.names = FALSE)

      # Insert rows one by one
      # for (i in seq_len(nrow(sim_sum))) {
      #   dbExecute(con(), 
      #             "INSERT INTO simulationSummaries (idSimulation, variable, min, q5,  
      #                                               q25, median, q75, q95, max, mean) 
      #             VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?);",
      #             params = as.list(sim_sum[i, ]) |> unname()
      #             )
      # }

    
    simulations$data <- dbReadTable(con(), "simulations")
    
    shinyalert(
      title = "Success",
      text = "Simulation saved successfully.",
      type = "success",
      timer = 1000,
    )
  })
  
  # Species ----
  # Show species lists
  output$pests <- renderTable({
    req(pests$data)
    pests$data
  })

  ## Modal for new pest ----
  observeEvent(input$new_pest, {
    showModal(modalDialog(
      title = "Add New Pest Species",
      # fluidRow(
      #   column(width = 6,
               textInput("new_sci", "Scientific Name*"),
               textInput("new_common", "Common Name"),
               textInput("new_synonyms", "Synonyms"),
               textInput("new_eppo", "EPPO code"),
               textInput("new_gbifuuid", "GBIF UUID"),
               selectInput("new_taxa", "Taxonomic Group*", choices = setNames(c("", taxa$data$idTaxa), 
                                                                              c("", taxa$data$name))),
               selectInput("new_quaran", "Quarantine Status*", choices = setNames(c("", quaran$data$idQuarantineStatus),
                                                                                  c("", quaran$data$name))),
               checkboxInput("new_ineu", "Is the pest species present in Europe?"),
        # ),
        # column(width = 6,
               
        # )
      # ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_pest", "Save")
      ),
      size = "m"
    ))
  })
  
  observeEvent(input$confirm_pest, {
    # Check required fields
    required_fields <- list(
      ScientificName = input$new_sci,
      Taxa = input$new_taxa,
      QuarantineStatus = input$new_quaran
    )
    
    missing_fields <- names(required_fields)[sapply(required_fields, function(x) is.null(x) || x == "")]
    if (length(missing_fields) > 0) {
      shinyalert(
        title = "Missing Required Fields",
        text = paste("Please fill in:", paste(missing_fields, collapse = ", ")),
        type = "warning"
      )
      return()
    }
    
    # Coalesce empty inputs to NA
    new_common   <- ifelse(input$new_common == "", NA, input$new_common)
    new_eppo     <- ifelse(input$new_eppo == "", NA, input$new_eppo)
    new_gbif     <- ifelse(input$new_gbifuuid == "", NA, input$new_gbifuuid)
    new_synonyms <- ifelse(input$new_synonyms == "", NA, input$new_synonyms)
    
    # Check for duplicates
    duplicate_sci  <- tolower(input$new_sci) %in% tolower(pests$data$scientificName)
    duplicate_eppo <- (input$new_eppo != "" && input$new_eppo %in% pests$data$eppoCode)
    duplicate_gbif <- (input$new_gbifuuid != "" && input$new_gbifuuid %in% pests$data$gbifuuid)
    
    if (duplicate_eppo || duplicate_gbif || duplicate_sci) {
      shinyalert(
        title = "Duplicate Entry",
        text = paste(
          if (duplicate_sci) "Scientific name already exists." else NULL,
          if (duplicate_eppo) "EPPO code already exists." else NULL,
          if (duplicate_gbif) "GBIF UUID already exists." else NULL,
          sep = "\n"
        ),
        type = "error"
      )
      return()
    }
    
    # Insert into pests table
    res <- dbExecute(conn = con(),
                     "INSERT INTO pests(scientificName, vernacularName, eppoCode, gbifuuid,
                                        synonyms, idTaxa, idQuarantineStatus, inEurope)
                        VALUES(?,?,?,?,?,?,?,?)
                        RETURNING idPest;",
                     params = list(input$new_sci, input$new_common, input$new_eppo, input$new_gbifuuid,
                                   input$new_synonyms, input$new_taxa, input$new_quaran, as.integer(input$new_ineu)))
    
    pests$data <- dbReadTable(con(), "pests")
    new_id <- pests$data |> 
      filter(scientificName == input$new_sci) |> 
      # alternativelly last()
      pull(idPest)
    
    shinyalert(
      title = "Success",
      text = "Pest and associated threats added successfully.",
      type = "success"
    )
    
    removeModal()
    # update_dropdowns()
  })
  
  # Assessors ----
  ## Modal for new user ----
  observeEvent(input$new_assessor, {
    showModal(modalDialog(
      title = "Add New Assessor",
      textInput("new_name", "First Name"),
      # textInputIcon(
      #   inputId = "ex1",
      #   label = "With an icon",
      #   icon = icon("circle-user")
      # ),
      textInput("new_last", "Last Name"),
      textInput("new_email", "Email"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_assessor", "Save")
      )
    ))
  })
  
  observeEvent(input$confirm_assessor, {
    dbExecute(con(), "INSERT INTO assessors(firstName, lastName, email) VALUES(?,?,?)",
              params = list(input$new_name, input$new_last, input$new_email))
    assessors$data <- dbReadTable(con(), "users")
    assessors$data$label <- paste(assessors$data$firstName, assessors$data$lastName)
    removeModal()
    # update_dropdowns()
  })
  
  # END ----
  session$onSessionEnded(function() {
    stopApp()
  })
} # END