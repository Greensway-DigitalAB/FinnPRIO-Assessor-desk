server <- function(input, output, session) {
  # Reactive values ####
  load <- reactiveValues(status = FALSE, timestamp = NULL)

  con <- reactiveVal()
  dbStatus <- reactiveValues(data = NULL, dibs = FALSE)
  assessors <- reactiveValues(data = NULL)
  threats <- reactiveValues(data = NULL)
  pests <- reactiveValues(data = NULL)
  taxa <- reactiveValues(data = NULL)
  quaran <- reactiveValues(data = NULL)
  pathways <- reactiveValues(data = NULL)
  pathways_n <- 0
  assessments <- reactiveValues(data = NULL, #questionarie = NULL, 
                                selected = NULL, entry = NULL,
                                selected_pathways = NULL,
                                threats = NULL)
  simulations <- reactiveValues(data = NULL, selected = NULL,
                                results = NULL)
  questions <- reactiveValues(main = NULL, entry = NULL)
  points <- reactiveValues(main = NULL, entry = NULL, table2 = NULL, table3 = NULL)
  answers <- reactiveValues(main = NULL, entry = NULL)
  frominput <- reactiveValues(main = NULL, entry = NULL)

  
  shinyFileChoose(input, "db_file", roots = volumes, session = session)
  db_file <- reactiveVal(NULL)
  
  ## In case you want to Uppload a file instead
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
  
  shinyDirChoose(input, "db_folder", roots = volumes, session = session,  
                 restrictions = system.file(package = "base"), allowDirCreate = FALSE)
  db_path <- reactiveVal(NULL)
  
  output$file_input_ui <- renderUI({
    if (load$status == FALSE){
      tagList(
        # fileInput("db_file", "Upload SQLite Database", accept = c(".sqlite", ".db")),
        shinyFilesButton("db_file", "Choose File", 
                         "Select file containing the SQLite database", 
                         multiple = FALSE, style = "margin-top: 20px;")
      )
    } else {
      l_path <- length(input$db_file$files$`0`)
      tagList(
        h3("Working with", input$db_file$files$`0`[[l_path]]),
        h5(db_path())#,
      )
    }
  })

  output$unload_db_ui <- renderUI({
    req(load$status)
    actionButton("unload_db", "Unload database", 
                 style = "margin-top: 20px;")
  })
  
  # output$close_app_ui <- renderUI({
  #   req(load$status)
  #   actionButton("close_app", "Close the app", 
  #                style = "margin-top: 20px;")
  # })
  
  observeEvent(input$db_file, {
    
    file_path <- parseFilePaths(volumes, input$db_file)$datapath
  
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
    req(db_path())
    load$status <- TRUE
    load$timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

    withProgress({
      setProgress(.1)
      consql <<- dbConnect(RSQLite::SQLite(), db_path())
      # consql <- dbConnect(RSQLite::SQLite(), dbname = input$db_file$datapath)
      con(consql)
      if (!is.null(con())) {
        message("Connection established")
        message("You are in Neo")
        setProgress(.3)
        dbStatus$data <- dbReadTable(con(), "dbStatus")
        
        ### if enough time has passed from the use set it to 0
        if (dbStatus$data$inUse == 1 ) {
          # then check time
          since <- difftime(now(), 
                            as_datetime(dbStatus$data$timeStamp), 
                            units = "hours")
          if(since > 4) { # hours
            # reset it  
            dbExecute(con(), "UPDATE dbStatus
                          SET inUse = 0,
                              timeStamp = CURRENT_TIMESTAMP
                          WHERE rowid = (SELECT MAX(rowid) FROM dbStatus);")
            dbStatus$data <- dbReadTable(con(), "dbStatus")
          } else {
            dbStatus$dibs <- FALSE
          }
        } else {
          dbStatus$dibs <- TRUE  
        }
        
        assessors$data <- dbReadTable(con(), "assessors")
        assessors$data$fullName <- paste(assessors$data$firstName, assessors$data$lastName)
        threats$data <- dbReadTable(con(), "threatenedSectors")
        pests$data <- dbReadTable(con(), "pests")
        taxa$data <- dbReadTable(con(), "taxonomicGroups")
        quaran$data <- dbReadTable(con(), "quarantineStatus")
        pathways$data <- dbReadTable(con(), "pathways")
        pathways_n <- nrow(pathways$data)
print(pathways_n)
        setProgress(.5)
        assessments$data <- dbReadTable(con(), "assessments")
        simulations$data <- dbReadTable(con(), "simulations")
        questions$main <- dbReadTable(con(), "questions")
        questions$entry <- dbReadTable(con(), "pathwayQuestions")
        points$main <- get_points_as_table(questions$main)
        points$entry <- get_points_as_table(questions$entry)
     } #else { stop() }
        setProgress(1)
    }, message = "Läser in bakgrund data")
  })
  
  # Load options and lists from DB
  observe({
    req(con())
    update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
  })
  
  ## disconnect or unload ----
  observeEvent(input$unload_db, {
    message("restarting session")
    if (dbStatus$dibs) { # make it available
      dbExecute(con(), "UPDATE dbStatus
                        SET inUse = 0,
                            timeStamp = CURRENT_TIMESTAMP
                        WHERE rowid = (SELECT MAX(rowid) FROM dbStatus);")
    }
    dbDisconnect(con())
    # session$close()
    session$reload()
    # shinyjs::runjs("location.reload();")
  })
  
  
  # observeEvent(input$disconnect_db, {
  #   runjs("document.getElementById('db_file').value = ''")
  #   dbDisconnect(con())
  #   con(NULL)
  #   session$reload()
  #   load$status <- FALSE
  # })
  
  # observeEvent(input$close_app,{
  #   message("closing session")
  #   dbDisconnect(con())
  #   session$close()
  # })
  
  ### Flag for usage ----
  output$db_status <- renderUI({
# print(load$status)
#     print(dbStatus$data)
    if (is.null(dbStatus$dibs) ) return(NULL)
    if (!load$status){
      return(NULL)
    } else {
      if(nrow(dbStatus$data) > 1){
        tagList(
          h5("it seems the tabel 'dbStatus' has been corrupted. It should contain only one row.")#,
        )
      } else { # if db ok
        if( dbStatus$dibs) {
          # is not being used
          # tagList(
          #   h5("")#,
          # )
          # you dont need any warning
          # Claim it DIBS
          dbExecute(con(), "UPDATE dbStatus 
                       SET inUse = 1,
                           timeStamp = CURRENT_TIMESTAMP
                       WHERE rowid = (SELECT MAX(rowid) FROM dbStatus);")
          return(NULL)
        } else {
          # it is being used
          shinyalert("Be careful", "It seems somebody else is using the database file. <br>
                     Although it is ok to browse the data, it may get damaged or sessions may act weird if you proceed.<br>
                     Do you want to proceed anyway?",
                     "warning",
                     showConfirmButton = TRUE, showCancelButton = TRUE,
                     confirmButtonText = "YES", cancelButtonText = "NO",
                     timer = 0, animation = TRUE, html = TRUE,
                     callbackR = function(value) {
                       if (value) {
                         
                       } else {
                         message("ending session due to simultaneous use")
                         dbDisconnect(con())
                         session$close()
                       }
                     })  # END if Value, callback, shinyAlert
          
          disable("new_ass")
          disable("save_answers")
          disable("save_general")
          # disable("save_sim")
          
          tagList(
            h4("The database is being used by your colleage",  style = "color: red; font-weight: bold;")#,
          )
        }

      }

    }
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
      left_join(simulations$data |> 
                  group_by(idAssessment) |> 
                  mutate(n_simulations = n()) |> 
                  slice_max(date, n = 1, with_ties = FALSE) |> 
                  ungroup(),
                by = "idAssessment") |> 
      mutate(n_simulations = replace_na(n_simulations, 0)) |> 
      select(idAssessment, scientificName, eppoCode, fullName, 
             startDate, endDate, finished,	valid,	notes,	version,
             iterations, lambda, weight1, weight2, date, n_simulations)

    datatable(tab, 
              class = 'row-border stripe compact hover',
              extensions = 'Buttons', 
              rownames = FALSE, selection = 'single', autoHideNavigation = FALSE,
              colnames = c("Pest", "Pest species", "EPPO code", "Assessor", "Started", 
                           "Last Edited", "Finished", "Valid", "Notes", "Version",
                           "Sim n. iterations", "Sim lambda", "Sim weight 1", "Sim weight 2", 
                           "Sim last date", "N. simulations"),
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
      formatDate(columns = c('endDate', 'date'), method = 'toLocaleString')
                     
  })
  
  proxyassessments <- dataTableProxy("assessments")
  
  
  ## Modal for new assessment ----
  observeEvent(input$new_ass, {
    answers$main <- NULL
    answers$entry <- NULL
    frominput$main <- NULL
    frominput$entry <- NULL
    assessments$selected <- NULL
    assessments$entry <- NULL
    assessments$threats <- NULL
    simulations$selected <- NULL
    simulations$results <- NULL
    simulations$summary <- NULL
    proxyassessments |> selectRows(NULL)  
    proxysimulations |> selectRows(NULL)  

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
    id_new_ass <- max(assessments$data$idAssessment) ## highest if they return disordered
    
    if (length(input$pot_entry_path) > 0) {
      for (p in input$pot_entry_path) {
        dbExecute(con(), "INSERT INTO entryPathways(idAssessment, idPathway) VALUES(?,?)",
                  params = list(id_new_ass, p))
      }
    }
    # assessments$entry <- dbReadTable(con(), "entryPathways")
    removeModal()
    # update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
  })
  
  ## Export wide table ----
  output$export_wide <- downloadHandler(
    filename = function() { "wide_table.csv" },
    content = function(file) {
      ## This function returns only ONE assessment per species, and it is the valid or latest one
      data <- export_wide_table(con(), only_one = !input$exp_all)
      write.csv(data, file, row.names = FALSE, 
                fileEncoding = "UTF-8")
    }
  )
  
  ## Select assessment ----
  # observeEvent(input$edit_ass, {
  observeEvent(input$assessments_rows_selected, {
    assessments$selected <- NULL
    if (!is.null(input$assessments_rows_selected)) {
      ## OBS! watch here the selection process if you use filters
      assessments$selected <- assessments$data[input$assessments_rows_selected, ]
      
      assessments$selected <- assessments$selected |> 
        left_join(pests$data, by = "idPest") |>
        left_join(assessors$data, by = "idAssessor") |> 
        mutate(label = paste(scientificName, eppoCode, 
                             paste(firstName, lastName), startDate, 
                             sep = "_"))
    }
  })
  
  ## Assessments summary ----
  observeEvent(assessments$selected,{
    frominput$main <- NULL
    frominput$entry <- NULL
     
    assessments$entry <- NULL
    assessments$threats <- NULL
    answers$main <- NULL
    answers$entry <- NULL
    simulations$selected <- NULL
    simulations$results <- NULL
    simulations$summary <- NULL
    if(!is.null(assessments$selected)){
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
      
      updateTabsetPanel(session, "all_assessments", selected = "sel")
    }
    
  })
  
  ### Tab name ----
  output$selectedAssName <- renderUI({
    if (!is.null(input$assessments_rows_selected)) {
      res <- tagList(icon("file-lines", class = "fas"), assessments$selected$label)
    } else {
      res <- tagList(icon("file", class = "fas"), "Selected Assessment")
    }
    return(res)
  })
  
  output$assessment_summary <- renderUI({
    # req(input$assessments_rows_selected)
    if(is.null(assessments$selected)){
      ui <- NULL
    } else {
      ass_info <- assessments$selected
      
      # Build card UI
      ui <- tagList(
                fluidRow(
                   column(4,
                          div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                              
                            h3(em(ass_info$scientificName)),
                            h4(ass_info$fullName),
                            p("Created on", ass_info$startDate),
                            p("Last edited on", ass_info$endDate),
                            p("Questionary ver.", ass_info$version),
                            checkboxInput("ass_finish", label = "Is finished?", value = ass_info$finished),
                            checkboxInput("ass_valid", label = "Is valid?", value = ass_info$valid),
                            div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                              downloadButton("download_report", "Download Assessment Report"),
                              actionButton("save_general", "Save assessment details")
                            ),
                            br(),
                            uiOutput("species_summary")
                          )
                          ),
                   column(4,
                            h4(strong("Hosts"), style = "color:#7C6A56"),
                            textAreaInput("ass_hosts", label = "", 
                                          value = ifelse(is.na(ass_info$hosts), "", ass_info$hosts),
                                          width = "auto", height = "200px", resize = "vertical"),
                            br(),         
                            uiOutput("threat_checkboxes")
                          ),
                    column(4,
                           uiOutput("entrypath_checkboxes")
                    )
                ),
               fluidRow(
                 column(12,
                    div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                        h4(strong("Notes"), style = "color:#7C6A56"),
                        textAreaInput("ass_notes", label = "", 
                                      value = ifelse(is.na(ass_info$notes), "", ass_info$notes),
                                      width = "auto", height = "500px", resize = "vertical")
                        )
                    )
                 )
      )
    }
    return(ui)
  })
  
  ### Download a report ----
  output$download_report <- downloadHandler(
    filename = function() { paste0("assessment_report_", assessments$selected$label, ".docx") },
    content = function(file) {
      req(assessments$selected)
      # Check if assessment is finished
      if (is.null(assessments$selected$finished) || !assessments$selected$finished) {
        shinyalert(
          title = "Assessment Not Finished",
          text = "You cannot download the report because the assessment is not completed.",
          type = "error"
        )
        return(NULL)  # Stop the download
      }
      
      # Proceed if finished
      file_path <- report_assessment(con(), assessments$selected, questions$main, answers$main, 
                                     assessments$entry, questions$entry, answers$entry, 
                                     simulations$data)
      file.copy(file_path, file)
    }
  )
  
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
          p(ifelse(!is.na(ass_info$vernacularName), ass_info$vernacularName, "")),
          p(strong("EPPO code: "), ass_info$eppoCode),
          p(strong("GBIF taxon key: "), ass_info$gbifTaxonKey),
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
    if(dbStatus$dibs) enable("save_general") else disable("save_general")
  })
   

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
  
  ## Questionaries ----
  output$questionarie <- renderUI({
    req(questions$main)
    req(answers$main)
    # req(assessments$selected)
    req(input$assessments_rows_selected)
    
    quesEnt <- questions$main |> filter(group == "ENT") |> arrange(number)
    quesEst <- questions$main |> filter(group == "EST") |> arrange(number)
    quesImp <- questions$main |> filter(group == "IMP") |> arrange(number)
    quesMan <- questions$main |> filter(group == "MAN") |> arrange(number)
    
    answers_logical <- answers_2_logical(answers$main, questions$main)
    
    if(is.null(assessments$selected)){
      ui <- NULL
    } else {
      ui <- tagList(
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("General Information"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            uiOutput("assessment_summary"),
          ),
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("Entry"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            tagList(
                     lapply(1:nrow(quesEnt),
                            function(x){
                              question <- quesEnt$question[x]
                              options <- quesEnt$list[x]
                              id <- quesEnt$number[x]
                              info <- quesEnt$info[x]
                              just <- answers$main |>
                                filter(idQuestion == quesEnt$idQuestion[x]) |>
                                pull(justification)
                              tagList(
                                div(style = "display: flex; align-items: center; gap: 8px;",
                                  h4(glue("ENT {id}: {question}")),
                                  h4("(i)",
                                   tags$span(HTML(info),
                                             style = "color:black; font-size: 12px;"),
                                   class = "bubble")
                                ),
                                fluidRow(
                                  div(id = glue("ent{id}_table_container"),
                                      style = "margin: 20px;",
                                      render_quest_tab("ENT", id, question,
                                                       fromJSON(options)$opt,
                                                       fromJSON(options)$text,
                                                       answers_logical),
                                      br(),
                                      textAreaInput(glue("justENT{id}"),
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
                     h4("Pathways"),
                     uiOutput("questionariePath")
              
            )
            # )
        ),
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("Establishment and Spread"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            lapply(1:nrow(quesEst), 
                          function(x){
                            question <- quesEst$question[x]
                            options <- quesEst$list[x]
                            id <- quesEst$number[x]
                            info <- quesEst$info[x]
                            just <- answers$main |> 
                              filter(idQuestion == quesEst$idQuestion[x]) |> 
                              pull(justification)
                            tagList(
                              div(style = "display: flex; align-items: center; gap: 8px;",
                                  h4(glue("EST {id}: {question}")),
                                  h4("(i)",
                                     tags$span(HTML(info), 
                                               style = "color:black; font-size: 12px;"),
                                     class = "bubble")
                              ),
                              fluidRow(
                                div(id = glue("est{id}_table_container"),
                                    style = "margin: 20px;",
                                    render_quest_tab("EST", id, question,
                                                     fromJSON(options)$opt,
                                                     fromJSON(options)$text,
                                                     answers_logical),
                                    br(),
                                    textAreaInput(glue("justEST{id}"),
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
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("Impact"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
                   lapply(1:nrow(quesImp),
                          function(x){
                            question <- quesImp$question[x]
                            options <- quesImp$list[x]
                            id <- quesImp$number[x]
                            info <- quesImp$info[x]
                            just <- answers$main |> 
                              filter(idQuestion == quesImp$idQuestion[x]) |> 
                              pull(justification)
                            type <- quesImp$type[x]
                            tagList(
                              div(style = "display: flex; align-items: center; gap: 8px;",
                                  h4(glue("IMP {id}: {question}")),
                                  h4("(i)",
                                     tags$span(HTML(info), 
                                               style = "color:black; font-size: 12px;"),
                                     class = "bubble")
                              ),
                              fluidRow(
                                div(id = glue("imp{id}_table_container"),
                                    style = "margin: 20px;",
                                    render_quest_tab("IMP", id, question,
                                                     fromJSON(options)$opt,
                                                     fromJSON(options)$text,
                                                     answers_logical,
                                                     type),
                                    br(),
                                    textAreaInput(glue("justIMP{id}"),
                                                  label = "Justification",
                                                  value = just,  
                                                  width = 'auto',
                                                  height = '150px',
                                                  resize = "vertical")
                                )
                              ),
                              hr(style = "border-color: gray;")
                            )
                          })
          ),
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("Management"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            lapply(1:nrow(quesMan),
                          function(x){
                            question <- quesMan$question[x]
                            options <- quesMan$list[x]
                            id <- quesMan$number[x]
                            info <- quesMan$info[x]
                            just <- answers$main |> 
                              filter(idQuestion == quesMan$idQuestion[x]) |> 
                              pull(justification)
                            sub <- quesMan$subgroup[x]
                            tagList(
                              div(style = "display: flex; align-items: center; gap: 8px;",
                                  h4(glue("MAN {id}: {question}")),
                                  h4("(i)",
                                     tags$span(HTML(info), 
                                               style = "color:black; font-size: 12px;"),
                                     class = "bubble")
                              ),
                              fluidRow(
                                div(id = glue("man{id}_table_container"),
                                    style = "margin: 20px;",
                                    render_quest_tab("MAN", id, question,
                                                     fromJSON(options)$opt,
                                                     fromJSON(options)$text,
                                                     answers_logical),
                                    br(),
                                    textAreaInput(glue("justMAN{id}"),
                                                  label = "Justification",
                                                  value = just,  
                                                  width = 'auto',
                                                  height = '150px',
                                                  resize = "horizontal")
                                )
                              ),
                              hr(style = "border-color: gray;")
                            )
                          })
          ),
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("References"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            br(),
                   textAreaInput("ass_reftext",
                                 label = "References",
                                 value = ifelse(is.na(assessments$selected$reference), "", 
                                                assessments$selected$reference),  
                                 width = 'auto',
                                 height = '500px',
                                 resize = "both")
          ),
        div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
            h3(strong("Simulations"), style = "color:#7C6A56; background:#F8F5F0; padding:5px;"),
            tagList(
                     div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                         h4(strong("All Simulations for this Assessment"), style = "color:#7C6A56"),
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
                      tagList(
                           div(class = "card", style = "padding: 20px; margin-top: 20px; border: 1px solid #ccc; border-radius: 8px;",
                               h4(strong("Results"), style = "color:#7C6A56"),
                               DTOutput("sim_results")
                           )
                     )
                   )
          )
        # )
      )
    } # end if(is.null(assessments$selected))
    
    return(ui)
  })
  
  ### Control all inputs dynamically ----
  observe({
    req(questions$main)
    answ_ent <- extract_answers(questions$main, groupTag = "ENT", input)
    answ_est <- extract_answers(questions$main, groupTag = "EST", input)
    answ_imp <- extract_answers(questions$main, groupTag = "IMP", input)
    answ_man <- extract_answers(questions$main, groupTag = "MAN", input)
    answ_all <- c(answ_ent, answ_est, answ_imp, answ_man)

    frominput$main <- get_inputs_as_df(answ_all, input) 
  })
  
  #### Error message for order of minimum likely maximum ----
  lapply(c("ENT1", "EST1", "EST2", "EST3", "EST4", "IMP1", "IMP3", 
           "MAN1", "MAN2", "MAN3", "MAN4", "MAN5"), function(tag){
    output[[paste0(tag, "_warning")]] <- renderUI({
      req(frominput$main)
      render_severity_warning(tag, frominput$main)
    })
  })
  
  lapply(c("IMP2.1", "IMP2.2", "IMP2.3", "IMP4.1", "IMP4.2", "IMP4.3"), function(tag){
    output[[paste0(tag, "_warning")]] <- renderUI({
      req(frominput$main)
      render_severity_boolean_warning(tag, frominput$main)
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
                    div(style = "display: flex; align-items: center; gap: 8px;",
                        h4(glue("ENT 2A: {questions$entry$question[1]}")),
                        h4("(i)",
                           tags$span(HTML(questions$entry$info[1]), 
                                     style = "color:black; font-size: 12px;"),
                           class = "bubble")
                    ),
                   render_quest_tab("ENT", paste0(questions$entry$number[1],"_", 
                                                  rep(x, length(questions$entry$number[1]))),
                                    questions$entry$question[1], 
                                    fromJSON(questions$entry$list[1])$opt,
                                    fromJSON(questions$entry$list[1])$text,
                                    answers_path_2_logical(answers$entry, questions$entry)),
                   uiOutput(glue("ENT2A_{x}_warning")), ## This is because the rendering inside render_quest_tab does not work with pathways
                   br(),
                   textAreaInput(glue("justENT2A_{x}"),
                                 label = "Justification",
                                 value = answers$entry |>
                                   filter(idPathway == x, idPathQuestion == 1) |>
                                   pull(justification),
                                 width = 'auto',
                                 height = '150px',
                                 resize = "vertical"),
                   hr(style = "border-color: gray;"),
                   div(style = "display: flex; align-items: center; gap: 8px;",
                       h4(glue("ENT 2b: {questions$entry$question[2]}")),
                       h4("(i)",
                          tags$span(HTML(questions$entry$info[2]), 
                                    style = "color:black; font-size: 12px;"),
                          class = "bubble")
                   ),
                    render_quest_tab("ENT", paste0(questions$entry$number[2],"_", 
                                                   rep(x, length(questions$entry$number[2]))),
                                     questions$entry$question[2], 
                                     fromJSON(questions$entry$list[2])$opt,
                                     fromJSON(questions$entry$list[2])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   uiOutput(glue("ENT2B_{x}_warning")), ## This is because the rendering inside render_quest_tab does not work with pathways
                   br(),
                    textAreaInput(glue("justENT2B_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idPathway == x, idPathQuestion == 2) |> 
                                    pull(justification),
                                  width = 'auto',
                                  height = '150px',
                                  resize = "vertical"),
                   tags$hr(style = "border-color: gray;"),
                   div(style = "display: flex; align-items: center; gap: 8px;",
                       h4(glue("ENT 3: {questions$entry$question[3]}")),
                       h4("(i)",
                          tags$span(HTML(questions$entry$info[3]), 
                                    style = "color:black; font-size: 12px;"),
                          class = "bubble")
                   ),
                    render_quest_tab("ENT", paste0(questions$entry$number[3],"_", 
                                                   rep(x, length(questions$entry$number[3]))),
                                     questions$entry$question[3],
                                     fromJSON(questions$entry$list[3])$opt,
                                     fromJSON(questions$entry$list[3])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   uiOutput(glue("ENT3_{x}_warning")), ## This is because the rendering inside render_quest_tab does not work with pathways
                   br(),
                    textAreaInput(glue("justENT3_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idPathway == x, idPathQuestion == 3) |> 
                                    pull(justification),
                                  width = 'auto',
                                  height = '150px',
                                  resize = "vertical"),
                   tags$hr(style = "border-color: gray;"),
                   div(style = "display: flex; align-items: center; gap: 8px;",
                       h4(glue("ENT 4: {questions$entry$question[4]}")),
                       h4("(i)",
                          tags$span(HTML(questions$entry$info[4]), 
                                    style = "color:black; font-size: 12px;"),
                          class = "bubble")
                   ),
                    render_quest_tab("ENT", paste0(questions$entry$number[4],"_", 
                                                   rep(x, length(questions$entry$number[4]))),
                                     questions$entry$question[4], 
                                     fromJSON(questions$entry$list[4])$opt,
                                     fromJSON(questions$entry$list[4])$text,
                                     answers_path_2_logical(answers$entry, questions$entry)),
                   uiOutput(glue("ENT4_{x}_warning")), ## This is because the rendering inside render_quest_tab does not work with pathways
                   br(),
                    textAreaInput(glue("justENT4_{x}"),
                                  label = "Justification",
                                  value = answers$entry |> 
                                    filter(idPathway == x, idPathQuestion == 4) |> 
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
  
  ### Observe inputs for Entries ----
  observe({
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
  })
  
  
  #### Error message for entry pathways questions ----
  # this is hardcoded for now i cant get to read the reactive pathways and a loop wont work
  lapply(c("ENT2A", "ENT2B","ENT3","ENT4"), function(tag){
      output[[paste0(tag,"_", 1, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 1))
      })
      output[[paste0(tag,"_", 2, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 2))
      })
      output[[paste0(tag,"_", 3, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 3))
      })
      output[[paste0(tag,"_", 4, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 4))
      })
      output[[paste0(tag,"_", 5, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 5))
      })
      output[[paste0(tag,"_", 6, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 6))
      })
      output[[paste0(tag,"_", 7, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 7))
      })
      output[[paste0(tag,"_", 8, "_warning")]] <- renderUI({
        req(frominput$entry)
        render_severity_warning(tag, frominput$entry |> filter(path == 8))
      })
  })
  

  
# Save Assessment ----
  ## Mark as finished and valid ----
  observeEvent(input$ass_finish, {
    req(answers$main)
    if (input$ass_finish == TRUE) {
      ## Check for the main questions
      answers_df <- answers$main |> 
        left_join(questions$main, by = "idQuestion")
  
    ## check if all questions are complete
      all_req_main <- all(questions$main |> 
                            filter(type == "minmax") |> 
                            pull(idQuestion) %in% answers_df$idQuestion)
      if (!all_req_main) {
        shinyalert(
          title = "Incomplete Assessment",
          text = "Please answer all assessment questions before saving.",
          type = "warning"
        )
        updateCheckboxInput(session, "ass_finish", value = FALSE)
        return()
      } 

      is_complete_main <- check_minmax_completeness(answers_df) 

      if (!is_complete_main) {
        shinyalert(
          title = "Incomplete Assessment",
          text = "Please answer all options in the assessment questions before saving.",
          type = "warning"
        )
        updateCheckboxInput(session, "ass_finish", value = FALSE)
        return()
      } 
      
      ## Check for the entry pathways questions
      if (length(assessments$entry) > 0) {
        for (p in 1:length(assessments$entry)){
          answers_df <- answers$entry |> 
            filter(idPathway == names(assessments$entry)[p]) |> 
            left_join(questions$entry, by = "idPathQuestion")

          all_req_entry <- all(questions$entry |> 
                                 pull(idPathQuestion) %in% answers_df$idPathQuestion)

          if (!all_req_entry) {
            shinyalert(
              title = "Incomplete Pathway Assessment",
              text = "Please answer all questions for each pathway before saving.",
              type = "warning"
            )
            updateCheckboxInput(session, "ass_finish", value = FALSE)
            return()
          }

          is_complete_entry <- check_minmax_completeness(answers_df, all = TRUE) 
          if (!is_complete_entry) {
            shinyalert(
              title = "Incomplete Pathway Assessment",
              text = "Please answer all options on each pathway question before saving.",
              type = "warning"
            )
            updateCheckboxInput(session, "ass_finish", value = FALSE)
            return()
          }
        }
      } 
      
      # If all checks pass
      dbExecute(con(), "UPDATE assessments SET finished = ?, endDate = ? WHERE idAssessment = ?",
                params = list(1, 
                              format(now("CET"), "%Y-%m-%d %H:%M:%S"), 
                              assessments$selected$idAssessment))
      assessments$selected$finished <- 1 
      
    } else {
      updateCheckboxInput(session, "ass_valid", value = FALSE)
      dbExecute(con(), "UPDATE assessments SET finished = ?, valid = ? WHERE idAssessment = ?",
                params = list(0, 0,
                              assessments$selected$idAssessment))
      assessments$selected$finished <- 0
      assessments$selected$valid <- 0
      
    }
    
    # reload the assessments data
    assessments$data <- dbReadTable(con(), "assessments")
    # assessments$selected <- assessments$data[input$assessments_rows_selected, ]
    # assessments$selected <- assessments$selected |> 
    #   left_join(pests$data, by = "idPest") |>
    #   left_join(assessors$data, by = "idAssessor") |> 
    #   mutate(label = paste(scientificName, eppoCode, 
    #                        paste(firstName, lastName), startDate, 
    #                        sep = "_"))
  }, ignoreInit = TRUE)
  
  observeEvent(input$ass_valid, {
    req(answers$main)
    if (input$ass_valid){
      
      if (assessments$selected$finished) {
        others <- assessments$data |> 
          filter(idAssessment != assessments$selected$idAssessment,
                 idPest == assessments$selected$idPest,
                 valid == 1)
        if(nrow(others) > 0) {
        ## is there another assessment for the species also valid?
          shinyalert(
            title = "There are other assessment marked as valid",
            text = "For this species, there is another assessment marked as valid. \n Would you lke to make this the valid one?",
            type = "info", 
            showConfirmButton = TRUE, showCancelButton = TRUE,
            confirmButtonText = "YES", cancelButtonText = "NO",  
            timer = 0, animation = TRUE,
            callbackR = function(value) { 
              if (value) {
                dbExecute(con(), "UPDATE assessments SET valid = ? WHERE idAssessment = ?",
                          params = list(0, others$idAssessment))
                
                dbExecute(con(), "UPDATE assessments SET valid = ? WHERE idAssessment = ?",
                          params = list(as.integer(input$ass_valid),
                                        assessments$selected$idAssessment))
                assessments$selected$valid <- as.integer(input$ass_valid)
              }})  # END if Value, callback, shinyAlert
          
          
        } else {
          dbExecute(con(), "UPDATE assessments SET valid = ? WHERE idAssessment = ?",
                    params = list(as.integer(input$ass_valid),
                                  assessments$selected$idAssessment))
          assessments$selected$valid <- as.integer(input$ass_valid)
        }
        
        assessments$data <- dbReadTable(con(), "assessments")
        # assessments$selected <- assessments$data[input$assessments_rows_selected, ]
        # assessments$selected <- assessments$selected |> 
        #   left_join(pests$data, by = "idPest") |>
        #   left_join(assessors$data, by = "idAssessor") |> 
        #   mutate(label = paste(scientificName, eppoCode, 
        #                        paste(firstName, lastName), startDate, 
        #                        sep = "_"))
        
      } else {
        updateCheckboxInput(session, "ass_valid", value = FALSE)
      }
    } # if not set to true, dont bother
  }, ignoreInit = TRUE)
  
  ## Save general information ----
  observeEvent(input$save_general, {
    
    req(assessments$selected)
    req(assessments$threats)
    # tab_now <- input$questionarie_tab
    # Save assessment general info
    dbExecute(con(), "UPDATE assessments SET endDate = ?, 
                                              hosts = ?,
                                              potentialEntryPathways = ?,
                                              reference = ?, 
                                              notes = ?
                      WHERE idAssessment = ?",
              params = list(format(now("CET"), "%Y-%m-%d %H:%M:%S"),
                            input$ass_hosts,
                            input$ass_pot_entry_path_text,
                            input$ass_reftext,
                            input$ass_notes,
                            assessments$selected$idAssessment))
    
    assessments$selected$endDate <- format(now("CET"), "%Y-%m-%d %H:%M:%S")
    assessments$selected$hosts <- input$ass_hosts
    assessments$selected$potentialEntryPathways <- input$ass_pot_entry_path_text
    assessments$selected$references <- input$ass_reftext
    assessments$selected$notes <- input$ass_notes
    
    assessments$data <- dbReadTable(con(), "assessments")
    # assessments$selected <- assessments$data[input$assessments_rows_selected, ]
    # 
    # assessments$selected <- assessments$selected |> 
    #   left_join(pests$data, by = "idPest") |>
    #   left_join(assessors$data, by = "idAssessor") |> 
    #   mutate(label = paste(scientificName, eppoCode, 
    #                        paste(firstName, lastName), startDate, 
    #                        sep = "_"))
    
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
    
    ### save entry pathways
    selected_pathways <- input$ass_pot_entry_path
    current_pathways <- names(assessments$entry)
    paths_to_add <- setdiff(selected_pathways, current_pathways) |> as.integer()
    paths_to_remove <- setdiff(current_pathways, selected_pathways) |> as.integer()
    
    # Add new pathways
    if (length(paths_to_add) > 0) {
      for (path_id in paths_to_add) {
        dbExecute(con(), "INSERT INTO entryPathways(idAssessment, idPathway) VALUES(?, ?)",
                  params = list(assessments$selected$idAssessment, path_id))
      }
      # updateTabsetPanel(session, "questionarie_tab", selected = tab_now)
    } # end add
    
    selected_entries <- dbGetQuery(con(), glue("SELECT * FROM entryPathways 
                                               WHERE idAssessment = {assessments$selected$idAssessment}"))
    if (nrow(selected_entries) > 0) {
      assessments$entry <- vector(mode = "list", length = nrow(selected_entries))
      names(assessments$entry) <- selected_entries$idPathway
    } else {
      assessments$entry <- NULL
    }
    
    # Remove unchecked pathways
    if (length(paths_to_remove) > 0) {
      for (path_id in paths_to_remove) {
        idEntryPath <- dbGetQuery(con(), "SELECT idEntryPathway FROM entryPathways 
                                 WHERE idAssessment = ? AND idPathway = ?",
                                  params = list(assessments$selected$idAssessment, path_id)) |>
          pull(idEntryPathway)
        ### CAUTION here we delete also the answers for this pathway in a cascade
        dbExecute(con(), "DELETE FROM pathwayAnswers WHERE idEntryPathway = ?",
                  params = list(idEntryPath))
        dbExecute(con(), "DELETE FROM entryPathways WHERE idEntryPathway = ?",
                  params = list(idEntryPath))
        
      }
      updateTabsetPanel(session, "all_assessments", selected = "all")
      proxyassessments |> selectRows(NULL)  
    } # end remove
    
    shinyalert(
      title = "Success",
      text = "Assessment details saved successfully.",
      type = "success",
      timer = 1000,
    )
    
  }, ignoreInit = TRUE)
  
  ## Save all answers in Assessment ----
  # observe({
  #   # req(input$questionarie_tab)
  #   if(input$questionarie_tab %in% c(1,7)) {
  #     shinyjs::hide(id = "save_answers")
  #   } else {
  #     shinyjs::show(id = "save_answers", anim = TRUE, animType = "fade")
  #   }
  # })
  
  observeEvent(input$save_answers, {
     # tab_now <- input$questionarie_tab
     dbExecute(con(), "UPDATE assessments SET endDate = ?,
                                              reference = ? 
                              WHERE idAssessment = ?",
               params = list(format(now("CET"), "%Y-%m-%d %H:%M:%S"),
                             input$ass_reftext,
                             assessments$selected$idAssessment))
     
     assessments$selected$endDate <- format(now("CET"), "%Y-%m-%d %H:%M:%S")
     assessments$selected$reference <- input$ass_reftext
     
     assessments$data <- dbReadTable(con(), "assessments")
     # assessments$selected <- assessments$data[input$assessments_rows_selected, ]
     # assessments$selected <- assessments$selected |> 
     #   left_join(pests$data, by = "idPest") |>
     #   left_join(assessors$data, by = "idAssessor") |> 
     #   mutate(label = paste(scientificName, eppoCode, 
     #                        paste(firstName, lastName), startDate, 
     #                        sep = "_"))
     
     selected_entries <- dbGetQuery(con(), glue("SELECT * FROM entryPathways
                                               WHERE idAssessment = {assessments$selected$idAssessment}"))
 
    # Proceed with saving the ANSWERS IN assessment
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
    answers$main <- dbGetQuery(con(), glue("SELECT * FROM answers WHERE idAssessment = {assessments$selected$idAssessment}"))

    if (!is.null(assessments$entry)) {
      answ_ent_path <- extract_answers_entry(questions$entry, groupTag = "ENT", 
                                         path = names(assessments$entry), 
                                         input)
      
      any_non <- any(!lapply(answ_ent_path, is.null) |> unlist())

      if(any_non){
        frominput$entry <- get_inputs_path_as_df(answ_ent_path, input) 
        resentry <- frominput$entry
        if (!is.null(resentry)) {
          if(nrow(resentry) >= 1) {
            for (i in 1:nrow(resentry)) {
              # Check if the answer already exists
              idQue <- questions$entry |> 
                filter(group == substr(resentry$question[i], 1, 3),
                       number == substr(resentry$question[i], 4, nchar(resentry$question[i]))) |> 
                pull(idPathQuestion)
        
              idEntry <- selected_entries |> 
                filter(idPathway == resentry$path[i]) |> 
                pull(idEntryPathway)
              if (length(idEntry) == 0 | length(idQue) == 0){
                next
              }
    
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
            } # end loop resentry
          } # end if resentry not empty
        } # end if resentry
      } # end if any non null
      answers$entry <- dbGetQuery(con(), glue("SELECT pa.*, ep.idAssessment, ep.idPathway
                                                FROM pathwayAnswers AS pa 
                                                LEFT JOIN entryPathways AS ep ON pa.idEntryPathway = ep.idEntryPathway
                                                WHERE pa.idEntryPathway IN ({paste(selected_entries$idEntryPathway, collapse = ', ')})"))
    } # end if entry not null
    
    shinyalert(
      title = "Success",
      text = "Assessment saved successfully.",
      type = "success",
      timer = 1000,
    )
    
    # updateTabsetPanel(session, "questionarie_tab", selected = tab_now)
  }, ignoreInit = TRUE)
  
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
                             stateSave = TRUE,
                             searching = FALSE))
  })
  
  proxysimulations <- dataTableProxy("simulations")
  
  observeEvent(input$simulations_rows_selected, {
    sims <-  simulations$data |> 
      filter(idAssessment == assessments$selected$idAssessment)
    
    # req(simulations$data)
    if (is.null(input$simulations_rows_selected)) {
      simulations$selected <- NULL
      simulations$results <- NULL
      simulations$summary <- NULL
    } else {
      simulations$selected <- sims[input$simulations_rows_selected, ]
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
    updateNumericInput(session, "n_sim", value = default_sim$n_sim)
    # updateNumericInput(session, "seed_sim", value = 12345)
    updateNumericInput(session, "lambda_sim", value = default_sim$lambda)
    updateNumericInput(session, "w1_sim", value = default_sim$w1)
    updateNumericInput(session, "w2_sim", value = default_sim$w2)
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
        ) |> 
        as.data.frame()
      
      #Check if IMP2&4 is present
      if (!"IMP2" %in% answers_df$question) {
        # Create a row with 0 values for IMP4
        answers_df <- rbind(answers_df, 
                            c("IMP2",0,0,0))
      }
      if (!"IMP4" %in% answers_df$question) {
        # Create a row with 0 values for IMP4
        answers_df <- rbind(answers_df, 
                            c("IMP4",0,0,0))
      }
      
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
  
      # Run simulation function
      simulations$results <- simulation(answers_df, answers_entry_df, pathways$data,
                                        iterations = input$n_sim, lambda = input$lambda_sim, 
                                        w1 = input$w1_sim , w2 = input$w2_sim)
      
      simulations$summary <- simulations$results |>
        as.data.frame() |> 
        reframe(across(everything(), list(
          min = ~min(.x, na.rm = TRUE) |> round(3),
          q5  = ~quantile(.x, 0.05, na.rm = TRUE) |> round(3),
          q25  = ~quantile(.x, 0.25, na.rm = TRUE) |> round(3),
          median  = ~quantile(.x, 0.50, na.rm = TRUE) |> round(3),
          q75  = ~quantile(.x, 0.75, na.rm = TRUE) |> round(3),
          q95  = ~quantile(.x, 0.95, na.rm = TRUE) |> round(3),
          max = ~max(.x, na.rm = TRUE) |> round(3),
          mean = ~mean(.x, na.rm = TRUE) |> round(3)
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
    # req()
    req(simulations$summary)
    datatable(simulations$summary |> 
                mutate(variable = c("Entry A*", "Entry B**", "Establishment", 
                                    "Invasion A*", "Invasion B**", "Impact",
                                    "Risk A*", "Risk B**",
                                    "Preventability", "Controllability", "Manageability")),
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
    
    req(simulations$results)  ## To prevent saving again what you selected
    
    res <- dbExecute(conn = con(),
                     "INSERT INTO simulations(idAssessment, iterations, lambda, weight1, weight2, date)
                        VALUES(?,?,?,?,?,?);",
                     params = list(assessments$selected$idAssessment,
                                   input$n_sim,
                                   input$lambda_sim,
                                   input$w1_sim,
                                   input$w2_sim,
                                   format(now("CET"), "%Y-%m-%d %H:%M:%S")) 
                     )
    
    # Insert simulation results
    sim_id <- dbGetQuery(con(), glue("SELECT MAX(idSimulation) AS idSimulation FROM simulations")) |> 
      as.integer()
    sim_sum <- simulations$summary |>
      as.data.frame() |>
      mutate(idSimulation = sim_id) |>
      select(idSimulation, everything())

    dbWriteTable(con(), "simulationSummaries", sim_sum, append = TRUE, row.names = FALSE)

    simulations$data <- dbReadTable(con(), "simulations")
    
    simulations$results <- NULL
    simulations$summary <- NULL
    
    shinyalert(
      title = "Success",
      text = "Simulation saved successfully.",
      type = "success",
      timer = 1000,
    )
  })
  
  # Pest ----
  # Show species lists
  output$pests <- renderDT({
    req(pests$data)
    tab <- pests$data |> 
      mutate(inEurope = as.logical(inEurope)) |> 
      left_join(taxa$data, by = "idTaxa") |>
      left_join(quaran$data, by = "idQuarantineStatus") |>
      select(scientificName, eppoCode, gbifTaxonKey, vernacularName, synonyms, name.x, name.y, inEurope)

    datatable(tab, 
              class = 'row-border stripe compact hover',
              extensions = 'Buttons', 
              rownames = FALSE, selection = 'single', autoHideNavigation = FALSE,
              colnames = c("Pest species", "EPPO code", "GBIF taxon key", "Common names", 
                           "Synonyms", "Taxonomic Group", "Quarantine Status", "Present in Europe"),
              options = list(
                dom = 'lftpB',
                stateSave = TRUE,
                searching = TRUE, autoFill = FALSE, ordering = TRUE,
                lengthMenu = list(c(10, 25, 50, 100, -1),
                                  c('10', '25', '50', '100','All')),
                pageLength = 25,
                lengthChange = TRUE, scrollX = TRUE, scrollY = FALSE,
                paging = TRUE)
    )
  })

  ## Modal for new pest ----
  observeEvent(input$new_pest, {
    showModal(modalDialog(
      title = "Add New Pest Species",
               textInput("new_sci", "Scientific Name*"),
               textInput("new_common", "Common Name"),
               textInput("new_synonyms", "Synonyms"),
               textInput("new_eppo", "EPPO code"),
               textInput("new_gbifTaxonKey", "GBIF taxon key"),
               selectInput("new_taxa", "Taxonomic Group*", choices = setNames(c("", taxa$data$idTaxa), 
                                                                              c("", taxa$data$name))),
               selectInput("new_quaran", "Quarantine Status*", choices = setNames(c("", quaran$data$idQuarantineStatus),
                                                                                  c("", quaran$data$name))),
               checkboxInput("new_ineu", "Is the pest species present in Europe?"),
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
    new_gbif     <- ifelse(input$new_gbifTaxonKey == "", NA, input$new_gbifTaxonKey)
    new_synonyms <- ifelse(input$new_synonyms == "", NA, input$new_synonyms)
    
    # Check for duplicates
    duplicate_sci  <- tolower(input$new_sci) %in% tolower(pests$data$scientificName)
    duplicate_eppo <- (input$new_eppo != "" && input$new_eppo %in% pests$data$eppoCode)
    duplicate_gbif <- (input$new_gbifTaxonKey != "" && input$new_gbifTaxonKey %in% pests$data$gbifTaxonKey)
    
    if (duplicate_eppo || duplicate_gbif || duplicate_sci) {
      shinyalert(
        title = "Duplicate Entry",
        text = paste(
          if (duplicate_sci) "Scientific name already exists." else NULL,
          if (duplicate_eppo) "EPPO code already exists." else NULL,
          if (duplicate_gbif) "GBIF taxon key already exists." else NULL,
          sep = "<br>"
        ),
        type = "error", html = TRUE
      )
      return()
    }
    
    # Insert into pests table
    res <- dbExecute(conn = con(),
                     "INSERT INTO pests(scientificName, vernacularName, eppoCode, gbifTaxonKey,
                                        synonyms, idTaxa, idQuarantineStatus, inEurope)
                        VALUES(?,?,?,?,?,?,?,?)
                        RETURNING idPest;",
                     params = list(input$new_sci, input$new_common, input$new_eppo, input$new_gbifTaxonKey,
                                   input$new_synonyms, input$new_taxa, input$new_quaran, as.integer(input$new_ineu)))
    
    pests$data <- dbReadTable(con(), "pests")
    new_id <- pests$data |> 
      filter(scientificName == input$new_sci) |> 
      pull(idPest)
    
    shinyalert(
      title = "Success",
      text = "Pest and associated threats added successfully.",
      type = "success"
    )
    
    update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
    removeModal()
  })
  
  # Assessors ----
  ## Modal for new user ----
  observeEvent(input$new_assessor, {
    showModal(modalDialog(
      title = "Add New Assessor",
      textInput("new_name", "First Name"),
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
    assessors$data <- dbReadTable(con(), "assessors")
    assessors$data$fullName <- paste(assessors$data$firstName, assessors$data$lastName)
    update_options(assessors$data, pests$data, taxa$data, quaran$data, pathways$data, session)
    removeModal()
  })
  
  # END ----
  # session$onSessionEnded(function() {
  #   message("ending session")
  #   
  #   try({
  #     if (dbIsValid(consql)) {
  #       dbExecute(consql, "
  #         UPDATE dbStatus
  #            SET inUse = 0,
  #                timeStamp = CURRENT_TIMESTAMP
  #          WHERE rowid = (SELECT MAX(rowid) FROM dbStatus)")
  #     }
  #   }, silent = TRUE)
  # 
  #   # Clean up
  #   try(DBI::dbDisconnect(consql), silent = TRUE)
  #   
  #   
  #   # stopApp()
  # })
} # END