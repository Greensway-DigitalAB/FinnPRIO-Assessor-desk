# Load UI content from a file
load_ui_content <- function(file) {
  source(file, local = TRUE)$value
}

# Capitalize first letter, lowercase next two, keep rest as is
capitalize_first <- function(x) {
  paste0(toupper(substr(x, 1, 1)), 
         tolower(substr(x, 2, 3)), 
         substr(x, 4, nchar(x)))
}


update_options <- function(assessors, pests, taxa, quaran, pathways, session) {
  updateSelectInput(session, "assessors", choices = setNames(c("", assessors$idAssessor), c("", assessors$fullName)))
  updateSelectInput(session, "pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateSelectInput(session, "new_taxa", choices = setNames(taxa$idTaxa, taxa$name))
  updateSelectInput(session, "new_quaran", choices = setNames(quaran$idQuarantineStatus, quaran$name))
  updateCheckboxGroupInput(session, "pot_entry_path", choices = setNames(pathways$idPathway, pathways$name))
  updateSelectInput(session, "assessors", choices = setNames(c("", assessors$idAssessor), c("", assessors$fullName)))
  updateSelectInput(session, "filter_pest", choices = setNames(c("", pests$idPest), c("", pests$scientificName)))
  updateCheckboxGroupInput(session, "filter_entry_path", choices = setNames(pathways$idPathway, pathways$name))
}

# Helper to generate UI for a group
render_group_ui <- function(group_name, threat_groups) {
  group_threats <- threat_groups[[group_name]]
  tagList(
    h5(group_name),
    lapply(1:nrow(group_threats), function(i) {
      radioButtons(
        inputId = paste0("threat_", group_threats$idThrSect[i]),
        label = group_threats$name[i],
        choices = c("None", "Most Likely", "Possible"),
        inline = TRUE
      )
    })
  )
}

render_quest_tab <- function(tag, qid, question, 
                             options, texts, 
                             answers = NULL,
                             type = "minmax"){
  input_names <- glue("{tag}{qid}_{options}")
  input_text <- glue("{tag}{qid}_{texts}")
  values <- c("Minimum", "Likely", "Maximum")
  
  table_data = matrix(
    values, nrow = length(options), ncol = length(values), byrow = TRUE,
    dimnames = list(input_names, values)
  )
  
  for (i in seq_len(nrow(table_data))) {
    if (!is.null(answers)) {
      is_checked <- answers |> 
        filter(ques_tag_opt == rownames(table_data)[i]) |> 
        select(Minimum, Likely, Maximum)
    } else {
      is_checked <- c(FALSE, FALSE, FALSE)
    }
    
    table_data[i, ] = sprintf(
      '<input type="checkbox" name="%s" value="%s" %s/>', # the last s if for adding 'checked'
      # '<input type="checkbox" name="%s" value="%s" checked="checked"/>', #the last s if for adding 'checked'
      input_names[i], table_data[i, ], ifelse(is_checked, ' checked="checked"', ""))
  }
  
  
  colnames <- if (type == "minmax") {
    c("Options", "Minimum", "Likely", "Maximum")
  } else {
    c("Sub-questions, check the box if the answer is Yes", "Minimum", "Likely", "Maximum")
  }
  # JavaScript callback: conditional based on type
  
  js_callback <- if (type == "minmax") {
    JS("
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-checkboxgroup');
      });

      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());

      var tableId = table.table().node().id || 'table_' + Math.random().toString(36).substr(2, 9);
      var limits = { Minimum: 1, Likely: 1, Maximum: 1 };

      $('#' + tableId + ' input[type=checkbox]').off('change').on('change', function() {
        var checkbox = this;
        var value = checkbox.value;

        var totalChecked = $('#' + tableId + ' input[type=checkbox][value=' + value + ']:checked').length;

        if (totalChecked > limits[value]) {
          console.warn('Limit reached for ' + value);
          $(checkbox).prop('checked', false);
        }
      });
    ")
  } else {
    JS("
      table.rows().every(function(i, tab, row) {
        var $this = $(this.node());
        $this.attr('id', this.data()[0]);
        $this.addClass('shiny-input-checkboxgroup');
      });

      Shiny.unbindAll(table.table().node());
      Shiny.bindAll(table.table().node());
    ")
  }
  
  tagList(
    # h4(glue("{tag} {qid}: {question}")),
    datatable(
      cbind(texts, table_data), #table_data,
      colnames = colnames,
      editable = TRUE,
      escape = FALSE,   # allow HTML rendering
      width = "600px",
      selection = "none", 
      # server = FALSE,
      rownames = TRUE,
      options = list(dom = 't', 
                     paging = FALSE, 
                     autoWidth = FALSE,
                     ordering = FALSE,
                     columnDefs = list(
                       list(width = '50px', targets = c(2,3,4)),
                       list(visible = FALSE, targets = c(0))
                     )
      ),
      callback = js_callback
    ),
    uiOutput(glue("{tag}{qid}_warning"))
  )
}

render_severity_warning <- function(groupTag, answers) {
  severity_map <- c(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6, g = 7, h = 8, i = 9)
  focal_ans <- answers |> 
    filter(question == groupTag) |> 
    select(minimum, likely, maximum) |> 
    unlist()
# print(focal_ans)
  renderUI({
    # Ensure names are "min", "likely", "max"
    sev_values <- severity_map[focal_ans]
    names(sev_values) <- c("minimum", "likely", "maximum")

    if (any(is.na(sev_values))) {
      return(
        tags$div(
          style = "color: red; font-weight: bold;",
          "Please ensure that there is one answer for each severity and that 'Minimum' < 'Likely' < 'Maximum'."
        )
      )
    }

    if (length(sev_values) == 3 &&
        sev_values["minimum"] <= sev_values["likely"] &&
        sev_values["likely"] <= sev_values["maximum"]) {
      return(NULL)
    } else {
      return(
        tags$div(
          style = "color: red; font-weight: bold;",
          "Please ensure that 'Minimum' <= 'Likely' <= 'Maximum' in severity."
        )
      )
    }
  })
}

render_severity_boolean_warning <- function(groupTag, answers) {
  focal_ans <- answers |>
    filter(question == groupTag) |>
    select(minimum, likely, maximum) |>
    unlist()

  if(length(focal_ans) == 0){
    return(NULL)
  }

  renderUI({
    # Ensure names are "min", "likely", "max"
    sev_values <- focal_ans
    names(sev_values) <- c("minimum", "likely", "maximum")
    # Conditional dependency checks
    if (all(is.na(sev_values))) {
      return(NULL)
    }

    if (!is.na(sev_values["minimum"]) &&
        (is.na(sev_values["likely"]) || is.na(sev_values["maximum"]))) {
      return(
        tags$div(
          style = "color: red; font-weight: bold;",
          "If 'Minimum' is checked, both 'Likely' and 'Maximum' must also be checked"
        )
      )
    }
    
    if (!is.na(sev_values["likely"]) && is.na(sev_values["maximum"])) {
      return(
        tags$div(
          style = "color: red; font-weight: bold;",
          "If 'Likely' is checked, 'Maximum' must also be checked"
        )
      )
    }
    
  })
}

extract_answers <- function(questions, groupTag, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  input_names <- character(0)
  
  for (i in seq(id)) {
    options <- fromJSON(quesExt$list[i])$opt
    input_names <- c(input_names, glue("{groupTag}{id[i]}_{options}"))
  }
  resp <- sapply(input_names, function(i) input[[i]])
  return(resp)
}

extract_answers_entry <- function(questions, groupTag, path, input){
  quesExt <- questions |> filter(group == groupTag)
  id <- quesExt$number
  # id <- paste0(quesExt$number, "_", rep(path, length(quesExt$number)))
  input_names <- character(0)
  for (i in seq(id)) {
    # options <- fromJSON(quesExt$list[i])$text
    options <- fromJSON(quesExt$list[i])$opt
    id_p <- paste0(id[i], "_", path)
    for (p in id_p) {
      input_names <- c(input_names, glue("{groupTag}{p}_{options}"))  
    }
  }
  resp <- sapply(input_names, function(i) input[[i]])
  return(resp)
}

get_points_as_table <- function(questions){
  groups <- unique(questions$group)
  # Loop over each group and parse its list column
  points_all <- lapply(groups, function(grp) {
    points <- questions |> 
      filter(group == grp) 

    lapply(seq(1,nrow(points)), function(i) {
      df <- fromJSON(points$list[i])
      df$question <- paste0(grp, points$number[i])
      df$points <- as.character(df$points)
      df
    }) |> bind_rows()
  }) |> bind_rows()
  
  # Final formatting
  points_all <- points_all |> 
    rename(Question = question, 
           Option = opt, 
           Text = text, 
           Points = points)
  
  return(points_all)
}

get_table2_points <- function(ent2_answer, ent3_answer, table2) {
  table2 |> 
    filter(ENT2 == tolower(ent2_answer),
           ENT3 == tolower(ent3_answer)) |> 
    pull(Points)
}

get_table3_points <- function(est2_answer, est3_answer, table3) {
  table3 |> 
    filter(EST2 == tolower(est2_answer),
           EST3 == tolower(est3_answer)) |> 
    pull(Points)
}

get_inputs_as_df <- function(answers, input){ ##, points_main
  df <- tibble(
    name = names(answers),
    question = sub("_.*", "", names(answers)),
    option = sub(".*_", "", names(answers)),
    answer = answers
  ) |>
    unnest(cols = c(answer))  # This expands each vector into separate rows
  
  if (nrow(df) == 0) {
    final_opt <- data.frame(question = NA, 
                            minimum = NA, 
                            likely = NA, 
                            maximum = NA)
  } else {
    final_opt <- df |> 
      select(question, answer, option) |> 
      pivot_wider(names_from = answer, values_from = option) |> 
      rename_with(tolower) |> 
      as.data.frame()
    
      # Ensure "min", "lik", and "max" columns exist
      required_cols <- c("minimum", "likely", "maximum")
      missing_cols <- setdiff(required_cols, names(final_opt))
      if (length(missing_cols) > 0) {
        for (col in missing_cols) {
          final_opt[[col]] <- NA  # Add missing columns with NA
        }
      }
  }
    
  # Extract justifications
  input_names_just <- names(input)[grepl("^just", names(input))]
  # Remove justifications for ENT Paths as they are not collected here
  input_names_just <- input_names_just[-grep("_",input_names_just)]  
  respJust <- sapply(input_names_just, function(i) input[[i]])


  # Create a full justification dataframe
  just_df <- tibble(
    question = toupper(sub("^just", "", input_names_just)),
    justification = unname(respJust)
  )

  if(nrow(just_df) > 0){
  # Merge with final_opt to include all justifications
  final_opt <- full_join(final_opt, just_df, by = "question")
  } else {
    final_opt$justification <- NA
  }

  final_opt <- final_opt |>
    filter(!is.na(question),
           !(is.na(minimum) &
               is.na(likely) &
               is.na(maximum) &
               (is.na(justification) |
                  justification == "")))
  return(final_opt)
}


get_inputs_path_as_df <- function(answers, input){ ## , points_path
  df <- tibble(
    name = names(answers),
    question = sub("_.*", "", names(answers)),
    path = lapply(str_split(names(answers), "_"), function(x) x[2]) |> unlist(),
    option = sub(".*_", "", names(answers)) |> tolower(),
    answer = answers
  ) |>
    unnest(cols = c(answer))  # This expands each vector into separate rows

  if (nrow(df) == 0) {
    final_opt <- data.frame(question = NA, 
                            path = NA,
                            minimum = NA, 
                            likely = NA, 
                            maximum = NA)
  } else {
    final_opt <- df |> 
      select(path, question, answer, option) |> 
      pivot_wider(names_from = answer, values_from = option) |> 
      rename_with(tolower) |> 
      as.data.frame()
    
    # Ensure "min", "lik", and "max" columns exist
    required_cols <- c("minimum", "likely", "maximum")
    missing_cols <- setdiff(required_cols, names(final_opt))
    if (length(missing_cols) > 0) {
      for (col in missing_cols) {
        final_opt[[col]] <- NA  # Add missing columns with NA
      }
    }
  }

  input_names_just <- names(input)[grepl("^justEnt", names(input))]
  # Remove justifications for ENT1 as they are not collected
  input_names_just <- input_names_just[-grep("Ent1",input_names_just)]  
  respJust <- sapply(input_names_just, function(i) input[[i]])
  
  # Create a full justification dataframe
  questions <- lapply(str_split(input_names_just, "_"), function(x) x[1]) |> 
    unlist()
  questions <- sub("^just", "", questions) |> 
    toupper()

  just_df <- tibble(
    question = questions,
    path = lapply(str_split(input_names_just, "_"), function(x) x[2]) |> unlist(),
    justification = unname(respJust)
  )
  
  # Merge with final_opt to include all justifications
  final_opt <- full_join(final_opt, just_df, by = c("question","path"))
  
  # remove path = na
  final_opt <- final_opt |> filter(!is.na(path),
                                   !(is.na(minimum) & 
                                      is.na(likely) & 
                                      is.na(maximum) & 
                                      (is.na(justification) | 
                                         justification == "")))
  
  return(final_opt)
}


answers_2_logical <- function(df, questions) {
  
    if (nrow(df) > 0) {
    result <- data.frame()
    
    for (i in seq_len(nrow(df))) {
      wQues <- questions |> 
        filter(idQuestion == df$idQuestion[i])
      question_tag <- paste0(wQues$group, wQues$number)
      df$question_tag[i] <- question_tag
      
      row <- df[i, ]
      options <- unique(c(row$min, row$likely, row$max))
      
      for (opt in options) {
        result <- rbind(result, data.frame(
          question_tag = row$question_tag,
          option = opt,
          ques_tag_opt = paste0(row$question_tag, "_", opt),
          Minimum = opt == row$min,
          Likely = opt == row$likely,
          Maximum = opt == row$max,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    result <- NULL
  }
  
  return(result)
}

answers_path_2_logical <- function(df, questions) {
  if (nrow(df) > 0) {
    result <- data.frame()
    
    for (i in seq_len(nrow(df))) {
      wQues <- questions |> 
        filter(idPathQuestion == df$idPathQuestion[i])
      question_tag <- paste0(wQues$group, wQues$number, "_", df$idPathway[i])
      df$question_tag[i] <- question_tag
      
      row <- df[i, ]
      options <- unique(c(row$min, row$likely, row$max))
      
      for (opt in options) {
        result <- rbind(result, data.frame(
          question_tag = row$question_tag,
          option = opt,
          ques_tag_opt = paste0(row$question_tag, "_", opt),
          Minimum = opt == row$min,
          Likely = opt == row$likely,
          Maximum = opt == row$max,
          stringsAsFactors = FALSE
        ))
      }
    }
  } else {
    result <- NULL
  }
  
  return(result)
}

check_minmax_completeness <- function(df, all = FALSE) {
  
  if (!all) {
    # Filter rows where type is 'minmax'
    minmax_rows <- df[df$type == "minmax", ]
  } else {
    minmax_rows <- df
  }
  
  # Check for missing values in min, likely, or max
  incomplete <- minmax_rows[is.na(minmax_rows$min) | is.na(minmax_rows$likely) | is.na(minmax_rows$max), ]
  
  # Return result
  if (nrow(incomplete) == 0) {
    # message("✅ All 'minmax' rows are complete.")
    return(TRUE)
  } else {
    # message("❌ Incomplete 'minmax' rows found:")
    shinyalert("title" = "Incomplete 'minmax' rows found",
               "text" = paste("Please complete the following questions:", 
                              paste(incomplete$question, collapse = ", ")),
               type = "error")
    # print(incomplete)
    return(FALSE)
  }
}


rpert_from_tag <- function(answers, tag, iterations = 5000, lambda = 1) {
  points <- answers[answers$question == tag, c("min_points", "likely_points", "max_points")] |> 
    as.numeric()
  res <- rpert(iterations, points[1], points[2], points[3], lambda)
  return(res)
}

generate_inclusion_exclusion_score <- function(score_matrix) {
  n <- ncol(score_matrix)
  iterations <- nrow(score_matrix)
  result <- numeric(iterations)
  
  for (k in 1:n) {
    combos <- combn(n, k, simplify = FALSE)
    for (combo in combos) {
      sign <- ifelse(k %% 2 == 1, 1, -1)
      term <- apply(score_matrix[, combo, drop = FALSE], 1, prod)
      result <- result + sign * term
    }
  }
  
  return(result)
}


simulation <- function(answers, answers_entry, 
                       iterations = 5000, lambda = 1, 
                       w1 = 0.5, w2 = 0.5){
  
  pathways <- unique(answers_entry$idpathway)
  scores <- array(0, dim = c(iterations, length(pathways), 2, 3)) #A, B
  rownames(scores) <- paste0("sim", 1:iterations)
  colnames(scores) <- paste0("path",pathways)
  dimnames(scores)[[3]] <- c("A", "B")
  dimnames(scores)[[4]] <- c("1", "2", "3")
  
  scorePathway <- array(0, dim = c(iterations, length(pathways), 2)) #A, B
  rownames(scorePathway) <- paste0("sim", 1:iterations)
  colnames(scorePathway) <- paste0("path",pathways)
  dimnames(scorePathway)[[3]] <- c("A", "B")

  
  ENT1 <- rpert_from_tag(answers, tag = "ENT1")
  
  for (p in pathways){
    ENT2A <- rpert_from_tag(answers_entry |> filter(idpathway == p), tag = "ENT2A")
    ENT2B <- rpert_from_tag(answers_entry |> filter(idpathway == p), tag = "ENT2B")
    ENT3 <- rpert_from_tag(answers_entry |> filter(idpathway == p), tag = "ENT3")
    ENT4 <- rpert_from_tag(answers_entry |> filter(idpathway == p), tag = "ENT4")
  
    ENT3A <- ENT3
    
    ## OBS equal is not consider 
    ENT3A <- case_when(
      ENT2A > 2.5 & ENT3 > 0.5 ~ 3,
      ENT2A < 2.5 & ENT2A > 1.5 & ENT3 > 1.5 ~ 3,
      ENT2A < 0.25 ~ 0,
      TRUE ~ ENT3A  # keep original value if no condition is met
    )
    
    scores[, paste0("path",p), "A", 1] <- ((ENT1 * ENT2A * ENT4) / 27)
    scores[, paste0("path",p), "A", 2] <- ((ENT2A * ENT4) / 9)
    scores[, paste0("path",p), "A", 3] <- ((ENT1 * ENT2A * ENT3A * ENT4) / 81)
  
    ## OBS equal is not consider 
    scorePathway[,paste0("path",p), "A"] <- case_when(
      p < 2 ~ scores[, paste0("path",p), "A", 2], #((ENT2A*ENT4)/9)
      p < 4 ~ scores[, paste0("path",p), "A", 1], #((ENT1*ENT2A*ENT4)/27)
      # p > 4 ~ scores[, paste0("path",p), "A", 3], #((ENT1*ENT2A*ENT3A*ENT4)/81)
      .default = scores[, paste0("path",p), "A", 3] #((ENT1*ENT2A*ENT3A*ENT4)/81) # Default case
    )
    
    ENT3B <- ENT3
    ENT3B <- case_when(
      ENT2B > 2.5 & ENT3 > 0.5 ~ 3,
      ENT2B < 2.5 & ENT2B > 1.5 & ENT3 > 1.5 ~ 3,
      ENT2B < 0.25 ~ 0,
      TRUE ~ ENT3B  # keep original value if no condition is met
    )
    
    scores[, paste0("path",p), "B", 1] <- ((ENT1 * ENT2A * ENT4) / 27)
    scores[, paste0("path",p), "B", 2] <- ((ENT2B * ENT4) / 9)
    scores[, paste0("path",p), "B", 3] <- ((ENT1 * ENT2B * ENT3B * ENT4) / 81)
    
    ## OBS equal is not consider 
    scorePathway[,paste0("path",p), "B"] <- case_when(
      p < 2 ~ scores[, paste0("path",p), "B", 2], #((ENT2A*ENT4)/9)
      p < 4 ~ scores[, paste0("path",p), "B", 1], #((ENT1*ENT2A*ENT4)/27)
      # p > 4 ~ scores[, paste0("path",p), "B", 3], #((ENT1*ENT2A*ENT3A*ENT4)/81)
      .default = scores[, paste0("path",p), "B", 3] #((ENT1*ENT2A*ENT3A*ENT4)/81) # Default case
    )
    
  } # end for pathways
  
  ENTRYA <- generate_inclusion_exclusion_score(scorePathway[,,"A"])
  # ENTRYA <- scorePathway[,1,"A"]+scorePathway[,2,"A"] - scorePathway[,1,"A"]*scorePathway[,2,"A"]
  ENTRYB <- generate_inclusion_exclusion_score(scorePathway[,,"B"])
  # ENTRYB<-ScorePathway1B+ScorePathway2B+ScorePathway3B+ScorePathway4B+ScorePathway5B-ScorePathway1B*ScorePathway2B-ScorePathway1B*ScorePathway3B-ScorePathway1B*ScorePathway4B-ScorePathway1B*ScorePathway5B-ScorePathway2B*ScorePathway3B-ScorePathway2B*ScorePathway4B-ScorePathway2B*ScorePathway5B-ScorePathway3B*ScorePathway4B-ScorePathway3B*ScorePathway5B-ScorePathway4B*ScorePathway5B+ScorePathway1B*ScorePathway2B*ScorePathway3B+ScorePathway1B*ScorePathway2B*ScorePathway4B+ScorePathway1B*ScorePathway2B*ScorePathway5B+ScorePathway1B*ScorePathway3B*ScorePathway4B+ScorePathway1B*ScorePathway3B*ScorePathway5B+ScorePathway1B*ScorePathway4B*ScorePathway5B+ScorePathway2B*ScorePathway3B*ScorePathway4B+ScorePathway2B*ScorePathway3B*ScorePathway5B+ScorePathway2B*ScorePathway4B*ScorePathway5B+ScorePathway3B*ScorePathway4B*ScorePathway5B-ScorePathway1B*ScorePathway2B*ScorePathway3B*ScorePathway4B-ScorePathway1B*ScorePathway2B*ScorePathway3B*ScorePathway5B-ScorePathway1B*ScorePathway2B*ScorePathway4B*ScorePathway5B-ScorePathway1B*ScorePathway3B*ScorePathway4B*ScorePathway5B-ScorePathway2B*ScorePathway3B*ScorePathway4B*ScorePathway5B+ScorePathway1B*ScorePathway2B*ScorePathway3B*ScorePathway4B*ScorePathway5B
  
  EST1 <- rpert_from_tag(answers, tag = "EST1")
  EST2 <- rpert_from_tag(answers, tag = "EST2")
  EST3 <- rpert_from_tag(answers, tag = "EST3")
  EST4 <- rpert_from_tag(answers, tag = "EST4")
  
  
  SPR1 <- case_when(
    EST3 > 2.5 & EST2 > 3.5 ~ 6,
    EST3 > 2.5 & EST2 > 2.5 & EST2 < 3.5 ~ 7,
    EST3 > 2.5 & EST2 > 1.5 & EST2 < 2.5 ~ 8,
    EST3 > 2.5 & EST2 > 0.5 & EST2 < 1.5 ~ 9,
    
    EST3 < 2.5 & EST3 > 1.5 & EST2 > 3.5 ~ 4,
    EST3 < 2.5 & EST3 > 1.5 & EST2 > 2.5 & EST2 < 3.5 ~ 5,
    EST3 < 2.5 & EST3 > 1.5 & EST2 > 1.5 & EST2 < 2.5 ~ 6,
    EST3 < 2.5 & EST3 > 1.5 & EST2 > 0.5 & EST2 < 1.5 ~ 7,
    
    EST3 < 1.5 & EST3 > 0.5 & EST2 > 3.5 ~ 2,
    EST3 < 1.5 & EST3 > 0.5 & EST2 > 2.5 & EST2 < 3.5 ~ 3,
    EST3 < 1.5 & EST3 > 0.5 & EST2 > 1.5 & EST2 < 2.5 ~ 4,
    EST3 < 1.5 & EST3 > 0.5 & EST2 > 0.5 & EST2 < 1.5 ~ 5,
    
    EST3 < 0.5 & EST2 > 2.5 ~ 1,
    EST3 < 0.5 & EST2 > 1.5 & EST2 < 2.5 ~ 2,
    EST3 < 0.5 & EST2 > 0.5 & EST2 < 1.5 ~ 3,
    
    EST2 < 0.5 ~ 0,
    
    TRUE ~ NA_real_  # default case if none match
  )
  
  ESTABLISHMENT <- case_when(
    EST1 < 0.75 ~ 0,
    EST2 < 0.5 ~ 0,
    TRUE ~ (EST1 + SPR1 + EST4) / 21
  )
  
  INVASIONA <- ENTRYA * ESTABLISHMENT
  INVASIONB <- ENTRYB * ESTABLISHMENT
  
  IMP1 <- rpert_from_tag(answers, tag = "IMP1")
  IMP2 <- rpert_from_tag(answers, tag = "IMP2")
  IMP3 <- rpert_from_tag(answers, tag = "IMP3")
  IMP4 <- rpert_from_tag(answers, tag = "IMP4")
  
  IMPACT <- ((w1 * (IMP1 + IMP2)) + 
              (w2 * (IMP3 + IMP4))) / 9
  
  MAN1 <- rpert_from_tag(answers, tag = "MAN1")
  MAN2 <- rpert_from_tag(answers, tag = "MAN2")
  MAN3 <- rpert_from_tag(answers, tag = "MAN3")
  MAN4 <- rpert_from_tag(answers, tag = "MAN4")
  MAN5 <- rpert_from_tag(answers, tag = "MAN5")
  
  PREVENTABILITY <- pmax(MAN1, MAN2, MAN3)
  CONTROLLABILITY <- pmax(MAN4, MAN5)
  MANAGEABILITY <- pmin(PREVENTABILITY, CONTROLLABILITY)
  
  SCORE <- cbind(ENTRYA, ENTRYB, ESTABLISHMENT, INVASIONA, INVASIONB, IMPACT, 
                 PREVENTABILITY, CONTROLLABILITY, MANAGEABILITY)
  return(SCORE)
}



#   
#   
#   ### Points for pathways
#    
# df_points <- df |> 
#   left_join(points_path, by = c("Question", "Option"))

# Step 1: Filter ENT3
# ent2A <- df_points |> filter(Question == "ENT2A")
# ent2B <- df_points |> filter(Question == "ENT2B")
# ent3 <- df_points |> filter(Question == "ENT3")
# 
# # Step 2: Merge ENT2 and ENT3 by Answer type
# ent2a_3 <- bind_rows(ent2A, ent3) |> 
#   group_by(Answer, Path) |> 
#   reframe(
#     ent2a_options = Option[Question == "ENT2A"],
#     ent3_options = Option[Question == "ENT3"]
#   ) |> 
#   mutate(
#     Points = mapply(get_table2_points, ent2a_options, ent3_options, 
#                     MoreArgs = list(table2 = table2_lexp)),
#     Question = "ENT3A"
#   ) |> 
#   select(Path, Question, Answer, Points)
# 
# ent2b_3 <- bind_rows(ent2B, ent3) |> 
#   group_by(Answer, Path) |> 
#   summarise(
#     ent2b_options = Option[Question == "ENT2B"],
#     ent3_options = Option[Question == "ENT3"],
#     .groups = "drop"
#   ) |> 
#   mutate(
#     Points = mapply(get_table2_points, ent2b_options, ent3_options, MoreArgs = list(table2 = table2_lexp)),
#     Question = "ENT3B"
#   ) |> 
#   select(Path, Question, Answer, Points)
# 
# 
# # Step 3: Filter out ENT3 from original data
# df_clean <- df_points  |> 
#   filter(!Question %in% c("ENT3"))  |> 
#   select(Path, Question, Answer, Points) |> 
#   mutate(Points = as.numeric(Points))
# 
# # Step 4: Combine and pivot
# final <- bind_rows(df_clean, ent2a_3, ent2b_3)  |> 
#   pivot_wider(names_from = Answer, values_from = Points) |> 
#   as.data.frame()
