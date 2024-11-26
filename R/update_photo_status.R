check_logs_for_df <- function(cleaning_log, df, tool_name, deleted_keys) {
  # Log issues in all the sheets
  # sheet = "data"
  for(sheet in names(df)){
    question_names <- names(df[[sheet]]) # All Columns
    keys <- df[[sheet]][["KEY"]] # Keys
    
    # Flag if question or key is not in dataset
    tool_sheet_indexes <- (cleaning_log$Tool %in% tool_name & cleaning_log$Tab_Name %in% sheet) # Logs for the current tool and sheet
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$Question %notin% question_names] <- "Question can't be found in the given sheet"
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$KEY_Unique %notin% keys] <- "KEY can't be found in the given sheet"
  }
  return(cleaning_log)
}

form_names_ps <- unique(photo_status_ps$Tool) |> na.omit()
form_names_cbe <- unique(photo_status_cbe$Tool) |> na.omit()

# Reviewing the Photo Status log ------------------------------------------------- PS
# Identify log's issue
photo_status_issues_ps <- photo_status_ps |>
  mutate(
    issue = case_when(
      # general checks
      is.na(KEY_Unique) | KEY_Unique == "" ~ "KEY can't be null, please provide the correct KEY.",
      is.na(Question) | Question == "" ~ "Question name can't be null, please provide the correct question name.",
      is.na(Tool) | Tool == "" ~ "Tool name can't be null, please provide the correct tool name.",
      is.na(Tab_Name) | Tab_Name == "" ~ "Tab/Sheet name can't be null, please provide the correct Tab name.",
      !Tool %in% form_names_ps ~ "Wrong tool name, please provide the correct tool name.", # Not necessary
      Tool == "Headmaster Interview" & !Tab_Name %in% names(raw_data.tool1) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Light Tool" & !Tab_Name %in% names(raw_data.tool2) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Student Document & Headcount" & !Tab_Name %in% names(raw_data.tool3) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Teacher Tool" & !Tab_Name %in% names(raw_data.tool4) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "WASH Tool" & !Tab_Name %in% names(raw_data.tool5) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Parent Tool" & !Tab_Name %in% names(raw_data.tool6) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Shura Tool" & !Tab_Name %in% names(raw_data.tool7) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      # tool == "Tool 0 - Data Entry" & !Tab_Name %in% names(raw_data.tool0) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      duplicated(paste0(KEY_Unique, Question), fromLast = T) | duplicated(paste0(KEY_Unique, Question), fromLast = F) ~ "Duplicate log records, please solve the duplication.",
      TRUE ~ NA_character_
    ),
    Sample_Type = "Public School"
  ) |> 
  select(KEY_Unique, Question, Value, issue, Tool, Tab_Name, Sample_Type)

# Log incorrect sheet name and UUIDs
# photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = raw_data.tool0, tool_name = "Tool 0 - Data Entry", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- check_logs_for_df(cleaning_log = photo_status_issues_ps,  df = clean_data.tool1, tool_name = "Headmaster Interview", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool2, tool_name = "Light Tool", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool3, tool_name = "Student Document & Headcount", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool4, tool_name = "Teacher Tool", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool5, tool_name = "WASH Tool", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool6, tool_name = "Parent Tool", deleted_keys = deleted_keys_ps)
photo_status_issues_ps <- photo_status_issues_ps |> check_logs_for_df(df = clean_data.tool7, tool_name = "Shura Tool", deleted_keys = deleted_keys_ps)

## Correction Log ready to apply
photo_status_ready_ps <- photo_status_issues_ps |>
  filter(is.na(issue))

## Correction Log issues
photo_status_issues_ps <- photo_status_issues_ps |>
  filter(!is.na(issue)) |>
  arrange(KEY_Unique, Question)


# Reviewing the Photo Status log ------------------------------------------------- CBE
# Identify log's issue
photo_status_issues_cbe <- photo_status_cbe |>
  mutate(
    issue = case_when(
      # general checks
      is.na(KEY_Unique) | KEY_Unique == "" ~ "KEY can't be null, please provide the correct KEY.",
      is.na(Question) | Question == "" ~ "Question name can't be null, please provide the correct question name.",
      is.na(Tool) | Tool == "" ~ "Tool name can't be null, please provide the correct tool name.",
      is.na(Tab_Name) | Tab_Name == "" ~ "Tab/Sheet name can't be null, please provide the correct Tab name.",
      !Tool %in% form_names_cbe ~ "Wrong tool name, please provide the correct tool name.", # Not necessary
      Tool == "Parent Tool" & !Tab_Name %in% names(raw_data.tool6) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Shura Tool" & !Tab_Name %in% names(raw_data.tool7) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Tool 8 - Class" & !Tab_Name %in% names(raw_data.tool8) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      Tool == "Tool 9 - IP" & !Tab_Name %in% names(raw_data.tool9) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      duplicated(paste0(KEY_Unique, Question), fromLast = T) | duplicated(paste0(KEY_Unique, Question), fromLast = F) ~ "Duplicate log records, please solve the duplication.",
      TRUE ~ NA_character_
    ),
    Sample_Type = "CBE", 
  ) |> 
  select(KEY_Unique, Question, Value, issue, Tool, Tab_Name, Sample_Type)

# Log incorrect sheet name and UUIDs
photo_status_issues_cbe <- photo_status_issues_cbe |> check_logs_for_df(df = raw_data.tool6, tool_name = "Parent Tool", deleted_keys = deleted_keys_cbe)
photo_status_issues_cbe <- photo_status_issues_cbe |> check_logs_for_df(df = raw_data.tool7, tool_name = "Shura Tool", deleted_keys = deleted_keys_cbe)
photo_status_issues_cbe <- photo_status_issues_cbe |> check_logs_for_df(df = raw_data.tool8, tool_name = "Tool 8 - Class", deleted_keys = deleted_keys_cbe)
photo_status_issues_cbe <- photo_status_issues_cbe |> check_logs_for_df(df = raw_data.tool9, tool_name = "Tool 9 - IP", deleted_keys = deleted_keys_cbe)

## Correction Log ready to apply
photo_status_ready_cbe <- photo_status_issues_cbe |>
  filter(is.na(issue))


## Correction Log issues
photo_status_issues_cbe <- photo_status_issues_cbe |>
  filter(!is.na(issue)) |>
  arrange(KEY_Unique, Question)

# Align column names -----------------------------------------------------------
photo_status_ready_ps <- photo_status_ready_ps %>%
  rename(
    new_value = Value,
    question = Question,
    KEY = KEY_Unique
  )

photo_status_ready_cbe <- photo_status_ready_cbe %>%
  rename(
    new_value = Value,
    question = Question,
    KEY = KEY_Unique
  )


clean_data.tool1_compare <- clean_data.tool1
clean_data.tool2_compare <- clean_data.tool2
clean_data.tool3_compare <- clean_data.tool3
clean_data.tool4_compare <- clean_data.tool4
clean_data.tool5_compare <- clean_data.tool5
clean_data.tool6_compare <- clean_data.tool6
clean_data.tool7_compare <- clean_data.tool7
clean_data.tool8_compare <- clean_data.tool8
clean_data.tool9_compare <- clean_data.tool9


# Apply logs -------------------------------------------------------------------
# Tool 0
# for(sheet in names(clean_data.tool0)){
#   # Apply Log
#   clean_data.tool0[[sheet]] <- apply_log(data=clean_data.tool0[[sheet]], log = filter(photo_status_ready_ps, Tool == "Tool 0 - Data Entry" & Tab_Name == sheet))
# }

# Tool 1
tool_name <- "Headmaster Interview"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool1)){
    clean_data.tool1[[sheet]] <- apply_log(data=clean_data.tool1[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 2
tool_name <- "Light Tool"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool2)){
    clean_data.tool2[[sheet]] <- apply_log(data=clean_data.tool2[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 3
tool_name <- "Student Document & Headcount"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool3)){
    clean_data.tool3[[sheet]] <- apply_log(data=clean_data.tool3[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 4
tool_name <- "Teacher Tool"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool4)){
    clean_data.tool4[[sheet]] <- apply_log(data=clean_data.tool4[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 5
tool_name <- "WASH Tool"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool5)){
    
    clean_data.tool5[[sheet]] <- clean_data.tool5[[sheet]] |>
      mutate(
        across(ends_with("_Translation"), as.character)
      )
    
    clean_data.tool5[[sheet]] <- apply_log(data=clean_data.tool5[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 6
tool_name <- "Parent Tool"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool6)){
    clean_data.tool6[[sheet]] <- apply_log(data=clean_data.tool6[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

if (any(photo_status_ready_cbe$Tool == tool_name)) {
  for(sheet in names(clean_data.tool6)){
    clean_data.tool6[[sheet]] <- apply_log(data=clean_data.tool6[[sheet]], log = filter(photo_status_ready_cbe, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 7
tool_name <- "Shura Tool"
if (any(photo_status_ready_ps$Tool == tool_name)) {
  for(sheet in names(clean_data.tool7)){
    clean_data.tool7[[sheet]] <- apply_log(data=clean_data.tool7[[sheet]], log = filter(photo_status_ready_ps, Tool == tool_name & Tab_Name == sheet))
  }
}

if (any(photo_status_ready_cbe$Tool == tool_name)) {
  for(sheet in names(clean_data.tool7)){
    clean_data.tool7[[sheet]] <- apply_log(data=clean_data.tool7[[sheet]], log = filter(photo_status_ready_cbe, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 8
tool_name <- "Tool 8 - Class"
if (any(photo_status_ready_cbe$Tool == tool_name)) {
  for(sheet in names(clean_data.tool8)){
    clean_data.tool8[[sheet]] <- apply_log(data=clean_data.tool8[[sheet]], log = filter(photo_status_ready_cbe, Tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 9
tool_name <- "Tool 9 - IP"
if (any(photo_status_ready_cbe$Tool == tool_name)) {
  for(sheet in names(clean_data.tool9)){
    
    clean_data.tool9[[sheet]] <- clean_data.tool9[[sheet]] |>
      mutate(
        across(ends_with("_Translation"), as.character)
      )
  }
  
  for(sheet in names(clean_data.tool9)){
    clean_data.tool9[[sheet]] <- apply_log(data=clean_data.tool9[[sheet]], log = filter(photo_status_ready_cbe, Tool == tool_name & Tab_Name == sheet))
  }
}


# Correction Log apply check --------------------------------------------------
message("Verifying Correction log, please wait!")

# Update the compare_df function in atrFunctions
photo_status_log_discrep <- data.frame()

# Tool 0
# for(sheet in names(clean_data.tool0)){
#   # Compare
#   photo_status_log_discrep <- rbind(
#     photo_status_log_discrep,
#     compare_dt(clean_data.tool0[[sheet]], raw_data.tool0[[sheet]]) |>
#       mutate(tool="Tool 0 - Data Entry", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
#   )
# }

# Tool 1
for(sheet in names(clean_data.tool1)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool1[[sheet]], clean_data.tool1_compare[[sheet]]) |>
      mutate(tool="Headmaster Interview", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 2
for(sheet in names(clean_data.tool2)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool2[[sheet]], clean_data.tool2_compare[[sheet]]) |>
      mutate(tool="Light Tool", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 3
for(sheet in names(clean_data.tool3)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool3[[sheet]], clean_data.tool3_compare[[sheet]]) |>
      mutate(tool="Student Document & Headcount", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}


# Tool 4
for(sheet in names(clean_data.tool4)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool4[[sheet]], clean_data.tool4_compare[[sheet]]) |>
      mutate(tool="Teacher Tool", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 5
for(sheet in names(clean_data.tool5)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool5[[sheet]], clean_data.tool5_compare[[sheet]]) |>
      mutate(tool="WASH Tool", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}


# Tool 6
for(sheet in names(clean_data.tool6)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool6[[sheet]], clean_data.tool6_compare[[sheet]]) |>
      mutate(tool="Parent Tool", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 7
for(sheet in names(clean_data.tool7)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool7[[sheet]], clean_data.tool7_compare[[sheet]]) |>
      mutate(tool="Shura Tool", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 8
for(sheet in names(clean_data.tool8)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool8[[sheet]], clean_data.tool8_compare[[sheet]]) |>
      mutate(tool="Tool 8 - Class", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 9
for(sheet in names(clean_data.tool9)){
  photo_status_log_discrep <- rbind(
    photo_status_log_discrep,
    compare_dt(clean_data.tool9[[sheet]], clean_data.tool9_compare[[sheet]]) |>
      mutate(tool="Tool 9 - IP", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Exclude the correction logs
required_cols <- c("KEY", "question", "old_value", "new_value", "tool", "Tab_Name")
photo_status_log_discrep <- photo_status_log_discrep |> 
  select(all_of(c(required_cols,"KEY_join"))) |>
  anti_join(bind_rows(photo_status_ready_ps, 
                      photo_status_ready_cbe) |>
              mutate(KEY_join = paste0(KEY, question, new_value, Tool, Tab_Name))
            , by = "KEY_join") |>
  mutate(question = as.character(question)) %>% 
  left_join(bind_rows(photo_status_ready_ps, photo_status_ready_cbe) |> # Join Sample Type
              select(KEY, question, Tab_Name, Sample_Type), by = c("KEY", "question", "Tab_Name")) |> 
  mutate(issue = "The new_value is not applied correctly, plz check if new_value is consistent with the question!") |>
  select(-KEY_join)

# Join with Correction log issues
photo_status_log_discrep <- rbind( 
  photo_status_log_discrep,
  photo_status_issues_ps |>
    select(any_of(required_cols), tool = Tool, KEY = KEY_Unique, Sample_Type, "issue"),
  photo_status_issues_cbe |>
    select(any_of(required_cols), tool = Tool, KEY = KEY_Unique, Sample_Type, "issue")
)

# Removing extra objects -------------------------------------------------------
rm(list = c("photo_status_issues_ps", "photo_status_issues_cbe",
            "photo_status_ready_ps", "photo_status_ready_cbe", 
            "tool_name", "form_names_ps", "form_names_cbe", "check_logs_for_df"))