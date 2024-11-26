sm_variables <- bind_rows(
  kobo_tool.tool0$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool1$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool2$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool3$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool4$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool5$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool6$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool7$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool8$survey |> filter(grepl("select_multiple", type)) |> select(name),
  kobo_tool.tool9$survey |> filter(grepl("select_multiple", type)) |> select(name)
) |> pull(name)

options(scipen = 999)

check_logs_for_df <- function(cleaning_log, df, tool_name, deleted_keys) {
  # Log issues in all the sheets
  for(sheet in names(df)){
    question_names <- names(df[[sheet]]) # All Columns
    keys <- df[[sheet]][["KEY"]] # Keys
    
    # Flag if question or key is not in dataset
    tool_sheet_indexes <- (cleaning_log$tool %in% tool_name & cleaning_log$Tab_Name %in% sheet) # Logs for the current tool and sheet
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$question %notin% question_names] <- "Question can't be found in the given sheet"
    cleaning_log$issue[tool_sheet_indexes & cleaning_log$KEY %notin% keys] <- "KEY can't be found in the given sheet"
  }
  return(cleaning_log)
}

# form names
# form_names <- c()
form_names_ps <- unique(correction_log_ps$tool) |> na.omit()
form_names_cbe <- unique(correction_log_cbe$tool) |> na.omit()

# Reviewing the correction log ------------------------------------------------- Public School
# Identify correction log's issue
correction_log_issues_ps <- correction_log_ps |>
  mutate(
    issue = case_when(
      # general checks
      is.na(KEY) | KEY == "" ~ "KEY can't be null, please provide the correct KEY.",
      is.na(question) | question == "" ~ "Question name can't be null, please provide the correct question name.",
      is.na(tool) | tool == "" ~ "Tool name can't be null, please provide the correct tool name.",
      is.na(Tab_Name) | Tab_Name == "" ~ "Tab/Sheet name can't be null, please provide the correct Tab name.",
      !tool %in% form_names_ps ~ "Wrong tool name, please provide the correct tool name.", # Not necessary
      tool == "Tool 1 - Headmaster" & !Tab_Name %in% names(raw_data.tool1) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 2 - Light" & !Tab_Name %in% names(raw_data.tool2) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 3 - Headcount" & !Tab_Name %in% names(raw_data.tool3) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 4 - Teacher" & !Tab_Name %in% names(raw_data.tool4) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 5 - WASH" & !Tab_Name %in% names(raw_data.tool5) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 6 - Parent" & !Tab_Name %in% names(raw_data.tool6) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 7 - Shura" & !Tab_Name %in% names(raw_data.tool7) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 0 - Data Entry" & !Tab_Name %in% names(raw_data.tool0) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      duplicated(paste0(KEY, question), fromLast = T) | duplicated(paste0(KEY, question), fromLast = F) ~ "Duplicate log records, please solve the duplication.",
      TRUE ~ NA_character_
    ),
    new_value = case_when(
      question %in% sm_variables ~ str_replace_all(new_value, "-|,|  | - ", " ") %>% str_squish(), # Unify the separator in SM questions
      TRUE ~ str_squish(new_value)
    ),
    Sample_Type = "Public School"
  ) |> 
  select(KEY, question, old_value, new_value, issue, tool, Tab_Name, Sample_Type)

# Log incorrect sheet name and UUIDs
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool0, tool_name = "Tool 0 - Data Entry", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool1, tool_name = "Tool 1 - Headmaster", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool2, tool_name = "Tool 2 - Light", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool3, tool_name = "Tool 3 - Headcount", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool4, tool_name = "Tool 4 - Teacher", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool5, tool_name = "Tool 5 - WASH", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool6, tool_name = "Tool 6 - Parent", deleted_keys = deleted_keys_ps)
correction_log_issues_ps <- correction_log_issues_ps |> check_logs_for_df(df = raw_data.tool7, tool_name = "Tool 7 - Shura", deleted_keys = deleted_keys_ps)

## Correction Log ready to apply
correction_log_ready_ps <- correction_log_issues_ps |>
  filter(is.na(issue))

## Correction Log issues
correction_log_issues_ps <- correction_log_issues_ps |>
  filter(!is.na(issue)) |>
  arrange(KEY, question)

# Reviewing the correction log ------------------------------------------------- CBE
# Identify correction log's issue
correction_log_issues_cbe <- correction_log_cbe |>
  mutate(
    issue = case_when(
      # general checks
      is.na(KEY) | KEY == "" ~ "KEY can't be null, please provide the correct KEY.",
      is.na(question) | question == "" ~ "Question name can't be null, please provide the correct question name.",
      is.na(tool) | tool == "" ~ "Tool name can't be null, please provide the correct tool name.",
      is.na(Tab_Name) | Tab_Name == "" ~ "Tab/Sheet name can't be null, please provide the correct Tab name.",
      !tool %in% form_names_cbe ~ "Wrong tool name, please provide the correct tool name.", # Not necessary
      tool == "Tool 6 - Parent" & !Tab_Name %in% names(raw_data.tool6) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 7 - Shura" & !Tab_Name %in% names(raw_data.tool7) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 8 - Class" & !Tab_Name %in% names(raw_data.tool8) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      tool == "Tool 9 - IP" & !Tab_Name %in% names(raw_data.tool9) ~ "Wrong Tab/Sheet name, please provide the correct Tab name",
      duplicated(paste0(KEY, question), fromLast = T) | duplicated(paste0(KEY, question), fromLast = F) ~ "Duplicate log records, please solve the duplication.",
      TRUE ~ NA_character_
    ),
    new_value = case_when(
      question %in% sm_variables ~ str_replace_all(new_value, "-|,|  | - ", " ") %>% str_squish(), # Unify the separator in SM questions
      TRUE ~ str_squish(new_value)),
    Sample_Type = "CBE", 
  ) |> 
  select(KEY, question, old_value, new_value, issue, tool, Tab_Name, Sample_Type)

# Log incorrect sheet name and UUIDs
correction_log_issues_cbe <- correction_log_issues_cbe |> check_logs_for_df(df = raw_data.tool6, tool_name = "Tool 6 - Parent", deleted_keys = deleted_keys_cbe)
correction_log_issues_cbe <- correction_log_issues_cbe |> check_logs_for_df(df = raw_data.tool7, tool_name = "Tool 7 - Shura", deleted_keys = deleted_keys_cbe)
correction_log_issues_cbe <- correction_log_issues_cbe |> check_logs_for_df(df = raw_data.tool8, tool_name = "Tool 8 - Class", deleted_keys = deleted_keys_cbe)
correction_log_issues_cbe <- correction_log_issues_cbe |> check_logs_for_df(df = raw_data.tool9, tool_name = "Tool 9 - IP", deleted_keys = deleted_keys_cbe)

## Correction Log ready to apply
correction_log_ready_cbe <- correction_log_issues_cbe |>
  filter(is.na(issue))


## Correction Log issues
correction_log_issues_cbe <- correction_log_issues_cbe |>
  filter(!is.na(issue)) |>
  arrange(KEY, question)


# Clone Sheets for log apply verification -------------------------------------
clean_data.tool0 <- raw_data.tool0
clean_data.tool1 <- raw_data.tool1
clean_data.tool2 <- raw_data.tool2
clean_data.tool3 <- raw_data.tool3
clean_data.tool4 <- raw_data.tool4
clean_data.tool5 <- raw_data.tool5
clean_data.tool6 <- raw_data.tool6
clean_data.tool7 <- raw_data.tool7
clean_data.tool8 <- raw_data.tool8
clean_data.tool9 <- raw_data.tool9

# Apply logs -------------------------------------------------------------------
# Tool 0
for(sheet in names(clean_data.tool0)){
  # Apply Log
  clean_data.tool0[[sheet]] <- apply_log(data=clean_data.tool0[[sheet]], log = filter(correction_log_ready_ps, tool == "Tool 0 - Data Entry" & Tab_Name == sheet))
}

# Tool 1
tool_name <- "Tool 1 - Headmaster"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool1)){
    clean_data.tool1[[sheet]] <- apply_log(data=clean_data.tool1[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 2
tool_name <- "Tool 2 - Light"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool2)){
    clean_data.tool2[[sheet]] <- apply_log(data=clean_data.tool2[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 3
tool_name <- "Tool 3 - Headcount"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool3)){
    clean_data.tool3[[sheet]] <- apply_log(data=clean_data.tool3[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 4
tool_name <- "Tool 4 - Teacher"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool4)){
    clean_data.tool4[[sheet]] <- apply_log(data=clean_data.tool4[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 5
tool_name <- "Tool 5 - WASH"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool5)){
    
    clean_data.tool5[[sheet]] <- clean_data.tool5[[sheet]] |>
      mutate(
        across(ends_with("_Translation"), as.character)
      )
    
    clean_data.tool5[[sheet]] <- apply_log(data=clean_data.tool5[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 6
tool_name <- "Tool 6 - Parent"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool6)){
    clean_data.tool6[[sheet]] <- apply_log(data=clean_data.tool6[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

if (any(correction_log_ready_cbe$tool == tool_name)) {
  for(sheet in names(clean_data.tool6)){
    clean_data.tool6[[sheet]] <- apply_log(data=clean_data.tool6[[sheet]], log = filter(correction_log_ready_cbe, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 7
tool_name <- "Tool 7 - Shura"
if (any(correction_log_ready_ps$tool == tool_name)) {
  for(sheet in names(clean_data.tool7)){
    clean_data.tool7[[sheet]] <- apply_log(data=clean_data.tool7[[sheet]], log = filter(correction_log_ready_ps, tool == tool_name & Tab_Name == sheet))
  }
}

if (any(correction_log_ready_cbe$tool == tool_name)) {
  for(sheet in names(clean_data.tool7)){
    clean_data.tool7[[sheet]] <- apply_log(data=clean_data.tool7[[sheet]], log = filter(correction_log_ready_cbe, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 8
tool_name <- "Tool 8 - Class"
if (any(correction_log_ready_cbe$tool == tool_name)) {
  for(sheet in names(clean_data.tool8)){
    clean_data.tool8[[sheet]] <- apply_log(data=clean_data.tool8[[sheet]], log = filter(correction_log_ready_cbe, tool == tool_name & Tab_Name == sheet))
  }
}

# Tool 9
tool_name <- "Tool 9 - IP"
if (any(correction_log_ready_cbe$tool == tool_name)) {
  for(sheet in names(clean_data.tool9)){
    
    clean_data.tool9[[sheet]] <- clean_data.tool9[[sheet]] |>
      mutate(
        across(ends_with("_Translation"), as.character)
      )
  }
  
  for(sheet in names(clean_data.tool9)){
    clean_data.tool9[[sheet]] <- apply_log(data=clean_data.tool9[[sheet]], log = filter(correction_log_ready_cbe, tool == tool_name & Tab_Name == sheet))
  }
}


# Correction Log apply check --------------------------------------------------
message("Verifying Correction log, please wait!")

# Update the compare_df function in atrFunctions
correction_log_discrep <- data.frame()

# Tool 0
for(sheet in names(clean_data.tool0)){
  # Compare
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool0[[sheet]], raw_data.tool0[[sheet]]) |>
      mutate(tool="Tool 0 - Data Entry", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 1
for(sheet in names(clean_data.tool1)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool1[[sheet]], raw_data.tool1[[sheet]]) |>
      mutate(tool="Tool 1 - Headmaster", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 2
for(sheet in names(clean_data.tool2)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool2[[sheet]], raw_data.tool2[[sheet]]) |>
      mutate(tool="Tool 2 - Light", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 3
for(sheet in names(clean_data.tool3)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool3[[sheet]], raw_data.tool3[[sheet]]) |>
      mutate(tool="Tool 3 - Headcount", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}


# Tool 4
for(sheet in names(clean_data.tool4)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool4[[sheet]], raw_data.tool4[[sheet]]) |>
      mutate(tool="Tool 4 - Teacher", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 5
for(sheet in names(clean_data.tool5)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool5[[sheet]], raw_data.tool5[[sheet]]) |>
      mutate(tool="Tool 5 - WASH", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}


# Tool 6
for(sheet in names(clean_data.tool6)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool6[[sheet]], raw_data.tool6[[sheet]]) |>
      mutate(tool="Tool 6 - Parent", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 7
for(sheet in names(clean_data.tool7)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool7[[sheet]], raw_data.tool7[[sheet]]) |>
      mutate(tool="Tool 7 - Shura", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 8
for(sheet in names(clean_data.tool8)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool8[[sheet]], raw_data.tool8[[sheet]]) |>
      mutate(tool="Tool 8 - Class", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Tool 9
for(sheet in names(clean_data.tool9)){
  correction_log_discrep <- rbind(
    correction_log_discrep,
    compare_dt(clean_data.tool9[[sheet]], raw_data.tool9[[sheet]]) |>
      mutate(tool="Tool 9 - IP", Tab_Name = sheet, KEY_join = paste0(KEY, question, old_value, tool, Tab_Name))
  )
}

# Exclude the correction logs
required_cols <- c("KEY", "question", "old_value", "new_value", "tool", "Tab_Name")
correction_log_discrep <- correction_log_discrep |> 
  select(all_of(c(required_cols,"KEY_join"))) |>
  anti_join(bind_rows(correction_log_ready_ps, 
                      correction_log_ready_cbe) |>
              mutate(KEY_join = paste0(KEY, question, new_value, tool, Tab_Name))
            , by = "KEY_join") |>
  left_join(bind_rows(correction_log_ready_ps, correction_log_ready_cbe) |> # Join Sample Type
              select(KEY, question, Tab_Name, Sample_Type), by = c("KEY", "question", "Tab_Name")) |> 
  mutate(issue = "The new_value is not applied correctly, plz check if new_value is consistent with the question!") |>
  select(-KEY_join)

# Join with Correction log issues
correction_log_discrep <- rbind( 
  correction_log_discrep,
  correction_log_issues_ps |>
    select(all_of(required_cols), Sample_Type, "issue"),
  correction_log_issues_cbe |>
    select(all_of(required_cols), Sample_Type, "issue")
)

# Removing extra objects -------------------------------------------------------
rm(list = c("correction_log_issues_ps", "correction_log_issues_cbe",
            "correction_log_ready_ps", "correction_log_ready_cbe", 
            "tool_name", "form_names_ps", "form_names_cbe", "check_logs_for_df"))
