# Organization:   ATR
# Date:           Nov 2024
# Script:         EERA Data Processing (Round 4)
# Author:         ATR Data Management Department

# Load required packages -------------------------------------------------------
rm(list = ls())
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(googlesheets4)) install.packages("googlesheets4")
if(!require(openxlsx)) install.packages("openxlsx")
if(!require(readxl)) install.packages("readxl")
if(!require(readxl)) install.packages("lubridate")
if(!require(readxl)) install.packages("janitor")
if(!require(remotes)) install.packages("remotes")
if(!require(atRfunctions)) remotes::install_github("atrcodebase/atRfunctions")
`%notin%` <- Negate(`%in%`)

# Declaring Global Variables ---------------------------------------------------
data_collection_start_date_ps = as.Date("23.11.2024", format("%d.%m.%Y")) # Confirm with QAT
data_collection_start_date_cbe = as.Date("23.11.2024", format("%d.%m.%Y")) # Confirm with QAT
# data_collection_end_date = as.Date("16.05.2024",  format("%d.%m.%Y"))
qa_sheet_url_ps = "https://docs.google.com/spreadsheets/d/1H-LSP9goLg7tlLTOpzrIht_r87sr-b6GF2i2ObBoo2k/edit?gid=1005923888#gid=1005923888"


# Rename DFs and Tools ------------------------------------------------------
source("./R/rename_dfs_tools.R")

output_data_path = "output/"
sample_files_path = "input/sample_files/"

raw_data_path = list.files("input/raw_data/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames(gsub(".*Tool (\\d).*", "tool\\1", .))
kobo_tools_path = list.files("input/tools/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames(gsub(".*Tool (\\d).*", "tool\\1", .))
relevancy_files_path = list.files("input/relevancy_files/", full.names = T) %>% .[!grepl("/\\~\\$", .)] |> as.list() %>% setNames(gsub(".*tool(\\d).*", "tool\\1", .))
meta_cols <- c("Site_Visit_ID", "EMIS_School_ID_CBE_KEY", "School_CBE_Name", "IP_Name", "Region", "Province", "District", "Area_Type", "Type_Of_School_CBE_Based_On_The_Sample", "School_CBE_Gender_Based_On_The_Sample", "School_Type_SV", "School_Gender_SV")
meta_cols.qa_sheet <- c(Visit_ID = "Site_Visit_ID", "School Code", "Sample_Type", Survey_Date = "SubmissionDate", Region = "Region", "KEY")

# Read inputs --------------------------------------------------------------
# Data sets
# raw_data.tool0 = read_xlsx_sheets(raw_data_path$tool0)
raw_data.tool1 = read_xlsx_sheets(raw_data_path$tool1)
raw_data.tool2 = read_xlsx_sheets(raw_data_path$tool2)
raw_data.tool3 = read_xlsx_sheets(raw_data_path$tool3)
raw_data.tool4 = read_xlsx_sheets(raw_data_path$tool4)
raw_data.tool5 = read_xlsx_sheets(raw_data_path$tool5)
raw_data.tool6 = read_xlsx_sheets(raw_data_path$tool6)
raw_data.tool7 = read_xlsx_sheets(raw_data_path$tool7)
raw_data.tool8 = read_xlsx_sheets(raw_data_path$tool8)
raw_data.tool9 = read_xlsx_sheets(raw_data_path$tool9)

# Tools
kobo_tool.tool0 = read_xlsx_sheets(kobo_tools_path$tool0)
kobo_tool.tool1 = read_xlsx_sheets(kobo_tools_path$tool1)
kobo_tool.tool2 = read_xlsx_sheets(kobo_tools_path$tool2)
kobo_tool.tool3 = read_xlsx_sheets(kobo_tools_path$tool3)
kobo_tool.tool4 = read_xlsx_sheets(kobo_tools_path$tool4)
kobo_tool.tool5 = read_xlsx_sheets(kobo_tools_path$tool5)
kobo_tool.tool6 = read_xlsx_sheets(kobo_tools_path$tool6)
kobo_tool.tool7 = read_xlsx_sheets(kobo_tools_path$tool7)
kobo_tool.tool8 = read_xlsx_sheets(kobo_tools_path$tool8)
kobo_tool.tool9 = read_xlsx_sheets(kobo_tools_path$tool9)


# read the relevancy files
relevancy_file.tool0 = read_xlsx_sheets(relevancy_files_path$tool0)
relevancy_file.tool1 = read_xlsx_sheets(relevancy_files_path$tool1)
relevancy_file.tool2 = read_xlsx_sheets(relevancy_files_path$tool2)
relevancy_file.tool3 = read_xlsx_sheets(relevancy_files_path$tool3)
relevancy_file.tool4 = read_xlsx_sheets(relevancy_files_path$tool4)
relevancy_file.tool5 = read_xlsx_sheets(relevancy_files_path$tool5)
relevancy_file.tool6 = read_xlsx_sheets(relevancy_files_path$tool6)
relevancy_file.tool7 = read_xlsx_sheets(relevancy_files_path$tool7)
relevancy_file.tool8 = read_xlsx_sheets(relevancy_files_path$tool8)
relevancy_file.tool9 = read_xlsx_sheets(relevancy_files_path$tool9)


# Read QA log from Google sheet ------------------------------------------------
qa_sheet_ps = read_sheet(qa_sheet_url_ps, sheet = "QA_Log") |> filter(Sample_Type == "Public School")
# To select the user to authenticate
2
qa_sheet_cbe = read_sheet(qa_sheet_url_ps, sheet = "QA_Log") |> filter(Sample_Type == "CBE")

correction_log = plyr::rbind.fill(
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Headmaster") |> mutate(Tool = "Tool 1 - Headmaster", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value),
                                                                              Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Light") |> mutate(Tool = "Tool 2 - Light", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Headcount") |> mutate(Tool = "Tool 3 - Headcount", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Teacher") |> mutate(Tool = "Tool 4 - Teacher", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value),
                                                                          Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log WASH") |> mutate(Tool = "Tool 5 - WASH", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Parent") |> rename(Key = KEY) |> mutate(Tool = "Tool 6 - Parent", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), Remarks = as.character(Remarks)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction _Log Shura") |> mutate(Tool = "Tool 7 - Shura", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction_Log Data_Entry") |> mutate(Tool = "Tool 0 - Data Entry", Index = NA_character_,  old_value = as.character(old_value), New_Value = as.character(New_Value), Key = as.character(Key))
) |>
  filter(!(Province == "Kandahar" & Tool == "Tool 1 - Headmaster"))


# Turn NULL values to NA for old and new value columns
correction_log <- correction_log %>% 
  mutate(
    New_Value = case_when(
      is.null(New_Value) | New_Value == "NULL" ~ NA_character_,
      TRUE ~ New_Value
    ),
    old_value = case_when(
      is.null(old_value) | old_value == "NULL" ~ NA_character_,
      TRUE ~ old_value
    )
  ) %>% 
  # Exclude if Unique_key, question, new value, and old value combo is NA
  filter(!((is.na(KEY_Unique)|KEY_Unique %in% c("", " ")) & is.na(Question) & is.na(New_Value) & is.na(old_value)))


correction_log_ps <- correction_log |> filter(is.na(Sample_Type) | Sample_Type == "Public School")

correction_log_cbe <- plyr::rbind.fill(
  correction_log |> filter(Sample_Type == "CBE"),
  read_sheet(qa_sheet_url_ps, sheet = "Correction _Log Class") |> mutate(Tool = "Tool 8 - Class", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), Key = as.character(Key)),
  read_sheet(qa_sheet_url_ps, sheet = "Correction _Log IP") |> mutate(Tool = "Tool 9 - IP", Index = as.character(Index), old_value = as.character(old_value), New_Value = as.character(New_Value), KEY_Unique = as.character(KEY_Unique),
                                                                      Key = as.character(Key))
) |>
filter(!(Province == "Kandahar" & Tool == "Tool 1 - Headmaster"))


# Turn NULL values to NA for old and new value columns
correction_log_cbe <- correction_log_cbe %>% 
  mutate(
    New_Value = case_when(
      is.null(New_Value) | New_Value == "NULL" ~ NA_character_,
      TRUE ~ New_Value
    ),
    old_value = case_when(
      is.null(old_value) | old_value == "NULL" ~ NA_character_,
      TRUE ~ old_value
    )
  ) %>% 
  # Exclude if Unique_key, question, new value, and old value combo is NA
  filter(!((is.na(KEY_Unique)|KEY_Unique %in% c("", " ")) & is.na(Question) & is.na(New_Value) & is.na(old_value)))

# Remove extra object
rm(correction_log)

deletion_log <- read_sheet(qa_sheet_url_ps, sheet = "To be removed from dataset")

# Prepare Data sets - Public School --------------------------------------------
qa_sheet_ps <- qa_sheet_ps |>
  rename(
    qa_status = "QA Status",
    tool = `Tool`, 
    KEY = KEY_Unique) |>
  mutate(
    qa_status = toupper(qa_status), 
    qa_status = case_when(
      is.na(qa_status) ~ "PENDING",
      TRUE ~ qa_status
    )
  )

# With KDR
table(qa_sheet_ps$qa_status, qa_sheet_ps$tool, useNA = "always") %>% 
  addmargins(2)

# Without KDR
# table(qa_sheet_ps |> filter(Province != "Kandahar") |> pull(qa_status), qa_sheet_ps |> filter(Province != "Kandahar") |> pull(tool), useNA = "always") %>% 
#   addmargins(2)

# Extract Approved Interviews
approved_keys_ps = qa_sheet_ps |> 
  filter(qa_status %in% c("APPROVED")) |> 
  pull(KEY) |> unique()

length(approved_keys_ps) == length(which(qa_sheet_ps$qa_status == "APPROVED"))

# Extract Pending interview to be included for weekly report
pending_key_ps = qa_sheet_ps |>
  filter(qa_status %in% c("PENDING")) |>
  pull(KEY) |> unique()

length(pending_key_ps) == length(which(qa_sheet_ps$qa_status %in% c("PENDING")))


# Extract deleted KEYs to be removed from data sets
deleted_keys_ps = deletion_log |> filter(Sample_Type == "Public School") |> pull(KEY_Unique)

# Align column names
correction_log_ps <- correction_log_ps %>% 
  mutate(across(everything(), as.character)) |> 
  rename(
    KEY = "KEY_Unique",
    # KEY = "Key",
    question = "Question",
    new_value = "New_Value",
    tool = "Tool"
  )


# Prepare Data sets - CBE ------------------------------------------------------
qa_sheet_cbe <- qa_sheet_cbe |>
  rename(
    qa_status = "QA Status",
    # qa_status = "QA.Status",
    tool = `Tool`,
    KEY = KEY_Unique) |>
  mutate(
    qa_status = toupper(qa_status), 
    qa_status = case_when(
      is.na(qa_status) ~ "PENDING",
      TRUE ~ qa_status
    )
  )

table(qa_sheet_cbe$qa_status, qa_sheet_cbe$tool, useNA = "always") %>% 
  addmargins(2)

# Without KDR
# table(qa_sheet_cbe |> filter(Province != "Kandahar") |> pull(qa_status), qa_sheet_cbe |> filter(Province != "Kandahar") |> pull(tool), useNA = "always") %>% 
#   addmargins(2)


# Extract Approved Interviews
approved_keys_cbe = qa_sheet_cbe |> 
  filter(qa_status %in% c("APPROVED")) |> 
  pull(KEY) |> unique()

length(approved_keys_cbe) == length(which(qa_sheet_cbe$qa_status == "APPROVED"))


# Extract Pending interview to be included for weekly report
pending_key_cbe = qa_sheet_cbe |>
  filter(qa_status %in% c("PENDING")) |>
  pull(KEY) |> unique()

length(pending_key_cbe) == length(which(qa_sheet_cbe$qa_status %in% c("PENDING")))

# Extract deleted KEYs to be removed from data sets
deleted_keys_cbe = deletion_log |> filter(Sample_Type == "CBE") |> pull(KEY_Unique)

# Align column names
correction_log_cbe <- correction_log_cbe %>% 
  mutate(across(everything(), as.character)) |> 
  rename(
    KEY = "KEY_Unique",
    # KEY = "Key",
    question = "Question",
    new_value = "New_Value",
    tool = "Tool"
  )


# Detailed Check log ------------------------------------------------------
detailed_check_log <- read_sheet(qa_sheet_url_ps, sheet = "Detailed_Check")

photo_status_ps <- detailed_check_log %>% 
  filter(Check_Type == "image" & `QA Status` == "Approved" & !is.na(Check_Status) & Sample_Type == "Public School")

photo_status_cbe <- detailed_check_log %>% 
  filter(Check_Type == "image" & `QA Status` == "Approved" & !is.na(Check_Status) & Sample_Type == "CBE")


# convert numeric dates to date and time formats -------------------------- DONE
source("R/convert_numbers_to_date_time.R")


# Apply correction log ---------------------------------------------------- DONE
if(nrow(correction_log_ps) > 0 | nrow(correction_log_cbe) > 0) { source("R/apply_correction_log.R") }


# Apply Photo status log -------------------------------------------------- Done
if(nrow(photo_status_ps) > 0 | nrow(photo_status_cbe) > 0){ source("R/update_photo_status.R") }


# Remove the rejected and pilot interviews -------------------------------- Done
source("R/remove_rejected_interviews.R")


# Merge meta data from main sheet to repeating groups --------------------- DONE
source("R/main_sheet_to_repeat_sheets.R")


# Update select multiple binary variables --------------------------------- DONE
source("R/fix_select_multiple_questions.R") 


# Data for Weekly Report -------------------------------------------------------
# source("R/data_for_weekly_report.R")


# Check repeat sheet count ------------------------------------------------ DONE
source("R/check_repeat_sheet_counts.R")


# missing translations (for QA)-------------------------------------------- DONE
source("R/create_translation_log.R")


# missing qa (for QA)------------------------------------------------------ S
# source("R/missing_qa.R")


# Check select multiple variables ----------------------------------------- DONE
source("R/check_select_multiple_questions.R")


# re-calculate the calculated variables and compare any changes not applied - NOT
source("R/calculate_cols_check.R") 


# Outlier Check ----------------------------------------------------------- NOT
# source("R/check_outliers.R") 


# Relevancy Check --------------------------------------------------------- NOT
source("R/check_relevancies.R")


# Check the responses with the tool --------------------------------------- DONE - must be checked
source("R/compare_df_values_with_tool.R")


# attach value labels  ---------------------------------------------------------
source("R/attach_labels.R")


# Logical inconsistencies ------------------------------------------------- NOT
source("R/logical_checks.R")


# remove extra columns  -------------------------------------------- Value Remaining
source("R/remove_extra_columns.R")


# attach labels to calculates cols ---------------------------------------- DONE
source("R/attach_calculate_label.R")


# change 7777, 8888, 9999 to Labels  -------------------------------------- NOT
source("R/recode_to_na.R")


# export data sets and issues --------------------------------------------- DONE
source("R/export_outputs.R")

