# last weeks data

# Clone data sets --------------------------------------------------------------
clean_data.tool0.report <- clean_data.tool0
clean_data.tool1.report <- clean_data.tool1
clean_data.tool2.report <- clean_data.tool2
clean_data.tool3.report <- clean_data.tool3
clean_data.tool4.report <- clean_data.tool4
clean_data.tool5.report <- clean_data.tool5
clean_data.tool6.report <- clean_data.tool6
clean_data.tool7.report <- clean_data.tool7
clean_data.tool8.report <- clean_data.tool8
clean_data.tool9.report <- clean_data.tool9


# Tool 0 - Data Entry
clean_data.tool0.report$data <- clean_data.tool0.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps, approved_keys_cbe, pending_key_cbe) & !KEY %in% c(deleted_keys_ps, deleted_keys_cbe))

for (sheet in names(clean_data.tool0.report)[c(2,3,4,9)]) {
  clean_data.tool0.report[[sheet]] <- clean_data.tool0.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool0.report$data$KEY & !KEY %in% c(deleted_keys_ps, deleted_keys_cbe))
}

for (sheet in names(clean_data.tool0.report)[c(5,6,7,8)]) {
  clean_data.tool0.report[[sheet]] <- clean_data.tool0.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool0.report$Tool1_Timetable_Year$KEY & !KEY %in% c(deleted_keys_ps, deleted_keys_cbe))
}

# Tool 1 - Headmaster
clean_data.tool1.report$data <- clean_data.tool1.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps) & !KEY %in% deleted_keys_ps)
for(sheet in names(clean_data.tool1.report)[-1]){
  clean_data.tool1.report[[sheet]] <- clean_data.tool1.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool1.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 2 - Light
clean_data.tool2.report$data <- clean_data.tool2.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps) & !KEY %in% deleted_keys_ps)
for(sheet in names(clean_data.tool2.report)[-1]){
  clean_data.tool2.report[[sheet]] <- clean_data.tool2.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool2.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 3 - Headcount
clean_data.tool3.report$data <- clean_data.tool3.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps) & !KEY %in% deleted_keys_ps)
for(sheet in names(clean_data.tool3.report)[-1]){
  clean_data.tool3.report[[sheet]] <- clean_data.tool3.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool3.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 4 - Teacher
clean_data.tool4.report$data <- clean_data.tool4.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps) & !KEY %in% deleted_keys_ps)
for(sheet in names(clean_data.tool4.report)[-1]){
  clean_data.tool4.report[[sheet]] <- clean_data.tool4.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool4.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 5 - WASH
clean_data.tool5.report$data <- clean_data.tool5.report$data %>% filter(KEY %in% c(approved_keys_ps, pending_key_ps) & !KEY %in% deleted_keys_ps)
for(sheet in names(clean_data.tool5.report)[-1]){
  clean_data.tool5.report[[sheet]] <- clean_data.tool5.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool5.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 6 - Parent
clean_data.tool6.report$data <- clean_data.tool6.report$data %>% filter((KEY %in% c(approved_keys_ps, pending_key_ps) | KEY %in% c(approved_keys_cbe, pending_key_cbe)) & !KEY %in% c(deleted_keys_ps,deleted_keys_cbe))
for(sheet in names(clean_data.tool6.report)[-1]){
  clean_data.tool6.report[[sheet]] <- clean_data.tool6.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool6.report$data$KEY & !KEY %in% deleted_keys_ps)
}

# Tool 7 - SHURA
clean_data.tool7.report$data <- clean_data.tool7.report$data %>% filter((KEY %in% c(approved_keys_ps, pending_key_ps) | KEY %in% c(approved_keys_cbe, pending_key_cbe)) & !KEY %in% c(deleted_keys_ps,deleted_keys_cbe))
for(sheet in names(clean_data.tool7.report)[-1]){
  clean_data.tool7.report[[sheet]] <- clean_data.tool7.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool7.report$data$KEY & !KEY %in% deleted_keys_ps)
}


# Tool 8 - Class
clean_data.tool8.report$data <- clean_data.tool8.report$data %>% filter(KEY %in% c(approved_keys_cbe, pending_key_cbe) & !KEY %in% deleted_keys_cbe)
for(sheet in names(clean_data.tool8.report)[-1]){
  clean_data.tool8.report[[sheet]] <- clean_data.tool8.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool8.report$data$KEY & !KEY %in% deleted_keys_ps)
}


# Tool 9 - IP
clean_data.tool9.report$data <- clean_data.tool9.report$data %>% filter(KEY %in% c(approved_keys_cbe, pending_key_cbe) & !KEY %in% deleted_keys_cbe)
for(sheet in names(clean_data.tool9.report)[-1]){
  clean_data.tool9.report[[sheet]] <- clean_data.tool9.report[[sheet]] %>% filter(PARENT_KEY %in% clean_data.tool9.report$data$KEY & !KEY %in% deleted_keys_ps)
}


# Attach Labels
tool0_path = "./input/tools/Tool 0.EERA Public School_CBE - Data Entry Tool - R4.xlsx"
tool1_path = "./input/tools/Tool 1.EERA Public School - Headmaster_Principle Interview - R4.xlsx"
tool2_path = "./input/tools/Tool 2.EERA Public School - Light Tool - R4.xlsx"
tool3_path = "./input/tools/Tool 3.EERA Public School - Student Document & Headcount - R4.xlsx"
tool4_path = "./input/tools/Tool 4.EERA Public School - Teacher Tool - R4.xlsx"
tool5_path = "./input/tools/Tool 5.EERA Public School - WASH Observation - R4.xlsx"
tool6_path = "./input/tools/Tool 6.EERA Public School_CBE - Parent Tool - R4.xlsx"
tool7_path = "./input/tools/Tool 7.EERA Public School_CBE - Shura Tool - R4.xlsx"
tool8_path = "./input/tools/Tool 8.EERA CBE - Class Level Tool - R4.xlsx"
tool9_path = "./input/tools/Tool 9.EERA CBE - IP Level Tool - R4.xlsx"

# Tool 0 ------------------------------------------------------------------
for(sheet in names(clean_data.tool0.report)){
  clean_data.tool0.report[[sheet]] <- labeler(
    data = clean_data.tool0.report[[sheet]],
    tool = tool0_path,
    survey_label = "label",
    choice_lable = "label"
  )
}


# Tool 1 ------------------------------------------------------------------
for(sheet in names(clean_data.tool1.report)){
  clean_data.tool1.report[[sheet]] <- labeler(
      data = clean_data.tool1.report[[sheet]],
      tool = tool1_path,
      survey_label = "label",
      choice_lable = "label"
  )
}

# Tool 2 ------------------------------------------------------------------
for(sheet in names(clean_data.tool2.report)){
  clean_data.tool2.report[[sheet]] <- labeler(
    data = clean_data.tool2.report[[sheet]],
    tool = tool2_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 3 ------------------------------------------------------------------
for(sheet in names(clean_data.tool3.report)){
  clean_data.tool3.report[[sheet]] <- labeler(
    data = clean_data.tool3.report[[sheet]],
    tool = tool3_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 4 ------------------------------------------------------------------
for(sheet in names(clean_data.tool4.report)){
  clean_data.tool4.report[[sheet]] <- labeler(
    data = clean_data.tool4.report[[sheet]],
    tool = tool4_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 5 ------------------------------------------------------------------
for(sheet in names(clean_data.tool5.report)){
  clean_data.tool5.report[[sheet]] <- labeler(
    data = clean_data.tool5.report[[sheet]],
    tool = tool5_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 6 ------------------------------------------------------------------
for(sheet in names(clean_data.tool6.report)){
  clean_data.tool6.report[[sheet]] <- labeler(
    data = clean_data.tool6.report[[sheet]],
    tool = tool6_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 7 ------------------------------------------------------------------
for(sheet in names(clean_data.tool7.report)){
  clean_data.tool7.report[[sheet]] <- labeler(
    data = clean_data.tool7.report[[sheet]],
    tool = tool7_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 8 ------------------------------------------------------------------
for(sheet in names(clean_data.tool8.report)){
  clean_data.tool8.report[[sheet]] <- labeler(
    data = clean_data.tool8.report[[sheet]],
    tool = tool8_path,
    survey_label = "label",
    choice_lable = "label"
  )
}

# Tool 9 ------------------------------------------------------------------
for(sheet in names(clean_data.tool9.report)){
  clean_data.tool9.report[[sheet]] <- labeler(
    data = clean_data.tool9.report[[sheet]],
    tool = tool9_path,
    survey_label = "label",
    choice_lable = "label"
  )
}


write.xlsx(clean_data.tool0.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool0_EERA_R4_Public_School_Data_Entry_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool1.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool1_EERA_R4_Public_School_Headmaster_Interview_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool2.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool2_EERA_R4_Public_School_Light_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool3.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool3_EERA_R4_Public_School_Student_Document_Headcount_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool4.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool4_EERA_R4_Public_School_Teacher_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool5.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool5_EERA_R4_Public_School_WASH_Observation_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool6.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool6_EERA_R4_Public_School_Parent_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool7.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool7_EERA_R4_Public_School_Shura_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool8.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool8_EERA_R4_CBE_Class_Level_Tool_For_Reporting_", Sys.Date(),".xlsx"))
write.xlsx(clean_data.tool9.report, paste0(output_data_path, "cleaned_dfs/unsterilized/for_report/Tool9_EERA_R4_CBE_IP_Level_Tool_For_Reporting_", Sys.Date(),".xlsx"))


# Removing PIIs
# source("./R/anonymize_data.R")
# 
# write.xlsx(clean_data.tool1.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool1_EERA_R2_Public_School_Headmaster_Interview_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool2.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool2_EERA_R2_Public_School_Light_Tool_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool3.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool3_EERA_R2_Public_School_Student_Headcount_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool4.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool4_EERA_R2_Public_School_Teacher_Tool_For_Reportin_Anonymizedg_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool5.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool5_EERA_R2_Public_School_WASH_Observation_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool6.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool6_EERA_R2_Public_School_Parent_Tool_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool7.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool7_EERA_R2_Public_School_Shura_Tool_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool8.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool8_EERA_R2_CBE_Class_Level_Tool_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))
# write.xlsx(clean_data.tool9.report_anonym, paste0(output_data_path, "cleaned_dfs/sterilized/for_report/Tool9_EERA_R2_CBE_IP_Level_Tool_For_Reporting_Anonymized_", Sys.Date(),".xlsx"))

# remove extra objects from environment  
# rm(list = 
#      c(ls(pattern = ".report$")
#        )
#    )

rm(tool1_path, 
   tool2_path,
   tool3_path,
   tool4_path,
   tool5_path,
   tool6_path,
   tool7_path,
   tool8_path,
   tool9_path)
