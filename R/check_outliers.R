if(!require(cleaninginspectoR)) devtools::install_github("ellieallien/cleaninginspectoR", dependencies = T)

# to_be_ignored_vars <- c("D7_Year", "G4_Minutes")
to_be_ignored_vars <- c("D2_Years", "D2_Months", "D7_Year", "D7_Month", "G4_Minutes")

# extracting the numeric indicators of each tool
tool0.numeric_vars <- kobo_tool.tool0$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool1.numeric_vars <- kobo_tool.tool1$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool2.numeric_vars <- kobo_tool.tool2$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool3.numeric_vars <- kobo_tool.tool3$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool4.numeric_vars <- kobo_tool.tool4$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool5.numeric_vars <- kobo_tool.tool5$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool6.numeric_vars <- kobo_tool.tool6$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool7.numeric_vars <- kobo_tool.tool7$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool8.numeric_vars <- kobo_tool.tool8$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)
tool9.numeric_vars <- kobo_tool.tool9$survey |> filter(type == "integer" & !name %in% to_be_ignored_vars) |> pull(name)


# creating the indices to facilitate joining of meta cols and KEY ---------
# Tool 0
for(sheet in names(clean_data.tool0)){
  assign(paste0("meta_cols_tool0",".",sheet), clean_data.tool0[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 1
for(sheet in names(clean_data.tool1)){
  assign(paste0("meta_cols_tool1",".",sheet), clean_data.tool1[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 2
for(sheet in names(clean_data.tool2)){
  assign(paste0("meta_cols_tool2",".",sheet), clean_data.tool2[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 3
for(sheet in names(clean_data.tool3)){
  assign(paste0("meta_cols_tool3",".",sheet), clean_data.tool3[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 4
for(sheet in names(clean_data.tool4)){
  assign(paste0("meta_cols_tool4",".",sheet), clean_data.tool4[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 5
for(sheet in names(clean_data.tool5)){
  assign(paste0("meta_cols_tool5",".",sheet), clean_data.tool5[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 6
for(sheet in names(clean_data.tool6)){
  assign(paste0("meta_cols_tool6",".",sheet), clean_data.tool6[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 7
for(sheet in names(clean_data.tool7)){
  assign(paste0("meta_cols_tool7",".",sheet), clean_data.tool7[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 8
for(sheet in names(clean_data.tool8)){
  assign(paste0("meta_cols_tool8",".",sheet), clean_data.tool8[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# Tool 9
for(sheet in names(clean_data.tool9)){
  assign(paste0("meta_cols_tool9",".",sheet), clean_data.tool9[[sheet]] %>% mutate(index = as.numeric(row.names(.))) %>% select(index, any_of(unname(meta_cols)), KEY))
}

# logging the outliers ---------------------------------------------------- HERE


outlier_check_result <- rbind(
  # Tool 0 -- Confirm values for 6666
  rbind(
    find_outliers(clean_data.tool0$data) |> left_join(meta_cols_tool0, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool0$Tool3_Grades_Repeat) |> left_join(meta_cols_tool0.Tool3_Grades_Repeat, by = "index") |> mutate(Tab_name = "Tool3_Grades_Repeat"),
    find_outliers(clean_data.tool0$Tool3_Class_Attendance) |> left_join(meta_cols_tool0.Tool3_Class_Attendance, by = "index") |> mutate(Tab_name = "Tool3_Class_Attendance"),
    find_outliers(clean_data.tool0$Tool3_T3_N_Classes_Repeat) |> left_join(meta_cols_tool0.Tool3_T3_N_Classes_Repeat, by = "index") |> mutate(Tab_name = "Tool3_T3_N_Classes_Repeat"),
    find_outliers(clean_data.tool0$Tool3_Headcount) |> left_join(meta_cols_tool0.Tool3_Headcount, by = "index") |> mutate(Tab_name = "Tool3_Headcount"),
    find_outliers(clean_data.tool0$Tool1_Timetable_Year) |> left_join(meta_cols_tool0.Tool1_Timetable_Year, by = "index") |> mutate(Tab_name = "Tool1_Timetable_Year"),
    find_outliers(clean_data.tool0$Tool1_Timetable1_Repeat) |> left_join(meta_cols_tool0.Tool1_Timetable1_Repeat, by = "index") |> mutate(Tab_name = "Tool1_Timetable1_Repeat"),
    find_outliers(clean_data.tool0$Tool1_Timetable2_Repeat) |> left_join(meta_cols_tool0.Tool1_Timetable2_Repeat, by = "index") |> mutate(Tab_name = "Tool1_Timetable2_Repeat"),
    find_outliers(clean_data.tool0$Tool1_Timetable3_Repeat) |> left_join(meta_cols_tool0.Tool1_Timetable3_Repeat, by = "index") |> mutate(Tab_name = "Tool1_Timetable3_Repeat"),
    find_outliers(clean_data.tool0$Tool1_Timetable4_Repeat) |> left_join(meta_cols_tool0.Tool1_Timetable4_Repeat, by = "index") |> mutate(Tab_name = "Tool1_Timetable4_Repeat")
  ) |> 
    filter(variable %in% tool0.numeric_vars) |> 
    mutate(tool = "Tool Data Entry") |> 
    select(KEY,all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name) |> filter(old_value != 6666),
  
  # Tool 1
  rbind(
    find_outliers(clean_data.tool1$data) |> left_join(meta_cols_tool1, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool1$School_Operationality) |> left_join(meta_cols_tool1.School_Operationality, by = "index") |> mutate(Tab_name = "School_Operationality"),
    find_outliers(clean_data.tool1$Shifts_Detail) |> left_join(meta_cols_tool1.Shifts_Detail, by = "index") |> mutate(Tab_name = "Shifts_Detail"),
    find_outliers(clean_data.tool1$Other_Shifts_Detail) |> left_join(meta_cols_tool1.Other_Shifts_Detail, by = "index") |> mutate(Tab_name = "Other_Shifts_Detail")
  ) |> 
    filter(variable %in% tool1.numeric_vars) |> 
    mutate(tool = "Tool 1 - Headmaster") |> 
    select(KEY,all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 2
  rbind(
    find_outliers(clean_data.tool2$data) |> left_join(meta_cols_tool2.data, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool2$Public_Stationary_Kit_Group) |> left_join(meta_cols_tool2.Public_Stationary_Kit_Group, by = "index") |> mutate(Tab_name = "Public_Stationary_Kit_Group"),
    find_outliers(clean_data.tool2$Teachers_Pack_Group) |> left_join(meta_cols_tool2.teacher_pack, by = "index") |> mutate(Tab_name = "Teachers_Pack_Group"),
    find_outliers(clean_data.tool2$Students_Pack_Group) |> left_join(meta_cols_tool2.student_pack, by = "index") |> mutate(Tab_name = "Students_Pack_Group")
  ) |> 
    filter(variable %in% tool2.numeric_vars) |> 
    mutate(tool = "Tool 2 - Light") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 3
  rbind(
    find_outliers(clean_data.tool3$data) |> left_join(meta_cols_tool3, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool3$Enrollement_Attendance_Summary) |> left_join(meta_cols_tool3.Enrollement_Attendance_Summary, by = "index") |> mutate(Tab_name = "Enrollement_Attendance_Summary"),
    find_outliers(clean_data.tool3$Todays_Attendance_Detail) |> left_join(meta_cols_tool3.Todays_Attendance_Detail, by = "index") |> mutate(Tab_name = "Todays_Attendance_Detail"),
    find_outliers(clean_data.tool3$LastWeek_Attendance_Detail) |> left_join(meta_cols_tool3.LastWeek_Attendance_Detail, by = "index") |> mutate(Tab_name = "LastWeek_Attendance_Detail"),
    find_outliers(clean_data.tool3$Student_Headcount) |> left_join(meta_cols_tool3.Student_Headcount, by = "index") |> mutate(Tab_name = "Student_Headcount")
  ) |> 
    filter(variable %in% tool3.numeric_vars) |> 
    mutate(tool = "Tool 3 - Headcount") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 4
  rbind(
    find_outliers(clean_data.tool4$data) |> left_join(meta_cols_tool4, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool4$Subjects_taught_by_this_teacher) |> left_join(meta_cols_tool4.Subjects_taught_by_this_teacher, by = "index") |> mutate(Tab_name = "Subjects_taught_by_this_teacher")
  ) |> 
    filter(variable %in% tool4.numeric_vars) |> 
    mutate(tool = "Tool 4 - Teacher") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),

  # Tool 5
  rbind(
    find_outliers(clean_data.tool5$data) |> left_join(meta_cols_tool5, by = "index") |> mutate(Tab_name = "data")
  ) |>  
    filter(variable %in% tool5.numeric_vars) |> 
    mutate(tool = "Tool 5 - WASH") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),

  # Tool 6
  find_outliers(clean_data.tool6$data) |> 
    filter(variable %in% tool6.numeric_vars) |> 
    mutate(tool = "Tool 6 - Parent", Tab_name = "data") |>
    left_join(meta_cols_tool6, by = "index") |> 
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 7
  rbind(
    find_outliers(clean_data.tool7$data) |> left_join(meta_cols_tool7, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool7$C6_list_members) |> left_join(meta_cols_tool7.C6_list_members, by = "index") |> mutate(Tab_name = "C6_list_members")
  ) |> 
    filter(variable %in% tool7.numeric_vars) |> 
    mutate(tool = "Tool 7 - Shura") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 8
  rbind(
    find_outliers(clean_data.tool8$data) |> left_join(meta_cols_tool8, by = "index") |> mutate(Tab_name = "data"),
    find_outliers(clean_data.tool8$Classes) |> left_join(meta_cols_tool8.Classes, by = "index") |> mutate(Tab_name = "Classes"),
    find_outliers(clean_data.tool8$Section_2_2_3_Attendance_Rec...) |> left_join(meta_cols_tool8.Section_2_2_3_Attendance_Rec..., by = "index") |> mutate(Tab_name = "Section_2_2_3_Attendance_Rec..."),
    find_outliers(clean_data.tool8$Section_2_2_4_Headcount) |> left_join(meta_cols_tool8.Section_2_2_4_Headcount, by = "index") |> mutate(Tab_name = "Section_2_2_4_Headcount"),
    find_outliers(clean_data.tool8$Section_2_4_Student_Ages) |> left_join(meta_cols_tool8.Section_2_4_Student_Ages, by = "index") |> mutate(Tab_name = "Section_2_4_Student_Ages"),
    find_outliers(clean_data.tool8$Classroom_Materials) |> left_join(meta_cols_tool8.Classroom_Materials, by = "index") |> mutate(Tab_name = "Classroom_Materials"),
    find_outliers(clean_data.tool8$Teacher_Kit) |> left_join(meta_cols_tool8.Teacher_Kit, by = "index") |> mutate(Tab_name = "Teacher_Kit"),
    find_outliers(clean_data.tool8$Student_Kit) |> left_join(meta_cols_tool8.Student_Kit, by = "index") |> mutate(Tab_name = "Student_Kit"),
    find_outliers(clean_data.tool8$V_list_of_all_members) |> left_join(meta_cols_tool8.V_list_of_all_members, by = "index") |> mutate(Tab_name = "V_list_of_all_members")
  ) |> 
    filter(variable %in% tool8.numeric_vars) |> 
    mutate(tool = "Tool 8 - Class") |>
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name),
  
  # Tool 9
  find_outliers(clean_data.tool9$data) |> 
    filter(variable %in% tool9.numeric_vars) |> 
    mutate(tool = "Tool 9 - IP", Tab_name = "data") |>
    left_join(meta_cols_tool9, by = "index") |> 
    select(KEY, all_of(meta_cols), index, question_name = variable, old_value = value, tool, Tab_name)
)


# removing extra elements from the environment
rm(list = c(
  objects(pattern = "\\.numeric_vars$"),
  objects(pattern = "^meta_cols_tool.*")
  ))
