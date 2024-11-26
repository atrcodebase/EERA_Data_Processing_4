source("R/functions/check_select_multiple.R")

sm_check_result <- plyr::rbind.fill(
  # Tool 0
  rbind(
    check_select_multiple(data = clean_data.tool0$data, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool3_Classes, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool3_T3_Classes_LW, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool3_T2_Classes_VD, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool1_Timetable_Year, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool1_Timetable1_Repeat, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool1_Timetable2_Repeat, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool1_Timetable3_Repeat, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool1_Timetable4_Repeat, tool = kobo_tool.tool0$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool0$Tool3_Grades_Repeat, tool = kobo_tool.tool0$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool Data Entry", Sample_Type = ""),
  
  # Tool 1
  rbind(
    check_select_multiple(data = clean_data.tool1$data, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Support_Respondents, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$School_Operationality, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$School_Operationality_Other_..., tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Shifts_Detail, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Other_Shifts_Detail, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Headmasters, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Weekly_Schedule_Old, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Weekly_Schedule_New, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Subjects_Detail, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Additional_Subjects, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Education_Quality, tool = kobo_tool.tool1$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool1$Relevant_photos, tool = kobo_tool.tool1$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 1 - Headmaster", Sample_Type = "Public School"),
  # Tool 2
  rbind(
    check_select_multiple(data = clean_data.tool2$data, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Support_Respondents, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Attendance_Sheet_Photos, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Public_Stationary_Kit_Group, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Teachers_Pack_Group, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Students_Pack_Group, tool = kobo_tool.tool2$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool2$Relevant_photos, tool = kobo_tool.tool2$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 2 - Light", Sample_Type = "Public School"),
  # Tool 3
  rbind(
    check_select_multiple(data = clean_data.tool3$data, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Support_Respondents, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Grade_Details, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Todays_Attendance_Detail, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$LastWeek_Attendance_Detail, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Student_Headcount, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Tool3_Grades_Repeat, tool = kobo_tool.tool3$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool3$Relevant_photos, tool = kobo_tool.tool3$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 3 - Headcount", Sample_Type = "Public School"),
  # Tool 4
  rbind(
    check_select_multiple(data = clean_data.tool4$data, tool = kobo_tool.tool4$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool4$Additional_Subjects, tool = kobo_tool.tool4$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool4$Subjects_taught_by_this_teacher, tool = kobo_tool.tool4$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool4$Subjects_Not_Being_Taught, tool = kobo_tool.tool4$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool4$Relevant_photos, tool = kobo_tool.tool4$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 3 - Teacher", Sample_Type = "Public School"),
  # Tool 5
  rbind(
    check_select_multiple(data = clean_data.tool5$data, tool = kobo_tool.tool5$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool5$Under_Construction_Toilets, tool = kobo_tool.tool5$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool5$Useable_Toilets, tool = kobo_tool.tool5$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool5$Non_Useable_Toilets, tool = kobo_tool.tool5$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool5$Relevant_photos, tool = kobo_tool.tool5$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 3 - Teacher", Sample_Type = "Public School"),
  # Tool 6
  rbind(
    check_select_multiple(data = clean_data.tool6$data, tool = kobo_tool.tool6$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool6$Subjects_Added, tool = kobo_tool.tool6$survey, question_separator = "_")
  ) |>
    mutate(KEY = as.character(KEY)) |>
    left_join(select(clean_data.tool6$data, KEY, Sample_Type), by = "KEY") |>
    mutate(Tab_name = "data", tool = "Tool 6 - Parent"),
  
  # Tool 7
  rbind(
    check_select_multiple(data = clean_data.tool7$data, tool = kobo_tool.tool7$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool7$C6_list_members, tool = kobo_tool.tool7$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool7$Subjects_Added, tool = kobo_tool.tool7$survey, question_separator = "_")
  ) |>
    mutate(KEY = as.character(KEY)) |>
    left_join(select(clean_data.tool7$data, KEY , Sample_Type), by = "KEY") |>
    mutate(Tab_name = "data", tool = "Tool 7 - Shura"),
  
  # Tool 8
  rbind(
    check_select_multiple(data = clean_data.tool8$data, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Classes, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Adults_At_The_CBE, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Section_2_2_3_Attendance_Rec..., tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Section_2_2_4_Headcount, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Students_Enrolment_Book, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Section_2_4_Student_Ages, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Classroom_Materials, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Teacher_Kit, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Student_Kit, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$V_list_of_all_members, tool = kobo_tool.tool8$survey, question_separator = "_"),
    check_select_multiple(data = clean_data.tool8$Subjects_Added, tool = kobo_tool.tool8$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 8 - Class", Sample_Type = "CBE"),
  
  # Tool 9
  rbind(
    check_select_multiple(data = clean_data.tool9$data, tool = kobo_tool.tool9$survey, question_separator = "_")
  ) |>
    mutate(Tab_name = "data", tool = "Tool 9 - IP", Sample_Type = "CBE")
) 

# removing extra elements from the environment
rm(list = c(
  objects(pattern = "^multi_vars_")
))



