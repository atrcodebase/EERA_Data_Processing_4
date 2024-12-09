# School Operationality - Light Tool
tool2.school_operationality_tobe_add <- tobe_added %>% 
  filter(Tool == "Light Tool" & Tab_Name == "School_Operationality") %>% 
  select(KEY = KEY_Unique) %>% 
  mutate(
    PARENT_KEY = gsub("/.*$", "", KEY),
    `SET-OF-School_Operationality` = paste0(PARENT_KEY,"/Passcode_correct-Valid_User-Section_2_Access_to_Education_ECA_1-Section_2_2_1_Shifts_and_Grades_of_School-School_Operationality")
  )


dd <- raw_data.tool2$School_Operationality <- raw_data.tool2$School_Operationality %>% 
  bind_rows(tool2.school_operationality_tobe_add)