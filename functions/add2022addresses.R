# function to deal with lack of (some) addresses in the 2022 apartments file,
# by completing them from the 2021 file where the same project is repeated
# in both files

# note that '2022' in the function name refers to the 2022 file year; the
# addresses that can be completed by this function are projects that were
# completed in 2021 (and so appear in both the 2021 and 2022 files)

add2022addresses <- function(completed.projects.second.filter,
                             intersecting.groups.flagged,
                             intersecting.groups.manual.checked) {
  
  # make dataframe containing flags for all intersecting 2021/2022 groups
  
  # 'intersecting.groups.flags' contains some keep/discard flags and some 'further
  # checking'; 'intersecting.groups.manual.checked' contains the keep/discard
  # results from the further checking, but the addresses in it cannot be used
  # because street numbers have been corrupted by the excel checking process,
  # eg 8-10 changed to 8-Oct)
  
  flag.results <- intersecting.groups.flagged %>%
    st_drop_geometry() %>%
    
    # just keep relevant fields
    dplyr::select(project_id, proj_name, proj_part, street_num, street_name,
                  street_type, add_misc, suburb, lga, region, 
                  group_id, file_year, first.flag = flag) %>%
    
    # add final flags for projects that required further checking
    left_join(., intersecting.groups.manual.checked %>%
                dplyr::select(project_id, second.flag = flag), by = "project_id") %>%
    
    # keep latest flag
    mutate(flag = ifelse(first.flag == "keep" | first.flag == "discard",
                         first.flag,
                         second.flag)) %>%
    dplyr::select(-first.flag, -second.flag)
  
  # keep just the groups where there is a 2021 discard and 2022 keep
  addresses.2021.2022 <- flag.results %>%
    group_by(group_id) %>%
    filter(any(file_year == 2021 & flag == "discard") &
             any(file_year == 2022 & flag == "keep"))
  
  # add 2022 file year addresses to completed projects where available
  output <- completed.projects.second.filter
  
  for (i in 1:nrow(output)) {
    # check whether 2022 file year project with available 2021 file year address
    if (output$file_year[i] == 2022 &
        output$project_id[i] %in% addresses.2021.2022$project_id) {
      
      # get the row containing the replacement addresses
      group_no <- addresses.2021.2022 %>%
        filter(project_id == output$project_id[i]) %>%
        .$group_id
      
      # get the 2021 row from the group
      row.2021 <- addresses.2021.2022 %>%
        filter(group_id == group_no) %>%
        filter(file_year == 2021)
      
      # replace the address details from the 2021 row
      output$proj_name[i] <- row.2021$proj_name
      output$proj_part[i] <- row.2021$proj_part
      output$street_num[i] <- row.2021$street_num
      output$street_name[i] <- row.2021$street_name
      output$street_type[i] <- row.2021$street_type
      output$add_misc[i] <- row.2021$add_misc
      output$suburb[i] <- row.2021$suburb
      output$lga[i] <- row.2021$lga
      output$region[i] <- row.2021$region
      
      # # checking
      # print(paste("Address completed for project_id", output$project_id[i]))
    }
  }
  
  return(output)
  
}
