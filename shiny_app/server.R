###############################################.
#
# App main server script
#
##############################################.



function(input, output, session) {
  
  # Keeps the shiny app from timing out quickly on Posit 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })
  
  

  
  
  
  ###########################################################################################.
  # CONDITIONAL UI FOR THE HEADER OF THE PROFILES TAB ----
  ###########################################################################################.
  
  
  # these 2 bits of dynamic text form the part of the two headers at the top
  # of the profiles tab that state the selected profile and the selected geography
  
  
  # running modules for each sub-tab
  #poverty_mod("trends", profile_data, geo_selections, selected_profile, session)
  # rank_mod_server("rank", areatype_data, geo_selections, selected_profile, session)
  # summary_table_server("summary", geo_selections, selected_profile, areatype_data)
  # simd_navpanel_server("simd", simd_data, geo_selections, selected_profile, session)
  # pop_groups_server("pop_groups",popgroup_data, geo_selections, selected_profile, session)

 
  
  
} # close main server function

##END

