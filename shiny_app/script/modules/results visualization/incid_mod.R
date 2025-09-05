###########################################################################.
# MODULE: inci_mod ---- 
# prepares the nav_panel layout displaying trends data
###########################################################################.


#######################################################.
## MODULE UI
#######################################################.

incid_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    bslib::navset_pill_list(
      id = ns("sub_tabs_incidence"),
      widths = c(2, 10),
      bslib::nav_panel(
        title = "Direct taxes", value = "incid_dtx",
        incid_dtx_mod_ui(ns("incid_dtx"))
      ),
      bslib::nav_panel(
        title = "Indirect taxes", value = "incid_itx",
        incid_itx_mod_ui(ns("incid_itx"))
      ),
      bslib::nav_panel(
        title = "Transfers", value = "incid_dtr",
        incid_dtr_mod_ui(ns("incid_dtr"))
      ),
      bslib::nav_panel(
        title = "Subsidies", value = "incid_sub",
        incid_sub_mod_ui(ns("incid_sub"))
      )
    ),
    hr(),
    fluidRow(
      column(
        width = 3,
        tags$img(
          src = "WorldBank_Logo_optimized.png",
          height = "100px",
          style = "display: block; margin-left: auto; margin-right: auto;"
        )
      ),
      column(
        width = 9,
        div(
          style = "font-size: 1em; padding-top: 10px; text-align: left;",
          p(strong("About this tool:")),
          p("This CEQ microsimulation tool was developed by the Poverty and Equity Global Practice of the World Bank on the Eastern and Southern Africa region. It supports policy analysis to help governments understand the distributional impact of fiscal policies and strengthen equity-driven reforms."),
          p("The Poverty Team works closely with national statistical offices and ministries of finance to promote evidence-based decision-making.")
        )
      )
    )
  )
}

incid_mod_server <- function(id, simulated_df, root_session) {
  moduleServer(id, function(input, output, session) {
    # namespace helper
    ns <- session$ns
   
    # mount the direct taxes submodule 
    incid_dtx_mod_server(
      id = "incid_dtx",
      simulated_df = simulated_df,
      root_session = session
    )
    
    # mount the indirect taxes submodule 
    incid_itx_mod_server(
      id = "incid_itx",
      simulated_df = simulated_df,
      root_session = session
    )
    
    # mount the direct transfert submodule 
    incid_dtr_mod_server(
      id = "incid_dtr",
      simulated_df = simulated_df,
      root_session = session
    )
    
    # mount the subsidies submodule 
    incid_sub_mod_server(
      id = "incid_sub",
      simulated_df = simulated_df,
      root_session = session
    )
    # (later: mount other sub-tabs here when you add them)
  })
}
