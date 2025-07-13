###############################################.
#
# App main server script
#
##############################################.



server <- function(input, output, session) {

    # Keeps the shiny app from timing out quickly on Posit 
  autoInvalidate <- reactiveTimer(10000)
  observe({
    autoInvalidate()
    cat(".")
  })

  # Create a reactiveVal to hold simulation output
  simulated_data <- reactiveVal(NULL)
  
  # Run simulation when button is clicked
  observeEvent(input$simulate_button, {
    # Use user inputs for tax rates
    # tax_rate_lowest <- input$tax_rate_lowest / 100
    # tax_rate_second <- input$tax_rate_second / 100
    # tax_rate_middle <- input$tax_rate_middle / 100
    # tax_rate_top  <- input$tax_rate_top / 100
    Sys.sleep(6)
    
    sim_result <- simulate_tax_policy(bl_df, bl_cncpts,
                                      input$tax_rate_lowest / 100,
                                      input$tax_rate_second / 100,
                                      input$tax_rate_middle / 100,
                                      input$tax_rate_top / 100)
    simulated_data(sim_result)
    resetLoadingButton("simulate_button")
    #showToast("success", "Simulation complete!")

  })
  
  # Load the poverty module, passing simulation result
  pov_mod_server("sim_poverty", simulated_data = simulated_data, root_session = session)
  
}

##END

