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

  # Initial state: Disable the "Results" tab when the app starts
  # This observe runs once on startup.
  observe({
    shinyjs::disable(selector = '#nav li a[data-value="results"]')
  })
  
  observeEvent(input$tax_rate_lowest, {
    if (is.na(input$tax_rate_lowest) || input$tax_rate_lowest < 0 || input$tax_rate_lowest > 100) {
      feedbackWarning("tax_rate_lowest", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_lowest))
      updateNumericInput(session, "tax_rate_lowest", value = corrected_value)
    } else {
      hideFeedback("tax_rate_lowest")
    }
  })
  
  observeEvent(input$tax_rate_second, {
    if (is.na(input$tax_rate_second) || input$tax_rate_second < 0 || input$tax_rate_second > 100) {
      feedbackWarning("tax_rate_second", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_second))
      updateNumericInput(session, "tax_rate_second", value = corrected_value)
    } else {
      hideFeedback("tax_rate_second")
    }
  })
  
  observeEvent(input$tax_rate_middle, {
    if (is.na(input$tax_rate_middle) || input$tax_rate_middle < 0 || input$tax_rate_middle > 100) {
      feedbackWarning("tax_rate_middle", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_middle))
      updateNumericInput(session, "tax_rate_middle", value = corrected_value)
    } else {
      hideFeedback("tax_rate_middle")
    }
  })
  
  observeEvent(input$tax_rate_top, {
    if (is.na(input$tax_rate_top) || input$tax_rate_top < 0 || input$tax_rate_top > 100) {
      feedbackWarning("tax_rate_top", "Value must be between 0 and 100")
      # Optionally auto-correct the value
      corrected_value <- pmax(0, pmin(100, input$tax_rate_top))
      updateNumericInput(session, "tax_rate_top", value = corrected_value)
    } else {
      hideFeedback("tax_rate_top")
    }
  })
  
  
  # Create a reactiveVal to hold simulation output
  simulated_data <- reactiveVal(NULL)
  
  # Create a reactiveVal to hold geo simulation output
  simulated_geo_data <- reactiveVal(NULL)
  
  # Run simulation when button is clicked
  observeEvent(input$simulate_button, {
    # Use user inputs for tax rates
    # tax_rate_lowest <- input$tax_rate_lowest / 100
    # tax_rate_second <- input$tax_rate_second / 100
    # tax_rate_middle <- input$tax_rate_middle / 100
    # tax_rate_top  <- input$tax_rate_top / 100
    
    
    # Agregate poverty estimates
    sim_result <- simulate_tax_policy(
                                      input$tax_rate_lowest,
                                      input$tax_rate_second,
                                      input$tax_rate_middle,
                                      input$tax_rate_top)
    simulated_data(sim_result)
    
    # Agregate geo poverty estimates
    sim_geo_result <- simulate_geo_tax_policy(
      input$tax_rate_lowest,
      input$tax_rate_second,
      input$tax_rate_middle,
      input$tax_rate_top)
    
    simulated_geo_data(sim_geo_result)
    
    Sys.sleep(3)
    
    # After simulation, enable the "Results" tab
    shinyjs::enable(selector = '#nav li a[data-value="results"]')
    
    resetLoadingButton("simulate_button")
    
    # Show a toast notification that simulation is complete
    showToast(
      "info", 
      "Simulation complete! You can now click on the Results tab to see the results.", 
      .options = list(
        positionClass = "toast-bottom-full-width",  # Full width across top of screen
        progressBar = TRUE,  # Enable progress bar
        timeOut = 5000,  # Longer display time for important message
        closeButton = TRUE,
        newestOnTop = TRUE,
        preventDuplicates = FALSE,
        showDuration = 200,  # Slightly longer fade in
        hideDuration = 500,
        extendedTimeOut = 2000,  # Extended time on hover
        showEasing = "swing",  # More noticeable animation
        hideEasing = "linear",
        showMethod = "slideDown",  # Slide down from top
        hideMethod = "slideUp"  # Slide up when hiding
      )
      
    )

  })
  
  # Load the poverty module, passing simulation result
  pov_mod_server("sim_poverty", simulated_data, session)
  
  # Load the poverty module, passing simulation result
  geo_pov_mod_server("sim_geo_poverty", simulated_geo_data, session)
  
}

##END

