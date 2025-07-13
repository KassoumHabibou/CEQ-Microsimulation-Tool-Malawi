###############################################################################.
# MODULE: download_data_btns_mod ---- 
# creates a menu button that when clicked on, provides 3 different options for downloading data (csv, rds or json)
# it then downloads the data in the format the user has selected
# there is also an optional arguments in the server function to specify which columns of data to include in the download
# by default all columns are included unless you use pass a vector of column names to the selectedColumns argument
# note this module can be nested inside other modules where download buttons are required.


# UI function ----
# id = unique id 
# size = button size (can be 'sm', 'md' or 'lg')
download_data_btns_ui <- function(id, size = "sm") {
  ns <- NS(id) # namespace
  shinyWidgets::dropdownButton(
    label = "Download data", icon = icon("download"), circle = FALSE, status = 'download', size = size,
    shiny::downloadLink(ns("downloadCSV"), label = "as CSV"),
    shiny::downloadLink(ns("downloadRDS"), label = "as RDS")
  )
}



# Server function ----
# id = unique id 
# data = name of data to download
# selectedColumns = vector of column names to include in data download (if required)
# file_name = name of downloaded file

download_data_btns_server <- function(id, data, file_name) {
  moduleServer(id, function(input, output, session) {
    
    dataset <- reactive({
      
      # Convert data.table to data.frame
      if ("data.table" %in% class(data())) {
        data <- as.data.frame(data())
      }
      
    })
    
    # download as csv
    output$downloadCSV <- downloadHandler(
      filename = paste0(file_name, "_", Sys.Date(), ".csv"),
      content = function(file) {
        write.csv(as.data.frame(dataset()), paste0(file_name, "_Malawi_", Sys.Date()), row.names = FALSE)
      }
    )
    
    # download as rds
    output$downloadRDS <- downloadHandler(
      filename = paste0(file_name, "_Malawi_", Sys.Date(), ".rds"),
      content = function(file) {
        saveRDS(dataset(), file)
      }
    )
    
  })
}

