###############################################################################.
#
# Global script ---- 
#
###############################################################################.


################################################################################
######### CEQ initial data wrangling file for MALAWI ###########################
################################################################################

# Data wrangling

######################## Importing library and external files ##################
### List of required packages
required_packages <- c("tidyverse", "dplyr","haven","here","DescTools","labelled",
                       "survey","wINEQ","Hmisc","purrr","bslib","shiny","tidyr",
                       "shinyFeedback","phsstyles","bsicons","shinyjs","cicerone")

### Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages
lapply(setdiff(required_packages,"plyr"), library, character.only = TRUE)

# Remove all objects
rm(list = ls())

# Set vector size to maximum value
mem.maxVSize(vsize = Inf)

################################################################################
######################## Importing the datasets ################################
################################################################################
## Set file-paths
data_folder <- "/input/Data"
output_folder <- "/output"
script_path <- paste0(data_folder, "/Script")

# 2. Sourcing modules, narrative text functions  ------------------------
list.files(paste0(here(),script_path, "/modules"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

list.files(paste0(here(),script_path, "/narrative"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))


# 3. Required datafiles ------------------------------------------------------------
bl_df <- readRDS(paste0(here(),output_folder,"/Shiny Data/before_data_wtht_na.rds"))
bl_cncpts <- readRDS(paste0(here(),output_folder,"/Shiny Data/Baseline_data_tab.rds"))


# shapefiles (for map) 
#ca_bound <- readRDS("data/CA_boundary.rds") # Council area



#module UI function ----
#name is mandatory but icon and class are optional
navigation_button_modUI <- function(button_id, button_name, button_icon=NULL, class = NULL){
  ns <- shiny::NS(button_id)
  tagList(
    shiny::actionButton(inputId= ns("button_nav"), 
                        label = button_name, 
                        class = class,
                        icon = button_icon))}


#module server function ----

navigation_button_modSERVER <- function(id, nav_id, parent_session) {
  shiny::moduleServer(id, function(input, output, session) {
    
    shiny::observeEvent(input$button_nav, {
      
      # navigate to the tab
      bslib::nav_select(id="nav",
                        selected = nav_id,
                        session = parent_session)
      
    }) 
  }) #close moduleServer
} #close server




# module ui function
# icon and description of the profile are optional arguments



# 5. Dashboard theme ---------------------------------------------------------------

# see https://rstudio.github.io/bslib/articles/bs5-variables/ for more details
phs_theme <- bs_theme(
  # high level theming
  version = 5, # bootstrap v5 required to use bslib components (like cards etc.)
  bg = "white", # make background white
  fg = "#222", # make foreground darkgrey/black
  "progress-bar-bg" = "0078D4",
  bootswatch = "shiny", # use default shiny theme
  primary = "#006D77", # make primary colour green - this will change i.e. active pill colour
  "form-label-font-weight" = "550"#, # font-weight for filter labels
) %>% 
  # create colour variables to use below
  bs_add_variables(
    "phs-gray" = "#F2F2F2",
    "phs-blue" = "#0078D4",
    "phs-teal" = "#006D77"
  ) %>% 
  # lower level theming
  bs_add_rules(
    list(
      ".info-box-header { background-color: $phs-teal; color: #FFF;}", # info box header lighter phs purple colour with white text
      ".methodology-table td{ border:thin solid black; padding:3px;}", # for indicator def tab - make nested table cells have black border
      ".rt-tr-details {padding: 0px; box-shadow: inset 0 1px 3px #dbdbdb; background: #FDFDFC;}", # for indicator definitions tab - make expandable panel grey
      ".chart-controls-icon {background-color:$phs-blue; color:white; border-radius:5em; padding:5px;}" # styling of the chart controls icon
    )
  )








