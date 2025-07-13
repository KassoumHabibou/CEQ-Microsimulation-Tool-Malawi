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
                       "shinyFeedback","phsstyles","bsicons","shinyjs","cicerone",
                       "shinyjs","highcharter","shinycssloaders","reactable")

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

# 2. Sourcing modules functions  ------------------------
list.files(paste0(here(),"/script/modules/results visualization"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

list.files(paste0(here(),"/script/modules/buttons"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

source(paste0(here(),"/script/graph_functions.R"))
source(paste0(here(),"/script/01_simulated function.R"))

# 3. Required datafiles ------------------------------------------------------------
bl_df <- readRDS(paste0(here(),output_folder,"/Shiny Data/before_data_wtht_na.rds"))
bl_cncpts <- readRDS(paste0(here(),output_folder,"/Shiny Data/Baseline_data_tab.rds"))


# shapefiles (for map) 
#ca_bound <- readRDS("data/CA_boundary.rds") # Council area



# List of possible selected output
# HSC partnership names - also used as the choices for an additional parent area filter 
# when intermediate zone/localities are selected to reduce the number of IZ/localities
pov_parameter_list <- sort(unique(bl_cncpts$Parameter)) 
pov_area_list <- sort(unique(bl_cncpts$Area)) 




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








