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



options(encoding = "UTF-8")
# Individual package loading

library(dplyr)
library(tidyverse)
library(purrr)
library(tibble)
library(haven)
library(here)
library(DescTools)
library(labelled)
library(survey)
library(wINEQ)
library(Hmisc)
library(purrr)
library(bslib)
library(shiny)
library(tidyr)
library(shinyFeedback)
library(bsicons)
library(shinyjs)
library(cicerone)
library(highcharter)
library(shinycssloaders)
library(reactable)
library(sf)
library(leaflet)
library(htmlwidgets)
library(shinytoastr)
library(RColorBrewer)
library(rlang)
library(shinyWidgets)

# Set vector size to maximum value
# mem.maxVSize(vsize = Inf)

################################################################################
######################## Importing the datasets ################################
################################################################################
## Set file-paths
data_folder <- "/input"
output_folder <- "/output"

# 2. Sourcing modules functions  ------------------------
list.files(paste0(here(),"/script/modules/results visualization"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

list.files(paste0(here(),"/script/modules/narrative"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

list.files(paste0(here(),"/script/modules/buttons"), full.names = TRUE, recursive = TRUE) %>% 
  map(~ source(.))

source(paste0(here(),"/script/graph_functions.R"))
source(paste0(here(),"/script/helper_UI.R"))
source(paste0(here(),"/script/00_main_estimates.R"))
source(paste0(here(),"/script/01_pov_estimates.R"))
source(paste0(here(),"/script/02_geo_estimates.R"))
source(paste0(here(),"/script/03_ineq_estimates.R"))
source(paste0(here(),"/script/04_revmob_estimates.R"))
source(paste0(here(),"/script/05_incid_estimates.R"))


# 3. Required datafiles ------------------------------------------------------------
bl_df <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_data.rds"))
bl_df_firm <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_data_firm.rds"))
bl_cncpts <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_pov_estimates.rds"))
bl_ineq <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_ineq_estimates.rds"))
bl_geo_cncpts <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_geo_pov_estimates.rds"))
bl_revmob <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_data_revmob.rds"))
bl_itx <- readRDS(paste0(here(),output_folder,"/shiny_data/baseline_data_itx.rds"))

# 4. Some tables -------------------------------------------------------------------
# ---- Taxes catalogs ----
vat_catalog        <- readRDS(paste0(here(), output_folder, "/shiny_data/vat_catalog.rds"))
excise_catalog     <- readRDS(paste0(here(), output_folder, "/shiny_data/excise_catalog.rds"))

# ---- Direct cash transfer catalogs (aggregate) ----
dct_catalog        <- readRDS(paste0(here(), output_folder, "/shiny_data/dct_catalog.rds"))
dct_fips_catalog   <- readRDS(paste0(here(), output_folder, "/shiny_data/dct_fips_catalog.rds"))

# ---- Near-cash transfer catalogs (each variable separately) ----
dtr_frmz_hh_catalog <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_frmz_hh_catalog.rds"))  # Free maize transfer (HH)
dtr_nfra_hh_catalog <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_nfra_hh_catalog.rds"))  # NFRA food aid (HH)
dtr_masaf_hh_catalog <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_masaf_hh_catalog.rds")) # MASAF public works (HH)
dtr_ffwk_hh_catalog <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_ffwk_hh_catalog.rds"))  # Food or cash for work (HH)
dtr_ifwp_hh_catalog <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_ifwp_hh_catalog.rds"))  # Input-for-work programme (HH)
dtr_ses_hh_catalog  <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_ses_hh_catalog.rds"))   # Secondary education scholarship (HH)
dtr_tes_hh_catalog  <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_tes_hh_catalog.rds"))   # Tertiary education scholarship (HH)
dtr_onc_hh_catalog  <- readRDS(paste0(here(), output_folder, "/shiny_data/dtr_onc_hh_catalog.rds"))   # Other near-cash transfers (HH)


######################## Loading the datasets ###############################
# shapefiles (for map) 
mlw_bound_region <- read_sf(paste0(here(),output_folder,"/Shapefile/region_geo.shp"))
mlw_bound_district <- read_sf(paste0(here(),output_folder,"/Shapefile/district_geo.shp"))


# List of possible selected output
# HSC partnership names - also used as the choices for an additional parent area filter 
# when intermediate zone/localities are selected to reduce the number of IZ/localities
pov_parameter_list <- sort(unique(bl_cncpts$Parameter)) 
pov_area_list <- sort(unique(bl_cncpts$Area)) 
ineq_parameter_list <- sort(unique(bl_ineq$Parameter)) 
ineq_area_list <- sort(unique(bl_ineq$Area)) 
pov_geo_area_list <- sort(unique(bl_geo_cncpts$Area)) 
pov_geo_income_list <- sort(unique(bl_geo_cncpts$Income)) 




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
      ".chart-header { font-weight: 700; font-size: 1.2rem;}", # make chart headers bold
      ".chart-controls-icon {background-color:$phs-blue; color:white; border-radius:5em; padding:5px;}" # styling of the chart controls icon
    )
  )








