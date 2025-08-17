# setup_renv.R
if (!requireNamespace("renv", quietly = TRUE)) install.packages("renv")

# Initialize renv
renv::init(bare = TRUE)

# Use a stable CRAN mirror (Posit Package Manager is best for shinyapps.io)
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/latest"))

# Comprehensive dependencies vector including all sub-dependencies
all_dependencies <- c(
  
  # Core tidyverse infrastructure (essential for tidyverse to work)
  "ggplot2", "tibble", "stringr", "forcats", "readr", "magrittr", "glue", 
  "cli", "lifecycle", "vctrs", "pillar", "pkgconfig", "withr", "fansi",
  "utf8", "crayon", "ellipsis", "generics", "tidyselect",
  
  # haven dependencies
  "cpp11", "readxl", "cellranger",
  
  # DescTools dependencies  
  "boot", "MASS", "expm", "mvtnorm", "e1071", "class",
  
  # survey package dependencies (critical for your app)
  "Matrix", "survival", "lattice", "minqa", "numDeriv", "mitools", "Rcpp",
  "RcppEigen", "splines", "grid", "methods",
  
  # Hmisc dependencies
  "latticeExtra", "cluster", "rpart", "nnet", "foreign", "gtable", "gridExtra",
  "data.table", "htmlTable", "viridis", "viridisLite", "base64enc", "htmltools",
  
  # bslib dependencies
  "jquerylib", "sass", "cachem", "memoise", "fastmap",
  
  # shiny ecosystem
  "httpuv", "mime", "jsonlite", "xtable", "digest", "R6", "sourcetools",
  "later", "promises", "commonmark", "fontawesome",
  
  # shinyjs dependencies
  "V8",
  
  # highcharter dependencies (JavaScript charting)
  "htmlwidgets", "igraph", "lubridate", "xts", "quantmod", "zoo",
  "TTR", "rlist", "assertthat", "purrr",
  
  # reactable dependencies
  "crosstalk",
  
  # sf (spatial data) dependencies - these can be heavy
  "units", "DBI", "classInt", "proxy", "s2", "wk",
  
  # leaflet dependencies
  "crosstalk", "raster", "sp", "scales", "viridis", "png",
  
  # RColorBrewer (already included but noting it's standalone)
  
  # Base R packages often explicitly needed
  "stats", "graphics", "grDevices", "utils", "datasets", "tools",
  
  # Additional common infrastructure packages
  "knitr", "rmarkdown", "evaluate", "highr", "yaml", "xfun",
  "bslib", "tinytex", "bit", "bit64", "hms", "progress", "prettyunits",
  "backports", "checkmate", "Rcpp", "BH", "plogr"
)


# List of required packages
required_packages <- c(
  "tidyverse", "dplyr", "haven", "here", "DescTools", "labelled",
  "survey", "wINEQ", "Hmisc", "purrr", "bslib", "shiny", "tidyr",
  "shinyFeedback", "bsicons", "shinyjs", "cicerone",
  "highcharter", "shinycssloaders", "reactable", "sf",
  "leaflet", "htmlwidgets", "shinytoastr", "RColorBrewer", "rlang"
)

# Install them in the renv environment
renv::install(all_dependencies)
renv::install(required_packages)


# Snapshot so renv.lock is created
renv::snapshot()

message("renv setup complete. Commit renv.lock and renv/ when deploying.")
