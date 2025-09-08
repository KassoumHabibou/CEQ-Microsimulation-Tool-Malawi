# CEQ Microsimulation Tool for Malawi

This repository contains a Shiny web application that implements the **Commitment to Equity (CEQ) Microsimulation Tool** for Malawi, developed by the World Bank's Poverty and Equity Global Practice for the Eastern and Southern Africa region.

## Overview

The CEQ Microsimulation Tool is an interactive dashboard that enables policymakers and researchers to analyze the distributional impact of fiscal policies in Malawi. The tool supports evidence-based policy analysis by simulating how changes in taxes, transfers, and subsidies affect poverty, inequality, and welfare distribution across different population groups.

**Live Application**: [Malawi CEQ Microsimulation Tool](https://1rycfl-ibrahim0kassoum-habibou.shinyapps.io/malawi_ceq_microsimulation_tool/)

## Key Features

### Policy Simulation Capabilities

The tool allows users to simulate reforms across multiple fiscal policy instruments:

## shiny_app
Within this folder are scripts require for functioning of the profiles tool shiny app
=======
#### **Direct Taxes**
- **PAYE Income Tax**: Adjust tax rates across four income brackets
- **Corporate Income Tax**: Modify tax rates by sector (18 economic sectors) with options to remove exemptions for agriculture and electricity companies

#### **Indirect Taxes**
- **Value Added Tax (VAT)**: Set item-level VAT rates for various goods and services
- **Excise Taxes**: Configure excise tax rates for specific products (alcohol, tobacco, fuel, luxury goods)

#### **Government Transfers**
- **Direct Cash Transfers**: Government cash transfers and Farm Input Subsidy Programme (FISP)
- **Near-Cash Transfers**: 8 different programs including:
  - Free maize transfers
  - NFRA food aid
  - MASAF public works
  - Food/cash for work programs
  - Education scholarships (secondary and tertiary)
  - Other near-cash transfers

#### **Subsidies**
- **Electricity Subsidies**: Modify subsidized rates, consumption blocks, and firm allocation
- **Fuel Subsidies**: Adjust national fuel subsidy allocation as percentage of GDP

### Analysis Outputs

The tool generates comprehensive results across five main areas:

1. **Summary Dashboard**: Overview of key distributional impacts
2. **Poverty Indicators**: Headcount rates, poverty gaps, and severity measures
3. **Geospatial Analysis**: Regional and district-level poverty mapping
4. **Inequality Measures**: Gini coefficients, income ratios, and distribution analysis
5. **Fiscal Incidence**: Impact analysis of taxes, transfers, and subsidies by income decile

## Data Sources and Methodology

### Primary Data
- **Malawi Fifth Integrated Household Survey 2019-20** (National Statistical Office)
- Population: ~18.6 million (84% rural)
- National poverty line: 454 MWK per day per capita
- International poverty line: 656.7 MWK (USD 2.15 2017 PPP) per day per capita

### Key Indicators (2019 Baseline)
- **Poverty**: 50.7% national headcount rate, 70.1% extreme poverty rate
- **Inequality**: Gini coefficient of 0.39, 90/10 income ratio of 8.6
- **Fiscal**: Tax-to-GDP ratio of 11.6%, transfers at 4.06% of GDP

### Methodology
The tool follows the [CEQ Methodology](https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124) developed by the CEQ Institute at Tulane University, implementing microsimulation techniques to assess fiscal incidence and distributional impacts.

## Repository Structure

```
├── shiny_app/                    # Main Shiny application
│   ├── global.R                  # Global variables and data loading
│   ├── ui.R                      # User interface definition
│   ├── server.R                  # Server logic and reactivity
│   ├── script/                   # Analysis scripts and modules
│   │   ├── modules/              # Shiny modules for different components
│   │   │   ├── buttons/          # UI button modules
│   │   │   ├── narrative/        # Information and help modules
│   │   │   └── results visualization/ # Results display modules
│   │   ├── 00_main_estimates.R   # Core simulation functions
│   │   ├── 01_pov_estimates.R    # Poverty analysis
│   │   ├── 02_geo_estimates.R    # Geospatial analysis
│   │   ├── 03_ineq_estimates.R   # Inequality analysis
│   │   ├── 04_revmob_estimates.R # Revenue mobilization
│   │   ├── 05_incid_estimates.R  # Incidence analysis
│   │   └── graph_functions.R     # Plotting utilities
│   ├── input/                    # Input data files (not included in repo)
│   ├── output/                   # Generated analysis outputs
│   │   ├── shiny_data/           # Processed data for app
│   │   └── shapefile/            # Geographic boundary files
│   └── www/                      # Static web assets (CSS, images)
├── draft/                        # Development and testing files
└── autre/                        # Additional analysis scripts
```

## Technical Requirements

### R Dependencies
The application requires the following R packages:
- **Shiny ecosystem**: `shiny`, `bslib`, `shinyjs`, `shinyFeedback`, `shinycssloaders`
- **Data manipulation**: `dplyr`, `tidyverse`, `purrr`, `tibble`, `tidyr`
- **Statistical analysis**: `survey`, `wINEQ`, `Hmisc`, `DescTools`
- **Visualization**: `highcharter`, `reactable`, `RColorBrewer`
- **Geospatial**: `sf`, `leaflet`
- **Data I/O**: `haven`, `here`, `labelled`

### Setup Instructions

1. **Clone the repository**:
   ```bash
   git clone https://github.com/KassoumHabibou/CEQ-Microsimulation-Tool-Malawi
   cd CEQ---assessement-tool
   ```

2. **Install dependencies**:
   ```r
   # Using renv (recommended)
   renv::restore()
   
   # Or install packages manually
   install.packages(c("shiny", "bslib", "dplyr", "tidyverse", ...))
   ```

3. **Prepare data files** (requires access permissions):
   - Run data preparation scripts in `draft/script/` to generate required datasets
   - Ensure input data and shapefiles are available in respective folders

4. **Launch the application**:
   ```r
   shiny::runApp("shiny_app")
   ```

## Usage

1. **Home Tab**: Review Malawi's socioeconomic profile and fiscal context
2. **Policy Choice Tab**: Adjust policy parameters across taxes, transfers, and subsidies
3. **Simulate**: Click "Simulate" button after making policy changes
4. **Results Tab**: Analyze impacts across poverty, inequality, and geographic dimensions

## Data Files

The application requires several preprocessed datasets (not included in repository):

### Main Datasets
- `baseline_data.rds`: Household-level microsimulation data
- `baseline_pov_estimates.rds`: Baseline poverty indicators
- `baseline_ineq_estimates.rds`: Baseline inequality measures
- `baseline_geo_pov_estimates.rds`: Geographic poverty data

### Policy Catalogs
- `vat_catalog.rds`: VAT rate structure by item
- `excise_catalog.rds`: Excise tax rates by product
- `dct_catalog.rds`: Direct cash transfer parameters
- Transfer program catalogs for near-cash programs

### Geographic Data
- Regional and district boundary shapefiles for mapping

## About

This tool was developed by the **World Bank's Poverty and Equity Global Practice** to support governments in understanding the distributional impact of fiscal policies and strengthening equity-driven reforms. The Poverty Team works closely with national statistical offices and ministries of finance to promote evidence-based decision-making.

### References
- [CEQ Methodology Guide](https://tulane.app.box.com/s/l72r8kez5b1r38fibghgyb439i6849pm/file/1696511034124)
- [Malawi Fifth Integrated Household Survey 2019-20](https://microdata.worldbank.org/index.php/catalog/3818/related-materials)
- [World Bank Poverty & Equity Brief: Malawi](https://databankfiles.worldbank.org/public/ddpext_download/poverty/987B9C90-CB9F-4D93-AE8C-750588BF00QA/current/Global_POVEQ_MWI.pdf)

## License

This project is developed for policy analysis and research purposes by the World Bank. Please contact the development team for usage permissions and collaboration opportunities.

## Contact

For technical questions or collaboration inquiries, please contact the World Bank's Poverty and Equity Global Practice team.

