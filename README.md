# NHS Oversight Framework

This Git repository contains the project used to automate the processing of [NHS Oversight Framework](https://www.england.nhs.uk/nhs-oversight-framework/) metrics.

The project was developed using the existing [Outcomes Framework](https://github.com/BBCS-PHI/2_BSOL_Outcomes_Framework) work as a foundation and adapted to meet the Oversight Framework's specific data structures, calculation methods, processing requirements and outputs.

The project is metadata-driven, using central reference metadata to control the downstream processing and calculation logic for each metric.

## metricengineR

This project relies on the `metricengineR` [R package](https://github.com/BBCS-PHI/metricengineR) to provide reusable functionality shared across metric-processing projects, including:

- metric calculations
- data extraction
- data quality checks
- data processing and transformation
- reusable helper functions

The package can be installed from GitHub using:

```r
remotes::install_github(
  "BBCS-PHI/metricengineR",
  upgrade = "never"
)
```

## Purpose
The project provides a consistent process for:
* importing metric data
* preparing and standardising source data
* applying the required metric calculations using `metricengineR`
* producing standardised outputs
* carrying out data quality checks
* preparing results for reporting and downstream analysis

## Supported calculations
Through `metricengineR`, the project can process a range of metric types, including:
* counts
* percentages
* proportions
* directly age-standardised rates
* slope index of inequality (SII)
* crude rates
* ratios
* percentage changes
* percentage point differences
  
## Running the project
1. Clone the repository
2. Open the R project in RStudio
3. Install `metricengineR` and the required project dependencies
4. Update the required input paths and configuration
5. Run the main processing script
6. Review the generated outputs and DQ checks
   
This repository is dual licensed under the [Open Government v3]([https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/) & MIT. All code and outputs are subject to Crown Copyright.
