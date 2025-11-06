```markdown
# 💥 CRIS Data Analysis Pipeline (R) 🚦

## Overview

This repository contains a robust, modular **Data Extraction, Transformation, and Loading (ETL)** pipeline written entirely in **R**. Its purpose is to efficiently process raw Texas Crash Records Information System (**CRIS**) data, perform extensive feature engineering, enrich the data with geographic attributes, and generate analytical-ready outputs for crash safety analysis.

The pipeline is designed for high performance, utilizing modern R packages like `data.table` for fast I/O and `tidyverse` for clean data manipulation.

### 🌟 Key Features

* **Modular and Orchestrated:** The entire process is broken down into numbered, single-purpose R scripts (`01_initial_setup.R` to `08_export.R`) and executed sequentially via a single `run_pipeline.R` file.
* **High Performance I/O:** Uses **`data.table::fread`** for rapid ingestion of large CRIS CSV files.
* **Intelligent Lookups:** Employs **`memoise`** caching to read the master lookup file only once, translating thousands of numeric CRIS codes (e.g., `INJ_SEV_ID`, `LIGHT_COND_ID`) into human-readable descriptions quickly and efficiently.
* **Geographic Enrichment:** Automatically queries and downloads spatial data for **TxDOT Districts** and **County Boundaries** from public ArcGIS REST services, joining them to the crash records.
* **Advanced Feature Engineering:**
    * Creates essential binary flags for analysis (e.g., **Intoxication**, **Fatal Crash**, **Vulnerable Road User** status).
    * Aggregates multi-column **Contributing Factors** (`Contrib_Factr_1_ID`, etc.) to the unit and crash levels.
* **Flexible Outputs:** Generates a comprehensive, multi-sheet **Excel workbook** and supports spatial outputs in modern GIS formats (**GeoPackage** or **File Geodatabase**).

***

## ⚙️ Setup and Prerequisites

### 1. Requirements

* **R** (version 4.0 or higher recommended)
* **RStudio** (recommended IDE)
* **Project Structure:** The repository structure must be maintained, with all main scripts residing in the `R/` sub-directory.

```

.
├── R/
│   ├── 01\_initial\_setup.R
│   ├── 02\_lookup.R
│   ├── ...
│   └── 08\_export.R
└── run\_pipeline.R

````

### 2. R Package Installation

The pipeline uses the `pacman` package to manage and load all other dependencies. Run the following R code once to ensure all required packages are installed:

```r
if (!requireNamespace('pacman', quietly = TRUE)) install.packages('pacman', repos='[https://cloud.r-project.org](https://cloud.r-project.org)')
pacman::p_load('memoise', 'data.table', 'tidyverse', 'magrittr', 'sf', 'writexl', 'httr')
````

### 3\. Input Data Structure

The pipeline expects raw CRIS data to be present in a directory structure defined in the configuration step below. The scripts automatically locate the required files based on their naming patterns (e.g., `*!!!crash_*.csv`).

| CRIS File Name Pattern | Purpose |
| :--- | :--- |
| `*!!!crash_*.csv` | Primary crash-level details |
| `*!!!person_*.csv` | General person details |
| `*!!!primaryperson_*.csv` | Driver/primary person details (merged with `person`) |
| `*!!!unit_*.csv` | Vehicle/Unit-level details |
| `*!!!lookup_*.csv` | Master lookup table for coded values |
| `CONTRIB_FACTR_ID.csv` | Lookup for Contributing Factors (CF/PCF) |

-----

## 🚀 Running the Pipeline

The entire ETL process is executed by the **`run_pipeline.R`** script.

### 1\. Configuration

Before running, you **must** edit the `CONFIG` list at the top of the `run_pipeline.R` file to set your local paths and processing parameters.

**Note:** It is strongly recommended to replace the hardcoded network paths (e.g., `J:/`, `C:/Temp`) with **relative paths** or environment variables for true portability across different systems.

```r
# CONFIG — Edit these values and run the script
CONFIG <- list(
  # Base directory where CRIS data is generally stored
  base_input_dir = "/path/to/your/cris/data/root", 
  # Sub-folder containing the specific batch of CSVs you want to process
  data_subdir    = "03_Extracted_files_Concatenated_YYYY-MM-DD", 
  # Output directory where results will be saved
  out_dir        = "output/pipeline_results",
  # Set a max number of crashes to process for testing. Use NA to process all.
  max_crashes    = NA, 
  # OUTPUT OPTIONS
  write_excel    = TRUE,
  write_gdb      = FALSE, # Requires specific dependencies for some systems
  write_gpkg     = TRUE,  # Recommended spatial output format
  # Optional tag for output file naming
  extra_name_tag = "MPO_Region" 
)
```

### 2\. Execution

Once configured, run the orchestrator script in your R console or RStudio:

```r
source("run_pipeline.R")
```

-----

## 🗺️ Pipeline Workflow (Execution Order)

The scripts are executed in a fixed, numbered sequence to ensure data dependencies are met.

| Step | Script | Description | Dependencies & Output |
| :--- | :--- | :--- | :--- |
| **01** | `01_initial_setup.R` | Initializes the environment, loads required packages, and resolves the exact file paths for all input CSV files. | *Output:* `paths` list (file paths) |
| **02** | `02_lookup.R` | Defines memoized helper functions (`read_lookup`, `make_lookup_tb`) for efficient, cached reading of the master lookup table. | *Output:* Caching of `lookup_df` |
| **03** | `03_geo.R` | Fetches Texas **County** and **TxDOT District** boundaries from external public ArcGIS services. | *Output:* `geo` list (`cnty_data`, `dist_data`) |
| **04** | `04_crash_import.R` | Imports the `crash` file. Performs date/time parsing, constructs highway identifiers, and joins with geographic boundary data. | *Input:* `crash.csv`, `geo` |
| **05** | `05_persons_import.R` | Imports and merges the `person` and `primary_person` files. Calculates injury severity flags, intoxication status, and identifies vulnerable road users. | *Input:* `person.csv`, `primaryperson.csv` |
| **06** | `06_units_import.R` | Imports the `unit` file. The primary feature engineering step: processes multi-column contributing factors (CF/PCF) and joins person-level summaries (e.g., number of motorcyclists) back to the unit level. | *Input:* `unit.csv`, `all_persons` |
| **07** | `07_summarize.R` | Aggregates unit and person data to create final **crash-level flags**. Generates detailed summary tables for contributing factor analysis by year. | *Input:* `crashes`, `units`, `all_persons` |
| **08** | `08_export.R` | Saves all resulting data frames and summary tables to the specified output formats. | *Input:* All final data frames |

-----

## 📊 Output Files

All output files are saved to the directory specified in `CONFIG$out_dir` and are named using a convention like `Crash_Data_[tag]_[date]`.

### 1\. Tabular Output (`.xlsx`)

A single, multi-sheet **Excel workbook** containing the following analytical tables:

  * **`crashes`**: The final, enriched crash-level dataset with geographic and summary flags.
  * **`units`**: The enriched unit-level dataset with driver action and full contributing factor information.
  * **`all_persons`**: The merged and cleaned person-level dataset with injury, age, and vulnerability flags.
  * **`totals_by_contrib_fact_per_year`**: Annual summary of crash counts, fatalities, and serious injuries for every contributing factor.
  * **`contrib_factors_by_crash`**: A wide-format table of crash IDs with binary flags for aggregated contributory factor groups (e.g., `Speeding_CF_or_PCF`).
  * **`lookup_df`**: The master lookup table used for reference.

### 2\. Spatial Output (`.gpkg` or `.gdb`)

If `write_gpkg` or `write_gdb` is set to `TRUE`, the following point layers (WGS 84 / EPSG: 4326) are created:

  * **`all_crashes`**
  * **`pedestrian_crashes`**
  * **`pedalcyclist_crashes`**
  * **`pedestrian_and_pedalcyclist_crashes`**

<!-- end list -->

```
```