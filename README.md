# ervissanalyse

An R package to analyze ERVISS (European Respiratory Virus Surveillance Summary) data, providing tools for visualizing COVID-19 positivity rates and variant proportions across European countries.

## Installation

```r
# Install from local source
devtools::install_local("path/to/ervissanalyse")

# Or load for development
devtools::load_all()
```
## Quick Start

```r
library(ervissanalyse)

# Plot positivity rates using the latest data
show_positivity_for_a_given_period(
 date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31"),
  pathogen_to_study = "SARS-CoV-2",
  countries = c("France", "Germany", "Italy")
)

# Plot variant proportions
show_variants_for_a_given_period(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31"),
  variant_to_study = c("XFG", "LP.8.1"),
  date_breaks = "1 month"
)
```

## Data Sources

The package fetches data directly from the [EU-ECDC Respiratory Viruses Weekly Data](https://github.com/EU-ECDC/Respiratory_viruses_weekly_data) repository.

Two data types are available:
- **Positivity**: Test positivity rates by pathogen and country
- **Variants**: SARS-CoV-2 variant proportions by country

### Using Latest Data vs Snapshots

By default, functions use the latest available data:

```r
# Latest data (default)
show_positivity_for_a_given_period(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31")
)
```

For reproducibility, you can use historical snapshots:

```r
# Use a specific snapshot
show_positivity_for_a_given_period(
  date_min = as.Date("2023-01-01"),
  date_max = as.Date("2023-12-31"),
  use_snapshot = TRUE,
  snapshot_date = as.Date("2023-11-24")
)
```

## Main Functions

### High-level functions (clean + plot)

| Function | Description |
|----------|-------------|
| `show_positivity_for_a_given_period()` | Display positivity rates plot |
| `show_variants_for_a_given_period()` | Display variant proportions plot |

### Data cleaning functions

| Function | Description |
|----------|-------------|
| `clean_erviss_positivity_for_a_given_period()` | Clean and filter positivity data |
| `clean_erviss_variants_for_a_given_period()` | Clean and filter variant data |

### Plotting functions

| Function | Description |
|----------|-------------|
| `plot_erviss_positivity_for_a_given_period()` | Plot cleaned positivity data |
| `plot_erviss_variants_for_a_given_period()` | Plot cleaned variant data |

### URL builders

| Function | Description |
|----------|-------------|
| `get_erviss_positivity_url()` | Get URL for positivity data |
| `get_erviss_variants_url()` | Get URL for variant data |

## Examples

### Custom workflow (clean then plot)

```r
# Step 1: Clean the data
data <- clean_erviss_positivity_for_a_given_period(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-06-30"),
  pathogen_to_study = c("SARS-CoV-2", "Influenza"),
  countries = c("France", "Spain", "Italy")
)

# Step 2: Inspect or modify the data
head(data)
nrow(data)

# Step 3: Plot
plot_erviss_positivity_for_a_given_period(data)
```

### Using a local CSV file

```r
show_variants_for_a_given_period(
  csv_file = "path/to/local/variants.csv",
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31"),
  date_breaks = "2 weeks"
)
```

## Dependencies
- dplyr
- ggplot2
- readr

## License

TBD
