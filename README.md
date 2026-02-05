# ervissanalyse

An R package to easily retrieve ERVISS (European Respiratory Virus Surveillance Summary) data from the EU-ECDC. Provides functions to fetch, filter, and optionally visualize respiratory virus positivity rates and SARS-CoV-2 variant proportions across European countries.

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

# Retrieve SARS-CoV-2 positivity data
positivity_data <- get_sentineltests_positivity(
 date_min = as.Date("2024-01-01"),
 date_max = as.Date("2024-12-31"),
 pathogen = "SARS-CoV-2",
 countries = c("France", "Germany", "Italy")
)

# Retrieve variant data
variant_data <- get_erviss_variants(
  date_min = as.Date("2024-06-01"),
  date_max = as.Date("2024-12-31"),
  variant = c("XFG", "LP.8.1")
)

# The data is ready for your own analysis
head(positivity_data)
summary(variant_data)
```

## Data Source

The package fetches data directly from the [EU-ECDC Respiratory Viruses Weekly Data](https://github.com/EU-ECDC/Respiratory_viruses_weekly_data) repository.

Two data types are available:
- **Positivity**: Test positivity rates by pathogen and country (from `sentinelTestsDetectionsPositivity.csv`)
- **Variants**: SARS-CoV-2 variant proportions by country (from `variants.csv`)

### Latest Data vs Snapshots

By default, functions fetch the latest available data:

```r
# Latest data (default)
data <- get_sentineltests_positivity(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31")
)
```

For reproducibility, you can use historical snapshots:

```r
# Use a specific snapshot for reproducible analyses
data <- get_sentineltests_positivity(
  date_min = as.Date("2023-01-01"),
  date_max = as.Date("2023-12-31"),
  use_snapshot = TRUE,
  snapshot_date = as.Date("2024-02-23")
)
```

## Main Functions

### Data retrieval

| Function | Description |
|----------|-------------|
| `get_sentineltests_positivity()` | Fetch and filter positivity data |
| `get_erviss_variants()` | Fetch and filter variant data |

### Visualization (optional)

| Function | Description |
|----------|-------------|
| `plot_erviss_positivity()` | Plot positivity data |
| `plot_erviss_variants()` | Plot variant data |
| `quick_plot_erviss_positivity()` | Fetch + plot positivity in one call |
| `quick_plot_erviss_variants()` | Fetch + plot variants in one call |

### URL builders

| Function | Description |
|----------|-------------|
| `get_sentineltests_positivity_url()` | Get URL for positivity data |
| `get_erviss_variants_url()` | Get URL for variant data |

## Examples

### Retrieve and analyze data

```r
# Get positivity data (returns a data.table)
data <- get_sentineltests_positivity(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-06-30"),
  pathogen = c("SARS-CoV-2", "Influenza"),
  countries = c("France", "Spain", "Italy")
)

# Your own analysis with data.table syntax
data[, .(
  mean_positivity = mean(value, na.rm = TRUE),
  max_positivity = max(value, na.rm = TRUE)
), by = .(countryname, pathogen)]
```

### Visualization

```r
# Option 1: Separate steps (more control)
data <- get_sentineltests_positivity(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-06-30"),
  pathogen = "SARS-CoV-2"
)
plot_erviss_positivity(data, date_breaks = "1 month")

# Option 2: Quick one-liner
quick_plot_erviss_positivity(
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31"),
  pathogen = "SARS-CoV-2",
  date_breaks = "1 month"
)
```

### Using a local CSV file

```r
# If you have downloaded the data locally
data <- get_erviss_variants(
  csv_file = "path/to/local/variants.csv",
  date_min = as.Date("2024-01-01"),
  date_max = as.Date("2024-12-31")
)
```

## Dependencies

- data.table
- ggplot2

## License

TBD
