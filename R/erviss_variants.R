#' Get ERVISS variants data
#'
#' Retrieves and filters SARS-CoV-2 variant data from the ERVISS (European Respiratory
#' Virus Surveillance Summary) for a specified date range, variant(s), and country(ies).
#'
#' @param csv_file Path to a local CSV file or URL containing the ERVISS data.
#'   If NULL (default), data is fetched from the official ERVISS repository.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param variant Character vector of variant names to filter.
#'   Use "" (default) to include all variants.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param min_value Minimum value threshold to include in the results (default: 0)
#' @param indicator Type of indicator: "proportion" (default) or "detections"
#' @param use_snapshot Logical. If TRUE, fetches a historical snapshot; if FALSE (default),
#'   fetches the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A data.table containing the filtered variant data with columns:
#'   date, value, variant, countryname, indicator, and other ERVISS fields.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get latest variant data for France
#' data <- get_erviss_variants(
#'   date_min = as.Date("2024-01-01"),
#'   date_max = as.Date("2024-12-31"),
#'   countries = "France"
#' )
#'
#' # Get specific variants with minimum proportion threshold
#' data <- get_erviss_variants(
#'   date_min = as.Date("2024-06-01"),
#'   date_max = as.Date("2024-12-31"),
#'   variant = c("XFG", "LP.8.1"),
#'   min_value = 5
#' )
#' }
get_erviss_variants <- function(
  csv_file = NULL,
  date_min,
  date_max,
  variant = "",
  countries = "",
  min_value = 0,
  indicator = "proportion",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (is.null(csv_file)) {
    csv_file <- get_erviss_variants_url(use_snapshot, snapshot_date)
  }
  assert_file_or_url(csv_file, "csv_file")
  assert_date(date_min, "date_min")
  assert_date(date_max, "date_max")

  match.arg(indicator, c("proportion", "detections"))

  dt <- data.table::fread(csv_file)
  dt[, date := yearweek_to_date(yearweek)]

  if (any(variant != "")) {
    variant_filter <- variant
    dt <- dt[variant %chin% variant_filter]
  }

  if (any(countries != "")) {
    dt <- dt[countryname %chin% countries]
  }

  indicator_filter <- indicator
  dt[date >= date_min & date <= date_max & indicator == indicator_filter & value >= min_value]
}

#' Plot ERVISS variants data
#'
#' Creates a ggplot2 visualization of variant data, with facets by country
#' and colored by variant. The y-axis shows percentage of all variants.
#'
#' @param data A data.table or data.frame containing variant data, typically output from
#'   \code{\link{get_erviss_variants}}. Must contain columns: date, value,
#'   variant, countryname.
#' @param date_breaks A string specifying the date breaks for the x-axis
#'   (e.g., "1 month", "2 weeks")
#' @param date_format A string specifying the date format for x-axis labels
#'   (e.g., `"%b %Y"` for "Jan 2024")
#'
#' @return A ggplot2 object
#'
#' @export
#' @examples
#' \dontrun{
#' data <- get_erviss_variants(
#'   date_min = as.Date("2024-06-01"),
#'   date_max = as.Date("2024-12-31"),
#'   variant = c("XFG", "LP.8.1")
#' )
#' plot_erviss_variants(data, date_breaks = "1 month")
#' }
plot_erviss_variants <- function(
  data,
  date_breaks = "1 month",
  date_format = "%b %Y"
) {
  ggplot(data, aes(x = date, y = value, color = variant)) +
    geom_line() +
    xlab("") +
    ylab("% of all variants") +
    facet_wrap(~countryname, scales = "free_x", ncol = 3) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_format) +
    scale_colour_viridis_d(name = "Variant") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme_erviss() +
    theme(panel.grid = element_blank())
}

#' Quick plot of ERVISS variants data
#'
#' Convenience function that fetches and plots ERVISS variant data in one step.
#' For more control, use \code{\link{get_erviss_variants}} followed by
#' \code{\link{plot_erviss_variants}}.
#'
#' @inheritParams get_erviss_variants
#' @inheritParams plot_erviss_variants
#'
#' @return A ggplot2 object showing variant proportions over time by country
#'
#' @import ggplot2
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' # Quick visualization of latest variant data
#' quick_plot_erviss_variants(
#'   date_min = as.Date("2024-06-01"),
#'   date_max = as.Date("2024-12-31"),
#'   variant = c("XFG", "LP.8.1"),
#'   date_breaks = "1 month"
#' )
#' }
quick_plot_erviss_variants <- function(
  csv_file = NULL,
  date_min,
  date_max,
  variant = "",
  countries = "",
  min_value = 0,
  indicator = "proportion",
  date_breaks = "1 month",
  date_format = "%b %Y",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  data <- get_erviss_variants(
    csv_file,
    date_min,
    date_max,
    variant,
    countries,
    min_value,
    indicator,
    use_snapshot,
    snapshot_date
  )

  plot_erviss_variants(data, date_breaks, date_format)
}
