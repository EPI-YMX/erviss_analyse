#' Get ERVISS positivity data
#'
#' Retrieves and filters positivity data from the ERVISS (European Respiratory
#' Virus Surveillance Summary) for a specified date range, pathogen(s), and country(ies).
#'
#' @param csv_file Path to a local CSV file or URL containing the ERVISS data.
#'   If NULL (default), data is fetched from the official ERVISS repository.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param pathogen Character vector of pathogen names to filter.
#'   Use "" (default) to include all pathogens.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param use_snapshot Logical. If TRUE, fetches a historical snapshot; if FALSE (default),
#'   fetches the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A data.table containing the filtered positivity data with columns:
#'   date, value, pathogen, countryname, and other ERVISS fields.
#'
#' @export
#' @examples
#' \dontrun{
#' # Get latest SARS-CoV-2 positivity data for France
#' data <- get_sentineltests_positivity(
#'   date_min = as.Date("2024-01-01"),
#'   date_max = as.Date("2024-12-31"),
#'   pathogen = "SARS-CoV-2",
#'   countries = "France"
#' )
#'
#' # Get historical data from a specific snapshot
#' data <- get_sentineltests_positivity(
#'   date_min = as.Date("2023-01-01"),
#'   date_max = as.Date("2023-12-31"),
#'   use_snapshot = TRUE,
#'   snapshot_date = as.Date("2024-02-23")
#' )
#' }
get_sentineltests_positivity <- function(
  csv_file = NULL,
  date_min,
  date_max,
  pathogen = "",
  countries = "",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (is.null(csv_file)) {
    csv_file <- get_sentineltests_positivity_url(use_snapshot, snapshot_date)
  }
  assert_file_or_url(csv_file, "csv_file")
  assert_date(date_min, "date_min")
  assert_date(date_max, "date_max")

  dt <- data.table::fread(csv_file)
  dt[, date := yearweek_to_date(yearweek)]

  if (any(pathogen != "")) {
    pathogen_filter <- pathogen
    dt <- dt[pathogen %chin% pathogen_filter]
  }

  if (any(countries != "")) {
    dt <- dt[countryname %chin% countries]
  }

  dt[date >= date_min & date <= date_max & indicator == "positivity"]
}

#' Plot ERVISS positivity data
#'
#' Creates a ggplot2 visualization of positivity data, with facets by country
#' and colored by pathogen. The plot title displays mean, min and max positivity values.
#'
#' @param data A data.table or data.frame containing positivity data, typically output from
#'   \code{\link{get_sentineltests_positivity}}. Must contain columns: date, value,
#'   pathogen, countryname.
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
#' data <- get_sentineltests_positivity(
#'   date_min = as.Date("2024-01-01"),
#'   date_max = as.Date("2024-06-30"),
#'   pathogen = "SARS-CoV-2"
#' )
#' plot_erviss_positivity(data, date_breaks = "1 month")
#' }
plot_erviss_positivity <- function(
  data,
  date_breaks = "2 weeks",
  date_format = "%b %Y"
) {
  mean_positivity <- mean(data$value)
  min_positivity <- min(data$value)
  max_positivity <- max(data$value)

  ggplot(data, aes(x = date, y = value, color = pathogen)) +
    geom_line() +
    xlab("") +
    ylab("Positivity") +
    facet_wrap(~countryname, scales = "free_x", ncol = 3) +
    scale_x_date(date_breaks = date_breaks, date_labels = date_format) +
    scale_colour_viridis_d(name = "Pathogen") +
    theme_erviss() +
    labs(
      title = sprintf(
        "Mean positivity: %.2f (Min %.2f - Max %.2f)",
        mean_positivity,
        min_positivity,
        max_positivity
      )
    )
}

#' Quick plot of ERVISS positivity data
#'
#' Convenience function that fetches and plots ERVISS positivity data in one step.
#' For more control, use \code{\link{get_sentineltests_positivity}} followed by
#' \code{\link{plot_erviss_positivity}}.
#'
#' @inheritParams get_sentineltests_positivity
#' @inheritParams plot_erviss_positivity
#'
#' @return A ggplot2 object showing positivity over time by country and pathogen
#'
#' @import ggplot2
#' @import data.table
#' @export
#' @examples
#' \dontrun{
#' # Quick visualization of latest data
#' quick_plot_erviss_positivity(
#'   date_min = as.Date("2024-01-01"),
#'   date_max = as.Date("2024-12-31"),
#'   pathogen = "SARS-CoV-2",
#'   date_breaks = "1 month"
#' )
#' }
quick_plot_erviss_positivity <- function(
  csv_file = NULL,
  date_min,
  date_max,
  pathogen = "",
  countries = "",
  date_breaks = "2 weeks",
  date_format = "%b %Y",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  data <- get_sentineltests_positivity(
    csv_file,
    date_min,
    date_max,
    pathogen,
    countries,
    use_snapshot,
    snapshot_date
  )

  plot_erviss_positivity(data, date_breaks, date_format)
}
