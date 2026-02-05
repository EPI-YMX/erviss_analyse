#' Clean ERVISS positivity data for a given period
#'
#' Filters and cleans positivity data from an ERVISS CSV file for a specified
#' date range, pathogen(s), and study site(s).
#'
#' @param csv_file Path to the CSV file or URL containing the ERVISS data.
#'   If NULL (default), the URL is built automatically using use_snapshot and snapshot_date.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param pathogen_to_study Character vector of pathogen names to filter.
#'   Use "" (default) to include all pathogens.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param use_snapshot Logical. If TRUE, uses a snapshot URL; if FALSE (default),
#'   uses the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A data frame containing the filtered positivity data with columns:
#'   date, value, pathogen, countryname, and other ERVISS fields.
#'
#' @export
clean_erviss_positivity_for_a_given_period <- function(
  csv_file = NULL,
  date_min,
  date_max,
  pathogen_to_study = "",
  countries = "",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (is.null(csv_file)) {
    csv_file <- get_erviss_positivity_url(use_snapshot, snapshot_date)
  }
  assert_file_or_url(csv_file, "csv_file")
  assert_date(date_min, "date_min")
  assert_date(date_max, "date_max")

  csv_variants <- readr::read_csv(
    csv_file
  ) %>%
    dplyr::mutate(
      date = yearweek_to_date(yearweek)
    )

  if (any(pathogen_to_study != "")) {
    csv_variants <- csv_variants %>%
      dplyr::filter(
        pathogen %in% pathogen_to_study
      )
  }

  if (any(countries != "")) {
    csv_variants <- csv_variants %>%
      dplyr::filter(
        countryname %in% countries
      )
  }

  csv_variants_filtered <- csv_variants %>%
    dplyr::filter(
      date >= date_min & date <= date_max
    ) %>%
    dplyr::filter(
      indicator == "positivity"
    )

  return(csv_variants_filtered)
}

#' Plot ERVISS positivity data
#'
#' Creates a ggplot2 visualization of positivity data, with facets by country
#' and colored by pathogen. The plot title displays mean, min and max positivity values.
#'
#' @param csv_variants_filtered A data frame containing cleaned positivity data,
#'   typically output from \code{\link{clean_erviss_positivity_for_a_given_period}}.
#'   Must contain columns: date, value, pathogen, countryname.
#'
#' @return A ggplot2 object
#'
#' @export
plot_erviss_positivity_for_a_given_period <- function(csv_variants_filtered) {
  mean_positivity <- mean(csv_variants_filtered$value)
  min_positivity <- min(csv_variants_filtered$value)
  max_positivity <- max(csv_variants_filtered$value)
  ggplot(csv_variants_filtered, aes(x = date, y = value, color = pathogen)) +
    geom_line() +
    xlab("") +
    ylab("Positivity") +
    facet_wrap(~countryname, scales = "free_x", ncol = 3) +
    theme_minimal() +
    scale_x_date(date_breaks = "2 weeks") +
    scale_colour_viridis_d(name = "Pathogen") +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
      strip.placement = "outside",
      legend.position = "bottom",
      panel.spacing = unit(1, "cm"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      strip.text = element_text(size = 14)
    ) +
    labs(
      title = paste0(
        "Mean positivity: ",
        mean_positivity,
        " (",
        min_positivity,
        " - ",
        max_positivity,
        ")"
      )
    )
}

#' Show positivity for a given period
#'
#' Cleans and plots ERVISS positivity data for a specified period.
#' This is a convenience function that combines
#' \code{\link{clean_erviss_positivity_for_a_given_period}} and
#' \code{\link{plot_erviss_positivity_for_a_given_period}}.
#'
#' @param csv_file Path to the CSV file or URL containing the ERVISS data.
#'   If NULL (default), the URL is built automatically using use_snapshot and snapshot_date.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param pathogen_to_study Character vector of pathogen names to filter.
#'   Use "" (default) to include all pathogens.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param use_snapshot Logical. If TRUE, uses a snapshot URL; if FALSE (default),
#'   uses the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A ggplot2 object showing positivity over time by country and pathogen
#'
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @export
#' @examples
#' \dontrun{
#' # Using latest data
#' show_positivity_for_a_given_period(
#'   date_min = as.Date("2024-01-01"),
#'   date_max = as.Date("2024-12-31"),
#'   pathogen_to_study = "SARS-CoV-2"
#' )
#'
#' # Using a snapshot
#' show_positivity_for_a_given_period(
#'   date_min = as.Date("2023-01-01"),
#'   date_max = as.Date("2023-12-31"),
#'   use_snapshot = TRUE,
#'   snapshot_date = as.Date("2023-11-24")
#' )
#' }
show_positivity_for_a_given_period <- function(
  csv_file = NULL,
  date_min,
  date_max,
  pathogen_to_study = "",
  countries = "",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  csv_variants_filtered <- clean_erviss_positivity_for_a_given_period(
    csv_file,
    date_min,
    date_max,
    pathogen_to_study,
    countries,
    use_snapshot,
    snapshot_date
  )

  plot_erviss_positivity_for_a_given_period(csv_variants_filtered)
}
