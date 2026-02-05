#' Clean ERVISS variants data for a given period
#'
#' Filters and cleans variant data from an ERVISS CSV file for a specified
#' date range, variant(s), study site(s), and indicator type.
#'
#' @param csv_file Path to the CSV file or URL containing the ERVISS data.
#'   If NULL (default), the URL is built automatically using use_snapshot and snapshot_date.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param variant_to_study Character vector of variant names to filter.
#'   Use "" (default) to include all variants.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param minimal_value Minimum value threshold to include in the results (default: 0)
#' @param indicator_to_study Type of indicator: "proportion" (default) or "detections"
#' @param use_snapshot Logical. If TRUE, uses a snapshot URL; if FALSE (default),
#'   uses the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A data frame containing the filtered variant data with columns:
#'   date, value, variant, countryname, indicator, and other ERVISS fields.
#'
#' @export
clean_erviss_variants_for_a_given_period <- function(
  csv_file = NULL,
  date_min,
  date_max,
  variant_to_study = "",
  countries = "",
  minimal_value = 0,
  indicator_to_study = "proportion",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (is.null(csv_file)) {
    csv_file <- get_erviss_variants_url(use_snapshot, snapshot_date)
  }
  assert_file_or_url(csv_file, "csv_file")
  assert_date(date_min, "date_min")
  assert_date(date_max, "date_max")

  match.arg(indicator_to_study, c("proportion", "detections"))
  csv_variants <- readr::read_csv(
    csv_file
  ) %>%
    dplyr::mutate(
      date = yearweek_to_date(yearweek)
    )

  if (any(variant_to_study != "")) {
    csv_variants <- csv_variants %>%
      dplyr::filter(
        variant %in% variant_to_study
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
      indicator == indicator_to_study
    ) %>%
    dplyr::filter(
      value >= minimal_value
    )

  return(csv_variants_filtered)
}

#' Plot ERVISS variants data
#'
#' Creates a ggplot2 visualization of variant data, with facets by country
#' and colored by variant. The y-axis shows percentage of all variants.
#'
#' @param csv_variants_filtered A data frame containing cleaned variant data,
#'   typically output from \code{\link{clean_erviss_variants_for_a_given_period}}.
#'   Must contain columns: date, value, variant, countryname.
#' @param date_breaks A string specifying the date breaks for the x-axis
#'   (e.g., "1 month", "2 weeks")
#' @param date_format A string specifying the date format for x-axis labels
#'   (e.g., `"%b %Y"` for "Jan 2024")
#'
#' @return A ggplot2 object
#'
#' @export
plot_erviss_variants_for_a_given_period <- function(
  csv_variants_filtered,
  date_breaks,
  date_format
) {
  ggplot(csv_variants_filtered, aes(x = date, y = value, color = variant)) +
    geom_line() +
    xlab("") +
    ylab("% of all variants") +
    facet_wrap(~countryname, scales = "free_x", ncol = 3) +
    theme_minimal() +
    scale_x_date(date_breaks = date_breaks, date_labels = date_format) +
    scale_colour_viridis_d(name = "Variant") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 25)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 0.5),
      strip.placement = "outside",
      legend.position = "bottom",
      panel.spacing = unit(1, "cm"),
      axis.text = element_text(size = 12),
      axis.title = element_text(size = 14),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 14),
      strip.text = element_text(size = 14),
      panel.grid = element_blank()
    )
}

#' Show variants for a given period
#'
#' Cleans and plots ERVISS variant data for a specified period.
#' This is a convenience function that combines
#' \code{\link{clean_erviss_variants_for_a_given_period}} and
#' \code{\link{plot_erviss_variants_for_a_given_period}}.
#'
#' @param csv_file Path to the CSV file or URL containing the ERVISS data.
#'   If NULL (default), the URL is built automatically using use_snapshot and snapshot_date.
#' @param date_min Start date of the period (Date object)
#' @param date_max End date of the period (Date object)
#' @param variant_to_study Character vector of variant names to filter.
#'   Use "" (default) to include all variants.
#' @param countries Character vector of country names to filter.
#'   Use "" (default) to include all countries.
#' @param minimal_value Minimum value threshold to include in the results (default: 0)
#' @param indicator_to_study Type of indicator: "proportion" (default) or "detections"
#' @param date_breaks A string specifying the date breaks for the x-axis
#'   (e.g., "1 month", "2 weeks")
#' @param date_format A string specifying the date format for x-axis labels
#'   (e.g., `"%b %Y"` for "Jan 2024")
#' @param use_snapshot Logical. If TRUE, uses a snapshot URL; if FALSE (default),
#'   uses the latest data. Ignored if csv_file is provided.
#' @param snapshot_date Date of the snapshot to retrieve.
#'   Required if use_snapshot = TRUE and csv_file is NULL.
#'
#' @return A ggplot2 object showing variant proportions over time by country
#'
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @export
#' @examples
#' \dontrun{
#' # Using latest data
#' show_variants_for_a_given_period(
#'   date_min = as.Date("2024-10-01"),
#'   date_max = as.Date("2025-09-30"),
#'   variant_to_study = c("XFG", "LP.8.1"),
#'   date_breaks = "1 month"
#' )
#'
#' # Using a snapshot
#' show_variants_for_a_given_period(
#'   date_min = as.Date("2024-10-01"),
#'   date_max = as.Date("2025-09-30"),
#'   use_snapshot = TRUE,
#'   snapshot_date = as.Date("2025-11-21"),
#'   date_breaks = "1 month"
#' )
#' }
show_variants_for_a_given_period <- function(
  csv_file = NULL,
  date_min,
  date_max,
  variant_to_study = "",
  countries = "",
  minimal_value = 0,
  indicator_to_study = "proportion",
  date_breaks,
  date_format = "%b %Y",
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  csv_variants_filtered <- clean_erviss_variants_for_a_given_period(
    csv_file,
    date_min,
    date_max,
    variant_to_study,
    countries,
    minimal_value,
    indicator_to_study,
    use_snapshot,
    snapshot_date
  )
  plot_erviss_variants_for_a_given_period(
    csv_variants_filtered,
    date_breaks,
    date_format
  )
}
