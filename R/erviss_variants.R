#' @title Clean the variants data for a given period
#' @description Clean the variants data for a given period
#' @param csv_variants_file The path to the CSV file containing the variants data
#' @param date_min The minimum date to show
#' @param date_max The maximum date to show
#' @param variant_to_study The variants to study
#' @param studysites_variants The studysites to study
#' @param minimal_value The minimal value to show
#' @param indicator_to_study The indicator to study
#' @return A data frame containing the variants data
#' #' @export
clean_erviss_variants_for_a_given_period <- function(
  csv_variants_file,
  date_min,
  date_max,
  variant_to_study = "",
  studysites_variants = "",
  minimal_value = 0,
  indicator_to_study = "proportion"
) {
  match.arg(indicator_to_study, c("proportion", "detections"))
  csv_variants <- readr::read_csv(
    csv_variants_file
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

  if (any(studysites_variants != "")) {
    csv_variants <- csv_variants %>%
      dplyr::filter(
        countryname %in% studysites_variants
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

#' @noRd
plot_erviss_variants_for_a_given_period <- function(csv_variants_filtered, date_breaks, date_format) {
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

#' @title Show variants for a given period
#' @description Show variants for a given period
#' @param csv_variants_file The path to the CSV file containing the variants data
#' @param date_min The minimum date to show
#' @param date_max The maximum date to show
#' @param variant_to_study The variants to study
#' @param studysites_variants The studysites to study
#' @param minimal_value The minimal value to show
#' @param indicator_to_study The indicator to study
#' @param date_breaks The breaks for the x-axis
#' @param date_format The format for the x-axis
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @return A plot of the variants
#' @export
#' @examples
#' show_variants_for_a_given_period(
#'   csv_variants_file = "https://raw.githubusercontent.com/EU-ECDC/Respiratory_viruses_weekly_data/refs/heads/main/data/snapshots/2025-11-21_variants.csv",
#'   date_min = as.Date("2024-10-01"),
#'   date_max = as.Date("2025-09-30"),
#'   variant_to_study = c("XFG", "LP.8.1", "BA.2.86"),
#'   minimal_value = 10,
#'   studysites_variants =  c("Belgium", "Denmark", "Italy", "Norway", "Portugal", "Spain", "Sweden"),
#'   date_breaks = "1 month", 
#'   date_labels = "%b %Y"
#' )
show_variants_for_a_given_period <- function(
  csv_variants_file,
  date_min,
  date_max,
  variant_to_study = "",
  studysites_variants = "",
  minimal_value = 0,
  indicator_to_study = "proportion",
  date_breaks,
  date_format = "%b %Y"
) {
  csv_variants_filtered <- clean_erviss_variants_for_a_given_period(
    csv_variants_file,
    date_min,
    date_max,
    variant_to_study,
    studysites_variants,
    minimal_value,
    indicator_to_study
  )
  plot_erviss_variants_for_a_given_period(csv_variants_filtered, date_breaks, date_format)
}
