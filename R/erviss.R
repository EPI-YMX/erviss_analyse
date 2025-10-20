#' @noRd
yearweek_to_date <- function(yearweek) {
  # Parse the yearweek string (e.g., "2020-W03")
  year <- as.numeric(substr(yearweek, 1, 4))
  week <- as.numeric(substr(yearweek, 7, 8))

  # Create a date string for January 4th of the year (always in week 1)
  jan4 <- as.Date(paste0(year, "-01-04"))

  # Find the Monday of week 1
  days_to_monday <- (as.numeric(format(jan4, "%u")) - 1) %% 7
  monday_week1 <- jan4 - days_to_monday

  # Calculate the Monday of the target week
  target_monday <- monday_week1 + (week - 1) * 7

  return(target_monday)
}

#' @noRd
clean_erviss_for_a_given_period <- function(
  csv_variants_file,
  date_min,
  date_max,
  variant_to_study = "",
  studysites_variants = ""
) {
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
      indicator == "proportion"
    )

  return(csv_variants_filtered)
}

#' @noRd
plot_erviss_for_a_given_period <- function(csv_variants_filtered) {
  ggplot(csv_variants_filtered, aes(x = date, y = value, color = variant)) +
    geom_line() +
    xlab("") +
    ylab("% of all variants") +
    facet_wrap(~countryname, scales = "free_x", ncol = 3) +
    theme_minimal() +
    scale_x_date(date_breaks = "2 weeks") +
    scale_colour_viridis_d(name = "Variant") +
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
    )
}

#' @title Show variants for a given period
#' @description Show variants for a given period
#' @param csv_variants_file The path to the CSV file containing the variants data
#' @param date_min The minimum date to show
#' @param date_max The maximum date to show
#' @param variant_to_study The variants to study
#' @param studysites_variants The studysites to study
#' @import ggplot2
#' @import dplyr
#' @import readr
#' @return A plot of the variants
#' @export
show_variants_for_a_given_period <- function(
  csv_variants_file,
  date_min,
  date_max,
  variant_to_study = "",
  studysites_variants = ""
) {
  csv_variants_filtered <- clean_erviss_for_a_given_period(
    csv_variants_file,
    date_min,
    date_max,
    variant_to_study,
    studysites_variants
  )
  plot_erviss_for_a_given_period(csv_variants_filtered)
}

# library(dplyr)
# erviss_files <- list.files("inst/erviss_data/", full.names = TRUE)
# not_variants_files <- erviss_files %>% 
#   grep("variants\\.csv$", ., invert = TRUE, value = TRUE)
# file.remove(not_variants_files)

