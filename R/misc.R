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