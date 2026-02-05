#' @noRd
assert_file_or_url <- function(path, arg_name = "csv_file") {
  if (!file.exists(path) && !grepl("^https?://", path)) {
    stop(sprintf("'%s' must be an existing file or a valid URL", arg_name))
  }
}

#' @noRd
assert_date <- function(x, arg_name) {
  if (!inherits(x, "Date")) {
    stop(sprintf("'%s' must be a Date object (use as.Date())", arg_name))
  }
}

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