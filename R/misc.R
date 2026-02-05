# Base URL for ERVISS data
ERVISS_BASE_URL <- "https://raw.githubusercontent.com/EU-ECDC/Respiratory_viruses_weekly_data/refs/heads/main/data"

#' Get ERVISS variants data URL
#'
#' Builds the URL to download ERVISS variants data, either the latest batch
#' or a specific snapshot.
#'
#' @param use_snapshot Logical. If TRUE, returns a snapshot URL; if FALSE (default),
#'   returns the URL for the latest data.
#' @param snapshot_date Date of the snapshot to retrieve (required if use_snapshot = TRUE).
#'   Must be a Date object.
#'
#' @return A character string containing the URL
#'
#' @export
#' @examples
#' # Get latest data URL
#' get_erviss_variants_url()
#'
#' # Get snapshot URL
#' get_erviss_variants_url(use_snapshot = TRUE, snapshot_date = as.Date("2023-11-24"))
get_erviss_variants_url <- function(
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (use_snapshot) {
    if (is.null(snapshot_date)) {
      stop("'snapshot_date' is required when use_snapshot = TRUE")
    }
    assert_date(snapshot_date, "snapshot_date")
    paste0(
      ERVISS_BASE_URL,
      "/snapshots/",
      format(snapshot_date, "%Y-%m-%d"),
      "_variants.csv"
    )
  } else {
    paste0(ERVISS_BASE_URL, "/variants.csv")
  }
}

#' Get ERVISS positivity data URL
#'
#' Builds the URL to download ERVISS positivity data, either the latest batch
#' or a specific snapshot.
#'
#' @param use_snapshot Logical. If TRUE, returns a snapshot URL; if FALSE (default),
#'   returns the URL for the latest data.
#' @param snapshot_date Date of the snapshot to retrieve (required if use_snapshot = TRUE).
#'   Must be a Date object.
#'
#' @return A character string containing the URL
#'
#' @export
#' @examples
#' # Get latest data URL
#' get_sentineltests_positivity_url()
#'
#' # Get snapshot URL
#' get_sentineltests_positivity_url(use_snapshot = TRUE, snapshot_date = as.Date("2023-11-24"))
get_sentineltests_positivity_url <- function(
  use_snapshot = FALSE,
  snapshot_date = NULL
) {
  if (use_snapshot) {
    if (is.null(snapshot_date)) {
      stop("'snapshot_date' is required when use_snapshot = TRUE")
    }
    assert_date(snapshot_date, "snapshot_date")
    paste0(
      ERVISS_BASE_URL,
      "/snapshots/",
      format(snapshot_date, "%Y-%m-%d"),
      "_sentinelTestsDetectionsPositivity.csv"
    )
  } else {
    paste0(ERVISS_BASE_URL, "/sentinelTestsDetectionsPositivity.csv")
  }
}

#' @noRd
theme_erviss <- function() {
  theme_minimal() +
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
