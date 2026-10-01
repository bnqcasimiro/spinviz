# Canonical list of body subcategories recognised by injury_heatmap().
# Kept as a single source of truth so the CSV template and the reader's
# validation cannot drift apart.
injury_subcategories <- function() {
  c(
    "Head",
    "Neck",
    "Shoulder",
    "Chest",
    "Upper Arm",
    "Elbow",
    "Abdomen",
    "Forearm",
    "Hip Groin",
    "Wrist",
    "Hand",
    "Thigh",
    "Knee",
    "Lower Leg",
    "Ankle",
    "Foot",
    "Thoracic Spine",
    "Lumbosacral"
  )
}

#' @title Create a template CSV file for injury data
#'
#' @description
#' Writes a CSV file in the format expected by [injury_heatmap()]: one row
#' per recognised body subcategory, with `Region.area` and `Subcategory`
#' columns and one empty column per sport for the user to fill in with
#' injury frequencies.
#'
#' @param path File path where the CSV should be written.
#' @param sports Character vector of sport names; each becomes an empty
#'   frequency column in the template.
#'
#' @return The path (invisibly), so the call can be used in a pipe.
#' @seealso [read_injury_data()] to read a completed file back in.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' create_injury_template(path, sports = c("boxing", "judo"))
create_injury_template <- function(path, sports = "sport1") {
  stopifnot(
    is.character(path),
    length(path) == 1,
    is.character(sports),
    length(sports) >= 1,
    !anyNA(sports),
    all(nzchar(sports))
  )

  template <- data.frame(
    Region.area = "",
    Subcategory = injury_subcategories(),
    stringsAsFactors = FALSE
  )
  for (sport in sports) {
    template[[sport]] <- NA_real_
  }

  utils::write.csv(template, path, row.names = FALSE, na = "")
  invisible(path)
}

#' @title Read and validate injury data from a CSV file
#'
#' @description
#' Reads a CSV file (e.g. one created with [create_injury_template()] and
#' filled in) and checks that it has the structure [injury_heatmap()]
#' expects: `Region.area` and `Subcategory` columns plus at least one sport
#' column of injury frequencies.
#'
#' @param path File path of the CSV to read.
#'
#' @return A data frame with `Region.area`, `Subcategory`, and one column
#'   per sport (coerced to numeric).
#' @seealso [create_injury_template()] to generate a correctly formatted
#'   file.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' create_injury_template(path, sports = "boxing")
#' read_injury_data(path)
read_injury_data <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  data <- utils::read.csv(path, check.names = FALSE)

  required <- c("Region.area", "Subcategory")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  sport_cols <- setdiff(names(data), required)
  if (length(sport_cols) == 0) {
    stop(
      "No sport column found. The file needs at least one injury frequency ",
      "column besides 'Region.area' and 'Subcategory'.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("File contains no data rows.", call. = FALSE)
  }

  data$Region.area <- as.character(data$Region.area)
  data$Subcategory <- as.character(data$Subcategory)

  if (anyNA(data$Subcategory) || any(!nzchar(trimws(data$Subcategory)))) {
    stop(
      "Column 'Subcategory' contains missing or empty values.",
      call. = FALSE
    )
  }

  # Match injury_heatmap()'s normalisation so validation reflects what will
  # actually be plotted.
  normalised <- stringr::str_to_title(trimws(data$Subcategory))
  unrecognised <- setdiff(unique(normalised), injury_subcategories())
  if (length(unrecognised) > 0) {
    warning(
      "Unrecognised Subcategory value(s), possibly misspelled: ",
      paste(unrecognised, collapse = ", "),
      ". Expected values are: ",
      paste(injury_subcategories(), collapse = ", "),
      call. = FALSE
    )
  }

  for (col in sport_cols) {
    data[[col]] <- coerce_injury_values(data[[col]], col)
  }

  data
}
