# Canonical list of body subcategories recognised by heatmap_diagram(),
# derived from the built-in body_categories taxonomy (minus "Unspecified",
# which is not drawn on the diagram) and title-cased the same way
# heatmap_diagram() normalises input. Single source of truth so the CSV
# template, the reader's validation, and the diagram cannot drift apart.
injury_subcategories <- function() {
  subcategories <- body_categories$subcategory
  subcategories <- subcategories[subcategories != "Unspecified"]
  stringr::str_to_title(subcategories)
}

#' @title Create a template CSV file for injury data
#'
#' @description
#' Writes a CSV file in the format expected by [heatmap_diagram()]: one row
#' per recognised body subcategory, with `region_area` and `subcategory`
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
    region_area = "",
    subcategory = injury_subcategories(),
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
#' filled in) and checks that it has the structure [heatmap_diagram()]
#' expects: `region_area` and `subcategory` columns plus at least one sport
#' column of injury frequencies.
#'
#' @param path File path of the CSV to read.
#'
#' @return A data frame with `region_area`, `subcategory`, and one column
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

  required <- c("region_area", "subcategory")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    legacy <- intersect(c("Region.area", "Subcategory"), names(data))
    hint <- if (length(legacy) > 0) {
      paste0(
        " Note: 'Region.area' and 'Subcategory' were renamed to snake_case; ",
        "please update your file to use the new names."
      )
    } else {
      ""
    }
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      hint,
      call. = FALSE
    )
  }

  sport_cols <- setdiff(names(data), required)
  if (length(sport_cols) == 0) {
    stop(
      "No sport column found. The file needs at least one injury frequency ",
      "column besides 'region_area' and 'subcategory'.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("File contains no data rows.", call. = FALSE)
  }

  data$region_area <- as.character(data$region_area)
  data$subcategory <- as.character(data$subcategory)

  if (anyNA(data$subcategory) || any(!nzchar(trimws(data$subcategory)))) {
    stop(
      "Column 'subcategory' contains missing or empty values.",
      call. = FALSE
    )
  }

  # Match heatmap_diagram()'s normalisation so validation reflects what will
  # actually be plotted.
  normalised <- stringr::str_to_title(trimws(data$subcategory))
  unrecognised <- setdiff(unique(normalised), injury_subcategories())
  if (length(unrecognised) > 0) {
    warning(
      "Unrecognised subcategory value(s), possibly misspelled: ",
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
