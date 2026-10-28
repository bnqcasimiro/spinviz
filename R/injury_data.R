# Canonical list of body areas drawn on the plot_heatmap() diagram,
# derived from the built-in body_categories taxonomy minus "Unspecified"
# (which has no SVG region and is shown as a count below the diagram
# instead). Used verbatim (no title-casing), so e.g. "Hip / groin" keeps
# its lowercase second word. Note the CSV template and reader validation
# accept the full taxonomy (including "Unspecified") via
# body_categories$body_area.
injury_body_areas <- function() {
  areas <- body_categories$body_area
  areas[areas != "Unspecified"]
}

#' @title Create a template CSV file for injury data
#'
#' @description
#' Writes a CSV file in the format expected by [plot_heatmap()]: one row
#' per body area in the [body_categories] taxonomy (including
#' "Unspecified", which [plot_heatmap()] reports as a count below the
#' diagram), with `body_region` and `body_area` columns pre-filled and
#' one empty column per sport for the user to fill in with injury
#' frequencies.
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

  template <- body_categories[, c("body_region", "body_area")]
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
#' filled in) and checks that it has the structure [plot_heatmap()]
#' expects: `body_region` and `body_area` columns plus at least one sport
#' column of injury frequencies.
#'
#' @param path File path of the CSV to read.
#'
#' @return A data frame with `body_region`, `body_area`, and one column
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

  required <- c("body_region", "body_area")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    legacy <- intersect(
      c("Region.area", "Subcategory", "region_area", "subcategory"),
      names(data)
    )
    hint <- if (length(legacy) > 0) {
      paste0(
        " Note: these columns were renamed to 'body_region' and 'body_area'; ",
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
      "column besides 'body_region' and 'body_area'.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("File contains no data rows.", call. = FALSE)
  }

  data$body_region <- as.character(data$body_region)
  data$body_area <- as.character(data$body_area)

  if (anyNA(data$body_area) || any(!nzchar(trimws(data$body_area)))) {
    stop(
      "Column 'body_area' contains missing or empty values.",
      call. = FALSE
    )
  }

  # Case-insensitive comparison against the canonical labels (which keep
  # lowercase second words, e.g. "Hip / groin"); input values themselves
  # are stored verbatim, so matching in plot_heatmap() is unaffected.
  canonical <- body_categories$body_area
  entered <- trimws(data$body_area)
  unrecognised <- unique(entered[!tolower(entered) %in% tolower(canonical)])
  if (length(unrecognised) > 0) {
    warning(
      "Unrecognised body_area value(s), possibly misspelled: ",
      paste(unrecognised, collapse = ", "),
      ". Expected values are: ",
      paste(canonical, collapse = ", "),
      call. = FALSE
    )
  }

  for (col in sport_cols) {
    data[[col]] <- coerce_injury_values(data[[col]], col)
    values <- data[[col]][!is.na(data[[col]])]
    invalid <- abs(values - round(values)) > sqrt(.Machine$double.eps) |
      values < 0
    if (any(invalid)) {
      stop(
        "Column '", col, "' must contain whole-number injury counts ",
        "(0, 1, 2, ...). Invalid value(s): ",
        paste(unique(values[invalid]), collapse = ", "),
        call. = FALSE
      )
    }
  }

  data
}
