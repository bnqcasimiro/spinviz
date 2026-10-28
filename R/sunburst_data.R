#' @title Create a template CSV file for sunburst injury data
#'
#' @description
#' Writes a CSV file in the format expected by
#' [plot_sunburst()]: one row per entry in the built-in
#' [injury_categories] taxonomy, with `tissue_type` and `pathology_type` columns and
#' one empty column per sport for the user to fill in with injury
#' frequencies.
#'
#' [injury_categories] is the single source of truth for the taxonomy, so
#' the template, the reader's validation, and the diagram cannot drift
#' apart.
#'
#' @param path File path where the CSV should be written.
#' @param group Character vector of sport names; each becomes an empty
#'   frequency column in the template.
#'
#' @return The path (invisibly), so the call can be used in a pipe.
#' @seealso [read_sunburst_data()] to read a completed file back in,
#'   [create_injury_template()] for the heatmap equivalent.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' create_sunburst_template(path, group = c("boxing", "judo"))
create_sunburst_template <- function(path, group = "group1") {
  stopifnot(
    is.character(path),
    length(path) == 1,
    is.character(group),
    length(group) >= 1,
    !anyNA(group),
    all(nzchar(group))
  )

  # Tissue is repeated on every row (rather than written only on the first
  # row of each group) so the file stays valid if a user sorts or filters
  # rows in a spreadsheet; read_sunburst_data() also accepts the compact
  # carry-down style.
  template <- data.frame(
    tissue_type = injury_categories$tissue_type,
    pathology_type = injury_categories$pathology_type,
    stringsAsFactors = FALSE
  )
  for (sport in group) {
    template[[sport]] <- NA_real_
  }

  utils::write.csv(template, path, row.names = FALSE, na = "")
  invisible(path)
}

#' @title Read and validate sunburst injury data from a CSV file
#'
#' @description
#' Reads a CSV file (e.g. one created with [create_sunburst_template()] and
#' filled in) and checks that it has the structure
#' [plot_sunburst()] expects: `tissue_type` and `pathology_type` columns
#' plus at least one sport column of injury frequencies.
#'
#' Blank cells in the `tissue_type` column are filled with the last seen tissue_type
#' value (carry-down), matching the behaviour of
#' [plot_sunburst()], so compact hand-edited files are accepted.
#'
#' @param path File path of the CSV to read.
#'
#' @return A data frame with `tissue_type`, `pathology_type`, and one column per
#'   sport (coerced to numeric), ready to pass to
#'   [plot_sunburst()].
#' @seealso [create_sunburst_template()] to generate a correctly formatted
#'   file, [read_injury_data()] for the heatmap equivalent.
#' @export
#' @examples
#' path <- tempfile(fileext = ".csv")
#' create_sunburst_template(path, group = "boxing")
#' read_sunburst_data(path)
read_sunburst_data <- function(path) {
  stopifnot(is.character(path), length(path) == 1)
  if (!file.exists(path)) {
    stop("File does not exist: ", path, call. = FALSE)
  }

  data <- utils::read.csv(path, check.names = FALSE)

  required <- c("tissue_type", "pathology_type")
  missing_cols <- setdiff(required, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Missing required column(s): ",
      paste(missing_cols, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  sport_cols <- setdiff(names(data), required)
  if (length(sport_cols) == 0) {
    stop(
      "No sport column found. The file needs at least one injury frequency ",
      "column besides 'tissue_type' and 'pathology_type'.",
      call. = FALSE
    )
  }

  if (nrow(data) == 0) {
    stop("File contains no data rows.", call. = FALSE)
  }

  data$tissue_type <- trimws(as.character(data$tissue_type))
  data$pathology_type <- trimws(as.character(data$pathology_type))

  # Carry the last seen tissue_type down over blank cells, mirroring
  # plot_sunburst()'s handling of compact hand-edited files.
  blank_tissue <- is.na(data$tissue_type) | !nzchar(data$tissue_type)
  if (any(blank_tissue)) {
    if (blank_tissue[1]) {
      stop(
        "Column 'tissue_type' starts with a blank value; the first row must ",
        "name a tissue type.",
        call. = FALSE
      )
    }
    last_seen <- ""
    for (i in seq_len(nrow(data))) {
      if (blank_tissue[i]) {
        data$tissue_type[i] <- last_seen
      } else {
        last_seen <- data$tissue_type[i]
      }
    }
  }

  if (anyNA(data$pathology_type) || any(!nzchar(data$pathology_type))) {
    stop(
      "Column 'pathology_type' contains missing or empty values.",
      call. = FALSE
    )
  }

  # Warn (not stop) on unrecognised labels: the diagram accepts custom
  # categories, but typos silently produce fragmented trees.
  unrecognised_tissue <- setdiff(
    unique(data$tissue_type),
    unique(injury_categories$tissue_type)
  )
  if (length(unrecognised_tissue) > 0) {
    warning(
      "Unrecognised tissue_type value(s), possibly misspelled: ",
      paste(unrecognised_tissue, collapse = ", "),
      ". Expected values are: ",
      paste(unique(injury_categories$tissue_type), collapse = ", "),
      call. = FALSE
    )
  }

  unrecognised_pathology <- setdiff(
    unique(data$pathology_type),
    unique(injury_categories$pathology_type)
  )
  if (length(unrecognised_pathology) > 0) {
    warning(
      "Unrecognised pathology_type value(s), possibly misspelled: ",
      paste(unrecognised_pathology, collapse = ", "),
      ". Expected values are: ",
      paste(unique(injury_categories$pathology_type), collapse = ", "),
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
