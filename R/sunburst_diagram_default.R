#' @title Sunburst diagram using the built-in injury taxonomy
#'
#' @description
#' A thin wrapper around [sunburst_diagram_echarts()] for using the standard
#' tissue/pathology taxonomy ([injury_categories]) and plugging in a set of
#' injury counts.
#'
#' @param counts A numeric vector of exactly `nrow(injury_categories)`
#' values (currently 25), in the same row order as [injury_categories],
#' i.e. `counts[i]` is the injury count for
#' `injury_categories[i, c("tissue", "pathology")]`.
#' @param column_name A string used as the injury-count column's name when
#' building the underlying data frame. Default "injury_count".
#' @param ... Passed straight through to [sunburst_diagram_echarts()].
#' Every other argument (`palette`, `plot_title`, `include_non_specific`,
#' radius/label tuning, etc.) works exactly the same way.
#'
#' @return An echarts4r htmlwidget, identical to what
#' [sunburst_diagram_echarts()] itself returns.
#' @seealso [injury_categories] for the taxonomy this merges your counts
#' into. [sunburst_diagram_echarts()] for the full version that accepts any
#' tissue/pathology taxonomy, if you need something other than the
#' built-in one (i.e. a different classification system).
#' @export
#'
#' @examples
#' boxing <- c(20,0,0,10,31,18,16,16,20,20,27,46,67,31,54,20,27,30,96,82,48,26,33,34,24)
#' p1 <- sunburst_diagram_default(boxing, plot_title = "Boxing Injuries")
sunburst_diagram_default <- function(counts, column_name = "injury_count", ...){

  expected_n <- nrow(injury_categories)
  if (!is.numeric(counts) || length(counts) != expected_n) {
    stop(
      "`counts` must be a numeric vector of exactly ", expected_n,
      " values, one per row of the built-in injury_categories taxonomy ",
      "(see ?injury_categories). Got ", length(counts), " value(s).",
      call. = FALSE
    )
  }

  df <- injury_categories
  df[[column_name]] <- counts

  sunburst_diagram_echarts(df, column_name = column_name, ...)
}
