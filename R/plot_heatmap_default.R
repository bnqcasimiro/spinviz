#' @title Heatmap diagram using the built-in body region taxonomy
#'
#' @description
#' A thin wrapper around [plot_heatmap()] for using the standard
#' tissue/pathology taxonomy ([body_categories]) and plugging in a set of
#' injury counts.
#'
#' @param counts A numeric vector of exactly `nrow(body_categories)` values
#' (currently 19), in the same row order as [body_categories],
#' i.e. `counts[i]` is the injury count for
#' `body_categories[i, c("body_region", "body_area")]`. See
#' `?body_categories` for the full numbered row order, or just run
#' `print(body_categories)` directly.
#' @param view which view to render the diagram. One of ("anterior",
#' "posterior", or "both"), same as [plot_heatmap()].
#' @param group A string used as the injury-count column's name
#' when building the underlying data frame. Default "injury_count".
#' @param sex which body diagram to use: `"male"` or `"female"`.
#' @param ... Passed straight through to [plot_heatmap()]. Every other
#' argument (`palette`, `opacity`, `show_labels`, `show_values`,
#' `show_scale`, etc.) works exactly the same way.
#'
#' @return A plot in R-studio viewer, identical to what
#' [plot_heatmap()] itself returns.
#' @seealso [body_categories] for the taxonomy this merges your counts
#' into. [plot_heatmap()] for the full version that accepts any
#' body_region/body_area taxonomy, if you need something other than the
#' built-in one. [save_diagram()] to save the result to file.
#' @export
#'
#' @examples
#' boxing <- c(1, 12, 24, 20, 11, 12, 8, 5, 40, 32, 12, 28, 20, 15, 12, 35, 18, 5, 0)
#' p1 <- plot_heatmap_default(boxing, "anterior", sex = "male")

plot_heatmap_default <- function(
  counts,
  view,
  group = "injury_count",
  sex = c("male", "female"),
  ...
) {
  expected_n <- nrow(body_categories)
  if (!is.numeric(counts) || length(counts) != expected_n) {
    stop(
      "`counts` must be a numeric vector of exactly ",
      expected_n,
      " values, one per row of the built-in body_categories taxonomy ",
      "(see ?body_categories). Got ",
      length(counts),
      " value(s).",
      call. = FALSE
    )
  }

  df <- body_categories
  df[[group]] <- counts

  plot_heatmap(df, group, view, sex = sex, ...)
}
