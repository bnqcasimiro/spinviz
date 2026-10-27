#' @title Example boxing injury counts by body area
#'
#' @description
#' An example dataset for [plot_heatmap()]: the standard
#' [body_categories] taxonomy with a column of (fictional) boxing injury
#' counts attached.
#'
#' @format A data frame with 19 rows and 3 columns:
#' \describe{
#'   \item{body_region}{Body region, e.g. "Upper limb", "Trunk".}
#'   \item{body_area}{Body area, e.g. "Shoulder", "Chest".}
#'   \item{boxing}{Number of recorded boxing injuries in that body area.}
#' }
#'
#' @details
#' Row order matches [body_categories], so the counts can be reused with
#' other taxonomies built on the same ordering.
#'
#' @seealso [body_categories] for the taxonomy without counts,
#' [plot_heatmap()] to plot this dataset, and [tissue_df] for the
#' equivalent example dataset for [plot_sunburst()].
#'
#' @examples
#' plot_heatmap(body_df, "boxing", "anterior", sex = "male")
"body_df"
