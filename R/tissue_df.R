#' @title Example boxing and judo injury counts by tissue/pathology type
#'
#' @description
#' An example dataset for [plot_sunburst()]: the standard
#' [injury_categories] taxonomy with two columns of (fictional) injury
#' counts attached.
#'
#' @format A data frame with 25 rows and 4 columns:
#' \describe{
#'   \item{tissue_type}{Tissue type, e.g. "Bone", "Nervous".}
#'   \item{pathology_type}{Pathology type, e.g. "Fracture", "Tendon rupture".}
#'   \item{boxing}{Number of recorded boxing injuries for that pathology.}
#'   \item{judo}{Number of recorded judo injuries for that pathology.}
#' }
#'
#' @details
#' Row order matches [injury_categories], so the counts can be reused with
#' other taxonomies built on the same ordering.
#'
#' @seealso [injury_categories] for the taxonomy without counts,
#' [plot_sunburst()] to plot this dataset, and [body_df] for the
#' equivalent example dataset for [plot_heatmap()].
#'
#' @examples
#' plot_sunburst(tissue_df, "boxing")
#' plot_sunburst(tissue_df, "judo")
"tissue_df"
