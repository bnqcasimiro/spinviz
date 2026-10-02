#' @title Standard body region/body area injury taxonomy
#'
#' @description
#' The standard body region/body area classification used by
#' [heatmap_diagram()], with no injury counts attached. Combine it with your
#' own counts via [heatmap_diagram_default()] rather than retyping the full
#' region_area/subcategory list for every analysis.
#'
#' @format A data frame with 19 rows and 2 columns:
#' \describe{
#'   \item{region_area}{Body region, e.g. "Upper limb", "Trunk".}
#'   \item{subcategory}{Body area, e.g. "Shoulder", "Chest".}
#' }
#'
#' @details
#' \strong{Row order matters.} [heatmap_diagram_default()] (and the manual
#' \code{df <- body_categories; df$my_counts <- counts} pattern) line up
#' your counts with this taxonomy purely by row position. \code{counts[i]}
#' is assumed to be the count for row \code{i}. The current order (also
#' always available by running \code{print(body_categories)} or
#' \code{View(body_categories)} directly, which is the authoritative source
#' if this list and the live object ever disagree) is:
#' \preformatted{
#'  1. Head and neck -- Head
#'  2. Head and neck -- Neck
#'  3. Upper limb    -- Shoulder
#'  4. Upper limb    -- Upper arm
#'  5. Upper limb    -- Elbow
#'  6. Upper limb    -- Forearm
#'  7. Upper limb    -- Wrist
#'  8. Upper limb    -- Hand
#'  9. Trunk         -- Chest
#' 10. Trunk         -- Thoracic spine
#' 11. Trunk         -- Lumbosacral
#' 12. Trunk         -- Abdomen
#' 13. Lower limb    -- Hip Groin
#' 14. Lower limb    -- Thigh
#' 15. Lower limb    -- Knee
#' 16. Lower limb    -- Lower leg
#' 17. Lower limb    -- Ankle
#' 18. Lower limb    -- Foot
#' 19. Unspecified   -- Unspecified
#' }
#'
#' @seealso [heatmap_diagram_default()] to build a heatmap diagram from
#' this taxonomy plus your own injury counts, without needing to retype the
#' region_area/subcategory labels yourself. [injury_categories] for the
#' equivalent taxonomy used by the sunburst functions.
#' @export
body_categories <- data.frame(
  region_area = c(
    "Head and neck", "Head and neck",
    "Upper limb", "Upper limb", "Upper limb", "Upper limb", "Upper limb", "Upper limb",
    "Trunk", "Trunk", "Trunk", "Trunk",
    "Lower limb", "Lower limb", "Lower limb", "Lower limb", "Lower limb", "Lower limb",
    "Unspecified"
  ),
  subcategory = c(
    "Head", "Neck",
    "Shoulder", "Upper arm", "Elbow", "Forearm", "Wrist", "Hand",
    "Chest", "Thoracic spine", "Lumbosacral", "Abdomen",
    "Hip Groin", "Thigh", "Knee", "Lower leg", "Ankle", "Foot",
    "Unspecified"
  ),
  stringsAsFactors = FALSE
)
