#' @title Standard tissue/pathology injury taxonomy
#'
#' @description
#' The standard Tissue/Pathology classification used throughout this
#' package's sunburst and heatmap functions, with no injury counts attached.
#' Combine it with your own counts via [sunburst_diagram_default()] rather
#' than retyping the full tissue/pathology category list for every analysis.
#'
#' @format A data frame with 25 rows and 2 columns:
#' \describe{
#'   \item{tissue}{Tissue type, e.g. "Bone", "Nervous".}
#'   \item{pathology}{Pathology type, e.g. "Fracture", "Tendon rupture".}
#' }
#'
#' @details
#' \strong{Row order matters.} [sunburst_diagram_default()] (and the manual
#' \code{df <- injury_categories; df$my_counts <- counts} pattern) line up
#' your counts with this taxonomy purely by row position. \code{counts[i]}
#' is assumed to be the count for row \code{i}. The current order (also
#' always available by running \code{print(injury_categories)} or
#' \code{View(injury_categories)} directly, which is the authoritative
#' source if this list and the live object ever disagree) is:
#' \preformatted{
#'  1. Muscle / Tendon       -- Muscle strain
#'  2. Muscle / Tendon       -- Muscle contusion
#'  3. Muscle / Tendon       -- Compartment syndrome
#'  4. Muscle / Tendon       -- Tendinopathy
#'  5. Muscle / Tendon       -- Tendon rupture
#'  6. Nervous               -- Brain or spinal cord injury
#'  7. Nervous               -- Peripheral nerve injury
#'  8. Bone                  -- Fracture
#'  9. Bone                  -- Bone stress injury
#' 10. Bone                  -- Bone contusion
#' 11. Bone                  -- Avascular necrosis
#' 12. Bone                  -- Physis injury
#' 13. Cartilage / Synovium  -- Cartilage injury
#' 14. Cartilage / Synovium  -- Arthritis
#' 15. Cartilage / Synovium  -- Synovitis / Capsulitis
#' 16. Cartilage / Synovium  -- Bursitis
#' 17. Ligament              -- Joint sprain
#' 18. Ligament              -- Chronic instability
#' 19. Superficial tissue    -- Contusion
#' 20. Superficial tissue    -- Laceration
#' 21. Superficial tissue    -- Abrasion
#' 22. Vessel                -- Vascular trauma
#' 23. Stump                 -- Stump injury
#' 24. Internal organ        -- Organ injury
#' 25. Unspecified           -- Unspecified
#' }
#'
#' @seealso [sunburst_diagram_default()] to build a sunburst diagram from
#' this taxonomy plus your own injury counts, without needing to retype the
#' tissue/pathology labels yourself. [body_categories] for the equivalent
#' taxonomy used by [heatmap_diagram()].
#' @export
injury_categories <- data.frame(
  tissue = c(
    'Muscle / Tendon','Muscle / Tendon','Muscle / Tendon','Muscle / Tendon','Muscle / Tendon',
    'Nervous','Nervous',
    'Bone','Bone','Bone','Bone','Bone',
    'Cartilage / Synovium','Cartilage / Synovium','Cartilage / Synovium','Cartilage / Synovium',
    'Ligament','Ligament',
    'Superficial tissue','Superficial tissue','Superficial tissue',
    'Vessel',
    'Stump',
    'Internal organ',
    'Unspecified'
  ),
  pathology = c(
    'Muscle strain','Muscle contusion','Compartment syndrome','Tendinopathy','Tendon rupture',
    'Brain or spinal cord injury','Peripheral nerve injury',
    'Fracture','Bone stress injury','Bone contusion','Avascular necrosis','Physis injury',
    'Cartilage injury','Arthritis','Synovitis / Capsulitis','Bursitis',
    'Joint sprain','Chronic instability',
    'Contusion','Laceration','Abrasion',
    'Vascular trauma',
    'Stump injury',
    'Organ injury',
    'Unspecified'
  ),
  stringsAsFactors = FALSE
)
