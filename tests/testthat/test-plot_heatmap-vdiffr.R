# Visual regression tests. Snapshots live in tests/testthat/_snaps/plot_heatmap-vdiffr/.
# After an intentional visual change, review and accept new snapshots with
# testthat::snapshot_review() / vdiffr::manage_cases().

vdiffr_injury_data <- function() {
  df <- data.frame(
    region_area = rep("Example", 18),
    subcategory = c(
      "Head",
      "Neck",
      "Shoulder",
      "Chest",
      "Upper arm",
      "Elbow",
      "Abdomen",
      "Forearm",
      "Hip / groin",
      "Wrist",
      "Hand",
      "Thigh",
      "Knee",
      "Lower leg",
      "Ankle",
      "Foot",
      "Thoracic spine",
      "Lumbosacral"
    ),
    boxing = c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)
  )
  df
}

test_that("single-view heatmaps render consistently", {
  skip_if_not_installed("vdiffr")
  df <- vdiffr_injury_data()

  vdiffr::expect_doppelganger(
    "heatmap front male",
    plot_heatmap(df, "boxing", "front", sex = "male")
  )
  vdiffr::expect_doppelganger(
    "heatmap back female",
    plot_heatmap(df, "boxing", "back", sex = "female")
  )
})

test_that("both-view heatmap renders consistently", {
  skip_if_not_installed("vdiffr")
  df <- vdiffr_injury_data()

  vdiffr::expect_doppelganger(
    "heatmap both male",
    plot_heatmap(df, "boxing", "both", sex = "male")
  )
})

test_that("heatmap display options render consistently", {
  skip_if_not_installed("vdiffr")
  df <- vdiffr_injury_data()

  vdiffr::expect_doppelganger(
    "heatmap percentage labels",
    plot_heatmap(
      df,
      "boxing",
      "front",
      value_format = "percentage",
      show_labels = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    "heatmap custom palette no scale",
    plot_heatmap(
      df,
      "boxing",
      "front",
      palette = "magma",
      show_scale = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    "heatmap no text",
    plot_heatmap(
      df,
      "boxing",
      "front",
      show_labels = FALSE,
      show_values = FALSE
    )
  )
})

test_that("heatmap with Unspecified row renders consistently", {
  skip_if_not_installed("vdiffr")
  df <- vdiffr_injury_data()
  df <- rbind(
    df,
    data.frame(region_area = "Example", subcategory = "Unspecified", boxing = 7)
  )

  vdiffr::expect_doppelganger(
    "heatmap with unspecified",
    plot_heatmap(df, "boxing", "front")
  )
})
