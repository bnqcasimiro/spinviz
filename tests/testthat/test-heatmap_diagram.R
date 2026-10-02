sample_injury_data <- function() {
  subcategory <- c(
    "Head",
    "Neck",
    "Shoulder",
    "Chest",
    "Upper Arm",
    "Elbow",
    "Abdomen",
    "Forearm",
    "Hip Groin",
    "Wrist",
    "Hand",
    "Thigh",
    "Knee",
    "Lower Leg",
    "Ankle",
    "Foot",
    "Thoracic Spine",
    "Lumbosacral"
  )
  data.frame(
    region_area = rep("Example", length(subcategory)),
    subcategory = subcategory,
    boxing = c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)
  )
}

test_that("label_position_lookup shares identical rows for unchanged regions", {
  front <- spinviz:::label_position_lookup("front")
  back <- spinviz:::label_position_lookup("back")

  # These regions use the same coordinates in both views; Head/Neck/
  # Shoulder/Forearm/Hip Groin intentionally differ between views.
  common_regions <- c(
    "Chest",
    "Upper Arm",
    "Elbow",
    "Wrist",
    "Hand",
    "Thigh",
    "Knee",
    "Lower Leg",
    "Ankle",
    "Foot"
  )

  merged <- merge(
    front,
    back,
    by = "region_area",
    suffixes = c(".front", ".back")
  )
  merged <- merged[merged$region_area %in% common_regions, ]
  expect_equal(merged$label_x.front, merged$label_x.back)
  expect_equal(merged$label_y.front, merged$label_y.back)
  expect_equal(merged$target_x.front, merged$target_x.back)
  expect_equal(merged$target_y.front, merged$target_y.back)
})

test_that("label_position_lookup includes view-only regions", {
  front <- spinviz:::label_position_lookup("front")
  back <- spinviz:::label_position_lookup("back")

  expect_true("Abdomen" %in% front$region_area)
  expect_false("Abdomen" %in% back$region_area)
  expect_true(all(c("Thoracic Spine", "Lumbosacral") %in% back$region_area))
  expect_false(any(c("Thoracic Spine", "Lumbosacral") %in% front$region_area))
})

test_that("both_label_position_lookup agrees with view_exclusive_regions", {
  exclusive <- spinviz:::view_exclusive_regions()
  positions <- spinviz:::both_label_position_lookup()

  expect_setequal(
    positions$region_area[is.na(positions$front_target_x)],
    exclusive$back_only
  )
  expect_setequal(
    positions$region_area[is.na(positions$back_target_x)],
    exclusive$front_only
  )
})

test_that("svg_id_lookup and label_position_lookup agree with view_exclusive_regions", {
  exclusive <- spinviz:::view_exclusive_regions()

  svg_front <- spinviz:::svg_id_lookup("front")
  svg_back <- spinviz:::svg_id_lookup("back")
  expect_setequal(
    setdiff(names(svg_front), names(svg_back)),
    exclusive$front_only
  )
  expect_setequal(
    setdiff(names(svg_back), names(svg_front)),
    exclusive$back_only
  )

  label_front <- spinviz:::label_position_lookup("front")
  label_back <- spinviz:::label_position_lookup("back")
  expect_setequal(
    setdiff(label_front$region_area, label_back$region_area),
    exclusive$front_only
  )
  expect_setequal(
    setdiff(label_back$region_area, label_front$region_area),
    exclusive$back_only
  )
})

test_that("heatmap_diagram returns a ggplot for a single view", {
  df <- sample_injury_data()
  p <- heatmap_diagram(df, "boxing", "front", sex = "male", show_values = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("heatmap_diagram returns a patchwork object for 'both' views", {
  df <- sample_injury_data()
  p <- heatmap_diagram(df, "boxing", "both", sex = "female")
  expect_s3_class(p, "patchwork")
})

test_that("heatmap_diagram does not call print() as a side effect", {
  # heatmap_diagram() used to call print() explicitly before returning,
  # which forces a render even when the result is only assigned (causing
  # double rendering in knitr/Quarto documents). Guard against a
  # regression by checking the function body directly.
  body_text <- paste(deparse(body(heatmap_diagram)), collapse = "\n")
  expect_no_match(body_text, "(?<![.[:alnum:]_])print\\(", perl = TRUE)
})

test_that("heatmap_diagram returns a ggplot for a single view without printing", {
  df <- sample_injury_data()
  p <- heatmap_diagram(df, "boxing", "front", show_values = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("heatmap_diagram validates its arguments", {
  df <- sample_injury_data()
  expect_snapshot(error = TRUE, heatmap_diagram(df, "not_a_column", "front"))
  expect_snapshot(error = TRUE, heatmap_diagram(df, "boxing", "sideways"))
  expect_snapshot(
    error = TRUE,
    heatmap_diagram(df, "boxing", "front", opacity = 2)
  )
})

test_that("heatmap_diagram warns when injury values are not numeric", {
  df <- sample_injury_data()
  df$boxing[1] <- "not-a-number"
  expect_snapshot(invisible(heatmap_diagram(
    df,
    "boxing",
    "front",
    show_values = FALSE
  )))
})
