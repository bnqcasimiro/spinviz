sample_sunburst_data <- function() {
  data.frame(
    tissue = c(
      "Muscle / Tendon",
      "Muscle / Tendon",
      "Bone",
      "Bone",
      "Unspecified"
    ),
    pathology = c(
      "Muscle strain",
      "Tendon rupture",
      "Fracture",
      "Bone contusion",
      "Unspecified"
    ),
    boxing = c(20, 10, 16, 6, 4),
    judo = c(12, 8, 20, 3, 2),
    stringsAsFactors = FALSE
  )
}

test_that("sunburst_diagram_echarts returns an htmlwidget with recorded size metadata", {
  df <- sample_sunburst_data()
  sb <- sunburst_diagram_echarts(df, "boxing")
  expect_s3_class(sb, "htmlwidget")

  meta <- attr(sb, "spinviz_sunburst_meta")
  expect_equal(meta$widget_width, "1200px")
  expect_equal(meta$widget_height, "1500px")

  sb2 <- sunburst_diagram_echarts(
    df,
    "boxing",
    widget_width = "800px",
    widget_height = "600px"
  )
  meta2 <- attr(sb2, "spinviz_sunburst_meta")
  expect_equal(meta2$widget_width, "800px")
  expect_equal(meta2$widget_height, "600px")
})

test_that("sunburst_diagram_echarts defaults to the third column when column_name is empty", {
  df <- sample_sunburst_data()
  # Should not error; "boxing" is the third column
  expect_no_error(sunburst_diagram_echarts(df))
})

test_that("sunburst_diagram_echarts drops the pathology ring for depth = 1", {
  df <- sample_sunburst_data()
  sb_full <- sunburst_diagram_echarts(df, "boxing")
  sb_depth1 <- sunburst_diagram_echarts(df, "boxing", depth = 1)
  expect_length(sb_full$x$opts$series, 2)
  expect_length(sb_depth1$x$opts$series, 1)
})

test_that("sunburst_diagram_echarts can exclude non-specific tissue rows", {
  df <- sample_sunburst_data()
  sb <- sunburst_diagram_echarts(df, "boxing", include_non_specific = FALSE)
  labels <- vapply(sb$x$opts$series[[1]]$data, `[[`, "", "name")
  expect_false(any(grepl("unspecified", labels, ignore.case = TRUE)))

  everything_unspecified <- df[df$tissue == "Unspecified", ]
  expect_error(
    sunburst_diagram_echarts(
      everything_unspecified,
      "boxing",
      include_non_specific = FALSE
    ),
    "No rows remain"
  )
})

test_that("sunburst_diagram_echarts validates its input data", {
  df <- sample_sunburst_data()

  expect_error(
    sunburst_diagram_echarts(df[, 1:2]),
    "less than 3 columns"
  )
  expect_error(
    sunburst_diagram_echarts(df, "not_a_column"),
    "Column name does not exist"
  )

  df_zero <- df
  df_zero$boxing <- 0
  expect_error(sunburst_diagram_echarts(df_zero, "boxing"), "Sum of column")
})

test_that("sunburst_diagram_echarts warns and substitutes 0 for missing counts", {
  df <- sample_sunburst_data()
  df$boxing[1] <- NA
  expect_warning(
    sunburst_diagram_echarts(df, "boxing"),
    "null values found"
  )
})

test_that("sunburst_diagram_default returns an htmlwidget for valid counts", {
  counts <- rep(1, nrow(injury_categories))
  sb <- sunburst_diagram_default(counts)
  expect_s3_class(sb, "htmlwidget")
})

test_that("sunburst_diagram_default validates the counts vector", {
  expect_error(
    sunburst_diagram_default(1:5),
    "numeric vector of exactly"
  )
  expect_error(
    sunburst_diagram_default(rep("a", nrow(injury_categories))),
    "numeric vector of exactly"
  )
})
