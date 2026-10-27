sample_injury_data <- function() {
  body_area <- c(
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
  )
  data.frame(
    body_region = rep("Example", length(body_area)),
    body_area = body_area,
    boxing = c(15, 5, 18, 12, 20, 6, 10, 14, 9, 9, 11, 3, 16, 13, 7, 8, 18, 22)
  )
}

test_that("label_position_lookup shares identical rows for unchanged regions", {
  front <- spinviz:::label_position_lookup("front")
  back <- spinviz:::label_position_lookup("back")

  # These regions use the same coordinates in both views; Head/Neck/
  # Shoulder/Forearm/Hip / groin intentionally differ between views.
  common_regions <- c(
    "Chest",
    "Upper arm",
    "Elbow",
    "Wrist",
    "Hand",
    "Thigh",
    "Knee",
    "Lower leg",
    "Ankle",
    "Foot"
  )

  merged <- merge(
    front,
    back,
    by = "body_region",
    suffixes = c(".front", ".back")
  )
  merged <- merged[merged$body_region %in% common_regions, ]
  expect_equal(merged$label_x.front, merged$label_x.back)
  expect_equal(merged$label_y.front, merged$label_y.back)
  expect_equal(merged$target_x.front, merged$target_x.back)
  expect_equal(merged$target_y.front, merged$target_y.back)
})

test_that("label_position_lookup includes view-only regions", {
  front <- spinviz:::label_position_lookup("front")
  back <- spinviz:::label_position_lookup("back")

  expect_true("Abdomen" %in% front$body_region)
  expect_false("Abdomen" %in% back$body_region)
  expect_true(all(c("Thoracic spine", "Lumbosacral") %in% back$body_region))
  expect_false(any(c("Thoracic spine", "Lumbosacral") %in% front$body_region))
})

test_that("both_label_position_lookup agrees with view_exclusive_regions", {
  exclusive <- spinviz:::view_exclusive_regions()
  positions <- spinviz:::both_label_position_lookup()

  expect_setequal(
    positions$body_region[is.na(positions$front_target_x)],
    exclusive$back_only
  )
  expect_setequal(
    positions$body_region[is.na(positions$back_target_x)],
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
    setdiff(label_front$body_region, label_back$body_region),
    exclusive$front_only
  )
  expect_setequal(
    setdiff(label_back$body_region, label_front$body_region),
    exclusive$back_only
  )
})

test_that("plot_heatmap returns a ggplot for a single view", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior", sex = "male", show_values = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap returns a patchwork object for 'both' views", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "both", sex = "female")
  expect_s3_class(p, "patchwork")
})

test_that("plot_heatmap does not call print() as a side effect", {
  # plot_heatmap() used to call print() explicitly before returning,
  # which forces a render even when the result is only assigned (causing
  # double rendering in knitr/Quarto documents). Guard against a
  # regression by checking the function body directly.
  body_text <- paste(deparse(body(plot_heatmap)), collapse = "\n")
  expect_no_match(body_text, "(?<![.[:alnum:]_])print\\(", perl = TRUE)
})

test_that("plot_heatmap returns a ggplot for a single view without printing", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior", show_values = FALSE)
  expect_s3_class(p, "ggplot")
})

test_that("plot_heatmap validates its arguments", {
  df <- sample_injury_data()
  expect_snapshot(error = TRUE, plot_heatmap(df, "not_a_column", "anterior"))
  expect_snapshot(error = TRUE, plot_heatmap(df, "boxing", "sideways"))
  expect_snapshot(
    error = TRUE,
    plot_heatmap(df, "boxing", "anterior", opacity = 2)
  )
})

# Helpers to pull the geom_text label data out of a built plot
text_layer_data <- function(p) {
  is_text <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomText"),
    logical(1)
  )
  p$layers[[which(is_text)[1]]]$data
}

test_that("plot_heatmap rejects an invalid value_format", {
  df <- sample_injury_data()
  expect_error(
    plot_heatmap(df, "boxing", "anterior", value_format = "fraction"),
    "should be one of"
  )
})

test_that("plot_heatmap percentage view uses a percentage legend and labels", {
  df <- sample_injury_data()

  p_count <- plot_heatmap(df, "boxing", "anterior", show_labels = FALSE)
  p_pct <- plot_heatmap(
    df,
    "boxing",
    "anterior",
    value_format = "proportion",
    show_labels = FALSE
  )

  expect_equal(p_count$scales$scales[[1]]$name, "Injuries (n)")
  expect_equal(p_pct$scales$scales[[1]]$name, "Injuries (%)")

  pct_labels <- text_layer_data(p_pct)$label_text
  expect_true(all(grepl("%", pct_labels, fixed = TRUE)))
  # Percentages should sum to ~100 across regions
  values <- as.numeric(sub('.*"([0-9.]+)%".*', "\\1", pct_labels))
  expect_equal(sum(values), 100, tolerance = 0.5)
})

test_that("plot_heatmap percentage labels are italic-n free and counts use italic n", {
  df <- sample_injury_data()
  p_count <- plot_heatmap(df, "boxing", "anterior", show_labels = FALSE)
  count_labels <- text_layer_data(p_count)$label_text
  expect_true(all(grepl("italic(n)", count_labels, fixed = TRUE)))
})

test_that("plot_heatmap shows an Unspecified row as a label", {
  df <- sample_injury_data()
  df <- rbind(
    df,
    data.frame(body_region = "Example", body_area = "Unspecified", boxing = 7)
  )

  p <- plot_heatmap(df, "boxing", "anterior")
  labels <- text_layer_data(p)$label_text
  expect_true(any(grepl("Unspecified", labels, fixed = TRUE)))
  # Unspecified has no leader-line target
  ld <- text_layer_data(p)
  expect_true(is.na(ld$target_y[ld$body_region == "Unspecified"]))
})

test_that("plot_heatmap omits the Unspecified label when no such row exists", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior")
  labels <- text_layer_data(p)$label_text
  expect_false(any(grepl("Unspecified", labels, fixed = TRUE)))
})

test_that("plot_heatmap percentage total includes Unspecified injuries", {
  df <- sample_injury_data()
  df_uns <- rbind(
    df,
    data.frame(
      body_region = "Example",
      body_area = "Unspecified",
      boxing = 96
    )
  )
  p <- plot_heatmap(
    df_uns,
    "boxing",
    "anterior",
    value_format = "proportion",
    show_labels = FALSE
  )
  labels <- text_layer_data(p)$label_text
  values <- as.numeric(sub('.*"([0-9.]+)%".*', "\\1", labels))
  # Denominator includes the 96 unspecified injuries
  expect_equal(sum(values), 100, tolerance = 0.5)
})

test_that("plot_heatmap labels are spread to a minimum vertical gap", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior")
  ys <- sort(text_layer_data(p)$label_y, decreasing = TRUE)
  gaps <- -diff(ys)
  expect_true(all(gaps >= 0.055 - 1e-9))
})

test_that("plot_heatmap draws two leader-line segments per labelled region", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior")
  is_segment <- vapply(
    p$layers,
    function(l) inherits(l$geom, "GeomSegment"),
    logical(1)
  )
  # One horizontal (label -> bend) and one angled (bend -> body) layer
  expect_equal(sum(is_segment), 2)
  n_targets <- sum(!is.na(spinviz:::label_position_lookup("front")$target_y))
  for (l in p$layers[is_segment]) {
    expect_equal(nrow(l$data), n_targets)
  }
})

test_that("plot_heatmap count legend breaks are integers", {
  df <- sample_injury_data()
  p <- plot_heatmap(df, "boxing", "anterior", show_labels = FALSE)
  scale <- p$scales$scales[[1]]
  breaks <- scale$breaks(c(0, 22))
  expect_equal(breaks, as.integer(breaks))
  expect_equal(breaks, unique(breaks))
  # Percentage scale keeps fractional breaks and % labels
  p_pct <- plot_heatmap(
    df,
    "boxing",
    "anterior",
    value_format = "proportion",
    show_labels = FALSE
  )
  scale_pct <- p_pct$scales$scales[[1]]
  expect_equal(scale_pct$labels(c(0, 25, 50)), c("0%", "25%", "50%"))
})

test_that("plot_heatmap records spinviz_heatmap_meta for save_diagram", {
  df <- sample_injury_data()
  p_front <- plot_heatmap(df, "boxing", "anterior", show_scale = FALSE)
  meta <- attr(p_front, "spinviz_heatmap_meta")
  expect_equal(meta$view, "anterior")
  expect_false(meta$show_scale)

  p_both <- plot_heatmap(df, "boxing", "both")
  meta_both <- attr(p_both, "spinviz_heatmap_meta")
  expect_equal(meta_both$view, "both")
  expect_true(meta_both$show_scale)
})

test_that("plot_heatmap handles all-zero and all-NA data without error", {
  df <- sample_injury_data()
  df$boxing <- 0
  expect_no_error(plot_heatmap(
    df,
    "boxing",
    "anterior",
    value_format = "proportion"
  ))
  df$boxing <- NA_real_
  expect_no_error(plot_heatmap(df, "boxing", "anterior"))
})

test_that("plot_heatmap warns when injury values are not numeric", {
  df <- sample_injury_data()
  df$boxing[1] <- "not-a-number"
  expect_snapshot(invisible(plot_heatmap(
    df,
    "boxing",
    "anterior",
    show_values = FALSE
  )))
})

test_that("plot_heatmap renders the Unspecified count for both views", {
  df <- rbind(
    sample_injury_data(),
    data.frame(
      body_region = "Unspecified",
      body_area = "Unspecified",
      boxing = 7
    )
  )

  for (view in c("anterior", "posterior")) {
    p <- plot_heatmap(df, "boxing", view)
    expect_s3_class(p, "ggplot")
    labels <- unlist(lapply(ggplot2::ggplot_build(p)$data, function(layer) {
      if ("label" %in% names(layer)) as.character(layer$label) else NULL
    }))
    expect_true(
      any(grepl("Unspecified", labels)),
      info = paste("view:", view)
    )
    expect_true(any(grepl("7", labels[grepl("Unspecified", labels)])))
  }
})
