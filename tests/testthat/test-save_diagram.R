sample_heatmap_plot <- function(view_choice = "front", ...) {
  df <- body_categories
  df$boxing <- c(
    15,
    5,
    18,
    20,
    6,
    14,
    9,
    11,
    12,
    18,
    22,
    10,
    9,
    16,
    13,
    7,
    8,
    3,
    24
  )
  heatmap_diagram(df, "boxing", view_choice, sex = "male", ...)
}

test_that("save_diagram requires width or height", {
  p <- sample_heatmap_plot()
  expect_error(
    save_diagram(p, file = tempfile(fileext = ".png")),
    "at least one of `width` or `height`"
  )
})

test_that("save_diagram rejects unrecognised plot objects", {
  expect_error(
    save_diagram(list(a = 1), file = tempfile(fileext = ".png"), width = 800),
    "doesn't recognise this object's type"
  )
})

test_that("save_diagram errors when a heatmap lacks metadata and view_choice is not given", {
  p <- ggplot2::ggplot()
  expect_error(
    save_diagram(p, file = tempfile(fileext = ".png"), width = 800),
    "no recorded view_choice"
  )
})

test_that("save_diagram saves a heatmap PNG with the derived ratio", {
  p <- sample_heatmap_plot()
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)

  # Front view, labels + legend: ratio = 1.1 / (1.35 + 0.15)
  result <- save_diagram(p, file = out, width = 1500, units = "px")
  expect_true(file.exists(out))
  expect_identical(result, out)

  info <- magick::image_info(magick::image_read(out))
  expect_equal(info$width, 1500)
  expect_equal(info$height, 1500 * 1.1 / 1.5, tolerance = 2)
})

test_that("save_diagram derives width from height and honours show_scale metadata", {
  p <- sample_heatmap_plot(show_scale = FALSE)
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)

  # No legend: ratio = 1.1 / 1.35
  save_diagram(p, file = out, height = 1100, units = "px")
  info <- magick::image_info(magick::image_read(out))
  expect_equal(info$height, 1100)
  expect_equal(info$width, 1100 / (1.1 / 1.35), tolerance = 2)
})

test_that("save_diagram warns and overrides height on a ratio mismatch", {
  p <- sample_heatmap_plot()
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)

  expect_warning(
    save_diagram(p, file = out, width = 1500, height = 300, units = "px"),
    "doesn't match the fixed ratio"
  )
  info <- magick::image_info(magick::image_read(out))
  expect_equal(info$height, 1500 * 1.1 / 1.5, tolerance = 2)
})

test_that("save_diagram saves a both-view heatmap as PDF", {
  p <- sample_heatmap_plot("both")
  out <- tempfile(fileext = ".pdf")
  on.exit(unlink(out), add = TRUE)

  save_diagram(p, file = out, width = 2400, units = "px")
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("save_diagram saves a sunburst as PNG via chromote", {
  skip_if_not_installed("chromote")
  skip_if_not_installed("base64enc")
  # chromote also needs a Chromium-based browser binary
  skip_if(
    Sys.getenv("CHROMOTE_CHROME") == "" &&
      is.null(chromote::find_chrome()),
    "No Chromium-based browser found"
  )

  sb <- sunburst_diagram_default(rep(1, nrow(injury_categories)))
  out <- tempfile(fileext = ".png")
  on.exit(unlink(out), add = TRUE)

  save_diagram(sb, file = out, width = 800)
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("save_diagram rejects unsupported sunburst extensions", {
  sb <- sunburst_diagram_default(rep(1, nrow(injury_categories)))
  expect_error(
    save_diagram(sb, file = tempfile(fileext = ".svg"), width = 800),
    "only supports .jpg/.jpeg/.png/.pdf"
  )
})

test_that("save_diagram errors when a sunburst widget has no size metadata", {
  sb <- sunburst_diagram_default(rep(1, nrow(injury_categories)))
  attr(sb, "spinviz_sunburst_meta") <- NULL
  expect_error(
    save_diagram(sb, file = tempfile(fileext = ".png"), width = 800),
    "no recorded width/height metadata"
  )
})
