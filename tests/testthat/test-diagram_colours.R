test_that("diagram_colours returns NULL for empty or NULL palette", {
  expect_null(diagram_colours(""))
  expect_null(diagram_colours(NULL))
})

test_that("diagram_colours returns NULL for an unrecognised palette name", {
  expect_null(diagram_colours("not-a-real-palette"))
})

test_that("diagram_colours returns the requested number of viridis colours", {
  cols <- diagram_colours("viridis", n_colours = 5)
  expect_length(cols, 5)
  expect_match(cols, "^#[0-9A-Fa-f]{6,8}$", all = TRUE)
})

test_that("diagram_colours returns the requested number of HCL colours", {
  cols <- diagram_colours("Reds", n_colours = 4)
  expect_length(cols, 4)
  expect_match(cols, "^#[0-9A-Fa-f]{6,8}$", all = TRUE)
})

test_that("diagram_colours defaults n_colours to 10", {
  expect_length(diagram_colours("Reds"), 10)
})

test_that("diagram_colors alias forwards n_colors to diagram_colours", {
  expect_identical(
    spinviz:::diagram_colors("Reds", n_colors = 6),
    diagram_colours("Reds", n_colours = 6)
  )
})
