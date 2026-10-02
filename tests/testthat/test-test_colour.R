test_that("test_colour runs without error for a named HCL palette", {
  expect_no_error(test_colour("Greens"))
})

test_that("test_colour runs without error for a named viridis palette", {
  expect_no_error(test_colour("viridis"))
})

test_that("test_colour runs without error for a custom hex palette", {
  expect_no_error(test_colour(c("#FFFFFF", "#000000")))
})

test_that("test_colour runs without error for a list of mixed palettes", {
  expect_no_error(test_colour(list("Reds", c("#FFFFFF", "#000000"))))
})

test_that("test_colour restores the caller's par() settings", {
  old_par <- graphics::par(no.readonly = TRUE)
  test_colour("Reds")
  new_par <- graphics::par(no.readonly = TRUE)
  expect_identical(new_par$mfrow, old_par$mfrow)
})
