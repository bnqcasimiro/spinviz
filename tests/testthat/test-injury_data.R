test_that("create_injury_template writes a CSV with the expected structure", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_injury_template(path, sports = c("boxing", "judo"))

  expect_true(file.exists(path))
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("body_region", "body_area", "boxing", "judo"))
  expect_equal(written$body_area, body_categories$body_area)
  expect_equal(written$body_region, body_categories$body_region)
  expect_equal(nrow(written), 19)
  expect_true("Unspecified" %in% written$body_area)
  expect_true(all(is.na(written$boxing)))
  expect_true(all(is.na(written$judo)))
})

test_that("create_injury_template defaults to a single sport column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_injury_template(path)
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("body_region", "body_area", "sport1"))
})

test_that("create_injury_template rejects invalid sports argument", {
  path <- tempfile(fileext = ".csv")
  expect_error(create_injury_template(path, sports = character(0)))
  expect_error(create_injury_template(path, sports = ""))
  expect_error(create_injury_template(path, sports = NA_character_))
})

test_that("read_injury_data reads a filled-in template", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_injury_template(path, sports = "boxing")
  template <- utils::read.csv(path, check.names = FALSE)
  template$body_region <- "Example"
  template$boxing <- seq_len(nrow(template))
  utils::write.csv(template, path, row.names = FALSE)

  data <- read_injury_data(path)
  expect_s3_class(data, "data.frame")
  expect_named(data, c("body_region", "body_area", "boxing"))
  expect_type(data$boxing, "double")
  expect_equal(data$boxing, as.numeric(seq_len(19)))
})

test_that("read_injury_data preserves sport column names with spaces", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_injury_template(path, sports = "water polo")
  data <- read_injury_data(path)
  expect_true("water polo" %in% names(data))
})

test_that("read_injury_data errors for a non-existent file", {
  expect_error(
    read_injury_data(file.path(tempdir(), "no-such-file.csv")),
    "does not exist"
  )
})

test_that("read_injury_data errors when required columns are missing", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(body_region = "Example", boxing = 1),
    path,
    row.names = FALSE
  )
  expect_error(read_injury_data(path), "body_area")
})

test_that("read_injury_data errors when no sport column is present", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(body_region = "Example", body_area = "Head"),
    path,
    row.names = FALSE
  )
  expect_error(read_injury_data(path), "No sport column")
})

test_that("read_injury_data errors on empty body_area values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      body_region = "Example",
      body_area = c("Head", ""),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_error(read_injury_data(path), "body_area")
})

test_that("read_injury_data warns on unrecognised body areas", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      body_region = "Example",
      body_area = c("Head", "Hed", "Thorasic Spine"),
      boxing = c(1, 2, 3)
    ),
    path,
    row.names = FALSE
  )
  expect_warning(read_injury_data(path), "Hed, Thorasic Spine")
})

test_that("read_injury_data accepts Unspecified without warning", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      body_region = c("Head and neck", "Unspecified"),
      body_area = c("Head", "Unspecified"),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_no_warning(read_injury_data(path))
})

test_that("read_injury_data accepts recognised body areas in any case", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      body_region = "Example",
      body_area = c("head", "HIP / GROIN", " thoracic spine "),
      boxing = c(1, 2, 3)
    ),
    path,
    row.names = FALSE
  )
  expect_no_warning(read_injury_data(path))
})

test_that("read_injury_data warns when sport values are not numeric", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(body_region = "Example", body_area = "Head", boxing = "lots"),
    path,
    row.names = FALSE
  )
  expect_warning(read_injury_data(path), "could not be converted to numeric")
})
