test_that("create_sunburst_template writes a CSV with the expected structure", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, sports = c("boxing", "judo"))

  expect_true(file.exists(path))
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("tissue", "pathology", "boxing", "judo"))
  expect_equal(written$tissue, spinviz::injury_categories$tissue)
  expect_equal(written$pathology, spinviz::injury_categories$pathology)
  expect_equal(nrow(written), nrow(spinviz::injury_categories))
  expect_true(all(is.na(written$boxing)))
  expect_true(all(is.na(written$judo)))
})

test_that("create_sunburst_template defaults to a single sport column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path)
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("tissue", "pathology", "sport1"))
})

test_that("create_sunburst_template rejects invalid sports argument", {
  path <- tempfile(fileext = ".csv")
  expect_error(create_sunburst_template(path, sports = character(0)))
  expect_error(create_sunburst_template(path, sports = ""))
  expect_error(create_sunburst_template(path, sports = NA_character_))
})

test_that("create_sunburst_template/read_sunburst_data round-trip a filled-in file", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, sports = "boxing")
  template <- utils::read.csv(path, check.names = FALSE)
  template$boxing <- seq_len(nrow(template))
  utils::write.csv(template, path, row.names = FALSE)

  data <- read_sunburst_data(path)
  expect_s3_class(data, "data.frame")
  expect_named(data, c("tissue", "pathology", "boxing"))
  expect_type(data$boxing, "double")
  expect_equal(
    data$boxing,
    as.numeric(seq_len(nrow(spinviz::injury_categories)))
  )
  expect_equal(data$tissue, spinviz::injury_categories$tissue)
  expect_equal(data$pathology, spinviz::injury_categories$pathology)
})

test_that("read_sunburst_data preserves sport column names with spaces", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, sports = "water polo")
  data <- read_sunburst_data(path)
  expect_true("water polo" %in% names(data))
})

test_that("read_sunburst_data carries blank tissue cells down", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  utils::write.csv(
    data.frame(
      tissue = c("Bone", "", "", "Nervous", ""),
      pathology = c(
        "Fracture", "Bone contusion", "Physis injury",
        "Peripheral nerve injury", "Brain or spinal cord injury"
      ),
      boxing = c(1, 2, 3, 4, 5)
    ),
    path,
    row.names = FALSE
  )

  data <- read_sunburst_data(path)
  expect_equal(
    data$tissue,
    c("Bone", "Bone", "Bone", "Nervous", "Nervous")
  )
})

test_that("read_sunburst_data errors when the first tissue cell is blank", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  utils::write.csv(
    data.frame(
      tissue = c("", "Bone"),
      pathology = c("Fracture", "Bone contusion"),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "first row")
})

test_that("read_sunburst_data errors for a non-existent file", {
  expect_error(
    read_sunburst_data(file.path(tempdir(), "no-such-file.csv")),
    "does not exist"
  )
})

test_that("read_sunburst_data errors when required columns are missing", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(tissue = "Bone", boxing = 1),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "pathology")
})

test_that("read_sunburst_data errors when no sport column is present", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(tissue = "Bone", pathology = "Fracture"),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "No sport column")
})

test_that("read_sunburst_data errors on empty pathology values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      tissue = c("Bone", "Bone"),
      pathology = c("Fracture", ""),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "pathology")
})

test_that("read_sunburst_data warns on unrecognised tissue/pathology values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      tissue = c("Bone", "Boan"),
      pathology = c("Fracture", "Fractuer"),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_warning(
    expect_warning(read_sunburst_data(path), "Boan"),
    "Fractuer"
  )
})

test_that("read_sunburst_data accepts recognised values without warning", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      tissue = c(" Bone ", "Nervous"),
      pathology = c(" Fracture", "Tendinopathy"),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  # Tendinopathy is not a Nervous pathology, but validation is per-column,
  # so only the misspelling warning matters here.
  expect_no_warning(read_sunburst_data(path))
})

test_that("read_sunburst_data warns when sport values are not numeric", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(tissue = "Bone", pathology = "Fracture", boxing = "lots"),
    path,
    row.names = FALSE
  )
  expect_warning(read_sunburst_data(path), "could not be converted to numeric")
})
