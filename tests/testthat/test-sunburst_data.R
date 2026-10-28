test_that("create_sunburst_template writes a CSV with the expected structure", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, group = c("boxing", "judo"))

  expect_true(file.exists(path))
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("tissue_type", "pathology_type", "boxing", "judo"))
  expect_equal(written$tissue_type, spinviz::injury_categories$tissue_type)
  expect_equal(written$pathology_type, spinviz::injury_categories$pathology_type)
  expect_equal(nrow(written), nrow(spinviz::injury_categories))
  expect_true(all(is.na(written$boxing)))
  expect_true(all(is.na(written$judo)))
})

test_that("create_sunburst_template defaults to a single group column", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path)
  written <- utils::read.csv(path, check.names = FALSE)
  expect_named(written, c("tissue_type", "pathology_type", "group1"))
})

test_that("create_sunburst_template rejects invalid group argument", {
  path <- tempfile(fileext = ".csv")
  expect_error(create_sunburst_template(path, group = character(0)))
  expect_error(create_sunburst_template(path, group = ""))
  expect_error(create_sunburst_template(path, group = NA_character_))
})

test_that("create_sunburst_template/read_sunburst_data round-trip a filled-in file", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, group = "boxing")
  template <- utils::read.csv(path, check.names = FALSE)
  template$boxing <- seq_len(nrow(template))
  utils::write.csv(template, path, row.names = FALSE)

  data <- read_sunburst_data(path)
  expect_s3_class(data, "data.frame")
  expect_named(data, c("tissue_type", "pathology_type", "boxing"))
  expect_type(data$boxing, "double")
  expect_equal(
    data$boxing,
    as.numeric(seq_len(nrow(spinviz::injury_categories)))
  )
  expect_equal(data$tissue_type, spinviz::injury_categories$tissue_type)
  expect_equal(data$pathology_type, spinviz::injury_categories$pathology_type)
})

test_that("read_sunburst_data preserves group column names with spaces", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  create_sunburst_template(path, group = "water polo")
  data <- read_sunburst_data(path)
  expect_true("water polo" %in% names(data))
})

test_that("read_sunburst_data carries blank tissue_type cells down", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  utils::write.csv(
    data.frame(
      tissue_type = c("Bone", "", "", "Nervous", ""),
      pathology_type = c(
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
    data$tissue_type,
    c("Bone", "Bone", "Bone", "Nervous", "Nervous")
  )
})

test_that("read_sunburst_data errors when the first tissue_type cell is blank", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))

  utils::write.csv(
    data.frame(
      tissue_type = c("", "Bone"),
      pathology_type = c("Fracture", "Bone contusion"),
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
    data.frame(tissue_type = "Bone", boxing = 1),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "pathology_type")
})

test_that("read_sunburst_data errors when no group column is present", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(tissue_type = "Bone", pathology_type = "Fracture"),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "No group column")
})

test_that("read_sunburst_data errors on empty pathology_type values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      tissue_type = c("Bone", "Bone"),
      pathology_type = c("Fracture", ""),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  expect_error(read_sunburst_data(path), "pathology_type")
})

test_that("read_sunburst_data warns on unrecognised tissue_type/pathology_type values", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(
      tissue_type = c("Bone", "Boan"),
      pathology_type = c("Fracture", "Fractuer"),
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
      tissue_type = c(" Bone ", "Nervous"),
      pathology_type = c(" Fracture", "Tendinopathy"),
      boxing = c(1, 2)
    ),
    path,
    row.names = FALSE
  )
  # Tendinopathy is not a Nervous pathology_type, but validation is per-column,
  # so only the misspelling warning matters here.
  expect_no_warning(read_sunburst_data(path))
})

test_that("read_sunburst_data warns when group values are not numeric", {
  path <- tempfile(fileext = ".csv")
  on.exit(unlink(path))
  utils::write.csv(
    data.frame(tissue_type = "Bone", pathology_type = "Fracture", boxing = "lots"),
    path,
    row.names = FALSE
  )
  expect_warning(read_sunburst_data(path), "could not be converted to numeric")
})
