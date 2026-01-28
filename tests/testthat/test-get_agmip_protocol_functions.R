test_that("Protocol example and template helper functions work as expected", {
  ## ---- Test get_agmip_protocol_example() ----

  example_path <- get_agmip_protocol_example()

  expect_type(example_path, "character")
  expect_length(example_path, 1)
  expect_true(file.exists(example_path))
  expect_true(grepl("\\.xlsx$", example_path))
  expect_gt(file.size(example_path), 0)

  ## ---- Test get_agmip_protocol_template(): copy works ----

  tmp_dir <- file.path(tempdir(), paste0("croptimizr_test_", Sys.getpid()))
  dir.create(tmp_dir, showWarnings = FALSE, recursive = TRUE)

  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  dest1 <- get_agmip_protocol_template(path = tmp_dir, overwrite = FALSE)

  expect_type(dest1, "character")
  expect_length(dest1, 1)
  expect_true(file.exists(dest1))
  expect_true(grepl("agmip_protocol_template\\.xlsx$", dest1))
  expect_gt(file.size(dest1), 0)

  ## ---- Test: error if file exists and overwrite = FALSE ----

  expect_error(
    get_agmip_protocol_template(path = tmp_dir, overwrite = FALSE),
    "already exists"
  )

  ## ---- Test: overwrite = TRUE works ----

  info1 <- file.info(dest1)
  Sys.sleep(1) # ensure mtime can change on most filesystems

  dest2 <- get_agmip_protocol_template(path = tmp_dir, overwrite = TRUE)
  info2 <- file.info(dest2)

  expect_identical(dest1, dest2)
  expect_true(info2$mtime >= info1$mtime)
})
