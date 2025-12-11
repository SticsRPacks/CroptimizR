context("Test the load of the AgMIP protocol description file")

test_that("Test no error in reading a correct Agmip description file", {
  protocol_file_path <- file.path(
    system.file(package = "CroptimizR"), "extdata",
    "Agmip_protocol_example.xlsx"
  )
  expect_no_error(load_protocol_agmip(protocol_file_path))
})
