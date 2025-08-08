test_that("read_o2k_oxy_xlsx with single input", {

  # run import with single inout file
  testdata <- read_o2k_oxy_xlsx(
    input_files = system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito")
    )

  # check the resulting table has the expected number of rows and columns
  expect_equal(
    c(nrow(testdata), ncol(testdata)), c(8, 28)
  )
})

test_that("read_o2K_oxy_xlsx with multiple inputs", {

  # run import with two input file
  testdata <- read_o2k_oxy_xlsx(
    input_files = c(
      system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito"),
      system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito")
    )
  )

  # check the resulting table has the expected number of rows and columns
  expect_equal(
    c(nrow(testdata), ncol(testdata)), c(16, 28)
  )
})

test_that("read_o2K_oxy_xlsx with multiple mismatched inputs", {

  # check the resulting table has the expected number of rows and columns
  expect_error(
    read_o2k_oxy_xlsx(
      input_files = c(
        system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito"),
        system.file("extdata", "tidymito_example2_o2_data.xlsx", package = "tidymito")
      )
    ),
    "SUIT protocols are not consitent across files"
  )
})

test_that("read_o2K_oxy_xlsx with multiple inputs FCR", {

  # run import with two file but selecting FCR sheet
  testdata <- read_o2k_oxy_xlsx(
    input_files = c(
      system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito"),
      system.file("extdata", "tidymito_example_o2_data.xlsx", package = "tidymito")
    ),
    sheet = "FCR"
  )

  # check the resulting table has the expected number of rows and columns
  expect_equal(
    c(nrow(testdata), ncol(testdata)), c(16, 25)
  )
})
