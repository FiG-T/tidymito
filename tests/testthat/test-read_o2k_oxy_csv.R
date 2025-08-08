test_that("read_o2k_oxy_csv works", {

  # run function
  suppressWarnings(
    testdata <- read_o2k_oxy_csv(
      file_id = "2025-03-03.*.csv",
      directory_path = system.file("extdata", package = "tidymito"),
      exclude_events = c("11As")
    )
  )

  expect_equal(
    c(nrow(testdata), ncol(testdata)), c(19, 4)
  )

})

test_that("read_o2k_oxy_csv works wider ", {

  # run function
  suppressWarnings(
    testdata <- read_o2k_oxy_csv(
      file_id = "2025-03-03.*.csv",
      directory_path = system.file("extdata", package = "tidymito"),
      exclude_events = c("11As"),
      treat_opening = "after",
      window_sizes = 15,
      change_thresholds = 1,
      format_output = TRUE,
      sample_identifiers = c("NDi1-OE x gal4", "NDi1-OE ctrl"),
      info_col = "filename",
      wide_output = FALSE
    )
  )

  expect_equal(
    c(class(testdata), nrow(testdata), ncol(testdata)), c("rowwise_df","tbl_df","tbl","data.frame", 38, 8)
  )

})

test_that("read_o2k_oxy_csv works longer", {

  # run function
  suppressWarnings(
    testdata <- read_o2k_oxy_csv(
      file_id = "2025-03-03.*.csv",
      directory_path = system.file("extdata", package = "tidymito"),
      exclude_events = c("11As"),
      treat_opening = "after",
      window_sizes = 15,
      change_thresholds = 1,
      format_output = TRUE,
      sample_identifiers = c("NDi1-OE x gal4", "NDi1-OE ctrl"),
      info_col = "filename",
      wide_output = TRUE
    )
  )

  expect_equal(
    c(class(testdata), nrow(testdata), ncol(testdata)), c("tbl_df","tbl","data.frame", 2, 25)
  )

})
