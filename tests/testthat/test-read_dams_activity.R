test_that("check that read_dams_activity_csv works", {

  # run script to get DAMS data
  dams_test <- read_dams_activity_csv(
    dir_path = system.file("extdata/dams_test", package = "tidymito"),
    directory_pattern = "DAMS_b.*", # the regex that matches the folder with monitor data
    meta_pattern = ".*meta.*",  # the regex that uniquely matches the file with metadata
    meta_col_1 = "monitor", # the column names in the metadata file
    meta_col_2 = "bloc",
    meta_date_col = 3,  # numeric indicator of where the date values are in the metafile
    bloc_name = "DAMS_b", # the link between the metadata bloc value and folder name.
    numeric_bloc = TRUE
  )

  # run tests on dataframe
  expect_equal(
    c(
      unique(dams_test$monitor),
      unique(dams_test$light),
      unique(dams_test$bloc),
      unique(dams_test$genotype),
      nrow(dams_test),
      ncol(dams_test)
    ),
    c(
      23, 4,
      0, 1,
      1, 2,
      "mX", "tX",
      18264,
      41
    )
  )

})

