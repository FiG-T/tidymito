
test_that( "o2k_oxy_to_list", {

  test_data <- tidymito::o2k_oxy_to_list(
    csv = system.file("extdata", "2025-03-03 P1-01 NDi1-OE x gal4 vs NDi1-OE ctrl.csv", package = "tidymito")
  )

  expect_equal(
    c(class(test_data), length(test_data)), c("list", 3)
  )

})


test_that( "o2k_oxy_list_to_tbl", {

  # using test data
  test_data <- tidymito::o2k_oxy_to_list(
    csv = system.file("extdata", "2025-03-03 P1-01 NDi1-OE x gal4 vs NDi1-OE ctrl.csv", package = "tidymito")
  )

  # format unique events
  unique_events <- unique(test_data[[1]][[1]]$event_left)[!is.na(unique(test_data[[1]][[1]]$event_left))]
  unique_events <- unique_events[!unique_events %in% c("11As")]

  expect_warning(
    test_data2 <- o2k_oxy_list_to_tbl(
      chamber_list = test_data[[1]],
      unique_events = unique_events,
      open_timings = test_data[[2]],
      treat_opening = "after",
      window_sizes = 15,
      change_thresholds = 1
    ),
    "End point of state 4Pro reached without threshold being met. Returning average from final window: TREAT WITH CAUTION. You are suggested to adjust either the window size for this state or the change threshold."
  )

  expect_equal(
    c(class(test_data2),nrow(test_data2), ncol(test_data2)), c("tbl_df", "tbl", "data.frame" , 19, 3)
  )

})



