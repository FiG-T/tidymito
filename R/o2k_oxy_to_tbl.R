# get from tempate exported csv format to list of 2 (one tibble per chamber)
o2k_oxy_to_list <- function(
      csv = "NULL"
) {

  tmp_csv <- readr::read_csv(
    #file = system.file("extdata", "2025-03-03 P1-01 NDi1-OE x gal4 vs NDi1-OE ctrl.csv", package = "tidymito")
    file = csv
  )

  # check for WARNING events
  if ( "WARNING" %in% tmp_csv$`Event Name`) {
    warning(
      paste0(
        "WARNING events present in data (e.g: low Oxygen levels). You are advised to check the contents of file: ",
        csv, " before continuing. These events will be disregarded going forwards."
        )
      )
  }

  # remove unwanted events and separate by chamber
  tmp_data <- tmp_csv |>
    dplyr::mutate(
      `Event Name` = dplyr::na_if(`Event Name`, "WARNING"),
      `Event Name` = dplyr::na_if(`Event Name`, "open"),
      `Event Name` = dplyr::na_if(`Event Name`, "close"),
    )  |>
    dplyr::mutate(
      event_left = dplyr::case_when(
        Chamber %in% c("Both", "A", "Left") ~ `Event Name`
      ),
      event_right = dplyr::case_when(
        Chamber %in% c("Both", "B", "Right") ~ `Event Name`
      )
    )  |>
    tidyr::fill(
      event_left, event_right,
      .direction = "down"
    )

  # get tibble of when opening
  openclose <- tmp_csv |>
    dplyr::filter(`Event Name` == "open") |>
    dplyr::select(
      "time"=`Time [s]`, "o2_flux" = dplyr::contains("A:")
    )

  # convert chambers into a list
  chamber_list <- list()

  chamber_list[["left"]] <- tmp_data |>
    dplyr::select(
      "time"=`Time [s]`, event_left, "o2_flux" = dplyr::contains("A:")
    )

  chamber_list[["right"]] <- tmp_data |>
    dplyr::select(
      "time"=`Time [s]`, event_right, "o2_flux" = dplyr::contains("B:")
    )

  output <- list(chamber_list, openclose)
}


for(chamber in test_list) {

  chamber_tmp <- chamber

  print(names(chamber_tmp)[2])

}



o2k_oxy_list_to_tbl <- function(
      chamber_list,
      unique_events,
      open_timings,
      treat_opening = "after",
      window_size = 15,
      change_threshold = 1
){

  if ( ! treat_opening %in% c("after", "before")) {
    stop(
      "Unrecognised argument supplied to `treat_opening`. Value must be one of 'after' or 'before'."
    )
  }

  outout_tibble <- tibble::tibble(
    state = NA,
    meanflux = NA,
    chamber = NA
  )

  for(chamber in chamber_list){

    # treat left and right chamber separately
    chamber_tmp <- chamber

    # make an empty tibble
    chamber_tibble <- tibble::tibble(
      state = NA,
      meanflux = NA
    )

    for (event in seq_along(unique_events)) {

      state_tmp <- chamber_tmp |>
        dplyr::filter(.data[[names(chamber_tmp)[2]]] == unique_events[event] )

      # check if overlapping with open chamber
      if (length(dplyr::intersect(open_timings$time, state_tmp$time)) >= 1) {

        # either select only before the opening
        if (treat_opening == "before") {

          state_tmp <- state_tmp %>%
            dplyr::filter(
              time < dplyr::intersect(open_timings$time, state_tmp$time)
            )
        # or select only after opening and after either the minimum or maximim flux has been reached
        } else if (treat_opening == "after") {

          state_tmp <- state_tmp %>%
            dplyr::filter(
              time > dplyr::intersect(open_timings$time, state_tmp$time)
            )

          zero_point <- state_tmp %>% dplyr::slice_min(abs(o2_flux)) %>% dplyr::pull(time)
          max_point <- state_tmp %>% dplyr::slice_max(o2_flux) %>% dplyr::pull(time)

          if (zero_point < max_point){

            state_tmp <- state_tmp %>%
              dplyr::filter(
                time > zero_point
              )

          } else if ( zero_point > max_point) {

            state_tmp <- state_tmp %>%
              dplyr::filter(
                time > max_point
              )
          }
        }
      }

      # start counter
      counter <- 0

      for (measurement in 2:nrow(state_tmp)) {

        # break loop and take mean if threshold reached
        if (counter == window_size) {

          state_tbl <- tibble::tibble(
            state = unique_events[event],
            meanflux = state_tmp %>%
              dplyr::slice((measurement-window_size):measurement) %>% dplyr::reframe(mean = mean(o2_flux)) %>% dplyr::pull(mean)
          )

          chamber_tibble <- dplyr::bind_rows(
            chamber_tibble,
            state_tbl
          )

          break

        # break loop and return average if end of state limit is reached, add warning
        } else if (measurement == nrow(state_tmp)) {

          state_tbl <- tibble::tibble(
            state = unique_events[event],
            meanflux = state_tmp %>%
              dplyr::slice((measurement-window_size):measurement) %>% dplyr::reframe(mean = mean(o2_flux)) %>% dplyr::pull(mean)
          )

          chamber_tibble <- dplyr::bind_rows(
            chamber_tibble,
            state_tbl
          )

          warning(
            paste0("End point of state ", unique_events[event], " reached without threshold being met. Returning average from final window: TREAT WITH CAUTION. You are suggested to adjust either the window size for this state or the change threshold.")
          )

          break

        } else if (state_tmp$o2_flux[measurement] < 0 ) {

          counter <- 0

        } else { # if counter number has not been reached

          flux_change <- abs(state_tmp$o2_flux[measurement] - state_tmp$o2_flux[measurement-1])

          if ( flux_change < change_threshold) {

            counter <- counter + 1

          } else {

            counter <- 0

          }

        }

      }

    # message that state is complete
      message(paste0(
        unique_events[event], " mean added to output table"
      ))

    }

    chamber_tibble$chamber <- names(chamber_tmp)[2]

    outout_tibble <- dplyr::bind_rows(
      outout_tibble,
      chamber_tibble
    )

  }

  outout_tibble <- outout_tibble |>
    dplyr::filter(!is.na(meanflux)) |>
    tidyr::pivot_wider(
      names_from = chamber,
      values_from = meanflux
  )

  names(outout_tibble) <- c("state", "mean_o2_flux_left", "mean_o2_flux_right")

  output <- outout_tibble

}
