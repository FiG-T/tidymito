#' Format Oroboros O2K Output.csv Into Lists
#'
#' `o2k_oxy_to_list()` extracts data from a csv file which has been produced by DatLab and the Oroboros O2K. The output is a list with three elements. Element one is itself a list of two elements (left and right), which contain the raw data and events for the left and right chambers respectively. Element two is a tibble containing details of when the chambers were opened; the third and final element is a vector of unique events from within the run (excluding 'open', 'closed', and 'warnings'). The results of this function are designed so that they can be directly passed into `o2k_oxy_list_to_tbl()`.
#'
#' @param csv A csv file exported from the Oroboros-O2K. This should have six columns ("Time [s]", "Event Name", "Chamber", "Event Text", "1A: O2 flux per V [pmol/(s*mL)]" and "1B: O2 flux per V [pmol/(s*mL)])" ).
#'
#' @returns A list of 3 elements; a list of raw oxygen flux measurements per chamber, opening time details, and unique events.
#' @export
#'
#' @family read o2k files
#'
#' @examples
#'  data <- tidymito::o2k_oxy_to_list(csv = "path/to/data/file.csv")
#'
#'
o2k_oxy_to_list <- function(
      csv = "NULL"
) {

  tmp_csv <- readr::read_csv(
    #file = system.file("extdata", "2025-03-03 P1-01 NDi1-OE x gal4 vs NDi1-OE ctrl.csv", package = "tidymito")
    file = csv,
    show_col_types = FALSE
  )

  # check for WARNING events
  if ( "WARNING" %in% tmp_csv$`Event Name`) {
    warning(
      paste0(
        "WARNING events present in data (e.g: low Oxygen levels). You are advised to check the contents of file: ",
        csv, " before continuing. These events will be disregarded going forwards. \n"
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

  # get unique states
  unique_events <- unique(tmp_data$`Event Name`)[!is.na(unique(tmp_data$`Event Name`))]

  output <- list(chamber_list, openclose, unique_events)
}


#' Extract Mean Oxygen Flux Per State From a List of Chambers
#'
#' This function attempts to automate the extraction of relevant values from tables in the format of the Oroboros-O2K output csv. 'Stable' regions within each stage of the SUIT protocols are identified, with the mean values across these regions returned for downstream analysis. If multiple stable regions are present, the earliest (in terms of run time) will be used.  The requirements for "Stability" can be edited using the `window_size` and `change_threshold` arguments.  Negative values will not be considered.
#'
#' @param chamber_list  A list of tibbles or dataframes containing the raw oxygen flux measurements.  Each element in the list must have at three columns containing information on the time, the event name, and the calibrated oxygen flux readings for a *single* chamber (in this order). Any additional columns will be ignored. Each chamber should be a separate element in the list. This can be generated using `o2k_oxy_to_list()`.
#'
#' @param unique_events A vector of unique events that occur for which a window/mean value is to be calculated. This should not include 'open', 'close', nor 'WARNING'. If two events occur simultaneously, only the second event should be included. e.g.: If "11Tm" is added immediately after "11As", only "11Tm" should be included in this vector. If both events are included, an error will occur and the output will not be returned.
#'
#' @param open_timings A tibble with at least one column named 'time'. Each row should contain the time at which a chamber was opened.
#' @param treat_opening In instances where a chamber is opened during a state, should the reading be taken from "before" or "after" the event? Must be one of these two values.
#' @param window_sizes A numeric value specifying the size of the "window" that must be reached before mean values are calculated. If this window size is not met before the next event the final window of this size will be used and a warning will be returned. Example.: A window size of 15 (the default) means that the oxygen flux value must be stable for 30 seconds (15 x 2 second intervals) for a mean value to be calculated and returned.  If the signal is never stable for 30 seconds (in this example) then the final 30 seconds before the next event will be used.
#'
#' @param change_thresholds A numeric value specifying the tolerance for change between consecutive O2K measurements. Changes less than this value will be considered 'stable'.
#'
#' @returns A tibble with three columns: state, oxygen flux in left chamber, oxygen flux in right chamber.
#'
#' @family read o2k files
#'
#' @export
#'
#' @examples
#'  example_data <- o2k_oxy_list_to_tbl(
#'    chamber_list = test_data[[1]],
#'    unique_events = unique_events,
#'    open_timings = test_data[[2]],
#'    treat_opening = "after",
#'    window_sizes = 15,
#'    change_thresholds = 1
#'    )
#'
#'  # using output from `o2k_oxy_to_list()`:
#'
#'  # get unique events
#'    unique_events <- unique(output_from_o2k_oxy_to_list[[1]][[1]]$event_left)[!is.na(unique(output_from_o2k_oxy_to_list[[1]][[1]]$event_left))]
#'    unique_events <- unique_events[!unique_events %in% c("your_event")]
#'
#'  example_data <- o2k_oxy_list_to_tbl(
#'    chamber_list = output_from_o2k_oxy_to_list[[1]],
#'    unique_events = unique_events,
#'    open_timings = output_from_o2k_oxy_to_list[[2]],
#'    treat_opening = "after",
#'    window_sizes = 15,
#'    change_thresholds = 1
#'    )
#'
o2k_oxy_list_to_tbl <- function(
      chamber_list,
      unique_events,
      open_timings,
      treat_opening = "after",
      window_sizes = 15,
      change_thresholds = 1
){

  # check provided arguments
  if ( ! treat_opening %in% c("after", "before")) {
    stop(
      "Unrecognised argument supplied to `treat_opening`. Value must be one of 'after' or 'before'."
    )
  }

  # check that the window size vector matches the number of states supplied:
  if (length(window_sizes)==1) {

      window_sizes <- rep(window_sizes, length(unique_events))

  } else if (length(window_sizes) != length(unique_events)) {
      stop(
        paste0(
          "The number of window sizes provided must exactly match the number of unique events (", length(unique_events), ") \n"
        )
      )
  }

  # check that the change thresholds vector matches the number of states supplied:
  if (length(change_thresholds)==1) {
    change_thresholds <- rep(change_thresholds, length(unique_events))
  } else if (length(change_thresholds) != length(unique_events)) {
    stop("The number of change_thresholds provided must exactly match the number of unique events")
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

      # specify window size and change threshold for the specific state
      window_size <- window_sizes[event]
      change_threshold <- change_thresholds[event]

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
            paste0("End point of state ", unique_events[event], " reached without threshold being met. Returning average from final window: TREAT WITH CAUTION. You are suggested to adjust either the window size for this state or the change threshold. \n")
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

  names(outout_tibble) <- c("state", "mean_o2_flux_a", "mean_o2_flux_b")

  output <- outout_tibble

}



#' Format Tibble Output From `o2k_oxy_list_to_tbl()` Into More Convenient Structure
#'
#' This function extracts information on run data, O2K machine number and sample IDs from a user-defined column and converts the tibble into a cleaner structure.  This can either be in a 'wide' (one column per state, one row per sample) or 'longer' (each row is one state in one sample) format.
#'
#' @param data A tibble with at least four columns: one with information to extract, one with the respiratory state, and one for each chamber.
#'
#' @param info_col A character string of the name of the column containing the information to extract. Defaults to `filename`.
#'
#' @param sample_identifiers A vector of character strings that can be used to identify a sample. For example: if the samples are "abc - ctrl" (Chamber A) and "xyz - ctrl" (Chamber B), then `sample_identifiers = c("abc - ctrl", "xyz - ctrl")`. *Each identifier must uniquely match a specific sample* and the **specified information column must contain identifiers for both samples: if this is not correct the sole sample ID name will be duplicated for both Chambers.**
#'
#' @param wider Whether to return a wide (one column per state, one row per sample) or longer (each row is one state in one sample) tibble. Must be TRUE or FALSE.
#'
#' @returns A tibble containing formatted oxygen flux values.
#' @export
#'
#' @examples
#'  example_data <-  oxy_tbl_format(
#'    data = output_from_o2k_oxy_list_to_tbl,
#'    info_col = "filename",
#'    sample_identifiers = c("NDi1-OE x gal4", "NDi1-OE ctrl"),
#'    wider = FALSE
#'   )
#'
#'
#'
#'
oxy_tbl_format <- function(
        data,
        info_col = "filename",
        sample_identifiers = "NULL",
        wider = FALSE
){

  # extract standard file identifiers
  tmp_tbl <- data |>
    tidyr::separate_wider_position(
      cols = .data[[info_col]],
      too_many = "drop",
      widths = c("date" = 10, "machine"= 3, blank = 1, run = 2),
      cols_remove = FALSE
    )

  if (sample_identifiers[1] != "NULL") {

    sample_id_regex <- stringr::str_flatten(sample_identifiers, collapse = "|")

    tmp_tbl <- tmp_tbl |>
      dplyr::rowwise() |>
      dplyr::mutate(
        id_a = stringi::stri_extract_first_regex(
          str = .data[[info_col]],
          pattern = sample_id_regex
        ),
        id_b = stringi::stri_extract_last_regex(
          str = .data[[info_col]],
          pattern = sample_id_regex
        )
      )

  } else stop("No sample identifiers supplied")

  tmp_tbl_a <- tmp_tbl |>
    dplyr::select(
      date:state, id = id_a, o2_flux = mean_o2_flux_a
    )
  tmp_tbl_b <- tmp_tbl |>
    dplyr::select(
      date:state, id = id_b, o2_flux = mean_o2_flux_b
    )

  tmp_tbl <- dplyr::bind_rows(
    tmp_tbl_a,
    tmp_tbl_b
  )

  if (wider == TRUE) {
    output_tbl <- tmp_tbl |>
      tidyr::pivot_wider(
        names_from = state,
        values_from = o2_flux
      )
  } else output_tbl <- tmp_tbl

}
