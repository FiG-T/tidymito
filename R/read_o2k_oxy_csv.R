#' Import Oxygen Flux Data From One Or More Oroboros-O2K formatted .csv files.
#'
#' @description
#' A wrapper for three other tidymito functions (`o2k_oxy_to_list()`, `o2k_oxy_list_to_tbl()`, and `oxy_tbl_format()`) with added code links allowing a user to go from the .csv file exported from the Oroboros-O2K to a fully processed tibble. This can be applied to a single csv file or multiple files within a single directory, but in all cases will result in a single output tibble.
#'
#' .csv files matching a supplied regex pattern will be identified, 'events' (user defined durin the run, e.g: addition of Proline) will be identified and windows will be automatically selected based on the stability of the oxygen flux readings (completed separately for each state in each chamber). Average values per window will then be formatted and returned.
#'
#' If specified, sample information can be extracted from the .csv file names and added into the output.
#'
#' @param file_id A regex string that identifies the input csv files to process.  Suggested to end in ".csv", by default set to "*.csv" (matching all .csv files in the specified directory).  For more information on regex patterns I recommend the [stringr cheatsheets](https://github.com/rstudio/cheatsheets/blob/main/strings.pdf).
#'
#' @param directory_path A path to the directory containing the relevant csv files. Defaults to the current working directory.
#'
#' @param exclude_events A character vector with any events (e.g: addition of substrates or uncouplers) that you wish to ignore. See [o2k_oxy_list_to_tbl()] for more details. If multiple events occur simultaneously (e.g 11As and 11Tm), all but one should be excluded.
#'
#' @param treat_opening In instances where a chamber is opened during a run, should the window be place before or after this event? Passes argument to [o2k_oxy_list_to_tbl()].
#'
#' @param window_sizes A numeric vector (or vectors) specifying the window size. Defined in terms of instrument recordings (i.e: if the Oroboros returns a reading every 2 seconds, a specified window size of '10' equates to a window in absolute terms of 20seconds.)
#'
#' @param change_thresholds A numeric value specifying the tolerance for change between consecutive O2K measurements. Changes less than this value will be considered 'stable'.
#' @param format_output A TRUE/FALSE statement about whether to neaten and format the output.
#'
#' @param sample_identifiers A vector of character strings that can be used to identify a sample.
#'
#' @param info_col A character string of the name of the column containing the information to extract. Defaults to `filename`.
#'
#' @param wide_output Whether to return a wide (one column per state, one row per sample) or longer (each row is one state in one sample) tibble. Must be TRUE or FALSE.
#'
#' @family read o2k files
#'
#' @returns A tibble (either wide, long, or unformatted)
#' @export
#'
#' @examples
#'  # basic functionality: read in data without specifying any formatting steps or window sizes:
#'  testdata <- read_o2k_oxy_csv(
#'    file_id = "2025-03-03.*.csv", # using provided example script
#'    directory_path = "path/to/data/",
#'    exclude_events = c("11As")
#'    )
#'
#'  # basic functionality: read in data without specifying any formatting steps or window sizes:
#'  testdata <- read_o2k_oxy_csv(
#'    file_id = "2025-03-03.*.csv", # using provided example script
#'    directory_path = "path/to/data/",
#'    exclude_events = c("11As"),  # as this is added at the same time as 11Tm, remove 11As
#'    treat_opening = "after",  # if a chamber is opened during a state, place the window after then chamber has re-oxygenated and closed.
#'    window_sizes = 15,  # if a measurement is taken every 2 seconds, the signal must be stable for 30 (2x15) seconds to pass the cutoff.
#'    change_thresholds = 1,
#'    format_output = TRUE, # select that you want the output tibble to be formatted
#'    sample_identifiers = c("NDi1-OE x gal4", "NDi1-OE ctrl"), # specify your unique sample identifiers
#'    wide_output = FALSE # specify that you want the data to be returned in a 'wide' format.
#'    )
#'
read_o2k_oxy_csv <- function(
      file_id = "*.csv",
      directory_path = "NULL",
      exclude_events = "none",
      treat_opening = "after",
      window_sizes = 15,
      change_thresholds = 1,
      format_output = TRUE,
      sample_identifiers = "NULL",
      info_col = "filename",
      wide_output = FALSE
) {

  # run checks on input
  if (directory_path == "NULL") {
    warning("No directory provided, using current working directory")
    directory <- "."
  }

  if (file_id == "*.csv") warning("No file or files provided. Running on all available csv files")

  # get list of matching files
  input_files <- list.files(
    path = directory_path,
    pattern = file_id
  )

  # create output tibble template
  output_tibble <- tibble::tibble(
    filename = NA,
    state = NA,
    mean_o2_flux_a = NA,
    mean_o2_flux_b = NA
  )

  # start loop across inputs:
  for (file in input_files) {

    # go from raw csv to list of extracted information
    tmp_tbl <- tidymito::o2k_oxy_to_list(
      csv = paste0(directory_path,"/", file)
    )

    if (exclude_events == "none") unique_events <- tmp_tbl[[3]]
      else  unique_events <- tmp_tbl[[3]] [!tmp_tbl[[3]]  %in% exclude_events]

    # go from extracted information to neat tibble
    file_tbl <- tidymito::o2k_oxy_list_to_tbl(
      chamber_list = tmp_tbl[[1]],
      open_timings = tmp_tbl[[2]],
      unique_events = unique_events,
      treat_opening = treat_opening,
      window_sizes = window_sizes,
      change_thresholds = change_thresholds
    )

    # add column with csv filename
    file_tbl$filename <- file

    # reorder columns
    file_tbl <- file_tbl |> dplyr::select(filename, state, mean_o2_flux_a, mean_o2_flux_b)

    # add to big table with data from other files.
    output_tibble <- dplyr::bind_rows(
      output_tibble,
      file_tbl
    )
  }

  # final formatting
  if (format_output == FALSE) {

    # remove columns without a descriptor.
    output_tibble |> dplyr::filter(!is.na(filename))

  } else if (format_output == TRUE) {

    # check that sample identifiers are supplied.
    if (sample_identifiers[1] == "NULL") {

      warning( "No sample identifers supplied, returning unformatted tibble"
      )

      output_tibble |> dplyr::filter(!is.na(filename))

    } else {

      message("Sample identifiers used. Ensure that all filenames have identifiers for both Chambers.")

      # apply formatter function
      output_tibble <- tidymito::oxy_tbl_format(
        data = output_tibble |> dplyr::filter(!is.na(filename)),
        info_col = info_col,
        sample_identifiers = sample_identifiers,
        wider = wide_output
      )
    }

  }

}
