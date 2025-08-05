#' Import oxygen flux data from one or more O2k-formatted excel files
#'
#' @param input_files A character string (or vector of character strings) containing the path(s) to the input file(s). Every input file should follow the same format as the Oroboros template and follow the same SUIT protocol. If paths to multiple files are provided, these will be combined in the final output.
#'
#' @param sheet The sheet in the supplied excel files from which the oxygen flux measurements will be retrieved. By default set to 'Specific flux (bc)'.
#'
#' @returns A single tibble with oxygen flux data.
#'
#' @examples # read_o2k_oxy_xlsx(input_files = tidymito_example_o2_data.xlsx")
#'
#' @export

read_o2k_oxy_xlsx <- function(
      input_files = "NULL",
      sheet = "Specific flux (bc)"
) {

  # Check that input file is specified
  if(input_files[1] == "NULL") {
    stop("No input file provided, no data can be be imported. Please check the file path and try again.")
  }

  # Identify steps in SUIT protocol
  protcol_info <- readxl::read_xlsx(
      path = input_files[1],
      sheet = "Protocol page",
      range = "C12:V13",
      na = "",
      trim_ws = TRUE,
      .name_repair = "unique_quiet"
    ) |>
    dplyr::select(
      ! tidyr::contains(".")
    )

  # load in measurement data
  o2k_values <- dplyr::bind_cols(
    readxl::read_xlsx(
      path = input_files[1],
      sheet = sheet,
      col_names = TRUE,
      skip = 2,
      na = "",
      trim_ws = TRUE,
      .name_repair = "unique_quiet"
    ) |>
      dplyr::select(
        ! dplyr::contains(sheet)
      ) |>
      dplyr::filter(!is.na( `ID#` )) ,
    readxl::read_xlsx(
      path = input_files[1],
      sheet = sheet,
      col_names = TRUE,
      na = "",
      trim_ws = TRUE,
      .name_repair = "unique_quiet"
    )[-c(1,2), ] |>
      dplyr::select(
        tidyr::matches(names(protcol_info))
      ) |>
      dplyr::filter(!is.na(.data[[names(protcol_info)[1]]]))
  )


  if(length(input_files) != 1) {

    # check that SUIT protocols are consitent across files
    for (input in c(2:length(input_files))) {
      protocol_info_2 <- readxl::read_xlsx(
        path = input_files[input],
        sheet = "Protocol page",
        range = "C12:V13",
        na = "",
        trim_ws = TRUE,
        .name_repair = "unique_quiet"
      ) |>
        dplyr::select(
          ! tidyr::contains(".")
        )

      if (identical(names(protcol_info), names(protocol_info_2)) == FALSE) {
        stop("SUIT protocols are not consitent across files")
      } else {
        message("SUIT protocols consistent across files, combining data.")
      }
    }

    # if SUIT protocols are consistent, read and combine the oxygen measurements
    for (input in c(2:length(input_files))) {

      o2k_values <- dplyr::bind_rows(
        o2k_values,
        dplyr::bind_cols(
          readxl::read_xlsx(
            path = input_files[input],
            sheet = sheet,
            col_names = TRUE,
            skip = 2,
            na = "",
            trim_ws = TRUE,
            .name_repair = "unique_quiet"
          ) |>
            dplyr::select(
              ! dplyr::contains(sheet)
            ) |>
            dplyr::filter(!is.na( `ID#` )) ,
          readxl::read_xlsx(
            path = input_files[input],
            sheet = sheet,
            col_names = TRUE,
            na = "",
            trim_ws = TRUE,
            .name_repair = "unique_quiet"
          )[-c(1,2), ] |>
            dplyr::select(
              tidyr::matches(names(protcol_info))
            ) |>
            dplyr::filter(!is.na(.data[[names(protcol_info)[1]]]))
        )
      )
    }

  }

  o2k_values <- o2k_values

}
