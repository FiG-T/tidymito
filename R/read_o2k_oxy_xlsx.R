read_o2k_oxy_xlsx <- function(
      input_files = "NULL",
      output_file = "NULL",
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
  o2k_values <- readxl::read_xlsx(
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


  if(length(input_files) != 1) {

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

    for (input in c(2:length(input_files))) {

      o2k_values <- dplyr::bind_rows(
        o2k_values,
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
          dplyr::filter(
            !is.na(.data[[names(protcol_info)[1]]])
          )
        )

    }
  }

  o2k_values <- o2k_values

}
