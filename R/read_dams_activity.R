#' Convert Raw Drosophila Activity Monitor Outputs Into Tibble
#'
#' @description
#' Drosophila Activity Monitors (a.k.a DAMs) (TriKinetics) are a valuable tool for assessing the patterns of movement in fly studies. This function aims to automate the process of extracting information from raw the datafiles produced by the instrument, removing the need for tedious (and potentially incorrect) copy pasting of data into excel files.
#'
#' @param dir_path The path to the directory that holds your directories with your data of interest. By default set to the home directory.
#' @param directory_pattern A regex string that matches each of the directories which contain the data of interest. By default set to "DAMS_b.*".
#' @param meta_pattern A regex string that *uniquely* matches the metadata file.
#' @param meta_col_1 A character string with the name of the first column in the metadata file. Defaults to ".\*meta.\*" (i.e will match any file with "meta" somewhere in the name.)
#' @param meta_col_2 A character string with the name of the second column in the metadata file. Defaults to "bloc".
#' @param meta_date_col A single numeric value indicating the column position containing date values in the metadata file.
#' @param bloc_name A regex string that links the metadata bloc value and the folder name.
#' @param numeric_bloc A boolean value stating whether the bloc identifier values are numeric (TRUE; the default, recommended) or not (FALSE).
#'
#' @returns A single tibble containing count data for every Drosophila Activity Monitor listed.
#' @export
#'
#' @examples
#'  #
#'  # read_dams_activity_csv(
#'  #   "path/to/directory",
#'  #   "folder_name",
#'  #   ".*metafile.identifier.*",
#'  #   "monitor",
#'  #   "bloc",
#'  #   3,
#'  #   "DAMS_b)
#'
#'
#'
read_dams_activity_csv <- function(
      dir_path = ".",
      directory_pattern = "DAMS_b.*", # the regex that matches the folder with monitor data
      meta_pattern = ".*meta.*",  # the regex that uniquely matches the file with metadata
      meta_col_1 = "monitor", # the column names in the metadata file
      meta_col_2 = "bloc",
      meta_date_col = 3,  # numeric indicator of where the date values are in the metafile
      bloc_name = "DAMS_b", # the link between the metadata bloc value and folder name.
      numeric_bloc = TRUE
){

  # list relevant files
  dam_blocks <- list.files(
    path = dir_path,
    pattern = directory_pattern
  )

  # intiate a list
  monitor_list <- list()

  # create a list of paths to Monitor123.txt files
  for (i in dam_blocks){
    monitor_list[[i]] <- list.files(
      path = paste(
        dir_path,
        i,
        sep = "/"
      )
    )

    # report back on progress
    message(
      paste(
        "Monitors from",
        i,
        "added to list."
      )
    )
  }

  # read in metadata file
  monitor_info <- readxl::read_excel(
    path = paste0(
      dir_path,
      "/",
      list.files(
        path = dir_path,
        pattern = meta_pattern
      )
    ),
    col_names = TRUE
  )

  message(
    paste(
      "Metadata from ",
      list.files(
        path = dir_path,
        pattern = meta_pattern
      ),
      " successfully imported."
    )
  )

  # convert to date type
  monitor_info$date <- lubridate::dmy(monitor_info$date)

  # determine the number of blocs
  blocs <- unique(monitor_info$bloc)

  # initiate list
  monitor_included <- list()

  for (j in blocs) {

    filter_table <- monitor_info |>
      dplyr::filter(bloc == j)

    # separate each monitor in each bloc into a separate variable in a list
    monitor_included[[j]] <- unique(filter_table$monitor)

    monitor_included[[j]] <- stringr::str_c(
      "Monitor", monitor_included[[j]], ".txt"
    )
  }

  # name the components in this list
  names(monitor_included) <- stringr::str_c(
    bloc_name, blocs, sep = ""
  )

  for( i in seq_along(monitor_list)) {

    # only include monitors in list that are also included on the metadata file
    monitor_list[[i]] <- monitor_list[[i]][
      monitor_list[[i]] %in% monitor_included[[i]]
    ]
  }

  # initiate list
  monitor_path_list <- list()

  message("Combining activity and metadata...")

  # import and combine activity and metadata
  for (i in seq_along(monitor_list)) {

    bloc <- names(monitor_list)[i]

    bloc_path <- paste(
      dir_path,
      bloc,
      sep = "/"
    )

    # for each monitor in a bloc
    for(k in seq_along(monitor_list[[bloc]])) {

      monitor <- monitor_list[[bloc]][k]

      monitor_path <- paste(
        bloc_path,
        monitor,
        sep = "/"
      )

      monitor_path_list[[bloc]][k] <- monitor_path
    }
  }

  # remove monitors that are not included in the list of paths
  monitor_list <- monitor_list[
    names(monitor_list) %in% names(monitor_path_list) == TRUE
  ]

  # initiate loop through directories containing monitor files
  for (i in seq_along(monitor_path_list)) {

    path_lists <- monitor_path_list[[i]]

    monitor <- monitor_list[[i]]

    if (numeric_bloc == TRUE){
      # remove letters and punctuation from folder name
      bloc <- stringr::str_replace_all(
        string = names(monitor_path_list[i]),
        pattern = "[:alpha:]|[:punct:]",
        replacement = ""
      )
    } else {
      bloc <- names(monitor_path_list[i])
    }

    # loop through paths within a folder
    for (k in seq_along(path_lists)) {

      message(path_lists[k])

      tmp_table <- readr::read_tsv(
        file = path_lists[k],
        col_names = FALSE,
        show_col_types = FALSE
      )

      tmp_table <- tmp_table[,c(1:4,10:42)]

      names(tmp_table) <- c(
        "index", "date", "time", "status", "light",
        stringr::str_c(
          "chamber_",
          c(1:32),
          sep = ""
        )
      )

      message(
        paste(
          "Activity data imported from ",
          path_lists[k]
        )
      )

      tmp_table$monitor <- stringr::str_replace_all(
        string = monitor[k] ,
        pattern = "[:alpha:]|[:punct:]",
        replacement = ""
      )

      tmp_table$monitor <- as.numeric(tmp_table$monitor)

      if (numeric_bloc == TRUE){
        tmp_table$bloc <- as.numeric(bloc)
      }

      tmp_table$date <- lubridate::dmy(tmp_table$date)

      # combine with metadata
      tmp_table <- dplyr::left_join(
        x = tmp_table,
        y = monitor_info,
        by = dplyr::join_by(
          monitor == "monitor",
          bloc == "bloc"
        )
      )

      # start date taken from metafile
      start_date <- unique(tmp_table$date.y)

      tmp_table <- tmp_table |>
        dplyr::filter(
          date.x >= unique(tmp_table$date.y)
        )

      message(
        paste(
          "Metadata successfully combined with activity data"
        )
      )

      if (i == 1 & k == 1) {

        dams_table <- tmp_table
        message("DAMs table created")

      } else {
        dams_table <- dplyr::bind_rows(
          dams_table,
          tmp_table
        )

        message(
          "Information added to DAMs table"
        )
      }

    } # close loop through file
  }   # close loop through folders

  dams_table <- dams_table |>
    dplyr::select(
      index:light, names(monitor_info)[-meta_date_col],
      chamber_1:chamber_32
    )

  names(dams_table)[2] <- "date"

  message(
    "--------------------------------------------------------------------------------
    --------------------------------------------------------------------------------
    Operation complete - all files imported successfully
    --------------------------------------------------------------------------------
    --------------------------------------------------------------------------------"
  )
  output <- dams_table

} # close function
