#' Reads RE-INTEGRATE datasets for MAURITIUS
#'
#' Reads model results for the Test293 and Test292 scenarios.
#' All CSV files are combined into one data frame. Columns that are missing
#' from individual CSV files are automatically filled with NA. The original
#' name of the last column in each CSV file is stored in the variable column.
#'
#' @return A list containing a MAgPIE-compatible quitte object and metadata.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' MUS <- readSource("MAURITIUS")
#' }
#'
#' @importFrom dplyr bind_rows mutate %>%
#' @importFrom quitte as.quitte
#' @importFrom utils read.csv
#'
readMAURITIUS <- function() {

  scenarios <- c(
    "Test293",
    "Test292"
  )

  read_result_file <- function(file_path, scenario) {

    if (!file.exists(file_path)) {
      stop(
        "File does not exist: ",
        file_path
      )
    }

    df <- utils::read.csv(
      file = file_path,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (ncol(df) == 0) {
      stop(
        "File has no columns: ",
        file_path
      )
    }

    # Keep the original name of the last column
    variable_name <- names(df)[ncol(df)]

    # Identify and rename the year column to period
    year_column <- intersect(
      c(
        "period",
        "y",
        "year",
        "Year",
        "YEAR"
      ),
      names(df)
    )

    if (length(year_column) > 0) {

      names(df)[
        names(df) == year_column[1]
      ] <- "period"

      df$period <- suppressWarnings(
        as.integer(df$period)
      )
    } else {

      df$period <- NA_integer_
    }

    # Store the values from the last column
    df$value <- suppressWarnings(
      as.numeric(df[[variable_name]])
    )

    # Remove the original last column unless it is already named value
    if (variable_name != "value") {
      df[[variable_name]] <- NULL
    }

    # Add metadata
    df <- df %>%
      dplyr::mutate(
        scenario = scenario,
        variable = variable_name,
        source_file = basename(file_path),
        region = "MUS"
      )

    df
  }

  scenario_results <- lapply(
    scenarios,
    function(scenario) {

      scenario_path <- file.path(
        "res",
        scenario,
        "csv"
      )

      csv_files <- list.files(
        path = scenario_path,
        pattern = "\\.csv$",
        full.names = TRUE,
        recursive = TRUE,
        ignore.case = TRUE
      )

      if (length(csv_files) == 0) {
        stop(
          "No CSV files found in: ",
          scenario_path
        )
      }

      lapply(
        csv_files,
        read_result_file,
        scenario = scenario
      )
    }
  )

  # Flatten the nested list
  result_list <- unlist(
    scenario_results,
    recursive = FALSE
  )

  # bind_rows fills missing columns with NA
  output <- dplyr::bind_rows(
    result_list
  )

  # Remove rows where the last-column value is not numeric
  output <- output %>%
    dplyr::filter(
      !is.na(value)
    )

  result_files <- unique(
    output$source_file
  )

  output <- quitte::as.quitte(
    output
  )

  output[["model"]] <- "RE-INTEGRATE"
  output[["type"]] <- "output"

  qx <- quitte::as.quitte(
    output
  )

  list(x = qx,
       weight = NULL,
       class = "quitte",
    description = c(
      category = "Output of MAURITIUS",
      type = "Power mix",
      filename = paste(
        result_files,
        collapse = ", "
      ),
      `Indicative size (MB)` = 6,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
