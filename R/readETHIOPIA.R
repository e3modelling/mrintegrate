#' Reads RE-INTEGRATE datasets for ETHIOPIA
#'
#' Reads model results for the Baseline and Ambitious_85percentBy2040 scenarios
#'
#' @return A MAgPIE object containing the Ethiopian model results.
#'
#' @author Fotis Sioutas #' #' @examples
#'
#' \dontrun{
#' ETHIOPIA <- readSource("ETHIOPIA")
#' }
#'
#' @importFrom dplyr mutate bind_rows %>%
#'
#' @importFrom quitte as.quitte
#'
#' @importFrom utils read.csv
#'
readETHIOPIA <- function() {

  scenarios <- c(
    "Ambitious_85percentBy2040",
    "Baseline"
  )

  result_files <- c(
    "TotalCapacityAnnual.csv",
    "AnnualizedInvestmentCost.csv",
    "AnnualTechnologyEmission.csv",
    "ProductionByTechnologyByMode.csv"
  )

  read_result_file <- function(file_path, scenario) {

    if (!file.exists(file_path)) {
      stop("File does not exist: ", file_path)
    }

    df <- utils::read.csv(
      file = file_path,
      header = TRUE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    if (ncol(df) == 0) {
      stop("File has no columns: ", file_path)
    }

    variable_name <- names(df)[ncol(df)]

    df$value <- suppressWarnings(
      as.numeric(df[[variable_name]])
    )

    df$variable <- variable_name

    # Remove the original result column
    df[[variable_name]] <- NULL

    # Add metadata
    df$model <- "RE-INTEGRATE"
    df$scenario <- scenario
    df$region <- "ETH"
    df$period <- as.integer(df$y)
    df$unit <- "various"
    df$type <- "output"

    df
  }

  scenario_results <- lapply(
    scenarios,
    function(scenario) {

      files <- file.path(
        scenario,
        result_files
      )

      dfs <- lapply(
        files,
        read_result_file,
        scenario = scenario
      )

      dplyr::bind_rows(dfs)
    }
  )

  ETHIOPIA_results <- dplyr::bind_rows(
    scenario_results
  )

  ETHIOPIA_results <- ETHIOPIA_results %>%
    dplyr::select(
      model,
      scenario,
      region,
      variable,
      unit,
      period,
      type,
      dplyr::any_of(
        c(
          "r",
          "r_x",
          "r_y",
          "t",
          "e",
          "f",
          "m",
          "l"
        )
      ),
      value
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(
          c(
            "r",
            "r_x",
            "r_y",
            "t",
            "e",
            "f",
            "m",
            "l"
          )
        ),
        ~ replace(as.character(.x), is.na(.x), "")
      )
    )

  qx <- quitte::as.quitte(
    ETHIOPIA_results
  )

  output <- as.magpie(
    qx
  )

  list(
    x = output,
    weight = NULL,
    description = c(
      category = "output of ETH",
      type = "Power mix",
      filename = paste(
        result_files,
        collapse = ", "
      ),
      `Indicative size (MB)` = 2,
      dimensions = "3D",
      unit = "various",
      Confidential = "E3M"
    )
  )
}
