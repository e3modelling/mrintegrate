#' Read-in electricity production and energy balance data for DemocraticRepublicCongo
#'
#' Reads RE-INTEGRATE datasets containing electricity production by sector
#' and national energy balance statistics for DemocraticRepublicCongo.
#'
#' @return The read-in data as a MAgPIE object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' DRC <- readSource("DemocraticRepublicCongo")
#' }
#'
#' @importFrom stringr str_remove
#' @importFrom readxl read_excel
#' @importFrom dplyr mutate rename select filter %>%
#' @importFrom tidyr fill pivot_longer
#' @importFrom zoo na.locf
#' @importFrom quitte as.quitte
#' @export
#' @order 2
#'
readDemocraticRepublicCongo <- function() {



  unzip("DRC-LCS-Model data and results.zip")

  dir.create("DRC-LCS-Model data and results/BASE", showWarnings = FALSE)
  dir.create("DRC-LCS-Model data and results/MEAP", showWarnings = FALSE)

  unzip(
    "DRC-LCS-Model data and results/DRC-LCS-BASE_Results_csv.zip",
    exdir = "DRC-LCS-Model data and results/BASE"
  )

  unzip(
    "DRC-LCS-Model data and results/DRC-LCS-MEAP_Results_csv.zip",
    exdir = "DRC-LCS-Model data and results/MEAP"
  )

  read_scenario_csvs <- function(path, scenario) {

    files <- list.files(
      path = path,
      pattern = "\\.csv$",
      full.names = TRUE,
      recursive = TRUE
    )

    if (length(files) == 0) {
      stop("No CSV files found in: ", path)
    }

    data_list <- lapply(files, function(f) {

      df <- read.csv(
        f,
        header = TRUE,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )

      if (ncol(df) < 4) {
        stop("File has fewer than four columns: ", f)
      }

      # Save the name of the last column
      variable <- names(df)[ncol(df)]

      # Keep first four columns
      df <- df[, 1:4, drop = FALSE]

      names(df) <- c(
        "column1",
        "column2",
        "column3",
        "value"
      )

      # Add a new column containing the original last column name
      df$variable <- variable

      df
    })

    result <- do.call(rbind, data_list)
    rownames(result) <- NULL

    result
  }

  BASE <- read_scenario_csvs(
    "DRC-LCS-Model data and results/BASE",
    "BASE"
  )

  MEAP <- read_scenario_csvs(
    "DRC-LCS-Model data and results/MEAP",
    "MEAP"
  )

  BASE[["scenario"]] <- "BASE"
  MEAP[["scenario"]] <- "MEAP"

  DRC_results <- rbind(BASE, MEAP)




  # df <- read_excel("OSeMOSYS-DRC dataset for the Power Sector.xlsx",
  #                  sheet = "Demand", skip = 3, n_max = 5) %>%
  #   pivot_longer(
  #     cols = matches("^\\d{4}$"),
  #     names_to = "period",
  #     values_to = "value"
  #   ) %>%
  #   mutate(
  #     period = as.integer(period)
  #   )  %>% select(- Source)
  #
  # region_name <- "DemocraticRepublicCongo"
  #
  # names(df) <- c("description", "variable", "period", "value")
  #
  # df[["value"]] <- df[["value"]] * 277.7778
  # df[["unit"]] <- "GWh"
  # df[["region"]] <- region_name
  #
  # qx <- as.quitte(df)
  # suppressWarnings({
  #   levels(qx[["region"]]) <- toolCountry2isocode(
  #     levels(qx[["region"]]),
  #     mapping = c("DemocraticRepublicCongo" = "DRC")
  #   )
  # })
  #
  # qx <- dplyr::filter(qx, !is.na(qx[["region"]]))
  # x <- as.magpie(qx)

  list(x = x,
       weight = NULL,
       description = c(category = "Electricity Demand",
                       type = "Electricity Demand",
                       filename = "OSeMOSYS-DRC dataset for the Power Sector.xlsx",
                       `Indicative size (MB)` = 0.7,
                       dimensions = "3D",
                       unit = "GWh",
                       Confidential = "E3M"))
}

#' @rdname readDemocraticRepublicCongo
#' @importFrom utils download.file
#' @export
#' @order 1
downloadDemocraticRepublicCongo <- function() {

  base_url <- "https://zenodo.org/records/14981239/files/"

  files <- c(
    "OSeMOSYS-DRC dataset for the Power Sector.xlsx",
    "DRC-LCS-Model data and results.zip"
  )

  for (f in files) {
    utils::download.file(
      url = paste0(base_url, utils::URLencode(f), "?download=1"),
      destfile = f,
      mode = "wb",
      quiet = FALSE
    )

    if (!file.exists(f)) {
      stop("Download failed: ", f, " was not created.")
    }
  }

  list(
    url = paste0(base_url, utils::URLencode(files), "?download=1"),
    doi = "10.5281/zenodo.14981239",
    title = "OSeMOSYS-DRC dataset for the Power Sector",
    description = "RE-INTEGRATE power sector dataset for the Democratic Republic of the Congo.",
    unit = "-",
    author = "RE-INTEGRATE",
    release_date = "2025",
    license = "-",
    comment = "Automatically downloaded from Zenodo."
  )
}
