#' Read-in electricity production and energy balance data for South Africa
#'
#' Reads RE-INTEGRATE datasets containing electricity production by sector
#' and national energy balance statistics for South Africa
#'
#' @return The read-in data as a MAgPIE object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' DRC <- readSource("SouthAfrica")
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
readSouthAfrica <- function() {

  df <- read_excel("South African Disaggregated Provincial Electricity and Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx",
                   sheet = "Demand", skip = 3, n_max = 5) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period)
    )  %>% select(- Source)

  region_name <- "DemocraticRepublicCongo"

  names(df) <- c("description", "variable", "period", "value")

  df[["value"]] <- df[["value"]] * 277.7778
  df[["unit"]] <- "GWh"
  df[["region"]] <- region_name

  qx <- as.quitte(df)
  suppressWarnings({
    levels(qx[["region"]]) <- toolCountry2isocode(
      levels(qx[["region"]]),
      mapping = c("DemocraticRepublicCongo" = "DRC")
    )
  })

  qx <- dplyr::filter(qx, !is.na(qx[["region"]]))
  x <- as.magpie(qx)

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

#' @rdname readSouthAfrica
#' @importFrom utils download.file
#' @export
#' @order 1
downloadSouthAfrica <- function() {

  base_url <- "https://zenodo.org/records/14945793/files/"

  files <- c(
    "South African Disaggregated Provincial Electricity and Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx"
  )

  for (f in files) {
    utils::download.file(
      url = paste0(
        base_url,
        utils::URLencode(f, reserved = FALSE),
        "?download=1"
      ),
      destfile = f,
      mode = "wb",
      quiet = FALSE
    )

    if (!file.exists(f)) {
      stop("Download failed: ", f, " was not created.")
    }
  }

  list(
    url = paste0(
      base_url,
      utils::URLencode(files, reserved = FALSE),
      "?download=1"
    ),
    doi = "10.5281/zenodo.14945793",
    title = paste(
      "South African Disaggregated Provincial Electricity",
      "and Liquid Fuels Energy Balance for 2017"
    ),
    description = paste(
      "South African disaggregated provincial electricity",
      "and liquid fuels energy balance for 2017."
    ),
    unit = "-",
    author = "Merven",
    release_date = "2025",
    license = "-",
    comment = "Automatically downloaded from Zenodo."
  )
}
