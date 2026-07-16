#' Read-in electricity production and energy balance data for South Africa
#'
#' Reads RE-INTEGRATE datasets containing electricity production by sector
#' and national energy balance statistics for South Africa
#'
#' @return The read-in data.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' SouthAfrica <- readSource("SouthAfrica")
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

  file <- "South African Disaggregated Provincial Electricity and Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx"

  sheet_names <- excel_sheets(file)

  excel_data <- stats::setNames(
    lapply(sheet_names, function(sheet) {
      read_excel(
        path = file,
        sheet = sheet,
        col_names = FALSE
      )
    }),
    sheet_names
  )

  PartialProvincialEB2017 <- excel_data$PartialProvincialEB2017

  # Use row 2 as column names
  names(PartialProvincialEB2017) <- make.unique(
    as.character(unlist(PartialProvincialEB2017[2, ]))
  )

  # Remove the first three rows
  PartialProvincialEB2017 <- PartialProvincialEB2017[-c(1, 2), ]

  # Pivot year columns to long format
  PartialProvincialEB2017 <- PartialProvincialEB2017 %>%
    pivot_longer(
      cols = -c(`Row Labels`),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      value = as.numeric(value)
    )

  PartialProvincialEB2017[["unit"]] <- "PJ"
  PartialProvincialEB2017[["region"]] <- "ZAF"
  PartialProvincialEB2017[["type"]] <- "input"

  CombinedElc <- excel_data$`CombinedElc+LiqFuels`
  # Use row 1 as column names
  names(CombinedElc) <- make.unique(
    as.character(unlist(CombinedElc[1, ])))

  # Remove the first row
  CombinedElc <- CombinedElc[-c(1), ]
  CombinedElc[["unit"]] <- "PJ"
  CombinedElc[["region"]] <- "ZAF"
  CombinedElc[["type"]] <- "input"

  Electricity <- excel_data$Electricity
  Electricity1 <- Electricity[1:12,1:9]

  # Use row 2 as column names
  names(Electricity1) <- make.unique(
    as.character(unlist(Electricity1[2, ]))
  )

  # Remove the first three rows
  Electricity1 <- Electricity1[-c(1, 2), ]
  names(Electricity1)[1] <- "sector"

  # Pivot year columns to long format
  Electricity1 <- Electricity1 %>%
    pivot_longer(
      cols = -c(sector),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      value = as.numeric(value)
    )

  Electricity1[["unit"]] <- "GWh"
  Electricity1[["region"]] <- "ZAF"
  Electricity1[["type"]] <- "input"

  Electricity2 <- Electricity[,13:16]

  # Use row 2 as column names
  names(Electricity2) <- make.unique(
    as.character(unlist(Electricity2[2, ]))
  )

  # Remove the first three rows
  Electricity2 <- Electricity2[-c(1, 2), ]
  names(Electricity2)[1] <- "sector"

  # Pivot year columns to long format
  Electricity1 <- Electricity1 %>%
    pivot_longer(
      cols = -c(sector),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      value = as.numeric(value)
    )

  Electricity1[["unit"]] <- "GWh"
  Electricity1[["region"]] <- "ZAF"
  Electricity1[["type"]] <- "input"

  list(x = x,
       weight = NULL,
       class = "list",
       description = c(category = "Input, output of South Africa",
                       type = "Power mix",
                       filename = "South African Disaggregated Provincial Electricity and Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx",
                       `Indicative size (MB)` = 0.6,
                       dimensions = "3D",
                       unit = "various",
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
