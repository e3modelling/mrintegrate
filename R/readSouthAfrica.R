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
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr mutate rename select filter slice left_join %>%
#' @importFrom tidyr fill pivot_longer
#' @importFrom stringr str_remove
#' @importFrom zoo na.locf
#' @importFrom quitte as.quitte
#' @export
#' @order 2
#'
readSouthAfrica <- function() {

  file <- "SouthAfricaEnergyBalance2017.xlsx"

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
      names_to = "variable",
      values_to = "value"
    ) %>%
    mutate(
      value = as.numeric(value)
    )

  Electricity1[["unit"]] <- "GWh"
  Electricity1[["region"]] <- "ZAF"
  Electricity1[["type"]] <- "input"
  Electricity1[["Fuel"]] <- NA

  Electricity2 <- Electricity[,13:16]

  # Use row 2 as column names
  names(Electricity2) <- make.unique(
    as.character(unlist(Electricity2[2, ]))
  )

  # Remove the first three rows
  Electricity2 <- Electricity2[-c(1, 2), ]
  names(Electricity2)[1] <- "sector"
  names(Electricity2)[2] <- "variable"
  names(Electricity2)[4] <- "value"

  Electricity2[["unit"]] <- "GWh"
  Electricity2[["region"]] <- "ZAF"
  Electricity2[["type"]] <- "input"

  Electricity <- rbind(Electricity1, Electricity2)

  Liquid_Fuels <- excel_data$Liquid_Fuels

  # Use row 2 as column names
  names(Liquid_Fuels) <- make.unique(
    as.character(unlist(Liquid_Fuels[1, ]))
  )

  # Remove the first rows
  Liquid_Fuels <- Liquid_Fuels[-c(1), ]

  names(Liquid_Fuels)[5] <- "value"

  Liquid_Fuels[["unit"]] <- "PJ"
  Liquid_Fuels[["region"]] <- "ZAF"
  Liquid_Fuels[["type"]] <- "input"

  SectorMap <- excel_data$SectorMap

  SectorMap1 <- SectorMap[c(1:37),c(1,2)]

  # Use row 2 as column names
  names(SectorMap1) <- make.unique(
    as.character(unlist(SectorMap1[1, ]))
  )

  # Remove the first rows
  SectorMap1 <- SectorMap1[-c(1), ]

  SectorMap1[["region"]] <- "ZAF"
  SectorMap1[["type"]] <- "input"

  SectorMap2 <- SectorMap[c(3:nrow(SectorMap)),c(4:length(SectorMap))]

  # Use row 2 as column names
  names(SectorMap2) <- make.unique(
    as.character(unlist(SectorMap2[2, ]))
  )

  # Remove the first rows
  SectorMap2 <- SectorMap2[-c(2), ]
  names(SectorMap2)[1] <- "Sector Raw"
  names(SectorMap2)[2] <- "Fuel"
  SectorMap2 <- SectorMap2[-c(2), ]


  SectorMap2_long <- SectorMap2 %>%
    pivot_longer(
      cols = -c(`Sector Raw`,Fuel, CombinedString, Check),
      names_to = "sector",
      values_to = "value",
      values_drop_na = TRUE
    )

  # Mapping stored in the first row
  sector_groups <- SectorMap2 %>%
    slice(1) %>%
    pivot_longer(
      cols = -c(`Sector Raw`, Fuel, CombinedString, Check),
      names_to = "sector",
      values_to = "sector_group",
      values_drop_na = TRUE
    ) %>%
    select(sector, sector_group)

  # Pivot the actual data, excluding the first row
  SectorMap2_long <- SectorMap2 %>%
    slice(-1) %>%
    pivot_longer(
      cols = -c(`Sector Raw`, Fuel, CombinedString, Check),
      names_to = "sector",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    left_join(
      sector_groups,
      by = "sector"
    ) %>%
    mutate(
      value = as.numeric(value)
    ) %>%
    select(
      `Sector Raw`,
      Fuel,
      CombinedString,
      Check,
      sector,
      sector_group,
      value
    )

  Liquid_Fuels[["unit"]] <- "PJ"
  Liquid_Fuels[["region"]] <- "ZAF"
  Liquid_Fuels[["type"]] <- "input"
  period <- "2017"

  x <- stats::setNames(list(PartialProvincialEB2017, CombinedElc, Electricity,
                            Liquid_Fuels, SectorMap1, SectorMap2_long, period),
                       c("PartialProvincialEB2017", "CombinedElc", "ElectrityProduction",
                         "Liquid_Fuels", "SectorMap1", "SectorMap2", "period"))

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
#' @importFrom utils download.file URLencode
#' @export
#' @order 1
downloadSouthAfrica <- function() {

  base_url <- "https://zenodo.org/records/14945793/files/"

  remote_file <- paste(
    "South African Disaggregated Provincial Electricity",
    "and Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx"
  )

  local_file <- "SouthAfricaEnergyBalance2017.xlsx"

  download_url <- paste0(
    base_url,
    utils::URLencode(remote_file, reserved = FALSE),
    "?download=1"
  )

  utils::download.file(
    url = download_url,
    destfile = local_file,
    mode = "wb",
    quiet = FALSE
  )

  if (!file.exists(local_file)) {
    stop("Download failed: ", local_file, " was not created.")
  }

  list(
    url = download_url,
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
#' @rdname readSouthAfrica
#' @importFrom utils download.file
#' @export
#' @order 1
downSouthAfrica <- function() {

  file <- paste0(
    "South African Disaggregated Provincial Electricity and ",
    "Liquid Fuels Energy Balance for 2017 - Merven 2025.xlsx"
  )

  url <- paste0(
    "https://zenodo.org/records/14945793/files/",
    "South%20African%20Disaggregated%20Provincial%20Electricity%20",
    "and%20Liquid%20Fuels%20Energy%20Balance%20for%202017%20-%20",
    "Merven%202025.xlsx",
    "?download=1"
  )


  # Download only if file does not already exist
  if (!file.exists(file)) {

    utils::download.file(
      url = url,
      destfile = file,
      mode = "wb",
      quiet = FALSE
    )
  }


  # Check that the file exists
  if (!file.exists(file)) {
    stop(
      "Download failed: ",
      file,
      " was not created."
    )
  }


  list(
    url = url,
    doi = "10.5281/zenodo.14945793",
    title = paste0(
      "South African Disaggregated Provincial Electricity ",
      "and Liquid Fuels Energy Balance for 2017"
    ),
    description = paste0(
      "South African Disaggregated Provincial Electricity ",
      "and Liquid Fuels Energy Balance."
    ),
    unit = "",
    author = "RE-INTEGRATE",
    release_date = "2025",
    license = "-",
    comment = "Automatically downloaded from Zenodo."
  )
}
