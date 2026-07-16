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
#' @importFrom readxl read_excel excel_sheets
#' @importFrom dplyr mutate select bind_rows relocate if_else across matches %>%
#' @importFrom tidyr fill pivot_longer
#' @importFrom quitte as.quitte
#' @importFrom utils unzip read.csv
#' @importFrom stats setNames
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

    files <- files[
      basename(files) != "CostElectrictyGeneration.csv"
    ]

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

      if (ncol(df) == 0) {
        stop("File has no columns: ", f)
      }

      # Store the original name of the last column
      variable <- names(df)[ncol(df)]

      # Rename the last column to value
      names(df)[ncol(df)] <- "value"

      # Add metadata
      df$variable <- variable
      df$scenario <- scenario

      df
    })

    # Match columns by name and fill missing columns with NA
    result <- dplyr::bind_rows(data_list)

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

  BASECostElectrictyGeneration <- read.csv("DRC-LCS-Model data and results/BASE/csv/CostElectrictyGeneration.csv")
  BASECostElectrictyGeneration[["scenario"]] <- "BASE"
  MEAPCostElectrictyGeneration <- read.csv("DRC-LCS-Model data and results/MEAP/csv/CostElectrictyGeneration.csv")
  MEAPCostElectrictyGeneration[["scenario"]] <- "MEAP"
  CostElectrictyGeneration <- rbind(BASECostElectrictyGeneration, MEAPCostElectrictyGeneration)
  CostElectrictyGeneration_long <- CostElectrictyGeneration %>%
    pivot_longer(
      cols = -c(y, scenario),
      names_to = "variable",
      values_to = "value"
    )

  CostElectrictyGeneration_long <- CostElectrictyGeneration_long %>%
    mutate(
      r = NA_character_,
      e = NA_character_,
      t = NA_character_,
      l = NA_character_,
      f = NA_character_,
      m = NA_integer_
    )

  DRC_results <- dplyr::bind_rows(
    DRC_results,
    CostElectrictyGeneration_long
  )

  DRC_results[["unit"]] <- "various"
  DRC_results[["region"]] <- 'DRC'
  names(DRC_results) <- sub("y", "period", names(DRC_results))

  qx <- as.quitte(DRC_results)
  qx[["type"]] <- "output"
  output <- as.magpie(qx)

  file <- "OSeMOSYS-DRC dataset for the Power Sector.xlsx"

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

  SETS <- excel_data$SETS %>%
    select(-3)


  # Save first row
  top_header <- as.character(unlist(SETS[1, ]))

  # Use second row as names (make them unique)
  names(SETS) <- make.unique(as.character(unlist(SETS[2, ])))

  # Remove first two rows
  SETS <- SETS[-c(1, 2), ]

  # Add category columns
  SETS$Category1 <- top_header[1]
  SETS$Category2 <- top_header[3]

  # Reorder columns
  SETS <- SETS %>%
    relocate(Category1, .before = Code) %>%
    relocate(Category2, .before = Code.1) %>%
    mutate(
      Category2 = if_else(
        is.na(Code.1) & is.na(Description.1),
        NA_character_,
        Category2
      )
    )

  SETS[["unit"]] <- NA
  SETS[["region"]] <- "DRC"
  SETS[["type"]] <- "input"

  capacity_factor <- excel_data$`Capacity Factor & Lifetime`

  # Remove column 6
  capacity_factor <- capacity_factor[, -6]

  # Save first row
  top_header <- as.character(unlist(capacity_factor[1, ]))

  # Use second row as column names
  names(capacity_factor) <- make.unique(as.character(unlist(capacity_factor[2, ])))

  # Remove first two rows
  capacity_factor <- capacity_factor[-c(1, 2), ]

  # Add category columns
  capacity_factor$Category2 <- top_header[6]


  capacity_factor <- capacity_factor %>%
    fill(Technology, Code.1, .direction = "down")

  capacity_factor[["unit"]] <- NA
  capacity_factor[["region"]] <- "DRC"
  capacity_factor[["type"]] <- "input"


  ResidualCapacityAggregated <- excel_data$`Residual Capacity (aggregated)`

  # Use row 3 as column names
  names(ResidualCapacityAggregated) <- make.unique(
    as.character(unlist(ResidualCapacityAggregated[3, ]))
  )

  # Remove the first three rows
  ResidualCapacityAggregated <- ResidualCapacityAggregated[-c(1, 2, 3), ]
  ResidualCapacityAggregated <- ResidualCapacityAggregated[,-1]

  # Pivot year columns to long format
  ResidualCapacityAggregated <- ResidualCapacityAggregated %>%
    pivot_longer(
      cols = -c(Technology, Code),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.numeric(period),
      value = as.numeric(value)
    )

  ResidualCapacityAggregated[["variable"]] <- "Residual Capacity (aggregated)"
  ResidualCapacityAggregated[["unit"]] <- "GW"
  ResidualCapacityAggregated[["region"]] <- "DRC"
  ResidualCapacityAggregated[["type"]] <- "input"

  Costs <- excel_data$Costs

  # Use row 3 as column names
  names(Costs) <- make.unique(
    as.character(unlist(Costs[3, ]))
  )

  # Remove the first three rows
  Costs <- Costs[-c(1, 2, 3), ]
  Costs <- Costs[,-1]

  # Pivot year columns to long format
  Costs <- Costs %>%
    pivot_longer(
      cols = -c(Technology, Code),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.numeric(period),
      value = as.numeric(value)
    )

  Costs[["variable"]] <- "Overnight Capital Cost"
  Costs[["unit"]] <- "$/kW"
  Costs[["region"]] <- "DRC"
  Costs <- as.quitte(Costs)
  Costs[["type"]] <- "input"

  Investment <- excel_data$Investment

  # Use row 3 as column names
  names(Investment) <- make.unique(
    as.character(unlist(Investment[4, ]))
  )

  # Remove the first three rows
  Investment <- Investment[-c(1, 2, 3, 4), ]
  Investment <- Investment[,-1]

  Investment[["variable"]] <- "Planned Capacity Investment"
  Investment[["unit"]] <- "GW"
  Investment[["region"]] <- "DRC"
  Investment[["type"]] <- "input"

  ResourcePotential <- excel_data$`Resource Potential`
  ResourcePotential1 <- ResourcePotential[1:8,]
  # Use row 3 as column names
  names(ResourcePotential1) <- make.unique(
    as.character(unlist(ResourcePotential1[3, ]))
  )

  # Remove the first three rows
  ResourcePotential1 <- ResourcePotential1[-c(1, 2, 3), ]
  ResourcePotential1 <- ResourcePotential1[,-1]

  ResourcePotential1[["variable"]] <- "Renewable Energy Potential"
  ResourcePotential1[["unit"]] <- "various"
  ResourcePotential1[["region"]] <- "DRC"
  ResourcePotential1[["type"]] <- "input"
  ResourcePotential2 <- ResourcePotential[11:16,]
  # Use row 3 as column names
  names(ResourcePotential2) <- make.unique(
    as.character(unlist(ResourcePotential[3, ]))
  )

  # Remove the first three rows
  ResourcePotential2 <- ResourcePotential2[-c(1, 2, 3), ]
  ResourcePotential2 <- ResourcePotential2[,-1]

  ResourcePotential2[["variable"]] <- "Fossil Fuel Potential"
  ResourcePotential2[["unit"]] <- "PJ"
  ResourcePotential2[["region"]] <- "DRC"
  ResourcePotential2[["type"]] <- "input"
  ResourcePotential <- rbind(ResourcePotential1, ResourcePotential2)

  Demand <-excel_data$Demand
  # Use row 3 as column names
  names(Demand) <- make.unique(
    as.character(unlist(Demand[3, ]))
  )

  # Remove the first three rows
  Demand <- Demand[-c(1, 2, 3), ]
  Demand <- Demand[, -1]
  Demand <- Demand[1:4,] %>%
    mutate(
      across(matches("^\\d{4}$"), as.numeric)
    ) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period)
    )

  Demand[["unit"]] <- "PJ"
  Demand[["region"]] <- "DRC"
  Demand[["variable"]] <- "Electricity Demand"
  Demand[["type"]] <- "input"
  Demand <- as.quitte(Demand) %>% as.magpie()

  ImportsExports <-excel_data$`Imports & Exports`
  # Use row 3 as column names
  names(ImportsExports) <- make.unique(
    as.character(unlist(ImportsExports[3, ]))
  )

  # Remove the first three rows
  ImportsExports <- ImportsExports[-c(1, 2, 3), ]
  ImportsExports <- ImportsExports[, -1]
  ImportsExports <- ImportsExports %>%
    mutate(
      across(matches("^\\d{4}$"), as.numeric)
    ) %>%
    pivot_longer(
      cols = matches("^\\d{4}$"),
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(
      period = as.integer(period)
    )

  ImportsExports[["unit"]] <- "PJ"
  ImportsExports[["region"]] <- "DRC"
  ImportsExports[["variable"]] <- "Total Technology Annual Activity Upper Limit"
  ImportsExports[["type"]] <- "input"

  x <- stats::setNames(list(output,SETS,capacity_factor,ResidualCapacityAggregated,
      Costs,Investment,ResourcePotential,Demand,ImportsExports),
    c("output","SETS","capacity_factor","ResidualCapacityAggregated","Costs",
      "Investment","ResourcePotential","Demand","ImportsExports"))

  list(x = x,
       weight = NULL,
       class = "list",
       description = c(category = "Input, output of DRC",
                       type = "Power mix",
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
