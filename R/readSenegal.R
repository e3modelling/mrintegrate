#' Read-in electricity production and energy balance data for Senegal.
#'
#' Reads RE-INTEGRATE datasets containing electricity production by sector
#' and national energy balance statistics for Senegal. The function imports
#' and formats for electricity production data or detailed energy balance data.
#'
#' @return The read-in data.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' Senegal <- readSource("Senegal")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom dplyr mutate rename select filter %>%
#' @importFrom tidyr fill pivot_longer
#' @importFrom zoo na.locf
#' @importFrom quitte as.quitte
#' @importFrom stats setNames
#' @export
#' @order 2
#'
readSenegal <- function() {

  ###### EleProd
  df <- read_excel("Senegal-Data.xlsx")
  region_name <- str_remove(names(df)[1], "^Electrique data\\s*")

  # Remove columns that are entirely NA
  df <- df[, colSums(!is.na(df)) > 0]

  # Set row 2 as the column names
  colnames(df) <- as.character(unlist(df[2, ]))

  # Remove the first two rows
  df <- df[-c(1, 2), ]

  # Reset row names
  rownames(df) <- NULL

  # Convert all columns except the first (Branch) to numeric
  df[-1] <- lapply(df[-1], as.numeric)

  colnames(df)[1] <- "sector"

  df <- df %>%
    pivot_longer(
      cols = -sector,
      names_to = "period",
      values_to = "value"
    ) %>%
    mutate(period = as.numeric(period))

  df[["unit"]] <- "GWh"
  df[["variable"]] <- "Electricity Production"
  df[["region"]] <- region_name
  qx <- as.quitte(df)
  suppressWarnings({
    levels(qx[["region"]]) <- toolCountry2isocode(
      levels(qx[["region"]]),
      mapping = c("World" = "GLO")
    )
  })

  qx <- dplyr::filter(qx, !is.na(qx[["region"]]))
  qx[["type"]] <- "input"
  EleProd <- as.magpie(qx)

  ######## EnergyBalances

  EnergyBalances <- read_excel(
      "Energy-balance.xlsx",
      sheet = 1,
      col_names = FALSE
    )

  # Get year
  period <- as.numeric(EnergyBalances[[2, 1]])

  # Products
  products <- as.character(unlist(EnergyBalances[1, ]))
  products <- zoo::na.locf(products, na.rm = FALSE)

  # Sub-products
  subproducts <- as.character(unlist(EnergyBalances[2, ]))
  subproducts[is.na(subproducts)] <- ""

  # Units
  units <- as.character(unlist(EnergyBalances[3, ]))
  units[is.na(units)] <- ""

  product_names <- ifelse(
    subproducts == "",
    products,
    paste(products, subproducts, sep = " - ")
  )

  # Remove header rows
  df <- EnergyBalances[-c(1:3), ]

  names(df) <- c(
    "section",
    "flow",
    product_names[-c(1:2)]
  )

  # Remove the last three empty columns
  df <- df[, -((ncol(df) - 2):ncol(df))]

  # Product and unit vectors corresponding to the remaining energy columns
  product_lookup <- product_names[4:(length(product_names) - 3)]
  unit_lookup <- units[4:(length(units) - 3)]

  df_long <- df %>%
    rename(subflow = 3) %>%
    tidyr::fill(section, flow, .direction = "down") %>%
    mutate(period = period) %>%
    pivot_longer(
      cols = -c(section, flow, subflow, period),
      names_to = "product",
      values_to = "value"
    ) %>%
    mutate(
      value = suppressWarnings(as.numeric(value)),
      unit = unit_lookup[match(product, product_lookup)]
    ) %>%
    filter(!is.na(value)) %>%
    rename(variable = section) %>%
    select(
      variable,
      flow,
      subflow,
      product,
      unit,
      period,
      value
    )
  df_long[["region"]] <- "Senegal"

  qx <- as.quitte(df_long)
  suppressWarnings({
    levels(qx[["region"]]) <- toolCountry2isocode(
      levels(qx[["region"]]),
      mapping = c("World" = "GLO")
    )
  })

  qx <- dplyr::filter(qx, !is.na(qx[["region"]]))
  qx[["type"]] <- "input"

  Consumption <- as.magpie(qx)

  x <- stats::setNames(list(EleProd, Consumption),
                       c("ElectrityProduction", "EnergyBalance"))

  list(x = x,
       weight = NULL,
       class = "list",
       description = c(category = "Input, output of Senegal electricity production and energy balance data",
                       type = "electricity production and energy balance",
                       filename = "Senegal-Data.xlsx",
                       `Indicative size (MB)` = 0.12,
                       dimensions = "3D",
                       unit = "various",
                       Confidential = "E3M"))
}

#@rdname readSenegal
#@param x MAgPIE object returned by readSenegal
#@export
#@order 3
# convertSenegal <- function(x) {
#   x <- quitte::as.quitte(x)
#
#   suppressWarnings({
#     levels(x[["region"]]) <- toolCountry2isocode(
#       levels(x[["region"]]),
#       mapping = c("World" = "GLO")
#     )
#   })
#
#   x <- dplyr::filter(x, !is.na(x[["region"]]))
#   x <- magclass::as.magpie(x)
# }

#' @rdname readSenegal
#' @importFrom utils download.file
#' @export
#' @order 1
downloadSenegal <- function() {

  base_url <- "https://zenodo.org/records/15066501/files/"

  files <- c(
    "Senegal-Data.xlsx",
    "Energy-balance.xlsx"
  )

  for (f in files) {
    utils::download.file(
      url = paste0(base_url, f, "?download=1"),
      destfile = f,
      mode = "wb",
      quiet = FALSE
    )

    if (!file.exists(f)) {
      stop("Download failed: ", f, " was not created.")
    }
  }

  list(
    url = paste0(base_url, files, "?download=1"),
    doi = "10.5281/zenodo.15066501",
    title = "RE-INTEGRATE Electricity Production data",
    description = "RE-INTEGRATE Electricity Production datasets.",
    unit = "GWh",
    author = "RE-INTEGRATE",
    release_date = "2026",
    license = "-",
    comment = "Automatically downloaded from Zenodo."
  )
}
