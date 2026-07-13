#' Read-in data from RE-INTEGRATE about Electricity Production
#'
#' @return The read-in data into a magpie object.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' a <- readSource("ElecProd")
#' }
#'
#' @importFrom dplyr filter mutate %>%
#' @importFrom quitte as.quitte
#' @importFrom readxl read_excel
#' @importFrom stringr str_remove
#' @importFrom tidyr pivot_longer
#' @export
#' @order 2
#'
readElecProd <- function() {

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
  x <- as.magpie(qx)

  list(x = x,
       weight = NULL,
       description = c(category = "Electricity Production",
                       type = "Electricity Production",
                       filename = "Senegal-Data.xlsx",
                       `Indicative size (MB)` = 0.12,
                       dimensions = "3D",
                       unit = "GWh",
                       Confidential = "E3M"))
}

#@rdname readElecProd
#@param x MAgPIE object returned by readElecProd
#@export
#@order 3
# convertElecProd <- function(x) {
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

#' @rdname readElecProd
#' @importFrom utils download.file
#' @export
#' @order 1
downloadElecProd <- function() {

  url <- paste0(
    "https://zenodo.org/records/15066501/files/",
    "Senegal-Data.xlsx?download=1"
  )

  utils::download.file(
    url = url,
    destfile = "Senegal-Data.xlsx",
    mode = "wb",
    quiet = FALSE
  )

  if (!file.exists("Senegal-Data.xlsx")) {
    stop("Download failed: Senegal-Data.xlsx was not created.")
  }

  list(
    url = url,
    doi = "10.5281/zenodo.15066501",
    title = "RE-INTEGRATE Electricity Production data",
    description = paste(
      "RE-INTEGRATE Electricity Production data",
      "from 2012 to 2029"
    ),
    unit = "GWh",
    author = "RE-INTEGRATE",
    release_date = "2026",
    license = "-",
    comment = "Automatically downloaded from Zenodo."
  )
}
