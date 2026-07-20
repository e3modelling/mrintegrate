#' FussionInput
#'
#' @return A list of input reuslts
#'
#' @author Fotis Sioutas
#'
#' @examples
#'
#' \dontrun{
#' a <- calcOutput(type = "FussionInput", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when distinct filter left_join mutate recode rename_with transmute
#'

calcFussionInput <- function() {

  DRC <- readSource("DemocraticRepublicCongo")
  SouthAfrica <- readSource("SouthAfrica")
  Senegal <- readSource("Senegal")

  DEMAND_DRC <- DRC[[8]]
  DEMAND_SEN <- Senegal[[1]]
  DENAND_ZAF <- SouthAfrica[[3]] %>%
    dplyr::filter(
      sector == "Total",
      variable == "Tota"
    ) %>%
    dplyr::mutate(
      value = as.numeric(value)
    ) %>%
    quitte::as.quitte() %>%
    magclass::as.magpie()

  TotalElectricityProduction_SEN <- dimSums(DEMAND_SEN, dim = 3)
  getItems(TotalElectricityProduction_SEN, 3) <- "Electricity Production"
  TotalElectricityProduction_SEN <- add_dimension(TotalElectricityProduction_SEN, dim = 3.3, nm = "GWh", add = "unit")

  TotalElectricityProduction_DRC <- dimSums(DEMAND_DRC, dim = 3) * 277.777778 # PJ to GWh
  getItems(TotalElectricityProduction_DRC, 3) <- "Electricity Production"
  TotalElectricityProduction_DRC <- add_dimension(TotalElectricityProduction_DRC, dim = 3.3, nm = "GWh", add = "unit")

  TotalElectricityProduction_SEN <- add_columns(TotalElectricityProduction_SEN,
                                                  addnm = setdiff(getYears(TotalElectricityProduction_DRC),
                                                                  getYears(TotalElectricityProduction_SEN)),
                                                dim = "period", fill = NA)

  TotalElectricityProduction_DRC <- add_columns(TotalElectricityProduction_DRC,
                                                addnm = setdiff(getYears(TotalElectricityProduction_SEN),
                                                                getYears(TotalElectricityProduction_DRC)),
                                                dim = "period", fill = NA)

  getItems(DENAND_ZAF, 3) <- "Electricity Production"
  DENAND_ZAF <- add_dimension(DENAND_ZAF, dim = 3.3, nm = "GWh", add = "unit")
  getItems(DENAND_ZAF, 2) <- "y2017"

  ElecProd <- mbind(TotalElectricityProduction_SEN, TotalElectricityProduction_DRC)
  DENAND_ZAF <- add_columns(DENAND_ZAF, addnm = setdiff(getYears(ElecProd),
                                                        getYears(DENAND_ZAF)),
                                                dim = "period", fill = NA)

  ElecProd <- mbind(TotalElectricityProduction_SEN, TotalElectricityProduction_DRC, DENAND_ZAF)

  DEMAND_BALANC_SEN <- Senegal[[2]]

  x <- stats::setNames(list(ElecProd),
                       c("ElecProd"))

  list( x = x,
        class = "list",
        weight = NULL,
        unit = "various",
        description = paste( "Input of models"))
}
