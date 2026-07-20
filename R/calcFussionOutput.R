#' Calculate Democratic Republic of the Congo model outputs
#' Reads the Democratic Republic of the Congo source data, converts the model
#' results to a quitte data frame, and maps OSeMOSYS technologies, commodities,
#'
#' emissions, costs, capacity, generation, demand, and activity variables to
#' IAMC-style variable names.
#'
#' Technology and commodity descriptions are obtained from the SETS table.
#' The resulting data frame retains the original model dimensions and adds
#' descriptive technology, fuel, and IAMC variable columns.
#'
#' @return A list of DRC reuslts
#'
#' @author Fotis Sioutas
#'
#' @examples
#'
#' \dontrun{
#' a <- calcOutput(type = "FussionOutout", , aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when distinct filter left_join mutate recode rename_with transmute
#'

calcFussionOutout <- function() {
  DRC <- readSource("DemocraticRepublicCongo")

  DRC_results <- as.quitte(DRC[[1]])
  SETS <- as.data.frame(DRC[[2]]) %>%
    rename_with(~ sub("^SETS\\.", "", .x))

  variable_mapping <- c(
    AnnualEmissions = "Emissions",
    AnnualFixedOperatingCost = "Cost|Energy Supply|Electricity|Fixed O&M",
    AnnualTechnologyEmission = "Emissions|Energy|Supply|Electricity",
    AnnualVariableOperatingCost = "Cost|Energy Supply|Electricity|Variable O&M",
    CapitalInvestment = "Investment|Energy Supply|Electricity",
    Demand = "Final Energy|Electricity",
    DiscountedSalvageValue = "Model Accounting|Discounted Salvage Value",
    NewCapacity = "Capacity Additions|Electricity",
    ProductionByTechnology = "Secondary Energy|Electricity",
    ProductionByTechnologyAnnual = "Secondary Energy|Electricity",
    RateOfActivity = "Activity|Electricity",
    SalvageValue = "Model Accounting|Salvage Value",
    TotalAnnualTechnologyActivityByMode = "Activity|Electricity",
    TotalCapacityAnnual = "Capacity|Electricity",
    TotalTechnologyAnnualActivity = "Activity|Electricity",
    UseByTechnologyAnnual = "Technology Input",
    Capital_costs = "Capital Cost|Electricity",
    Fixed_costs = "Fixed Cost|Electricity",
    Variable_costs = "Variable Cost|Electricity",
    Fuel_costs = "Fuel Cost|Electricity"
  )

  technology_map <- SETS %>%
    transmute(
      t = Code,
      technology_iamc = Description
    ) %>%
    filter(!is.na(t)) %>%
    distinct(t, .keep_all = TRUE)

  commodity_map <- SETS %>%
    transmute(
      f = Code.1,
      fuel_iamc = Description.1
    ) %>%
    filter(!is.na(f)) %>%
    distinct(f, .keep_all = TRUE)

  DRC_results <- DRC_results %>%
    mutate(
      e = recode(e, EMIC02 = "CO2"),
      variable_iamc_base = unname(variable_mapping[variable])
    ) %>%
    left_join(technology_map, by = "t") %>%
    left_join(commodity_map, by = "f") %>%
    mutate(
      variable_iamc = case_when(
        variable == "AnnualEmissions" ~
          paste("Emissions", e, sep = "|"),

        variable == "AnnualTechnologyEmission" ~
          paste(
            "Emissions", e, "Energy", "Supply", "Electricity",
            technology_iamc,
            sep = "|"
          ),

        variable %in% c(
          "AnnualFixedOperatingCost",
          "AnnualVariableOperatingCost",
          "CapitalInvestment",
          "NewCapacity",
          "ProductionByTechnologyAnnual",
          "TotalCapacityAnnual",
          "TotalTechnologyAnnualActivity",
          "Capital_costs",
          "Fixed_costs",
          "Variable_costs",
          "SalvageValue",
          "DiscountedSalvageValue"
        ) ~ paste(variable_iamc_base, technology_iamc, sep = "|"),

        variable == "Demand" ~
          paste(variable_iamc_base, fuel_iamc, sep = "|"),

        variable == "ProductionByTechnology" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0("Time Slice ", l),
            sep = "|"
          ),

        variable == "RateOfActivity" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        variable == "TotalAnnualTechnologyActivityByMode" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0("Mode ", m),
            sep = "|"
          ),

        variable == "UseByTechnologyAnnual" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            sep = "|"
          ),

        variable == "Fuel_costs" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            fuel_iamc,
            sep = "|"
          ),

        TRUE ~ variable_iamc_base
      ),
      variable_iamc = variable_iamc %>%
        gsub("\\|NA(?=\\||$)", "", ., perl = TRUE) %>%
        gsub("\\|+", "|", .) %>%
        sub("\\|$", "", .)
    )

  x <- stats::setNames(list(DRC_results),
                       c("DRC_output_results"))

  list( x = x,
        class = "list",
        weight = NULL,
        unit = "various",
        description = paste( "Democratic Republic of the Congo OSeMOSYS model outputs",
                             "mapped to IAMC-style variables"))
  }
