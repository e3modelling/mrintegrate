#' Calculate Democratic Republic of the Congomodel outputs
#' Reads the Democratic Republic of the Congo source data,
#' converts the model results to a quitte data frame,
#' and maps technologies, commodities, emissions, costs, capacity, generation,
#' demand, and activity variables to IAMC-style variable names.
#'
#' Technology and commodity descriptions are obtained from the SETS table.
#' The resulting data frame retains the original model dimensions and adds
#' descriptive technology, fuel, and IAMC variable columns.
#'
#' @return A list of ouput reuslts
#'
#' @author Fotis Sioutas
#'
#' @examples
#'
#' \dontrun{
#' a <- calcOutput(type = "FussionOutputDRC", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% case_when distinct filter left_join mutate
#' @importFrom dplyr recode rename_with transmute bind_rows
#'

calcFussionOutputDRC <- function() {
  DRC <- readSource(
    "DemocraticRepublicCongo"
  )

  DRC_results <- as.quitte(
    DRC[[10]]
  )

  SETS <- as.data.frame(
    DRC[[2]]
  ) %>%
    rename_with(
      ~ sub(
        "^SETS\\.",
        "",
        .x
      )
    )


  # -------------------------------------------------------------------------
  # Helper functions
  # -------------------------------------------------------------------------

  clean_iamc_variable <- function(x) {

    x %>%
      gsub(
        "\\|NA(?=\\||$)",
        "",
        .,
        perl = TRUE
      ) %>%
      gsub(
        "\\|+",
        "|",
        .
      ) %>%
      sub(
        "\\|$",
        "",
        .
      )
  }


  convert_mode_to_character <- function(x) {

    if ("m" %in% names(x)) {
      x$m <- as.character(
        x$m
      )
    }

    x
  }

  DRC_variable_mapping <- c(
    AccumulatedNewCapacity = "Capacity Additions|Electricity|Accumulated",
    AnnualFixedOperatingCost = "Cost|Energy Supply|Electricity|Fixed O&M",
    AnnualizedInvestmentCost = "Cost|Energy Supply|Electricity|Annualized Investment",
    AnnualTechnologyEmission = "Emissions|Energy|Supply|Electricity",
    AnnualTechnologyEmissionByMode = "Emissions|Energy|Supply|Electricity",
    AnnualVariableOperatingCost = "Cost|Energy Supply|Electricity|Variable O&M",
    CapitalInvestment = "Investment|Energy Supply|Electricity",
    Demand = "Final Energy|Electricity",
    NewCapacity = "Capacity Additions|Electricity",
    ObjectiveValue = "Model Accounting|Objective Value",
    ProductionByTechnologyByMode = "Secondary Energy|Electricity",
    RateOfActivity = "Activity|Electricity",
    RateOfProductionByTechnologyByMode = "Secondary Energy|Electricity|Production Rate",
    RateOfUseByTechnologyByMode = "Technology Input|Rate",
    SalvageValue = "Model Accounting|Salvage Value",
    TechnologyEmissionsPenalty = "Cost|Emissions Penalty",
    TotalAnnualTechnologyActivityByMode = "Activity|Electricity",
    TotalCapacityAnnual = "Capacity|Electricity",
    TotalTechnologyModelPeriodActivity = "Activity|Electricity|Model Period",
    UseByTechnologyByMode = "Technology Input"
  )


  # -------------------------------------------------------------------------
  # DRC technology and commodity maps
  # -------------------------------------------------------------------------

  technology_map <- SETS %>%
    transmute(
      t = as.character(
        Code
      ),
      technology_iamc = as.character(
        Description
      )
    ) %>%
    mutate(
      t = na_if(
        trimws(t),
        ""
      ),
      technology_iamc = na_if(
        trimws(technology_iamc),
        ""
      )
    ) %>%
    filter(
      !is.na(t)
    ) %>%
    distinct(
      t,
      .keep_all = TRUE
    )


  commodity_map <- SETS %>%
    transmute(
      f = as.character(
        Code.1
      ),
      fuel_iamc = as.character(
        Description.1
      )
    ) %>%
    mutate(
      f = na_if(
        trimws(f),
        ""
      ),
      fuel_iamc = na_if(
        trimws(fuel_iamc),
        ""
      )
    ) %>%
    filter(
      !is.na(f)
    ) %>%
    distinct(
      f,
      .keep_all = TRUE
    )


  # =========================================================================
  # DEMOCRATIC REPUBLIC OF THE CONGO
  # =========================================================================

  DRC_results <- DRC_results %>%
    mutate(
      t = as.character(t),
      f = as.character(f),
      e = recode(as.character(e), EMIC02 = "CO2"),

      variable_iamc_base = unname(
        DRC_variable_mapping[as.character(variable)]
      )
    ) %>%
    left_join(
      technology_map,
      by = "t"
    ) %>%
    left_join(
      commodity_map,
      by = "f"
    ) %>%
    mutate(
      technology_iamc = coalesce(
        na_if(trimws(technology_iamc), ""),
        na_if(trimws(t), "")
      ),

      fuel_iamc = coalesce(
        na_if(trimws(fuel_iamc), ""),
        na_if(trimws(f), "")
      ),

      variable_iamc = case_when(

        # Annual technology emissions
        variable == "AnnualTechnologyEmission" ~
          paste(
            "Emissions", e, "Energy", "Supply",
            "Electricity", technology_iamc,
            sep = "|"
          ),

        # Annual technology emissions by mode
        variable == "AnnualTechnologyEmissionByMode" ~
          paste(
            "Emissions", e, "Energy", "Supply",
            "Electricity", technology_iamc,
            paste0("Mode ", m),
            sep = "|"
          ),

        # Final energy demand
        variable == "Demand" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        # Production by technology and mode
        variable == "ProductionByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        # Rate of activity
        variable == "RateOfActivity" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        # Rate of production by technology and mode
        variable == "RateOfProductionByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        # Rate of use by technology and mode
        variable == "RateOfUseByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        # Total annual activity by mode
        variable == "TotalAnnualTechnologyActivityByMode" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0("Mode ", m),
            sep = "|"
          ),

        # Technology input by mode
        variable == "UseByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0("Mode ", m),
            paste0("Time Slice ", l),
            sep = "|"
          ),

        # Objective value
        variable == "ObjectiveValue" ~
          variable_iamc_base,

        # Other technology-specific variables
        !is.na(technology_iamc) ~
          paste(
            variable_iamc_base,
            technology_iamc,
            sep = "|"
          ),

        # Other fuel-specific variables
        !is.na(fuel_iamc) ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        TRUE ~ variable_iamc_base
      ),

      variable_iamc = clean_iamc_variable(variable_iamc),

      model = "RE-INTEGRATE"
    )

  # -------------------------------------------------------------------------
  # Make mode type consistent before combining
  # -------------------------------------------------------------------------

  DRC_results <- convert_mode_to_character(
    DRC_results
  )

  # -------------------------------------------------------------------------
  # Combine all countries
  # -------------------------------------------------------------------------

  results <- DRC_results

  results <- as.quitte(
    results
  )

  results <- results %>%
    dplyr::mutate(
      model = as.character(model),
      scenario = as.character(scenario),
      region = as.character(region),
      variable = as.character(variable),
      unit = as.character(unit),
      type = as.character(type)
    )

  list(
    x = results,
    class = "quitte",
    weight = NULL,
    unit = "various",
    description = paste(
      "Democratic Republic of the Congo"
    )
  )
}
