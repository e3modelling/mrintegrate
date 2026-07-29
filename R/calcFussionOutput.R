#' Calculate Democratic Republic of the Congo,MAURITIUS and ETHIOPIA model outputs
#' Reads the Democratic Republic of the Congo,MAURITIUS and ETHIOPIA source data,
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
#' a <- calcOutput(type = "FussionOutput", aggregate = FALSE)
#' }
#'
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% case_when distinct filter left_join mutate
#' @importFrom dplyr recode rename_with transmute bind_rows
#'

calcFussionOutput <- function() {
  DRC <- readSource(
    "DemocraticRepublicCongo"
  )

  DRC_results <- as.quitte(
    DRC[[1]]
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

  DRC_variable_mapping <- c(
    AnnualEmissions =
      "Emissions",

    AnnualFixedOperatingCost =
      "Cost|Energy Supply|Electricity|Fixed O&M",

    AnnualTechnologyEmission =
      "Emissions|Energy|Supply|Electricity",

    AnnualVariableOperatingCost =
      "Cost|Energy Supply|Electricity|Variable O&M",

    CapitalInvestment =
      "Investment|Energy Supply|Electricity",

    Demand =
      "Final Energy|Electricity",

    DiscountedSalvageValue =
      "Model Accounting|Discounted Salvage Value",

    NewCapacity =
      "Capacity Additions|Electricity",

    ProductionByTechnology =
      "Secondary Energy|Electricity",

    ProductionByTechnologyAnnual =
      "Secondary Energy|Electricity",

    RateOfActivity =
      "Activity|Electricity",

    SalvageValue =
      "Model Accounting|Salvage Value",

    TotalAnnualTechnologyActivityByMode =
      "Activity|Electricity",

    TotalCapacityAnnual =
      "Capacity|Electricity",

    TotalTechnologyAnnualActivity =
      "Activity|Electricity",

    UseByTechnologyAnnual =
      "Technology Input",

    Capital_costs =
      "Capital Cost|Electricity",

    Fixed_costs =
      "Fixed Cost|Electricity",

    Variable_costs =
      "Variable Cost|Electricity",

    Fuel_costs =
      "Fuel Cost|Electricity"
  )


  DRC_results <- DRC_results %>%
    mutate(
      t = as.character(
        t
      ),
      f = as.character(
        f
      ),
      e = recode(
        e,
        EMIC02 = "CO2"
      ),
      variable_iamc_base = unname(
        DRC_variable_mapping[
          as.character(variable)
        ]
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
        na_if(
          trimws(technology_iamc),
          ""
        ),
        na_if(
          trimws(t),
          ""
        )
      ),
      fuel_iamc = coalesce(
        na_if(
          trimws(fuel_iamc),
          ""
        ),
        na_if(
          trimws(f),
          ""
        )
      ),
      variable_iamc = case_when(

        variable == "AnnualEmissions" ~
          paste(
            "Emissions",
            e,
            sep = "|"
          ),

        variable == "AnnualTechnologyEmission" ~
          paste(
            "Emissions",
            e,
            "Energy",
            "Supply",
            "Electricity",
            technology_iamc,
            sep = "|"
          ),

        variable == "Demand" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        variable == "ProductionByTechnology" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "ProductionByTechnologyAnnual" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            sep = "|"
          ),

        variable == "RateOfActivity" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "TotalAnnualTechnologyActivityByMode" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            sep = "|"
          ),

        variable == "UseByTechnologyAnnual" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            sep = "|"
          ),

        !is.na(technology_iamc) ~
          paste(
            variable_iamc_base,
            technology_iamc,
            sep = "|"
          ),

        !is.na(fuel_iamc) ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        TRUE ~
          variable_iamc_base
      ),

      variable_iamc = clean_iamc_variable(
        variable_iamc
      ),

      model = "RE-INTEGRATE"
    )


  # =========================================================================
  # ETHIOPIA
  # =========================================================================

  ETH <- readSource(
    "ETHIOPIA"
  ) %>%
    as.quitte()


  ETH_variable_mapping <- c(
    TotalCapacityAnnual =
      "Capacity|Electricity",

    AnnualizedInvestmentCost =
      "Cost|Energy Supply|Electricity|Annualized Investment",

    AnnualTechnologyEmission =
      "Emissions|Energy|Supply|Electricity",

    ProductionByTechnologyByMode =
      "Secondary Energy|Electricity"
  )


  ETH_results <- ETH %>%
    mutate(
      t = as.character(
        t
      ),
      f = as.character(
        f
      ),
      e = recode(
        e,
        EMIC02 = "CO2"
      ),
      variable_iamc_base = unname(
        ETH_variable_mapping[
          as.character(variable)
        ]
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
        na_if(
          trimws(technology_iamc),
          ""
        ),
        na_if(
          trimws(t),
          ""
        )
      ),
      fuel_iamc = coalesce(
        na_if(
          trimws(fuel_iamc),
          ""
        ),
        na_if(
          trimws(f),
          ""
        )
      ),
      variable_iamc = case_when(

        variable == "AnnualTechnologyEmission" ~
          paste(
            "Emissions",
            e,
            "Energy",
            "Supply",
            "Electricity",
            technology_iamc,
            sep = "|"
          ),

        variable == "ProductionByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable %in% c(
          "TotalCapacityAnnual",
          "AnnualizedInvestmentCost"
        ) ~
          paste(
            variable_iamc_base,
            technology_iamc,
            sep = "|"
          ),

        !is.na(technology_iamc) ~
          paste(
            variable_iamc_base,
            technology_iamc,
            sep = "|"
          ),

        !is.na(fuel_iamc) ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        TRUE ~
          variable_iamc_base
      ),

      variable_iamc = clean_iamc_variable(
        variable_iamc
      ),

      model = "RE-INTEGRATE"
    )


  # =========================================================================
  # MAURITIUS
  # =========================================================================

  MUS <- readSource(
    "MAURITIUS"
  )


  MUS_variable_mapping <- c(
    AccumulatedNewCapacity =
      "Accumulated Capacity Additions|Electricity",

    AnnualFixedOperatingCost =
      "Cost|Energy Supply|Electricity|Fixed O&M",

    AnnualizedInvestmentCost =
      "Cost|Energy Supply|Electricity|Annualized Investment",

    AnnualTechnologyEmission =
      "Emissions|Energy|Supply|Electricity",

    AnnualTechnologyEmissionByMode =
      "Emissions|Energy|Supply|Electricity",

    AnnualVariableOperatingCost =
      "Cost|Energy Supply|Electricity|Variable O&M",

    CapitalInvestment =
      "Investment|Energy Supply|Electricity",

    Demand =
      "Final Energy|Electricity",

    DiscountedSalvageValue =
      "Model Accounting|Discounted Salvage Value",

    DiscountRate =
      "Model Accounting|Discount Rate",

    InputToNewCapacity =
      "Technology Input|New Capacity",

    InputToTotalCapacity =
      "Technology Input|Total Capacity",

    NewCapacity =
      "Capacity Additions|Electricity",

    ObjectiveValue =
      "Model Accounting|Objective Value",

    ProductionByTechnologyByMode =
      "Secondary Energy|Electricity",

    RateOfActivity =
      "Activity|Electricity",

    RateOfProductionByTechnologyByMode =
      "Secondary Energy|Electricity|Rate",

    RateOfTotalActivity =
      "Activity|Electricity|Total Rate",

    RateOfUseByTechnologyByMode =
      "Technology Input|Rate",

    SalvageValue =
      "Model Accounting|Salvage Value",

    TechnologyEmissionsPenalty =
      "Cost|Emissions Penalty",

    TotalAnnualTechnologyActivityByMode =
      "Activity|Electricity",

    TotalCapacityAnnual =
      "Capacity|Electricity",

    TotalTechnologyAnnualActivity =
      "Activity|Electricity",

    TotalTechnologyModelPeriodActivity =
      "Activity|Electricity|Model Period",

    Trade =
      "Trade|Energy",

    UseByTechnologyByMode =
      "Technology Input"
  )


  MUS_results <- MUS %>%
    mutate(
      t = as.character(
        t
      ),
      f = as.character(
        f
      ),
      e = recode(
        e,
        CO2E = "CO2"
      ),
      variable_iamc_base = unname(
        MUS_variable_mapping[
          as.character(variable)
        ]
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
        na_if(
          trimws(technology_iamc),
          ""
        ),
        na_if(
          trimws(t),
          ""
        )
      ),
      fuel_iamc = coalesce(
        na_if(
          trimws(fuel_iamc),
          ""
        ),
        na_if(
          trimws(f),
          ""
        )
      ),
      variable_iamc = case_when(

        variable == "AnnualTechnologyEmission" ~
          paste(
            "Emissions",
            e,
            "Energy",
            "Supply",
            "Electricity",
            technology_iamc,
            sep = "|"
          ),

        variable == "AnnualTechnologyEmissionByMode" ~
          paste(
            "Emissions",
            e,
            "Energy",
            "Supply",
            "Electricity",
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            sep = "|"
          ),

        variable == "Demand" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        variable == "ProductionByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "RateOfActivity" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "RateOfProductionByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "RateOfUseByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "UseByTechnologyByMode" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            paste0(
              "Time Slice ",
              l
            ),
            sep = "|"
          ),

        variable == "TotalAnnualTechnologyActivityByMode" ~
          paste(
            variable_iamc_base,
            technology_iamc,
            paste0(
              "Mode ",
              m
            ),
            sep = "|"
          ),

        variable == "Trade" ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        variable %in% c(
          "InputToNewCapacity",
          "InputToTotalCapacity"
        ) ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            technology_iamc,
            sep = "|"
          ),

        !is.na(technology_iamc) ~
          paste(
            variable_iamc_base,
            technology_iamc,
            sep = "|"
          ),

        !is.na(fuel_iamc) ~
          paste(
            variable_iamc_base,
            fuel_iamc,
            sep = "|"
          ),

        TRUE ~
          variable_iamc_base
      ),

      variable_iamc = clean_iamc_variable(
        variable_iamc
      ),

      model = "RE-INTEGRATE"
    )


  # -------------------------------------------------------------------------
  # Make mode type consistent before combining
  # -------------------------------------------------------------------------

  DRC_results <- convert_mode_to_character(
    DRC_results
  )

  ETH_results <- convert_mode_to_character(
    ETH_results
  )

  MUS_results <- convert_mode_to_character(
    MUS_results
  )


  # -------------------------------------------------------------------------
  # Combine all countries
  # -------------------------------------------------------------------------

  results <- bind_rows(
    DRC_results,
    ETH_results,
    MUS_results
  )

  results <- as.quitte(
    results
  )


  list(
    x = results,
    class = "quitte",
    weight = NULL,
    unit = "various",
    description = paste(
      "Democratic Republic of the Congo, Mauritius and Ethiopia",
      "model outputs mapped to IAMC-style variables"
    )
  )
  }
