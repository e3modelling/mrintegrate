#' toolmrintegrate
#'
#' Creates and processes RE-INTEGRATE model outputs for DRC, Ethiopia,
#' Mauritius, and Senegal. The function generates harmonized energy and
#' emissions indicators and produces diagnostic plots for electricity
#' generation, CO2 emissions, final energy demand, renewable electricity
#' shares, and relative changes over time.
#'
#' @return NULL. The function is called for its side effects, including
#'   processing model outputs and saving plots as PNG files.
#'
#' @author Fotis Sioutas
#'
#' @examples
#' \dontrun{
#' toolmrintegrate()
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass as.magpie collapseDim
#' @importFrom quitte as.quitte
#' @importFrom dplyr %>% arrange case_when filter first group_by left_join
#'   mutate select summarise ungroup
#' @importFrom tidyr replace_na
#' @importFrom stringr regex str_detect str_remove str_starts str_wrap
#' @importFrom ggplot2 aes element_text facet_wrap geom_col geom_hline
#'   geom_line geom_point ggsave ggplot guide_legend guides labs
#'   scale_fill_discrete scale_x_continuous scale_y_continuous
#'   theme theme_minimal
#' @importFrom postprom helperAggregateLevel
#'
#' @export

toolmrintegrate <- function() {
  a <- calcOutput(type = "FussionOutput", aggregate = FALSE)

  #####breakdown co2
  breakdown_co2<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  breakdown_co2 <- filter(breakdown_co2, region %in% c("DRC"))
  breakdown_co2 <- breakdown_co2 %>%
    dplyr::filter(stringr::str_detect(variable_iamc, "Emissions\\|CO2\\|"))
  library(dplyr)
  library(ggplot2)
  library(stringr)

  breakdown_co2_plot <- breakdown_co2 %>%
    filter(
      period %% 5 == 0,
      str_starts(
        variable_iamc,
        "Emissions|CO2|Energy|Supply|Electricity|"
      ),
      !is.na(value)
    ) %>%
    mutate(
      technology = str_remove(
        variable_iamc,
        "Emissions\\|CO2\\|Energy\\|Supply\\|Electricity\\|"
      )
    )

  p <- ggplot(
    breakdown_co2_plot,
    aes(
      x = period,
      y = value,
      fill = technology
    )
  ) +
    geom_col() +
    facet_wrap(
      ~ scenario,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = sort(unique(breakdown_co2_plot$period))
    ) +
    scale_fill_discrete(
      labels = function(x) stringr::str_wrap(x, width = 18)
    ) +
    labs(
      title = "CO2 Emissions from Electricity Generation - DRC",
      x = "Year",
      y = "Mt CO2/yr",
      fill = "Technology"
    ) +
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE
      )
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        face = "bold"
      )
    )

  ggsave(
    filename = "breakdown_co2_DRC_from_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  ###################
  data <- NULL
  drc<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  drc <- filter(drc, region %in% c("DRC"))
  # drc <- filter(drc, scenario %in% c("BASE"))
  drcCO2 <- filter(drc, variable_iamc %in% c("Emissions|CO2"))
  drcCO2 <- filter(drcCO2, variable %in% c("AnnualEmissions"))
  drcCO2 <- select(drcCO2, - variable)
  drcCO2[["unit"]] <- "Mt CO2/yr"

  data <- rbind(data,drcCO2)
  names(data) <- sub("variable_iamc", "variable", names(data))

  drc_secondary <- drc %>%
    dplyr::filter(grepl("^Secondary Energy", variable_iamc))

  drc_secondary <- filter(drc_secondary, variable %in% c("ProductionByTechnologyAnnual"))
  drc_secondary <- select(drc_secondary, - variable)

  drc_secondary <- drc_secondary %>%
    dplyr::filter(!is.na(value))

  names(drc_secondary) <- sub("variable_iamc", "variable", names(drc_secondary))

  drc_secondary <- drc_secondary %>%
    dplyr::filter(!stringr::str_detect(variable, "Time Slice"))

  check <- drc_secondary %>%
    dplyr::filter(stringr::str_detect(variable, "Electricity after production"))

  library(dplyr)
  library(ggplot2)
  library(stringr)

  plot_data <- check %>%
    filter(
      region == "DRC",
      period %% 5 == 0,
      str_starts(
        variable,
        "Secondary Energy|Electricity|Electricity after production|"
      )
    ) %>%
    mutate(
      technology = str_remove(
        variable,
        "Secondary Energy\\|Electricity\\|Electricity after production\\|"
      ),
      unit = "PJ"
    )

  p <- ggplot(
    plot_data,
    aes(
      x = period,
      y = value,
      fill = technology
    )
  ) +
    geom_col() +
    facet_wrap(
      ~ scenario,
      ncol = 1,
      scales = "free_y"
    ) +
    scale_x_continuous(
      breaks = sort(unique(plot_data$period))
    ) +
    labs(
      title = "Electricity after production - DRC",
      x = "Year",
      y = "PJ",
      fill = "Technology"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      plot.title = element_text(
        face = "bold"
      )
    )

  # ggsave(
  #   filename = "Electricity_after_production_DRC.png",
  #   plot = p,
  #   width = 12,
  #   height = 9,
  #   dpi = 300
  # )

  # drc_secondary <- drc_secondary %>%
  #   dplyr::filter(stringr::str_detect(variable, "Electricity after production"))
  #
  # check <- drc_secondary

  drcSEtotal <- drc_secondary %>%
    dplyr::filter(
      stringr::str_detect(
        variable,
        "^Secondary Energy\\|Electricity\\|Electricity after production\\|"
      )
    ) %>%
    dplyr::group_by(scenario, region, period) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      variable = "Secondary Energy|Electricity",
      unit = "PJ"
    )

  data <- rbind(data,drcSEtotal)

  library(ggplot2)
  library(dplyr)

  p <- drcSEtotal %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "DRC Electricity Generation",
      subtitle = "BASE vs MEAP",
      x = "Year",
      y = "Electricity Generation (PJ)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  p <- drcSEtotal %>%
    filter(period <= 2042) %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "DRC Electricity Generation",
      subtitle = "Historical and near-term period",
      x = "Year",
      y = "Electricity Generation (PJ)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_Electricity_Generation_Historical_and_near_term_period.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  library(ggplot2)
  library(dplyr)

  p <- drcCO2 %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "DRC CO2 Emissions from Electricity Generation",
      subtitle = "BASE vs MEAP",
      x = "Year",
      y = "CO2 Emissions (Mt CO2/yr)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_CO2_Emissions_from_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  p <- drcCO2 %>%
    filter(period <= 2042) %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "DRC CO2 Emissions",
      subtitle = "Historical and near-term period",
      x = "Year",
      y = "CO2 Emissions (Mt CO2/yr)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_CO2_Emissions_Historical_near_term_period_2.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  library(dplyr)
  library(stringr)

  renewable_share <- check %>%
    filter(
      str_detect(
        variable,
        "^Secondary Energy\\|Electricity\\|Electricity after production\\|"
      )
    ) %>%
    mutate(
      renewable = str_detect(
        variable,
        regex("biomass|hydro|solar|wind|geothermal", ignore_case = TRUE)
      )
    ) %>%
    group_by(scenario, region, period) %>%
    summarise(
      total_generation = sum(value, na.rm = TRUE),
      renewable_generation = sum(value[renewable], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      renewable_share = 100 * renewable_generation / total_generation
    )

  # data <- rbind(data,renewable_share)

  p <- renewable_share %>%
    ggplot(aes(
      x = period,
      y = renewable_share,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "DRC Renewable Electricity Share",
      subtitle = "BASE vs MEAP",
      x = "Year",
      y = "Renewable electricity share (%)",
      color = "Scenario"
    ) +
    scale_y_continuous(
      limits = c(50, 100)
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_Renewable_Electricity_Share.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  # drc<- a[,c("scenario","period","region","variable_iamc","unit","value")]
  # drc <- filter(drc, region %in% c("DRC"))
  # drc <- filter(drc, scenario %in% c("BASE"))
  drcco2_share <- drc

  drcco2_share <- filter(drcco2_share, region %in% c("DRC"))
  drcco2_share <- filter(drcco2_share, variable %in% c("AnnualTechnologyEmission"))
  drcco2_share <- select(drcco2_share, - variable)

  names(drcco2_share) <- sub("variable_iamc", "variable", names(drcco2_share))
  drcco2_share <- drcco2_share %>%
    filter(
      str_detect(
        variable,
        "^Emissions"
      ))

  library(dplyr)
  library(stringr)
  library(ggplot2)
  library(tidyr)

  drc_emissions_breakdown <- drcco2_share %>%
    filter(
      str_detect(
        variable,
        "^Emissions\\|CO2\\|Energy\\|Supply\\|Electricity\\|"
      )
    ) %>%
    mutate(
      value = replace_na(value, 0),
      fuel = case_when(
        str_detect(variable, regex("natural gas", ignore_case = TRUE)) ~ "Natural gas",
        str_detect(variable, regex("heavy fuel oil|oil", ignore_case = TRUE)) ~ "Oil",
        str_detect(variable, regex("coal", ignore_case = TRUE)) ~ "Coal",
        str_detect(variable, regex("biomass", ignore_case = TRUE)) ~ "Biomass",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(fuel)) %>%
    group_by(scenario, region, period, fuel) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  # data <- rbind(data,drc_emissions_breakdown)

  p <- drc_emissions_breakdown %>%
    ggplot(aes(
      x = period,
      y = value,
      fill = fuel
    )) +
    geom_col() +
    facet_wrap(~ scenario) +
    labs(
      title = "DRC CO2 Emissions from Electricity Generation",
      subtitle = "Emissions breakdown by fuel",
      x = "Year",
      y = "CO2 Emissions",
      fill = "Fuel"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_CO2_Emissions_from_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  p <- drc_emissions_breakdown %>%
    filter(period <= 2040) %>%
    ggplot(aes(
      x = period,
      y = value,
      fill = fuel
    )) +
    geom_col() +
    facet_wrap(~ scenario) +
    labs(
      title = "DRC CO2 Emissions from Electricity Generation",
      subtitle = "Emissions breakdown by fuel, until 2040",
      x = "Year",
      y = "CO2 Emissions",
      fill = "Fuel"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_CO2_Emissions_from_Electricity_Generation_until_2040.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  drc_FE <- select(drc, - variable) %>%
    dplyr::filter(grepl("^Final Energy", variable_iamc))

  drc_FE <- drc_FE %>%
    dplyr::filter(!is.na(value))

  names(drc_FE) <- sub("variable_iamc", "variable", names(drc_FE))



  drc_FE <- drc_FE %>%
    dplyr::filter(!stringr::str_detect(variable, "Time Slice"))

  drcFE_breakdown <- drc_FE %>%
    filter(
      str_detect(
        variable,
        "^Final Energy\\|Electricity\\|"
      )
    ) %>%
    mutate(
      value = replace_na(value, 0),

      sector = case_when(
        str_detect(variable, "Residential") ~ "Residential",
        str_detect(variable, "Commercial") ~ "Commercial",
        str_detect(variable, "Industrial") ~ "Industry",
        str_detect(variable, "Transport") ~ "Transport",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(sector)) %>%
    group_by(
      scenario,
      region,
      period,
      sector
    ) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      unit = "PJ"
    )

  drcFEtotal <- drcFE_breakdown %>%
    group_by(
      scenario,
      region,
      period
    ) %>%
    summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      variable_iamc = "Final Energy|Electricity",
      unit = "PJ"
    )

  rb <- drcFEtotal
  names(rb) <- sub("variable_iamc", "variable", names(rb))
  data <- rbind(data,rb)

  p <- drcFE_breakdown %>%
    ggplot(aes(
      x = period,
      y = value,
      fill = sector
    )) +
    geom_col() +
    facet_wrap(~ scenario) +
    labs(
      title = "DRC Final Energy Electricity Demand",
      subtitle = "Breakdown by sector",
      x = "Year",
      y = "Final Energy (PJ)",
      fill = "Sector"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_Final_Energy_Electricity_Demand.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  # data <- rbind(data,drcFE_breakdown)

  p <- drcFE_breakdown %>%
    filter(period <= 2040) %>%
    ggplot(aes(
      x = period,
      y = value,
      fill = sector
    )) +
    geom_col() +
    facet_wrap(~ scenario) +
    labs(
      title = "DRC Final Energy Electricity Demand",
      subtitle = "Breakdown by sector, until 2040",
      x = "Year",
      y = "Final Energy (PJ)",
      fill = "Sector"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "DRC_Final_Energy_Electricity_Demand_until_2040.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  ############## ETH
  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value")]
  drcETH <- filter(drcETH, region %in% c("ETH"))
  drcETH <- filter(drcETH, scenario %in% c("Baseline"))

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Emissions\\|CO2"
      )
    )


  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.3)
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Emissions|CO2"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "Mt CO2/yr"
  EMICO2ETH[["scenario"]] <- "Baseline"
  one <- EMICO2ETH

  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value")]
  drcETH <- filter(drcETH, region %in% c("ETH"))
  drcETH <- filter(drcETH, scenario %in% c("Ambitious_85percentBy2040"))

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Emissions\\|CO2"
      )
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.3)
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Emissions|CO2"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "Mt CO2/yr"
  EMICO2ETH[["scenario"]] <- "Ambitious_85percentBy2040"
  two <- EMICO2ETH

  totalCO2ETH <- rbind(one, two)
  totalCO2ETH[["value"]] <- totalCO2ETH[["value"]] / 1000

  data <- rbind(data,select(totalCO2ETH, - model))

  p <- totalCO2ETH %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "ETH CO2 Emissions from Electricity Generation",
      subtitle = "Baseline vs Ambitious_85percentBy2040",
      x = "Year",
      y = "CO2 Emissions (Mt CO2/yr)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "ETH_CO2_Emissions_from_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )



  ############## ETH
  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","t")]
  drcETH <- filter(drcETH, region %in% c("ETH"))
  drcETH <- filter(drcETH, scenario %in% c("Baseline"))

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  generation_t <- c(
    "PWRBIO",
    "PWRGEO",
    "PWRHYD001",
    "PWRHYD002",
    "PWRSOL001",
    "PWRSOLOFF",
    "PWRWND001"
  )

  drcETH <- drcETH %>%
    dplyr::filter(
      region == "ETH",
      scenario == "Baseline",
      t %in% generation_t
    )

  drcETH <- select(drcETH, -t)

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Secondary Energy\\|Electricity"
      )
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.3)
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Secondary Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Baseline"
  one <- EMICO2ETH

  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","t")]
  drcETH <- filter(drcETH, region %in% c("ETH"))
  drcETH <- filter(drcETH, scenario %in% c("Ambitious_85percentBy2040"))

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  generation_t <- c(
    "PWRBIO",
    "PWRGEO",
    "PWRHYD001",
    "PWRHYD002",
    "PWRSOL001",
    "PWRSOLOFF",
    "PWRWND001"
  )

  drcETH <- drcETH %>%
    dplyr::filter(
      region == "ETH",
      scenario == "Ambitious_85percentBy2040",
      t %in% generation_t
    )


  drcETH <- select(drcETH, -t)

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Secondary Energy\\|Electricity"
      )
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.3)
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Secondary Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Ambitious_85percentBy2040"
  two <- EMICO2ETH

  totalCO2ETH <- rbind(one, two)
  totalCO2ETH[["value"]] <- totalCO2ETH[["value"]]

  data <- rbind(data,select(totalCO2ETH, - model))

  p <- totalCO2ETH %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "ETH Electricity Generation",
      subtitle = "Baseline vs Ambitious_85percentBy2040",
      x = "Year",
      y = "Electricity Generation (PJ)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )


  ggsave(
    filename = "ETH_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )



  ############## MUS
  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test293"))
  drcETH <- filter(drcETH, variable %in% c("AnnualTechnologyEmission"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Emissions\\|CO2"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)
  drcETHCO2m <- drcETHCO2m[,2023:2050]

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Emissions|CO2"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "Mt CO2/yr"
  EMICO2ETH[["scenario"]] <- "Test293"
  one <- EMICO2ETH

  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test292"))
  drcETH <- filter(drcETH, variable %in% c("AnnualTechnologyEmission"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Emissions\\|CO2"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)
  drcETHCO2m <- drcETHCO2m[,2023:2050]

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Emissions|CO2"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "Mt CO2/yr"
  EMICO2ETH[["scenario"]] <- "Test292"
  two <- EMICO2ETH

  totalCO2ETH <- rbind(one, two)

  data <- rbind(data,select(totalCO2ETH, - model))

  p <- totalCO2ETH %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "MUS CO2 Emissions from Electricity Generation",
      subtitle = "Test293 vs Test292",
      x = "Year",
      y = "CO2 Emissions (Mt CO2/yr)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "MUS_CO2_Emissions_from_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  ############## MUS
  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable","t")]

  generation_t <- c(
    "PWRCOA001",
    "PWRCOA002",
    "PWRCOA003",
    "PWRCOA004",
    "PWRHFO001",
    "PWRHFO002",
    "PWRHFO003",
    "PWRHYD001",
    "PWRKER001",
    "PWRSOL001",
    "PWRSOL002",
    "PWRSOL003",
    "PWRSOL004",
    "PWRSOL005",
    "PWRWND001"
  )

  drcETH <- drcETH %>%
    dplyr::filter(
      t %in% generation_t
    )

  drcETH <- select(drcETH, -t)
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test293"))
  drcETH <- filter(drcETH, variable %in% c("ProductionByTechnologyByMode"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Secondary Energy\\|Electricity"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)
  drcETHCO2m <- drcETHCO2m[,2023:2050]

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Secondary Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Test293"
  one <- EMICO2ETH

  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable","t")]

  drcETH <- drcETH %>%
    dplyr::filter(
      t %in% generation_t
    )

  drcETH <- select(drcETH, -t)
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test292"))
  drcETH <- filter(drcETH, variable %in% c("ProductionByTechnologyByMode"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Secondary Energy\\|Electricity"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Secondary Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Test292"
  two <- EMICO2ETH

  totalCO2ETH <- rbind(one, two)
  totalCO2ETH[["value"]] <- totalCO2ETH[["value"]]

  data <- rbind(data,select(totalCO2ETH, - model))

  p <- totalCO2ETH %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "MUS Electricity Generation",
      subtitle = "Test293 vs Test292",
      x = "Year",
      y = "Electricity Generation (PJ)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "MUS_Electricity_Generation.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  ############## MUS
  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test293"))
  drcETH <- filter(drcETH, variable %in% c("Demand"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Final Energy\\|Electricity"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)
  drcETHCO2m <- drcETHCO2m[,2023:2050]

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Final Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Test293"
  one <- EMICO2ETH

  drcETH<- a[,c("scenario","period","region","variable_iamc","unit","value","variable")]
  drcETH <- filter(drcETH, region %in% c("MUS"))
  drcETH <- filter(drcETH, scenario %in% c("Test292"))
  drcETH <- filter(drcETH, variable %in% c("Demand"))
  drcETH <- select(drcETH, -variable)

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETHCO2 <- drcETH %>%
    filter(
      str_detect(
        variable,
        "^Final Energy\\|Electricity"
      )
    )

  drcETHCO2 <- drcETHCO2 %>%
    dplyr::group_by(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETHCO2m <- as.quitte(drcETHCO2) %>% as.magpie()

  drcETHCO2m[is.na(drcETHCO2m)] <- 0
  drcETHCO2m <- collapseDim(drcETHCO2m, 3.1)

  drcETHCO2m <- helperAggregateLevel(drcETHCO2m, level = 1, recursive = TRUE)
  drcETHCO2m <- drcETHCO2m[,2023:2050]

  EMICO2ETH <- as.quitte(drcETHCO2m[,,"Final Energy|Electricity"]) %>% as.data.frame()
  EMICO2ETH[["unit"]] <- "PJ"
  EMICO2ETH[["scenario"]] <- "Test292"
  two <- EMICO2ETH

  totalCO2ETH <- rbind(one, two)

  data <- rbind(data,select(totalCO2ETH, - model))

  p <- totalCO2ETH %>%
    ggplot(aes(
      x = period,
      y = value,
      color = scenario
    )) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 1.5) +
    labs(
      title = "MUS Final Energy Electricity Demand",
      subtitle = "Test293 vs Test292",
      x = "Year",
      y = " Final Energy Electricity Demand (PJ)",
      color = "Scenario"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  ggsave(
    filename = "MUS_Final_Energy_Electricity_Demand.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )


  ############## SEN
  drcETH<- a[,c("period","region","variable_iamc","unit","value")]
  drcETH <- filter(drcETH, region %in% c("SEN"))

  names(drcETH) <- sub("variable_iamc", "variable", names(drcETH))

  drcETH <- drcETH %>%
    dplyr::group_by(
      region,
      variable,
      unit,
      period
    ) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    )

  drcETH[["unit"]] <- "PJ"
  drcETH[["value"]] <- drcETH[["value"]] * 0.0036

  p <- ggplot(drcETH, aes(x = period, y = value)) +
    geom_line(linewidth = 1) +
    geom_point(size = 2) +
    labs(
      title = "Senegal Secondary Energy|Electricity",
      x = "Year",
      y = "PJ"
    ) +
    scale_x_continuous(
      breaks = 2012:2029
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )

  ggsave(
    filename = "Senegal_Secondary_Energy_Electricity.png",
    plot = p,
    width = 12,
    height = 9,
    dpi = 300
  )

  library(dplyr)
  library(ggplot2)

  # data_increase <- data %>%
  #   arrange(
  #     scenario,
  #     region,
  #     variable,
  #     unit,
  #     period
  #   ) %>%
  #   group_by(
  #     scenario,
  #     region,
  #     variable,
  #     unit
  #   ) %>%
  #   mutate(
  #     baseline_value = first(value),
  #     baseline_period = first(period),
  #     increase = (value - baseline_value) / baseline_value * 100
  #   ) %>%
  #   ungroup() %>%
  #   mutate(
  #     region_scenario = paste(region, scenario, sep = " - ")
  #   )

  data_bigger_than_2022 <- filter(data, period > 2022)

  data_increase <- data_bigger_than_2022 %>%
    arrange(
      scenario,
      region,
      variable,
      unit,
      period
    ) %>%
    group_by(
      scenario,
      region,
      variable,
      unit
    ) %>%
    mutate(
      baseline_period = first(period[value != 0 & !is.na(value)]),
      baseline_value = first(value[value != 0 & !is.na(value)]),
      increase = ifelse(
        period < baseline_period,
        NA_real_,
        (value - baseline_value) / baseline_value * 100
      )
    ) %>%
    ungroup() %>%
    mutate(
      region_scenario = paste(region, scenario, sep = " - ")
    )

  data_increase <- filter(data_increase, period < 2036)

  plots <- data_increase %>%
    split(.$variable) %>%
    lapply(function(df) {

      ggplot(
        df,
        aes(
          x = period,
          y = increase,
          color = region_scenario,
          group = region_scenario
        )
      ) +
        geom_hline(
          yintercept = 0,
          linetype = "dashed",
          color = "grey50"
        ) +
        geom_line(linewidth = 1) +
        geom_point(size = 1.5) +
        scale_x_continuous(
          breaks = seq(
            floor(min(df$period, na.rm = TRUE)),
            ceiling(max(df$period, na.rm = TRUE)),
            by = 1
          )
        ) +
        labs(
          title = unique(df$variable),
          x = "Year",
          y = "Change relative to 2023 (%)",
          color = "Region - Scenario"
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(
            size = 11,
            face = "bold"
          ),
          axis.text.x = element_text(
            angle = 45,
            hjust = 1
          ),
          legend.position = "bottom"
        )
    })

  for (var in names(plots)) {

    filename <- gsub(
      "[^A-Za-z0-9_-]",
      "_",
      var
    )

    ggsave(
      filename = paste0(filename, "bigger_than_2022_less_than_2036.png"),
      plot = plots[[var]],
      width = 10,
      height = 6,
      dpi = 300
    )
  }

  x <- NULL

  return(x)
}

