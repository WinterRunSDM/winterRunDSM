#' @title Adult Straying
#' @description Calculate the proportion of adults straying to non-natal streams to spawn
#' @details See \code{\link{params}} for details on parameter sources
#' @param wild Variable indicator of wild fish returning
#' @param natal_flow Variable describing proportion flows at tributary junctions coming from natal watershed in October
#' @param south_delta_watershed Variable indicator if watershed feeds into South Delta
#' @param cross_channel_gates_closed Variable describing number of days gates are closed for each month
#' @param prop_bay_trans Variable describing proportion transport to the bay
#' @param prop_delta_trans Variable describing proportion transport to the delta
#' @param .intercept Intercept
#' @param .wild Coefficient for \code{wild} variable
#' @param .natal_flow Coefficient for \code{natal_flow} variable
#' @param .cross_channel_gates_closed Coefficient for \code{cross_channel_gates_closed} variable
#' @param .prop_bay_trans Coefficient for \code{prop_bay_trans} variable
#' @param .prop_delta_trans Coefficient for \code{prop_delta_trans} variable
#' @source IP-117068
#' @export
adult_stray <- function(wild, natal_flow, south_delta_watershed, cross_channel_gates_closed,
                        prop_bay_trans = 0, prop_delta_trans = 0, # bring these up top level so we can have as params based on hatchery logic (can also manipualate props in R2R scenario)
                        .intercept = winterRunDSM::wr_sdm_baseline_params$.adult_stray_intercept,
                        .wild = winterRunDSM::wr_sdm_baseline_params$.adult_stray_wild,
                        .natal_flow = winterRunDSM::wr_sdm_baseline_params$.adult_stray_natal_flow,
                        .cross_channel_gates_closed = winterRunDSM::wr_sdm_baseline_params$.adult_stray_cross_channel_gates_closed,
                        .prop_bay_trans = winterRunDSM::wr_sdm_baseline_params$.adult_stray_prop_bay_trans,
                        .prop_delta_trans = winterRunDSM::wr_sdm_baseline_params$.adult_stray_prop_delta_trans){

  boot::inv.logit(
    .intercept +
    .wild * wild +
    .natal_flow * natal_flow +
    .cross_channel_gates_closed * south_delta_watershed * cross_channel_gates_closed +
    .prop_bay_trans * prop_bay_trans * ( 1 - wild) +
    .prop_delta_trans * prop_delta_trans * (1 - wild)
  )

}

#' @title Adult En Route Survival
#' @description Calculate adult survival en route to spawning grounds
#' @details See \code{\link{params}} for details on parameter sources
#' @param migratory_temp Variable representing proportion of migratory corridor temperature above  20°C
#' @param bypass_overtopped Indicator for bypass overtopped
#' @param ..surv_adult_enroute_int Intercept
#' @param .migratory_temp Coefficient for \code{migratory_temp} variable
#' @param .bypass_overtopped Coefficient for \code{bypass_overtopped} variable
#' @source IP-117068
#' @export
surv_adult_enroute <- function(migratory_temp, bypass_overtopped, 
                               ..surv_adult_enroute_int = winterRunDSM::wr_sdm_baseline_params$..surv_adult_enroute_int,
                               .migratory_temp = winterRunDSM::wr_sdm_baseline_params$.adult_en_route_migratory_temp,
                               .bypass_overtopped = winterRunDSM::wr_sdm_baseline_params$.adult_en_route_bypass_overtopped,
                               # winter run SDM parameter
                               adult_enroute_surv_mult = winterRunDSM::wr_sdm_baseline_params$adult_enroute_surv_mult) {

  pmax(boot::inv.logit(..surv_adult_enroute_int +
                       .migratory_temp * migratory_temp +
                       .bypass_overtopped * bypass_overtopped), 0) * adult_enroute_surv_mult
}

#' Apply enroute survival to adult salmon
#'
#' This function calculates the survival of adult salmon during their enroute migration.
#'
#' @param year The year of the simulation.
#' @param month The starting month of the simulation.
#' @param adults A data frame containing information about adult salmon.
#' @param month_return_proportions A vector of proportions for each month.
#' @param gates_overtopped A three-dimensional array containing information about gates overtopped.
#' @param tisdale_bypass_watershed A parameter related to the Tisdale bypass watershed.
#' @param yolo_bypass_watershed A parameter related to the Yolo bypass watershed.
#' @param migratory_temperature_proportion_over_20 A matrix containing proportions of migratory temperatures over 20 degrees.
#' @param ..surv_adult_enroute_int Additional parameters for adult enroute survival.
#' @param .adult_en_route_migratory_temp Parameter for adult enroute survival related to migratory temperature.
#' @param .adult_en_route_bypass_overtopped Parameter for adult enroute survival related to bypass overtopped.
#' @param stochastic Logical indicating whether to include stochastic elements in the simulation.
#'
#' @return A list containing the initial number of adults, the proportion of natural adults, and the initial number of adults by month.
#'
#'
#' @export
apply_enroute_survival <- function(year,
                                   month,
                                   adults,
                                   month_return_proportions,
                                   gates_overtopped,
                                   tisdale_bypass_watershed,
                                   yolo_bypass_watershed,
                                   migratory_temperature_proportion_over_20,
                                   ..surv_adult_enroute_int,
                                   .adult_en_route_migratory_temp,
                                   .adult_en_route_bypass_overtopped,
                                   hatchery_release,
                                   stochastic,
                                   # winter Run SDM parameter
                                   adult_enroute_surv_mult) {
  # Do adults by month
  # Natural Adults
  natural_adults_by_month <- t(sapply(1:31, function(watershed) {
    if (stochastic) {
      rmultinom(1, rowSums(adults$natural_adults)[watershed], month_return_proportions)
    } else {
      round(rowSums(adults$natural_adults)[watershed] * month_return_proportions)
    }
  }))
  
  # Hatchery Adults
  hatchery_adults_by_month <- t(sapply(1:31, function(watershed) {
    if (stochastic) {
      rmultinom(1, rowSums(adults$hatchery_adults)[watershed], month_return_proportions)
    } else {
      round(rowSums(adults$hatchery_adults)[watershed] * month_return_proportions)
    }
  }))
  
  
  # for all years and months 1-4 there is always at least one true
  bypass_is_overtopped <- sapply(1:4, function(month) {
    
    tis <- gates_overtopped[month, year, 1] * tisdale_bypass_watershed
    yolo <- gates_overtopped[month, year, 2] * yolo_bypass_watershed
    as.logical(tis + yolo)
  })
  
  en_route_temps <- migratory_temperature_proportion_over_20[, 1:4]
  
  adult_en_route_surv <- pmin(sapply(1:4, function(month) {
    adult_en_route_surv <- surv_adult_enroute(migratory_temp = en_route_temps[,month],
                                              bypass_overtopped = bypass_is_overtopped[,month],
                                              ..surv_adult_enroute_int = ..surv_adult_enroute_int,
                                              .migratory_temp = .adult_en_route_migratory_temp,
                                              .bypass_overtopped = .adult_en_route_bypass_overtopped,
                                              adult_enroute_surv_mult = adult_enroute_surv_mult)
  }), 1)
  
  
  # Natural adults
  natural_adults_survived_to_spawning <- sapply(1:4, function(month) {
    if (stochastic) {
      rbinom(31, round(natural_adults_by_month[, month]), adult_en_route_surv[, month])
    } else {
      round(natural_adults_by_month[, month] * adult_en_route_surv[, month])
    }
  })
  # Hatchery
  # APPLY NAT ADULT REMOVAL RATE (APPLIED TO HATCHERY FISH instead of natural fish)
  hatchery_adults_survived_to_spawning <- sapply(1:4, function(month) {
    if (stochastic) {
      rbinom(31, round(hatchery_adults_by_month[, month]), adult_en_route_surv[, month])
    } else {
      round(hatchery_adults_by_month[, month] * adult_en_route_surv[, month])
    }
  })
  # Removes logic to remove hatchery fish if no
  if (sum(hatchery_release) > 0 ) {
    hatchery_adults_survived_to_spawning <- sapply(1:4, function(month) {
      if (stochastic) {
        rbinom(31, round(hatchery_adults_survived_to_spawning[, month]), (1 - natural_adult_removal_rate))
      } else {
        round(hatchery_adults_survived_to_spawning[, month] * (1 - natural_adult_removal_rate))
      }
    })
  }
  # TODO remove fish in non spawn regions
  surviving_natural_adults <- ifelse(rowSums(natural_adults_survived_to_spawning) < 0, 0,
                                     rowSums(natural_adults_survived_to_spawning))
  surviving_hatchery_adults <- ifelse(rowSums(hatchery_adults_survived_to_spawning) < 0, 0, rowSums(hatchery_adults_survived_to_spawning))
  init_adults <- surviving_natural_adults + surviving_hatchery_adults
  proportion_natural <- surviving_natural_adults / init_adults
  
  list(init_adults = round(init_adults),
       proportion_natural = proportion_natural
  ) # TODO do we need inut adult by month
}

#' @title Adult Prespawn Survival
#' @description Calculate the adult prespawn survival
#' @details See \code{\link{params}} for details on parameter sources
#' @param deg_day Variable describing average degree days
#' @param .adult_prespawn_int Intercept
#' @param .deg_day Coefficient for \code{deg_day} variable
#' @source IP-117068
#' @export
surv_adult_prespawn <- function(deg_day,
                                .adult_prespawn_int = winterRunDSM::wr_sdm_baseline_params$.adult_prespawn_int,
                                .deg_day = winterRunDSM::wr_sdm_baseline_params$.adult_prespawn_deg_day,
                                # WR SDM addition adding the prespawn survival multiplier
                                surv_adult_prespawn_mult= winterRunDSM::wr_sdm_baseline_params$surv_adult_prespawn_mult){

  pmin(boot::inv.logit(.adult_prespawn_int + .deg_day * deg_day) * surv_adult_prespawn_mult, 1)
}

#' @title Prepare Beta-regression Data
#' @description
#' Create data frame of model data to be used in the beta-regression model for stray rates
#' @param hatchery name of hatchery to produce data for
#' @param type either in river or bay release
#' @param sim_year simulation year, used to calculate run year
#' @param flow_oct_nov the mean flow for Oct-Nov, use DSMflow::hatchery_oct_nov_flows
#' @param flow_apr_may the mean flow for Apr-May, use DSMflow::hatchery_apr_may_flows
#' @param released the total number of released hatchery fish
#' @param mean_PDO_return ...
#'
#' @details
#' Natural origin fish get a stray rate equal to Feather stray at distance = 0.
#'
#' @keywords internal
prepare_stray_model_data <- function(hatchery, type = c("natural", "hatchery"), sim_year,
                                     flow_oct_nov, flow_apr_may, releases, mean_PDO_return) {
  run_year <- sim_year + 1980
  flow_discrep <- flow_oct_nov - flow_apr_may
  return(
    tidyr::expand_grid(
      hatchery = hatchery,
      dist_hatch = normalize_with_params(
        if (type == "hatchery") c(fallRunDSM::hatchery_to_bay_distance[hatchery], 0) else  0,
        betareg_normalizing_context$dist_hatch$mean,
        betareg_normalizing_context$dist_hatch$sd),
      run_year = normalize_with_params(
        run_year, betareg_normalizing_context$run_year$mean, betareg_normalizing_context$run_year$sd
      ),
      age = normalize_with_params(2:5, betareg_normalizing_context$age$mean, betareg_normalizing_context$age$sd),
      Total_N = normalize_with_params(releases, betareg_normalizing_context$Total_N$mean, betareg_normalizing_context$Total_N$sd),
      rel_month = normalize_with_params(1, betareg_normalizing_context$rel_month$mean, betareg_normalizing_context$rel_month$sd),
      flow.1011 = normalize_with_params(flow_oct_nov, betareg_normalizing_context$flow.1011$mean, betareg_normalizing_context$flow.1011$sd),
      flow_discrep = normalize_with_params(
        flow_discrep,
        betareg_normalizing_context$flow_discrep$mean,
        betareg_normalizing_context$flow_discrep$sd),
      mean_PDO_retn = normalize_with_params(mean_PDO_return, betareg_normalizing_context$mean_PDO_retn$mean, betareg_normalizing_context$mean_PDO_retn$sd)
    )
  )
}


#' @title Apply Harvest to Adult Salmon
#' @description Applies age-structured ocean harvest (weighted by proportion mature)
#' and a flat tributary harvest rate to hatchery or natural adult salmon.
#' @param adults A named numeric vector of length 31 representing the number of
#' adults per watershed
#' @param age_dist A tibble with columns \code{prop_2}, \code{prop_3},
#' \code{prop_4}, and \code{prop_5} representing the proportion of adults in
#' each age class per watershed
#' @param harvest_rate_ocean A named numeric vector of length 4 with names
#' \code{"2"}, \code{"3"}, \code{"4"}, \code{"5"} representing the ocean
#' harvest rate for each age class
#' @param prop_mature_by_age A named numeric vector of length 4 with names
#' \code{"2"}, \code{"3"}, \code{"4"}, \code{"5"} representing the proportion
#' of fish that are mature and therefore subject to harvest for each age class
#' @param harvest_rate_trib A scalar representing the flat incidental harvest
#' rate applied to all age classes in tributaries
#' @return A list with two elements:
#' \itemize{
#'   \item \code{after_harvest}: A 31 x 4 matrix of adults remaining after
#'   ocean and tributary harvest, by watershed and age class
#'   \item \code{harvested}: A named numeric vector of length 31 representing
#'   the total number of adults harvested per watershed
#' }
#' @source IP-117068
#' @export
apply_harvest <- function(adults, age_dist, harvest_rate_ocean, prop_mature_by_age, harvest_rate_trib) {
  effective_ocean_harvest <- harvest_rate_ocean * prop_mature_by_age
  
  by_age <- round(unname(adults) * as.matrix(age_dist[2:5]))
  row.names(by_age) <- winterRunDSM::watershed_labels
  colnames(by_age) <- 2:5
  
  after_ocean <- t(apply(by_age, 1, function(ws) round(ws * (1 - effective_ocean_harvest))))
  
  by_age_final <- round(after_ocean * (1 - harvest_rate_trib))
  row.names(by_age_final) <- winterRunDSM::watershed_labels
  colnames(by_age_final) <- 2:5
  
  list(after_harvest = by_age_final,
       harvested = adults - rowSums(by_age_final))
}
