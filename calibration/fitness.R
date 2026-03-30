# Fitness Function ------------------
winter_run_fitness <- function(
  known_adults,
  seeds,
  params,
  surv_adult_enroute_int,
  surv_juv_rear_int,
  surv_juv_rear_contact_points,
  surv_juv_rear_prop_diversions,
  surv_juv_rear_total_diversions,
  surv_juv_bypass_int,
  surv_juv_delta_int,
  surv_juv_delta_contact_points,
  surv_juv_delta_total_diverted,
  surv_juv_outmigration_sj_int,
  ocean_entry_success_int,
  surv_egg_to_fry_temp_effect,
  keep = c(1),
  comp_years = c(6, 20)
) {
  
  params_init <- params
  
  params_init$..surv_adult_enroute_int = surv_adult_enroute_int
  params_init$..surv_juv_rear_int = rep(surv_juv_rear_int, 31)
  params_init$..surv_juv_rear_contact_points = surv_juv_rear_contact_points
  params_init$..surv_juv_rear_prop_diversions = surv_juv_rear_prop_diversions
  params_init$..surv_juv_rear_total_diversions = surv_juv_rear_total_diversions
  params_init$..surv_juv_bypass_int = surv_juv_bypass_int
  params_init$..surv_juv_delta_int = surv_juv_delta_int
  params_init$..surv_juv_delta_contact_points = surv_juv_delta_contact_points
  params_init$..surv_juv_delta_total_diverted = surv_juv_delta_total_diverted
  params_init$..surv_juv_outmigration_sj_int = surv_juv_outmigration_sj_int
  params_init$..surv_egg_to_fry_mean_egg_temp_effect = surv_egg_to_fry_temp_effect
  params_init$..ocean_entry_success_int = rep(ocean_entry_success_int, 31)

  tryCatch(
    withCallingHandlers({
      preds <- winter_run_model(mode = "calibrate",
                                seeds = seeds,
                                stochastic = FALSE,
                                ..params = params_init)
      
      known_nats <- known_adults[keep, comp_years[1]:comp_years[2], drop = FALSE] 
      mean_escapent <- rowMeans(known_nats, na.rm = TRUE)
      
      sse <- sum(((preds[keep, , drop = FALSE] - known_nats)^2)/mean_escapent, na.rm = TRUE)
      
      return(sse)
    },
    warning = function(w) invokeRestart("muffleWarning")
    ),
    error = function(e) return(1e12)
  )
}
