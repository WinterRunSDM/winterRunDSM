# remotes::install_github("cvpia-osc/DSMflow")
# remotes::install_github("cvpia-osc/DSMtemperature")
# remotes::install_github("cvpia-osc/DSMhabitat")
# remotes::install_github("cvpia-osc/DSMscenario")


library(winterRunDSM)
library(GA)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DSMCalibrationData)

source("calibration/fitness.R")
source("calibration/update-params.R")

params <- DSMCalibrationData::set_synth_years(winterRunDSM::r_to_r_baseline_params)
params_LTO_comparison <- DSMCalibrationData::set_synth_years(winterRunDSM::r_to_r_baseline_params)

# Test model 
test <- winterRunDSM::winter_run_model(mode = "calibrate", 
                                      seeds = DSMCalibrationData::grandtab_imputed$winter,
                                      ..params = params, 
                                      stochastic = FALSE, 
                                      delta_surv_inflation = FALSE)

test


# Comparison to LTO -------------------------------------------------------


# ensure using same inputs as LTO DSM inputs for comparison
# also updated to bring in extra calibrated parameters per LTO DSM
# read in synthetic diversion
params_LTO_comparison$total_diverted[1,,] <- readRDS(here::here("data-raw", "synth.t.diver.rds"))

# Changed Battle Creek abundances to 0 to match LTO. 
# Also edited known_adults in res to call known_adults = grandtab_observed$winter instead of DSMCalibrationData::grandtab_observed$winter
grandtab_observed <- DSMCalibrationData::grandtab_observed
grandtab_observed$winter[3,]<- 0

# Lines 94-103 of LTO Wrapper script
  # Q_free, Q_vern, Q_stck, through SWP_exp 
  # are different years (1998-2017) and values from 
  # r_to_r_baseline_params$freeport_flows, vernalis_flows, stockton_flows, etc.

# read in calibration data to set above parameters
# TODO why are they so big?
# TODO what is Trap_trans? 
LTO_calib_data <- readRDS("calibration/LTO_inputs/delta-calibration-1980_2017.rds")
params_LTO_comparison$freeport_flows <- LTO_calib_data$Q_free[,19:38]
row.names(params_LTO_comparison$freeport_flows) <- month.abb

params_LTO_comparison$vernalis_flows <- LTO_calib_data$Q_vern[,19:38]
row.names(params_LTO_comparison$vernalis_flows) <- month.abb

params_LTO_comparison$stockton_flows <- LTO_calib_data$Q_stck[,19:38]
row.names(params_LTO_comparison$stockton_flows) <- month.abb

params_LTO_comparison$prisoners_point_temps <- LTO_calib_data$Temp_pp[,19:38]
row.names(params_LTO_comparison$prisoners_point_temps) <- month.abb

params_LTO_comparison$vernalis_temps <- LTO_calib_data$Temp_vern[,19:38]
row.names(params_LTO_comparison$vernalis_temps) <- month.abb

params_LTO_comparison$SWP_exports <- LTO_calib_data$SWP_exp[,19:38]
row.names(params_LTO_comparison$SWP_exports) <- month.abb

params_LTO_comparison$CVP_exports <- LTO_calib_data$CVP_exp[,19:38]
row.names(params_LTO_comparison$CVP_exports) <- month.abb


# Lines 136-138: what are these decay values?
# TODO this is a difference in model structure due to changes proposed and accepted to CVPIA in 2023
# for now we will set Upper Sac to the LTO value for all months (uniform) because it is lower
params_LTO_comparison$spawn_decay_multiplier["Upper Sacramento River",,] <- 0.9736472 # pulled from calibration script from LTO 
params_LTO_comparison$spawn_decay_multiplier["Cottonwood Creek",,] <- 0.9736472 
params_LTO_comparison$spawn_decay_multiplier["Cow Creek",,] <- 0.9736472 
params_LTO_comparison$spawn_decay_multiplier["Clear Creek",,] <- 0.9736472

params_LTO_comparison$spawn_decay_multiplier["Battle Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Antelope Creek",,] <- 0.9949492
params_LTO_comparison$spawn_decay_multiplier["Bear Creek",,] <- 0.9949492
params_LTO_comparison$spawn_decay_multiplier["Big Chico Creek",,] <- 0.9949492
params_LTO_comparison$spawn_decay_multiplier["Butte Creek",,] <- 0.9949492
params_LTO_comparison$spawn_decay_multiplier["Deer Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Elder Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Mill Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Paynes Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Stony Creek",,] <- 0.9949492 
params_LTO_comparison$spawn_decay_multiplier["Thomes Creek",,] <- 0.9949492 

# update mins and maxes to match LTO script
map_params <- tibble("LTO_index" = c(1:16, 13),
                     "R2R_index" = c(2, 6, 7, 10,
                                     NA, NA, NA, NA,
                                     NA, 12, 1, 11, 
                                     3, 4, 5, 9, 8)) |> 
  mutate("LTO_mins" = c(rep(-3.5,10),0,-3.5,rep(0,4), 0), # set 0 for lower bound for en route survival [11]
         "LTO_maxes" = c(rep(3.5,9),-1,rep(3.5,6), 3.5),
         "LTO_suggested" = c(-0.6558315, -3.4999845,  1.4933417, -3.0188308,  2.0000003,  0.7999889, -3.5000000, -0.1999996,
                             -3.4999920, -2.9839253,  3.4999976,  0.6466230,  0.0194795,  0.1000000,  0.3000000,  0.4820249,
                             0.0194795)) |> 
  arrange(R2R_index)

map_params_R2R <- map_params |> 
  filter(!is.na(R2R_index))

# run LTO calibration, use set.seed(), set same pop size, iterations, etc.
set.seed(1234)
pop_size <- 100
iter <- 10000 

LTO_suggestions_matrix <- map_params_R2R |> 
  pull(LTO_suggested) |> 
  matrix(nrow = pop_size, ncol = 12, byrow = TRUE)

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -winter_run_fitness(
              known_adults = grandtab_observed$winter,
              seeds = DSMCalibrationData::grandtab_imputed$winter,
              params = params,
              #params = params_LTO_comparison,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12]
            ),
          lower = c(2.5, rep(-3.5, 11)), # map_params_R2R$LTO_mins,
          upper = rep(3.5, 12), # map_params_R2R$LTO_maxes,
          #suggestions = LTO_suggestions_matrix,
          popSize = pop_size,
          maxiter = iter,
          run = 50,
          parallel = TRUE,
          pmutation = .4)

readr::write_rds(res, paste0("calibration/res-", Sys.Date(), ".rds"))

readr::write_rds(res, paste0("calibration/res-", Sys.Date(), "-LTO_comparison-pop", pop_size, ".rds"))


res <- readr::read_rds(paste0("calibration/res-", Sys.Date(), ".rds"))

# Evaluate Results ------------------------------------
keep <- c(1, 3)
r1_solution <- res@solution[1, ]

r1_params <- update_params(x = r1_solution, winterRunDSM::r_to_r_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter, mode = "calibrate",
                           ..params = r1_params,
                           stochastic = FALSE)


r1_nat_spawners <- as_tibble(r1_sim[keep, ,drop = F]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)


r1_observed <- as_tibble((1 - winterRunDSM::wr_sdm_baseline_params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$winter[keep, ]) |>
  mutate(watershed = DSMscenario::watershed_labels[keep]) |>
  gather(year, spawners, -watershed) |>
  mutate(type = "observed", year = as.numeric(year) - 1997) |>
  filter(!is.na(spawners),
         year > 5)



r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)


r1_eval_df |> 
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df |>
  spread(type, spawners) |>
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Observed vs Predicted updated",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 20000) +
  ylim(0, 20000)

r1_eval_df |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  group_by(watershed) |>
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  ) |> arrange(desc(abs(r)))

r1_eval_df |>
  spread(type, spawners) |>
  filter(!is.na(observed)) |>
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )



