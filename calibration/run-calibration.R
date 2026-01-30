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
# TODO create new_diver from .rds file
params_LTO_comparison$total_diverted[1,,] <- readRDS(here::here("data-raw", "synth.t.diver.rds"))

# TODO check inputs

# Changed Battle Creek abundances to 0 to match LTO. 
# Also edited known_adults in res to call known_adults = grandtab_observed$winter instead of DSMCalibrationData::grandtab_observed$winter
grandtab_observed <- DSMCalibrationData::grandtab_observed
grandtab_observed$winter[3,]<- 0

# Lines 94-103 of LTO Wrapper script
  # Q_free, Q_vern, Q_stck, through SWP_exp 
  # are different years (1998-2017) and values from 
  # r_to_r_baseline_params$freeport_flows, vernalis_flows, stockton_flows, etc.

# Lines 136-138: what are these decay values? 

# TODO update mins and maxes to match LTO script
# TODO run LTO calibration, use set.seed(), set same pop size, iterations, etc.

# Perform calibration --------------------
res <- ga(type = "real-valued",
          fitness =
            function(x) -winter_run_fitness(
              known_adults = grandtab_observed$winter,
              seeds = DSMCalibrationData::grandtab_imputed$winter,
              params = params_LTO_comparison,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11]
            ),
          lower = c(2.5, rep(-3.5, 11)),
          upper = rep(3.5, 12),
          popSize = 100,
          maxiter = 100000,
          run = 50,
          parallel = TRUE,
          pmutation = .4)

readr::write_rds(res, paste0("calibration/res-", Sys.Date(), ".rds"))

res <- readr::read_rds(paste0("calibration/res-", Sys.Date(), ".rds"))

# Evaluate Results ------------------------------------
keep <- c(1, 3)
r1_solution <- res@solution[1, ]

r1_params <- update_params(x = r1_solution, winterRunDSM::r_to_r_baseline_params)
r1_params <- DSMCalibrationData::set_synth_years(r1_params)
r1_sim <- winter_run_model(seeds = DSMCalibrationData::grandtab_imputed$winter, mode = "calibrate",
                           ..params = r1_params,
                           stochastic = FALSE)


r1_nat_spawners <- as_tibble(r1_sim[keep, ,drop = F]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "simulated",
         year = readr::parse_number(year) + 5)


r1_observed <- as_tibble((1 - winterRunDSM::params$proportion_hatchery[keep]) * DSMCalibrationData::grandtab_observed$winter[keep, ]) %>%
  mutate(watershed = DSMscenario::watershed_labels[keep]) %>%
  gather(year, spawners, -watershed) %>%
  mutate(type = "observed", year = as.numeric(year) - 1997) %>%
  filter(!is.na(spawners),
         year > 5)



r1_eval_df <- bind_rows(r1_nat_spawners, r1_observed)


r1_eval_df %>% 
  ggplot(aes(year, spawners, color = type)) + geom_line() + facet_wrap(~watershed, scales = "free_y")

r1_eval_df %>%
  spread(type, spawners) %>%
  ggplot(aes(observed, simulated)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = "Observed vs Predicted updated",
       x = "Observed Natural Spawners",
       y = "Predicted Natural Spawners") +
  xlim(0, 20000) +
  ylim(0, 20000)

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  group_by(watershed) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  ) %>% arrange(desc(abs(r)))

r1_eval_df %>%
  spread(type, spawners) %>%
  filter(!is.na(observed)) %>%
  summarise(
    r = cor(observed, simulated, use = "pairwise.complete.obs")
  )



