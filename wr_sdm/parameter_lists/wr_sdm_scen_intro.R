# Scenario for 3/18/2026 SDM meeting
wr_sdm_scen_intro_params <- wr_sdm_baseline_params

# H1
wr_sdm_scen_intro_params$hatchery_release["Upper Sacramento River","l",] <- rep(280000, 20)
wr_sdm_scen_intro_params$hatchery_release["Upper-mid Sacramento River","l",] <- rep(90000, 20)
wr_sdm_scen_intro_params$hatchery_release["Lower-mid Sacramento River","l",] <- rep(90000, 20)
wr_sdm_scen_intro_params$hatchery_release["Lower Sacramento River","l",] <- rep(90000, 20)

# H2a
wr_sdm_scen_intro_params$natural_adult_removal_rate["Upper Sacramento River"] <- 0.15

# H2b
wr_sdm_scen_intro_params$natural_adult_removal_rate["Upper Sacramento River"] <- 0.25

# H3
wr_sdm_scen_intro_params$adult_enroute_surv_mult["Upper Sacramento River"] <- wr_sdm_scen_intro_params$adult_enroute_surv_mult["Upper Sacramento River"] * 1.1


# SR5
wr_sdm_scen_intro_params$.surv_juv_rear_prop_diversions <- wr_sdm_scen_intro_params$.surv_juv_rear_prop_diversions * 0.75

# SR11
wr_sdm_scen_intro_params$addl_juv_chipps <- 100000

# O2
wr_sdm_scen_intro_params$harvest_rate_ocean <- wr_sdm_scen_intro_params$harvest_rate_ocean * 0.99

# BC1
wr_sdm_scen_intro_params$harvest_rate_trib["Battle Creek"] <- 0.05

# BC8
wr_sdm_scen_intro_params$hatchery_release["Battle Creek","l",] <- rep(200000, 20)


usethis::use_data(wr_sdm_scen_intro_params, overwrite = TRUE)