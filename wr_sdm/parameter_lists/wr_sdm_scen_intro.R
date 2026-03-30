# Create parameter list for 3/18/26 meeting portfolio introduction 
# Scenario for 3/18/2026 SDM meeting

# Copy baseline parameters and build off
wr_sdm_scen_intro_params <- winterRunDSM::wr_sdm_baseline_params

# Update params --------------

## Hatchery ------------

# H1
wr_sdm_scen_intro_params$hatchery_release["Upper Sacramento River","l",] <- rep(280000, 20)
wr_sdm_scen_intro_params$hatchery_release["Upper-mid Sacramento River","l",] <- rep(90000, 20)
wr_sdm_scen_intro_params$hatchery_release["Lower-mid Sacramento River","l",] <- rep(90000, 20)
wr_sdm_scen_intro_params$hatchery_release["Lower Sacramento River","l",] <- rep(90000, 20)

# H2b
# Baseline is 0.086
wr_sdm_scen_intro_params$natural_adult_removal_rate["Upper Sacramento River"] <- 0.15

# H3
wr_sdm_scen_intro_params$adult_enroute_surv_mult["Upper Sacramento River"] <- wr_sdm_scen_intro_params$adult_enroute_surv_mult["Upper Sacramento River"] * 1.1

## Sacramento River --------------


# SR4a 
# This one relies on contact points
wr_sdm_scen_intro_params$contact_points["Upper Sacramento River"] <- round(wr_sdm_scen_intro_params$contact_points * 0.75)

# SR7
# Confirm DCC gate action

# SR11
# Add chipps juveniles in drought years
wr_sdm_scen_intro_params$addl_juv_chipps <- 50000

## Other -------------

# O2
# Reduce ocean harvest by 1% (baseline is 0.11)
wr_sdm_scen_intro_params$harvest_rate_ocean <- wr_sdm_scen_intro_params$harvest_rate_ocean - 0.01


## Battle Creek --------------

# BC1
# Decrease river harvest by 50% 
wr_sdm_scen_intro_params$harvest_rate_trib["Battle Creek"] <- wr_sdm_scen_intro_params$harvest_rate_trib["Battle Creek"] * 0.5

# BC8
wr_sdm_scen_intro_params$hatchery_release["Battle Creek","l",] <- rep(200000, 20)


# Overwrite params ------------------
usethis::use_data(wr_sdm_scen_intro_params, overwrite = TRUE)