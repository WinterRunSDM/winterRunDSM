# This script calculates proportion of rearing and spawning habitat in each tributary.

# Portfolio values ---------------
load("wr_sdm/portfolios/portfolio_params.Rdata")

habitat_additions <- calculate_habitat_additions_ASD_BC()

# single helper works for both params slices and ASD addition matrices
extract_annual_hab <- function(mat) {
  mat |>
    as.data.frame() |>
    pivot_longer(`1980`:`2000`, names_to = "year", values_to = "hab") |>
    group_by(year) |>
    summarize(hab = sum(hab, na.rm = TRUE), .groups = "drop") |>
    mutate(sim_year = as.numeric(year) - 1979) |>
    filter(sim_year <= 20) |>  # add this
    select(sim_year, hab)
}

calc_trib_prop <- function(portfolio_params, mccloud_juv, mccloud_fry, mccloud_fp, mccloud_spawn,
                           little_sac_juv, little_sac_fry, little_sac_fp, little_sac_spawn) {
  
  upper_sac_juv    <- extract_annual_hab(portfolio_params$inchannel_habitat_juvenile["Upper Sacramento River",,]) |> rename(upper_sac_juv   = hab)
  upper_sac_fry    <- extract_annual_hab(portfolio_params$inchannel_habitat_fry["Upper Sacramento River",,])     |> rename(upper_sac_fry   = hab)
  upper_sac_fp     <- extract_annual_hab(portfolio_params$floodplain_habitat["Upper Sacramento River",,])        |> rename(upper_sac_fp    = hab)
  upper_sac_spawn  <- extract_annual_hab(portfolio_params$spawning_habitat["Upper Sacramento River",,])          |> rename(upper_sac_spawn = hab)
  bc_juv           <- extract_annual_hab(portfolio_params$inchannel_habitat_juvenile["Battle Creek",,])          |> rename(bc_juv          = hab)
  bc_fry           <- extract_annual_hab(portfolio_params$inchannel_habitat_fry["Battle Creek",,])               |> rename(bc_fry          = hab)
  bc_fp            <- extract_annual_hab(portfolio_params$floodplain_habitat["Battle Creek",,])                  |> rename(bc_fp           = hab)
  bc_spawn         <- extract_annual_hab(portfolio_params$spawning_habitat["Battle Creek",,])                    |> rename(bc_spawn        = hab)
  mccloud_juv_ann  <- extract_annual_hab(mccloud_juv)                                                            |> rename(mccloud_juv     = hab)
  mccloud_fry_ann  <- extract_annual_hab(mccloud_fry)                                                            |> rename(mccloud_fry     = hab)
  mccloud_fp_ann   <- extract_annual_hab(mccloud_fp)                                                             |> rename(mccloud_fp      = hab)
  mccloud_spawn_ann <- extract_annual_hab(mccloud_spawn)                                                         |> rename(mccloud_spawn   = hab)
  ls_juv_ann       <- extract_annual_hab(little_sac_juv)                                                         |> rename(little_sac_juv  = hab)
  ls_fry_ann       <- extract_annual_hab(little_sac_fry)                                                         |> rename(little_sac_fry  = hab)
  ls_fp_ann        <- extract_annual_hab(little_sac_fp)                                                          |> rename(little_sac_fp   = hab)
  ls_spawn_ann     <- extract_annual_hab(little_sac_spawn)                                                       |> rename(little_sac_spawn = hab)
  
  upper_sac_juv |>
    left_join(upper_sac_fry,    by = "sim_year") |>
    left_join(upper_sac_fp,     by = "sim_year") |>
    left_join(upper_sac_spawn,  by = "sim_year") |>
    left_join(bc_juv,           by = "sim_year") |>
    left_join(bc_fry,           by = "sim_year") |>
    left_join(bc_fp,            by = "sim_year") |>
    left_join(bc_spawn,         by = "sim_year") |>
    left_join(mccloud_juv_ann,  by = "sim_year") |>
    left_join(mccloud_fry_ann,  by = "sim_year") |>
    left_join(mccloud_fp_ann,   by = "sim_year") |>
    left_join(mccloud_spawn_ann, by = "sim_year") |>
    left_join(ls_juv_ann,       by = "sim_year") |>
    left_join(ls_fry_ann,       by = "sim_year") |>
    left_join(ls_fp_ann,        by = "sim_year") |>
    left_join(ls_spawn_ann,     by = "sim_year") |>
    mutate(
      # rearing
      bc_rear_hab         = pmax(bc_fry, bc_juv) + bc_fp,
      mccloud_rear_hab    = pmax(mccloud_fry, mccloud_juv) + mccloud_fp,
      little_sac_rear_hab = pmax(little_sac_fry, little_sac_juv) + little_sac_fp,
      asd_rear_hab        = mccloud_rear_hab + little_sac_rear_hab,
      upper_sac_rear_hab  = pmax(upper_sac_fry, upper_sac_juv) + upper_sac_fp,
      # spawning
      bc_spawn_hab         = bc_spawn,
      mccloud_spawn_hab    = mccloud_spawn,
      little_sac_spawn_hab = little_sac_spawn,
      asd_spawn_hab        = mccloud_spawn_hab + little_sac_spawn_hab,
      upper_sac_spawn_hab  = upper_sac_spawn,
      # totals
      total_rear_hab  = upper_sac_rear_hab  + bc_rear_hab,
      total_spawn_hab = upper_sac_spawn_hab + bc_spawn_hab,
      # rearing proportions
      prop_bc_rear         = bc_rear_hab         / total_rear_hab,
      prop_mccloud_rear    = mccloud_rear_hab    / total_rear_hab,
      prop_little_sac_rear = little_sac_rear_hab / total_rear_hab,
      prop_asd_rear        = asd_rear_hab        / total_rear_hab,
      prop_trib_rear       = (bc_rear_hab + asd_rear_hab) / total_rear_hab,
      # spawning proportions
      prop_bc_spawn         = bc_spawn_hab         / total_spawn_hab,
      prop_mccloud_spawn    = mccloud_spawn_hab    / total_spawn_hab,
      prop_little_sac_spawn = little_sac_spawn_hab / total_spawn_hab,
      prop_asd_spawn        = asd_spawn_hab        / total_spawn_hab,
      prop_trib_spawn       = (bc_spawn_hab + asd_spawn_hab) / total_spawn_hab
    ) |>
    select(sim_year, 
           prop_bc_rear, prop_mccloud_rear, prop_little_sac_rear, prop_asd_rear, prop_trib_rear,
           prop_bc_spawn, prop_mccloud_spawn, prop_little_sac_spawn, prop_asd_spawn, prop_trib_spawn)
}
# zero addition placeholder (same dim as habitat matrices)
zero_mat <- habitat_additions$lower_mccloud$juv * 0

# ASD additions per portfolio
asd_additions <- list(
  p1  = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv   = habitat_additions$little_sac$juv,
             little_sac_fry   = habitat_additions$little_sac$fry,
             little_sac_fp    = habitat_additions$little_sac$fp,
             little_sac_spawn = habitat_additions$little_sac$spawn),
  p2  = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, 
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p3  = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p4  = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p5  = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p6  = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p7  = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p8  = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p9  = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p10 = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p11 = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p12 = list(mccloud_juv    = habitat_additions$full_mccloud$juv,
             mccloud_fry    = habitat_additions$full_mccloud$fry,
             mccloud_fp     = habitat_additions$full_mccloud$fp,
             mccloud_spawn  = habitat_additions$full_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat),
  p13 = list(mccloud_juv = zero_mat, mccloud_fry = zero_mat, mccloud_fp = zero_mat, mccloud_spawn = zero_mat,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat, little_sac_fp = zero_mat, little_sac_spawn = zero_mat),
  p14 = list(mccloud_juv    = habitat_additions$lower_mccloud$juv,
             mccloud_fry    = habitat_additions$lower_mccloud$fry,
             mccloud_fp     = habitat_additions$lower_mccloud$fp,
             mccloud_spawn  = habitat_additions$lower_mccloud$spawn,
             little_sac_juv = zero_mat, little_sac_fry = zero_mat,
             little_sac_fp  = zero_mat, little_sac_spawn = zero_mat)
)
# run for all portfolios
portfolio_params_list <- list(p1=p1_params, p2=p2_params, p3=p3_params, 
                              p4=p4_params, p5=p5_params, p6=p6_params, 
                              p7=p7_params, p8=p8_params, p9=p9_params, 
                              p10=p10_params, p11=p11_params, p12=p12_params, 
                              p13=p13_params, p14=p14_params)

trib_hab_portfolios <- purrr::imap_dfr(portfolio_params_list, function(params, portfolio_name) {
  calc_trib_prop(
    portfolio_params  = params,
    mccloud_juv       = asd_additions[[portfolio_name]]$mccloud_juv,
    mccloud_fry       = asd_additions[[portfolio_name]]$mccloud_fry,
    mccloud_fp        = asd_additions[[portfolio_name]]$mccloud_fp,
    mccloud_spawn     = asd_additions[[portfolio_name]]$mccloud_spawn,
    little_sac_juv    = asd_additions[[portfolio_name]]$little_sac_juv,
    little_sac_fry    = asd_additions[[portfolio_name]]$little_sac_fry,
    little_sac_fp     = asd_additions[[portfolio_name]]$little_sac_fp,
    little_sac_spawn  = asd_additions[[portfolio_name]]$little_sac_spawn
  ) |>
    mutate(portfolio = portfolio_name, scenario = "portfolio")
}) |>
  relocate(portfolio, .before = sim_year)

# BC baseline ----------

# calculate bc baseline proportion
bc_sac_baseline <- bind_rows(
  wr_sdm_baseline_params$inchannel_habitat_juvenile[c("Battle Creek", "Upper Sacramento River"),,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "juv"),
  wr_sdm_baseline_params$inchannel_habitat_fry[c("Battle Creek", "Upper Sacramento River"),,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "fry"),
  wr_sdm_baseline_params$floodplain_habitat[c("Battle Creek", "Upper Sacramento River"),,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "fp"),
  wr_sdm_baseline_params$spawning_habitat[c("Battle Creek", "Upper Sacramento River"),,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "spawn")
) |>
  rename(watershed = Var1, month = Var2, sim_year = Var3) |>
  pivot_wider(names_from = habitat, values_from = Freq) |>
  group_by(watershed, sim_year) |>
  summarize(
    rear  = sum(pmax(fry, juv) + fp, na.rm = TRUE),
    spawn = sum(spawn, na.rm = TRUE),
    .groups = "drop"
  ) |>
  group_by(sim_year) |>
  summarize(
    bc_sac_rear  = sum(rear),
    bc_sac_spawn = sum(spawn),
    .groups = "drop"
  ) |>
  mutate(sim_year = as.numeric(as.character(sim_year)) - 1979) |>
  filter(sim_year <= 20)

bc_baseline <- bind_rows(
  wr_sdm_baseline_params$inchannel_habitat_juvenile["Battle Creek",,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "juv"),
  wr_sdm_baseline_params$inchannel_habitat_fry["Battle Creek",,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "fry"),
  wr_sdm_baseline_params$floodplain_habitat["Battle Creek",,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "fp"),
  wr_sdm_baseline_params$spawning_habitat["Battle Creek",,] |>
    as.table() |> as.data.frame() |> mutate(habitat = "spawn")
) |>
  rename(month = Var1, sim_year = Var2) |>
  pivot_wider(names_from = habitat, values_from = Freq) |>
  group_by(sim_year) |>
  summarize(
    bc_rear  = sum(pmax(fry, juv) + fp, na.rm = TRUE),
    bc_spawn = sum(spawn, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(sim_year = as.numeric(as.character(sim_year)) - 1979) |>
  filter(sim_year <= 20) |>
  mutate(scenario = "baseline") |>
  left_join(bc_sac_baseline, by = "sim_year") |>
  mutate(
    prop_rear  = bc_rear  / bc_sac_rear,
    prop_spawn = bc_spawn / bc_sac_spawn
  )

trib_hab_baseline <- expand.grid(
  portfolio = paste0("p", 1:14),
  sim_year  = 1:20
) |>
  left_join(
    bc_baseline |>
      mutate(
        sim_year              = as.integer(sim_year),
        prop_bc_rear          = bc_rear  / bc_sac_rear,
        prop_mccloud_rear     = 0,
        prop_little_sac_rear  = 0,
        prop_asd_rear         = 0,
        prop_trib_rear        = bc_rear  / bc_sac_rear,
        prop_bc_spawn         = bc_spawn / bc_sac_spawn,
        prop_mccloud_spawn    = 0,
        prop_little_sac_spawn = 0,
        prop_asd_spawn        = 0,
        prop_trib_spawn       = bc_spawn / bc_sac_spawn
      ) |>
      select(sim_year, prop_bc_rear, prop_mccloud_rear, prop_little_sac_rear, 
             prop_asd_rear, prop_trib_rear, prop_bc_spawn, prop_mccloud_spawn, 
             prop_little_sac_spawn, prop_asd_spawn, prop_trib_spawn),
    by = "sim_year"
  ) |>
  mutate(scenario = "baseline")