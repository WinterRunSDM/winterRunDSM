library(tidyverse)

# for each ASD action, calculate the addition of juvenile habitat compared to 
# below dam. This could be baseline, or depending on the portfolio, it could
# include other sacramento river habitat actions.

habitat_additions <- calculate_habitat_additions_ASD_BC()

# this code taken from create_portfolios.R

# generate params for relevant portfolios ---------------------------------
h_actions <- c("H-1", "H2a", "H-2b", "H-2c", "H-3")
sr_actions <- c("SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4a", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-9", "SR-10", "SR-11")
bc_actions <- c("BC-1", "BC-2", "BC-3", "BC-5", "BC-6", "BC-7", "BC-8", "BC-9")
o_actions <- c("O-1", "O-2", "O-3")

p1 <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5c", "F-1",
                                              h_actions, sr_actions, bc_actions, o_actions))

p2 <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5a", "F-1",
                                              h_actions, sr_actions, bc_actions, o_actions))

p3 <- create_param_list(action_ids = c("ASD-2", "ASD-6", "ASD-7", "ASD-8", "F-1",
                                              h_actions, sr_actions, bc_actions, o_actions))

p4 <- create_param_list(action_ids = c(h_actions, sr_actions, bc_actions, o_actions, "F-1"))

p5 <- create_param_list(action_ids = c(h_actions, bc_actions))

p6 <- create_param_list(action_ids = c(h_actions, sr_actions, o_actions, "F-1"))

p7 <- create_param_list(action_ids = c("ASD-1", "ASD-2", 
                                              h_actions, 
                                              "SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4a", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-10", "SR-11",
                                              "BC-1", "BC-2", "BC-3", "BC-5", "BC-7", "BC-8", "BC-9",
                                              "O-2", "O-3"))

p8 <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4",
                                              "H-1", "H-2c",
                                              "SR-2b", "SR-4b","SR-8", "SR-11",
                                              "BC-1", "BC-3", "BC-5", "BC-8", 
                                              "O-2", "O-3"))
p9 <- create_param_list(action_ids = c("ASD-3", "ASD-4",
                                              "H-2a", "H-2c",
                                              "SR-2b",  "SR-4b", "SR-11",
                                              "O-3"))
p10 <- create_param_list(action_ids = c("H-3", bc_actions))

p11 <- create_param_list(action_ids = c("ASD-3", "ASD-4", "ASD-5a",
                                               "H-2a", "H-2c",
                                               "SR-2b",  "SR-4b", "SR-10", "SR-11",
                                               "O-3"))

p12 <- create_param_list(action_ids = c("ASD-1", "ASD-3", "ASD-4", "ASD-5a", 
                                               "SR-3", "SR-4a", "SR-10", "SR-11",
                                               "BC-7", "BC-8"))

p13 <- create_param_list(action_ids = c("ASD-1", h_actions, 
                                               "SR-11", 
                                               "BC-8"))

p14 <- create_param_list(action_ids = c("ASD-5a", 
                                               "SR-1","SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-10",
                                               "BC-2", "BC-5", "BC-6", "BC-7"))


# delineate asd habitat for each portfolio --------------------------------

portfolios_with_ASD_habitat_actions <- tibble("portfolio" = c("p1", "p2", "p3",
                                                              "p4", "p5", "p6", 
                                                              "p7","p8", "p9", 
                                                              "p10", "p11", "p12", 
                                                              "p13", "p14"),
                                              "asd_habitat" = c("full mccloud + little sac",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "none",
                                                                "none",
                                                                "none",
                                                                "none",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "none",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "none",
                                                                "lower mccloud" # just ASD-5a
                                                                ),
                                              "sac_habitat" = c("all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "none",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "SR-2b",
                                                                "SR-2b",
                                                                "none",
                                                                "SR-2b + SR-10",
                                                                "SR-10",
                                                                "none",
                                                                "SR-1 + SR-2a + SR-2b + SR-10"),
                                              "bc_habitat" = c("all bc actions (hab: bc-2 + bc-5)",
                                                               "all bc actions (hab: bc-2 + bc-5)",
                                                               "all bc actions (hab: bc-2 + bc-5)",
                                                               "all bc actions (hab: bc-2 + bc-5)",
                                                               "all bc actions (hab: bc-2 + bc-5)",
                                                               "none",
                                                               "bc-2 + bc-5",
                                                               "bc-5",
                                                               "none",
                                                               "all bc actions (hab: bc-2 + bc-5)",
                                                               "none",
                                                               "none",
                                                               "none",
                                                               "bc-2 + bc-5"
                                                               ))

calculate_juv_prop_added <- function(params_hab, habitat_addition) {
  
  prop_added <- (habitat_addition / params_hab["Upper Sacramento River",,]) |> 
    as.data.frame() |> 
    mutate(month = month.abb) |> 
    pivot_longer(`1980`:`2000`, names_to = "year", values_to = "prop_added") |> 
    mutate(sim_year = as.numeric(year) - 1979) |> 
    group_by(sim_year) |> 
    summarise(avg_prop_added_asd = mean(prop_added)) |> 
    ungroup()
  
  #final_prop <- prop_added |> mean()
  
  return(prop_added)
}

calculate_juv_prop_added_bc <- function(params_hab, habitat_addition) {
  
  prop_added <- (habitat_addition / params_hab["Battle Creek",,]) |> 
    as.data.frame() |> 
    mutate(month = month.abb) |> 
    pivot_longer(`1980`:`2000`, names_to = "year", values_to = "prop_added") |> 
    mutate(sim_year = as.numeric(year) - 1979,
           prop_added = ifelse(is.nan(prop_added), 0, prop_added)) |> 
    group_by(sim_year) |> 
    summarise(avg_prop_added_bc = mean(prop_added)) |> 
    ungroup()
  
  #final_prop <- prop_added |> mean()
  
  return(prop_added)
}

# TODO for each portfolio, calculate the proportion of ASD habitat over the param habitat AND 
# prop of BC habitat

# ASD ---------------------------------------------------------------------


# p1 (full mccloud + little sac)
p1_fry_hab_prop <- calculate_juv_prop_added(params_hab = p1$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry + 
                                                                  habitat_additions$little_sac$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p1_juv_hab_prop <- calculate_juv_prop_added(params_hab = p1$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv + 
                                                                  habitat_additions$little_sac$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p1_fp_hab_prop <- calculate_juv_prop_added(params_hab = p1$floodplain_habitat, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                  habitat_additions$upper_mccloud$fp + 
                                                                  habitat_additions$little_sac$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)

p1_avg_by_year_asd <- p1_fry_hab_prop |> 
  left_join(p1_juv_hab_prop) |> 
  left_join(p1_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p1_avg_asd <- mean(p1_avg_by_year_asd$avg_across_rear_asd)

# p2 (full mccloud)
p2_fry_hab_prop <- calculate_juv_prop_added(params_hab = p2$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p2_juv_hab_prop <- calculate_juv_prop_added(params_hab = p2$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p2_fp_hab_prop <- calculate_juv_prop_added(params_hab = p2$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)

p2_avg_by_year_asd <- p2_fry_hab_prop |> 
  left_join(p2_juv_hab_prop) |> 
  left_join(p2_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p2_avg_asd <- mean(p2_avg_by_year_asd$avg_across_rear_asd)

# p3 (full mccloud)
p3_fry_hab_prop <- calculate_juv_prop_added(params_hab = p3$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p3_juv_hab_prop <- calculate_juv_prop_added(params_hab = p3$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p3_fp_hab_prop <- calculate_juv_prop_added(params_hab = p3$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)

p3_avg_by_year_asd <- p3_fry_hab_prop |> 
  left_join(p3_juv_hab_prop) |> 
  left_join(p3_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p3_avg_asd <- mean(p3_avg_by_year_asd$avg_across_rear_asd)

# p8 (full mccloud)
p8_fry_hab_prop <- calculate_juv_prop_added(params_hab = p8$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p8_juv_hab_prop <- calculate_juv_prop_added(params_hab = p8$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p8_fp_hab_prop <- calculate_juv_prop_added(params_hab = p8$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)
p8_avg_by_year_asd <- p8_fry_hab_prop |> 
  left_join(p8_juv_hab_prop) |> 
  left_join(p8_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p8_avg_asd <- mean(p8_avg_by_year_asd$avg_across_rear_asd)

# p9 (full mccloud)
p9_fry_hab_prop <- calculate_juv_prop_added(params_hab = p9$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p9_juv_hab_prop <- calculate_juv_prop_added(params_hab = p9$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p9_fp_hab_prop <- calculate_juv_prop_added(params_hab = p9$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)
p9_avg_by_year_asd <- p9_fry_hab_prop |> 
  left_join(p9_juv_hab_prop) |> 
  left_join(p9_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p9_avg_asd <- mean(p9_avg_by_year_asd$avg_across_rear_asd)

# p11 (full mccloud)
p11_fry_hab_prop <- calculate_juv_prop_added(params_hab = p11$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p11_juv_hab_prop <- calculate_juv_prop_added(params_hab = p11$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p11_fp_hab_prop <- calculate_juv_prop_added(params_hab = p11$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)
p11_avg_by_year_asd <- p11_fry_hab_prop |> 
  left_join(p11_juv_hab_prop) |> 
  left_join(p11_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p11_avg_asd <- mean(p11_avg_by_year_asd$avg_across_rear_asd)

# p12 (full mccloud)
p12_fry_hab_prop <- calculate_juv_prop_added(params_hab = p12$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p12_juv_hab_prop <- calculate_juv_prop_added(params_hab = p12$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p12_fp_hab_prop <- calculate_juv_prop_added(params_hab = p12$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)
p12_avg_by_year_asd <- p12_fry_hab_prop |> 
  left_join(p12_juv_hab_prop) |> 
  left_join(p12_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p12_avg_asd <- mean(p12_avg_by_year_asd$avg_across_rear_asd)

# p14 (lower mccloud)
p14_fry_hab_prop <- calculate_juv_prop_added(params_hab = p14$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry)) |> 
  rename(avg_prop_added_asd_fry = avg_prop_added_asd)
p14_juv_hab_prop <- calculate_juv_prop_added(params_hab = p14$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv)) |> 
  rename(avg_prop_added_asd_juv = avg_prop_added_asd)
p14_fp_hab_prop <- calculate_juv_prop_added(params_hab = p14$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp)) |> 
  rename(avg_prop_added_asd_fp = avg_prop_added_asd)
p14_avg_by_year_asd <- p14_fry_hab_prop |> 
  left_join(p14_juv_hab_prop) |> 
  left_join(p14_fp_hab_prop) |> 
  mutate(avg_across_rear_asd = (avg_prop_added_asd_fry + avg_prop_added_asd_juv + avg_prop_added_asd_fp)/3)
p14_avg_asd <- mean(p14_avg_by_year_asd$avg_across_rear_asd)


# BC ----------------------------------------------------------------------
# p1 all bc actions
p1_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p1$inchannel_habitat_fry, 
                                            habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p1_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p1$inchannel_habitat_juvenile, 
                                            habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p1_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p1$floodplain_habitat, 
                                           habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p1_avg_by_year_bc <- p1_fry_hab_prop_bc |> 
  left_join(p1_juv_hab_prop_bc) |> 
  left_join(p1_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p1_avg_bc <- mean(p1_avg_by_year_bc$avg_across_rear_bc)

# p2 all bc actions
p2_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p2$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p2_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p2$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p2_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p2$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p2_avg_by_year_bc <- p2_fry_hab_prop_bc |> 
  left_join(p2_juv_hab_prop_bc) |> 
  left_join(p2_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p2_avg_bc <- mean(p2_avg_by_year_bc$avg_across_rear_bc)

# p3 all bc actions
p3_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p3$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p3_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p3$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p3_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p3$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p3_avg_by_year_bc <- p3_fry_hab_prop_bc |> 
  left_join(p3_juv_hab_prop_bc) |> 
  left_join(p3_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p3_avg_bc <- mean(p3_avg_by_year_bc$avg_across_rear_bc)

# p4 all bc actions
p4_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p4$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p4_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p4$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p4_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p4$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p4_avg_by_year_bc <- p4_fry_hab_prop_bc |> 
  left_join(p4_juv_hab_prop_bc) |> 
  left_join(p4_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p4_avg_bc <- mean(p4_avg_by_year_bc$avg_across_rear_bc)

# p5 all bc actions
p5_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p5$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p5_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p5$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p5_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p5$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p5_avg_by_year_bc <- p5_fry_hab_prop_bc |> 
  left_join(p5_juv_hab_prop_bc) |> 
  left_join(p5_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p5_avg_bc <- mean(p5_avg_by_year_bc$avg_across_rear_bc)

# p7 all bc actions
p7_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p7$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p7_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p7$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p7_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p7$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p7_avg_by_year_bc <- p7_fry_hab_prop_bc |> 
  left_join(p7_juv_hab_prop_bc) |> 
  left_join(p7_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p7_avg_bc <- mean(p7_avg_by_year_bc$avg_across_rear_bc)

# p8 bc-5
p8_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p8$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p8_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p8$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)

p8_fp_hab_prop_bc <- tibble("sim_year" = 1:21,
                            "avg_prop_added_bc_fp" = rep(0.0, 21)) # no fp added for bc 5

p8_avg_by_year_bc <- p8_fry_hab_prop_bc |> 
  left_join(p8_juv_hab_prop_bc) |> 
  left_join(p8_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p8_avg_bc <- mean(p8_avg_by_year_bc$avg_across_rear_bc)

# p10 all bc actions
p10_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p10$inchannel_habitat_fry, 
                                                  habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p10_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p10$inchannel_habitat_juvenile, 
                                                  habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p10_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p10$floodplain_habitat, 
                                                 habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p10_avg_by_year_bc <- p10_fry_hab_prop_bc |> 
  left_join(p10_juv_hab_prop_bc) |> 
  left_join(p10_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p10_avg_bc <- mean(p10_avg_by_year_bc$avg_across_rear_bc)

# p14 bc-2 and bc-5
p14_fry_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p14$inchannel_habitat_fry, 
                                                   habitat_addition = habitat_additions$bc_2_5$fry) |> 
  rename(avg_prop_added_bc_fry = avg_prop_added_bc)
p14_juv_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p14$inchannel_habitat_juvenile, 
                                                   habitat_addition = habitat_additions$bc_2_5$juv) |> 
  rename(avg_prop_added_bc_juv = avg_prop_added_bc)
p14_fp_hab_prop_bc <- calculate_juv_prop_added_bc(params_hab = p14$floodplain_habitat, 
                                                  habitat_addition = habitat_additions$bc_2_5$fp) |> 
  rename(avg_prop_added_bc_fp = avg_prop_added_bc)

p14_avg_by_year_bc <- p14_fry_hab_prop_bc |> 
  left_join(p14_juv_hab_prop_bc) |> 
  left_join(p14_fp_hab_prop_bc) |> 
  mutate(avg_across_rear_bc = (avg_prop_added_bc_fry + avg_prop_added_bc_juv + avg_prop_added_bc_fp)/3)
p14_avg_bc <- mean(p14_avg_by_year_bc$avg_across_rear_bc)

# combine -----------------------------------------------------------------

juv_prop_added <- portfolios_with_ASD_habitat_actions |> 
  mutate(juv_prop_added_from_asd_actions = c(p1_avg_asd, p2_avg_asd, p3_avg_asd,
                                             0.0, 0.0, 0.0, 0.0,
                                             p8_avg_asd, p9_avg_asd, 0.0, 
                                             p11_avg_asd, p12_avg_asd, 0.0,
                                             p14_avg_asd),
         juv_prop_added_from_bc_actions = c(p1_avg_bc, p2_avg_bc, p3_avg_bc, 
                                            p4_avg_bc, p5_avg_bc, 0.0, p7_avg_bc,
                                            p8_avg_bc, 0.0, p10_avg_bc, 0.0,
                                            0.0, 0.0, p14_avg_bc))

all_asd <- p1_avg_by_year_asd |> 
  mutate(portfolio = "p1") |> 
  select(portfolio, sim_year, asd_rear = avg_across_rear_asd) |> 
  bind_rows(p2_avg_by_year_asd |> 
              mutate(portfolio = "p2") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |>
  bind_rows(p3_avg_by_year_asd |> 
              mutate(portfolio = "p3") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |>
  bind_rows(p8_avg_by_year_asd |> 
              mutate(portfolio = "p8") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |>  
  bind_rows(p9_avg_by_year_asd |> 
              mutate(portfolio = "p9") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |> 
  bind_rows(p11_avg_by_year_asd |> 
              mutate(portfolio = "p11") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |> 
  bind_rows(p12_avg_by_year_asd |> 
              mutate(portfolio = "p12") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd)) |> 
  bind_rows(p14_avg_by_year_asd |> 
              mutate(portfolio = "p14") |> 
              select(portfolio, sim_year, asd_rear = avg_across_rear_asd))

all_bc <- p1_avg_by_year_bc |> 
  mutate(portfolio = "p1") |> 
  select(portfolio, sim_year, bc_rear = avg_across_rear_bc) |> 
  bind_rows(p2_avg_by_year_bc |> 
              mutate(portfolio = "p2") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |>
  bind_rows(p3_avg_by_year_bc |> 
              mutate(portfolio = "p3") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |>
  bind_rows(p4_avg_by_year_bc |> 
              mutate(portfolio = "p4") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |>  
  bind_rows(p5_avg_by_year_bc |> 
              mutate(portfolio = "p5") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |> 
  bind_rows(p7_avg_by_year_bc |> 
              mutate(portfolio = "p7") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |> 
  bind_rows(p8_avg_by_year_bc |> 
              mutate(portfolio = "p8") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |> 
  bind_rows(p10_avg_by_year_bc |> 
              mutate(portfolio = "p10") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) |> 
  bind_rows(p14_avg_by_year_bc |> 
              mutate(portfolio = "p14") |> 
              select(portfolio, sim_year, bc_rear = avg_across_rear_bc)) 
  

juv_prop_added_all_rear <- expand.grid(portfolio = paste0("p", rep(1:14)),
                                        sim_year = 1:20) |> 
  left_join(portfolios_with_ASD_habitat_actions) |> 
  left_join(all_bc) |> 
  left_join(all_asd) |> 
  mutate(bc_rear = ifelse(is.na(bc_rear), 0, bc_rear),
         asd_rear = ifelse(is.na(asd_rear), 0, asd_rear),
         avg_all_rear = (bc_rear + asd_rear) / 2) |> 
  relocate(sim_year, .before = bc_rear)

usethis::use_data(juv_prop_added_all_rear, overwrite = T)
