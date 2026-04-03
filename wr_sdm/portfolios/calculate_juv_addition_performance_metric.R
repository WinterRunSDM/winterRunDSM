library(tidyverse)

# for each ASD action, calculate the addition of juvenile habitat compared to 
# below dam. This could be baseline, or depending on the portfolio, it could
# include other sacramento river habitat actions.

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

p8 <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4",
                                       "H-1", "H-2c",
                                       "SR-2b", "SR-4b","SR-8", "SR-11",
                                       "BC-1", "BC-3", "BC-5", "BC-8", 
                                       "O-2", "O-3"))
p9 <- create_param_list(action_ids = c("ASD-3", "ASD-4",
                                       "H-2a", "H-2c",
                                       "SR-2b",  "SR-4b", "SR-11",
                                       "O-3"))

p11 <- create_param_list(action_ids = c("ASD-3", "ASD-4", "ASD-5a",
                                        "H-2a", "H-2c",
                                        "SR-2b",  "SR-4b", "SR-10", "SR-11",
                                        "O-3"))

p12 <- create_param_list(action_ids = c("ASD-1", "ASD-3", "ASD-4", "ASD-5a", 
                                        "SR-3", "SR-4a", "SR-10", "SR-11",
                                        "BC-7", "BC-8"))

p14 <- create_param_list(action_ids = c("ASD-5a", 
                                        "SR-1","SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-10",
                                        "BC-2", "BC-5", "BC-6", "BC-7"))


# delineate asd habitat for each portfolio --------------------------------

portfolios_with_ASD_habitat_actions <- tibble("portfolio" = c("p1", "p2", "p3",
                                                              "p8", "p9", "p11", 
                                                              "p12",  "p14"),
                                              "asd_habitat" = c("full mccloud + little sac",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "full mccloud",
                                                                "lower mccloud" # just ASD-5a
                                                                ),
                                              "sac_habitat" = c("all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "all sr actions (hab: SR-1, SR-2a, SR-2b, SR-9, SR-10)",
                                                                "SR-2b",
                                                                "SR-2b",
                                                                "SR-2b + SR-10",
                                                                "SR-10",
                                                                "SR-1 + SR-2a + SR-2b + SR-10"))

calculate_juv_prop_added <- function(params_hab, habitat_addition) {
  
  prop_added <- habitat_addition / params_hab["Upper Sacramento River",,]
  
  final_prop <- prop_added |> mean()
  
  return(final_prop)
}

# TODO for each portfolio, calculate the proportion of ASD habitat over the param habitat. 

# p1 (full mccloud + little sac)
p1_fry_hab_prop <- calculate_juv_prop_added(params_hab = p1$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry + 
                                                                  habitat_additions$little_sac$fry))
p1_juv_hab_prop <- calculate_juv_prop_added(params_hab = p1$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv + 
                                                                  habitat_additions$little_sac$juv))
p1_fp_hab_prop <- calculate_juv_prop_added(params_hab = p1$floodplain_habitat, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                  habitat_additions$upper_mccloud$fp + 
                                                                  habitat_additions$little_sac$fp))
p1_avg <- mean(p1_fry_hab_prop, p1_juv_hab_prop, p1_fp_hab_prop)
# p2 (full mccloud)
p2_fry_hab_prop <- calculate_juv_prop_added(params_hab = p2$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p2_juv_hab_prop <- calculate_juv_prop_added(params_hab = p2$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p2_fp_hab_prop <- calculate_juv_prop_added(params_hab = p2$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p2_avg <- mean(p2_fry_hab_prop, p2_juv_hab_prop, p2_fp_hab_prop)

# p3 (full mccloud)
p3_fry_hab_prop <- calculate_juv_prop_added(params_hab = p3$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p3_juv_hab_prop <- calculate_juv_prop_added(params_hab = p3$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p3_fp_hab_prop <- calculate_juv_prop_added(params_hab = p3$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p3_avg <- mean(p3_fry_hab_prop, p3_juv_hab_prop, p3_fp_hab_prop)

# p8 (full mccloud)
p8_fry_hab_prop <- calculate_juv_prop_added(params_hab = p8$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p8_juv_hab_prop <- calculate_juv_prop_added(params_hab = p8$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p8_fp_hab_prop <- calculate_juv_prop_added(params_hab = p8$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p8_avg <- mean(p8_fry_hab_prop, p8_juv_hab_prop, p8_fp_hab_prop)
# p9 (full mccloud)
p9_fry_hab_prop <- calculate_juv_prop_added(params_hab = p9$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p9_juv_hab_prop <- calculate_juv_prop_added(params_hab = p9$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p9_fp_hab_prop <- calculate_juv_prop_added(params_hab = p9$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p9_avg <- mean(p9_fry_hab_prop, p9_juv_hab_prop, p9_fp_hab_prop)
# p11 (full mccloud)
p11_fry_hab_prop <- calculate_juv_prop_added(params_hab = p11$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p11_juv_hab_prop <- calculate_juv_prop_added(params_hab = p11$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p11_fp_hab_prop <- calculate_juv_prop_added(params_hab = p11$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p11_avg <- mean(p11_fry_hab_prop, p11_juv_hab_prop, p11_fp_hab_prop)
# p12 (full mccloud)
p12_fry_hab_prop <- calculate_juv_prop_added(params_hab = p12$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry + 
                                                                  habitat_additions$upper_mccloud$fry))
p12_juv_hab_prop <- calculate_juv_prop_added(params_hab = p12$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv + 
                                                                  habitat_additions$upper_mccloud$juv))
p12_fp_hab_prop <- calculate_juv_prop_added(params_hab = p12$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp + 
                                                                 habitat_additions$upper_mccloud$fp))
p12_avg <- mean(p12_fry_hab_prop, p12_juv_hab_prop, p12_fp_hab_prop)
# p14 (lower mccloud)
p14_fry_hab_prop <- calculate_juv_prop_added(params_hab = p14$inchannel_habitat_fry, 
                                            habitat_addition = (habitat_additions$lower_mccloud$fry))
p14_juv_hab_prop <- calculate_juv_prop_added(params_hab = p14$inchannel_habitat_juvenile, 
                                            habitat_addition = (habitat_additions$lower_mccloud$juv))
p14_fp_hab_prop <- calculate_juv_prop_added(params_hab = p14$floodplain_habitat, 
                                           habitat_addition = (habitat_additions$lower_mccloud$fp))
p14_avg <- mean(p14_fry_hab_prop, p14_juv_hab_prop, p14_fp_hab_prop)

juv_prop_asd_hab_added <- portfolios_with_ASD_habitat_actions |> 
  mutate(juv_prop_added_from_asd_actions = c(p1_avg, p2_avg, p3_avg, 
                                             p8_avg, p9_avg, p11_avg,
                                             p12_avg, p14_avg),
         prop_fry_added = round(c(p1_fry_hab_prop, p2_fry_hab_prop, p3_fry_hab_prop, 
                                  p8_fry_hab_prop, p9_fry_hab_prop, p11_fry_hab_prop,
                                  p12_fry_hab_prop, p14_fry_hab_prop), 2),
         prop_juv_added = round(c(p1_juv_hab_prop, p2_juv_hab_prop, p3_juv_hab_prop, 
                                  p8_juv_hab_prop, p9_juv_hab_prop, p11_juv_hab_prop,
                                  p12_juv_hab_prop, p14_juv_hab_prop), 2),
         prop_fp_added = round(c(p1_fp_hab_prop, p2_fp_hab_prop, p3_fp_hab_prop, 
                                 p8_fp_hab_prop, p9_fp_hab_prop, p11_fp_hab_prop,
                                 p12_fp_hab_prop, p14_fp_hab_prop), 2)
         )

usethis::use_data(juv_prop_asd_hab_added)
