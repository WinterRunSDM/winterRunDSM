library(winterRunDSM)
library(dplyr)
library(ggplot2)
library(tidyr)

## Build portfolios ---------------------------------
h_actions <- c("H-1", "H2a", "H-2b", "H-2c", "H-3")
sr_actions <- c("SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4a", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-9", "SR-10", "SR-11")
bc_actions <- c("BC-1", "BC-2", "BC-3", "BC-5", "BC-6", "BC-7", "BC-8", "BC-9")
o_actions <- c("O-1", "O-2", "O-3")

#TODO add F-1
## P1: All Actions with Trap and Haul for All Tributaries
p1_params <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5c",
                                              h_actions, sr_actions, bc_actions, o_actions))

## P2: All Actions with Trap and Haul for McCloud River
p2_params <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5a",
                                              h_actions, sr_actions, bc_actions, o_actions))

## P3: All Actions with Shasta Volitional Passage
p3_params <- create_param_list(action_ids = c("ASD-2", "ASD-6", "ASD-7", "ASD-8",
                                              h_actions, sr_actions, bc_actions, o_actions))

## P4: Population Enhancement Below Shasta
p4_params <- create_param_list(action_ids = c(h_actions, sr_actions, bc_actions, o_actions))

## P5: Battle Creek + Hatchery Actions
p5_params <- create_param_list(action_ids = c(h_actions, bc_actions))

## P6: Mainstem + Hatchery Actions
p6_params <- create_param_list(action_ids = c(h_actions, sr_actions, o_actions))

## P7: Implementable Within 5 years
p7_params <- create_param_list(action_ids = c("ASD-1", "ASD-2", 
                                              h_actions, 
                                              "SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4a", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-10", "SR-11",
                                              "BC-1", "BC-2", "BC-3", "BC-5", "BC-7", "BC-8", "BC-9",
                                              "O-2", "O-3"))

## P8: All Actions with estimated costs < $10M
p8_params <- create_param_list(action_ids = c("ASD-1", "ASD-2", "ASD-3", "ASD-4",
                                              "H-1", "H-2c",
                                              "SR-2b", "SR-4b","SR-8", "SR-11",
                                              "BC-1", "BC-3", "BC-5", "BC-8", 
                                              "O-2", "O-3"))
## P9: Near-Term Low Hanging Fruit (Proponent 1)
p9_params <- create_param_list(action_ids = c("ASD-3", "ASD-4",
                                              "H-2a", "H-2c",
                                              "SR-2b",  "SR-4b", "SR-11",
                                              "O-3"))
## P10: Battle Creek Only (Proponent 1)
p10_params <- create_param_list(action_ids = c("H-3", bc_actions))

## P11: Near-Term Low Hanging Fruit + Trap and Haul + ACID (Proponent 1)
p11_params <- create_param_list(action_ids = c("ASD-3", "ASD-4", "ASD-5a",
                                              "H-2a", "H-2c",
                                              "SR-2b",  "SR-4b", "SR-10", "SR-11",
                                              "O-3"))

## P12: Drought Mitigation (Proponent 2)
p12_params <- create_param_list(action_ids = c("ASD-1", "ASD-3", "ASD-4", "ASD-5a", 
                                                "SR-3", "SR-4a", "SR-10", "SR-11",
                                                "BC-7", "BC-8"))

## P13: Hatchery Influence and Production (Proponent 2)
p13_params <- create_param_list(action_ids = c("ASD-1", h_actions, 
                                                "SR-11", 
                                                "BC-8"))

## P14: Habitat Enhancement and Restoration (Proponent 2)
p14_params <- create_param_list(action_ids = c("ASD-5a", 
                                                "SR-1","SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-10",
                                                "BC-2", "BC-5", "BC-6", "BC-7"))



## Run models -----------------------
run_portfolio_model <-  function(params) {
  
  alt_seeds <- winterRunDSM::winter_run_model(scenario = NULL, 
                                              mode = "seed",
                                              seeds = NULL, 
                                              ..params = params)
  
  alt_results <- winterRunDSM::winter_run_model(mode = "simulate", 
                                                ..params = params,
                                                seeds = alt_seeds)
  
  return(alt_results)
}

p1_results <- run_portfolio_model(p1_params)
p2_results <- run_portfolio_model(p2_params)
p3_results <- run_portfolio_model(p3_params)
p4_results <- run_portfolio_model(p4_params)
p5_results <- run_portfolio_model(p5_params)
p6_results <- run_portfolio_model(p6_params)
p7_results <- run_portfolio_model(p7_params)
p8_results <- run_portfolio_model(p8_params)
p9_results <- run_portfolio_model(p9_params)
p10_results <- run_portfolio_model(p10_params)
p11_results <- run_portfolio_model(p11_params)
p12_results <- run_portfolio_model(p12_params)
p13_results <- run_portfolio_model(p13_params)
p14_results <- run_portfolio_model(p14_params)

## Save params and results -----------------------------
save(p1_params, p2_params, p3_params, p4_params, p5_params, 
     p6_params, p7_params, p8_params,  p9_params, p10_params, 
     p11_params, p12_params, p13_params, p14_params, file = "wr_sdm/portfolios/portfolio_params.Rdata", 
     compress = "xz")
save(p1_results, p2_results, p3_results, p4_results, p5_results, 
     p6_results, p7_results, p8_results,  p9_results, p10_results, 
     p11_results, p12_results, p13_results, p14_results, file = "wr_sdm/portfolios/portfolio_results.Rdata", compress = "xz")


