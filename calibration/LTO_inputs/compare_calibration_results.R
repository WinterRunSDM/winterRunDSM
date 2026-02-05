

# compare results
map_params <- tibble("LTO_index" = 1:16,
                     "R2R_index" = c(2, 6, 7, 10,
                                     NA, NA, NA, NA,
                                     NA, 12, 1, 11, 
                                     3, 4, 5, 9))

LTO_res <- readRDS("calibration/LTO_inputs/LTO_calib_output_popsize_10-2026-02-03.rds")
R2R_res <- readRDS("calibration/res-2026-02-03-LTO_comparison-pop10.rds")

LTO_results <- tibble("LTO_index" = 1:16) |> 
  mutate(LTO_value = unname(LTO_res@solution[LTO_index]))

R2R_results <- tibble("R2R_index" = 1:12) |> 
  mutate(R2R_value = unname(R2R_res@solution[R2R_index])) |> 
  left_join(map_params)

LTO_results |> 
  left_join(R2R_results,
            by = "LTO_index")
