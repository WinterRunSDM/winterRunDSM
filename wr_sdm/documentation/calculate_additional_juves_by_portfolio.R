library(tidyverse)
load("wr_sdm/portfolios/portfolio_params.Rdata")

get_additional_hatchery <- function(params, baseline_params) {
  
  # calculate additional hatchery releases vs baseline
  additional_releases <- params$hatchery_release - baseline_params$hatchery_release
  
  # sum across all watersheds and years, keep by size class, divide by 20 for annual average
  total_additional <- apply(additional_releases, 2, function(size) sum(pmax(size, 0)) / 20)
  
  # nz juveniles additional (already per year since it's a fixed annual addition)
  additional_nz <- sum(pmax(params$nz_juveniles - baseline_params$nz_juveniles, 0))
  
  tibble::tibble(
    additional_s  = total_additional["s"],
    additional_m  = total_additional["m"],
    additional_l  = total_additional["l"],
    additional_vl = total_additional["vl"],
    additional_nz_juveniles = additional_nz
  )
}

hatchery_table <- purrr::map_df(
  setNames(paste0("p", 1:14, "_params"), paste0("P", 1:14)),
  ~ get_additional_hatchery(get(.x), baseline_params),
  .id = "portfolio"
)
hatchery_table |> 
  dplyr::mutate(dplyr::across(where(is.numeric), ~ pretty_num(.x, places = 0))) |> 
  clipr::write_clip()
  View()

