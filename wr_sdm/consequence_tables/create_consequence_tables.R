library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)
library(readr)
# Load metrics results for portfolios
load("wr_sdm/portfolios/portfolio_performance_metrics.Rdata")
ct_scales <- read_csv("wr_sdm/consequence_tables/ct_scales.csv") # best and worst values
raw_ct_nonmod <- read_csv("wr_sdm/consequence_tables/nonmodeled_metrics.csv") # raw nonmodeled
nonmodlookup <- read_csv("wr_sdm/consequence_tables/nonmodeled_metrics_lookup.csv") # conversion from raw to normalized
timeliness <- read_csv("wr_sdm/consequence_tables/timeliness_norm.csv") # timeliness normalized vals
weights <- read_excel("wr_sdm/consequence_tables/weights.xlsx") # participant weights
obj_met <- read_excel("wr_sdm/documentation/objectives_metrics_v2.xlsx") 

# Display metrics nicely
format_metric <- function(x) {
  dplyr::case_when(
    is.na(x)        ~ "NA",
    abs(x) >= 1000  ~ formatC(x, format = "f", digits = 0, big.mark = ","),
    abs(x) >= 1   ~ formatC(x, format = "f", digits = 0),
    # abs(x) >= 1     ~ formatC(x, format = "f", digits = 2),
    abs(x) > 0      ~ formatC(x, format = "f", digits = 3),
    TRUE            ~ formatC(x, format = "f", digits = 0)
  )
}

## Create raw consequence table ------
raw_ct_mod <- map(1:14, \(i) {
  get(paste0("p", i, "_metrics"))$metrics_table_raw |>
    select(metric, portfolio) |>
    rename("p{i}" := portfolio) 
}) |>
  reduce(full_join, by = "metric") |> 
  # left_join(get(paste0("p", 1, "_metrics"))$metrics_table_raw |>
              # select(metric, baseline)) |> 
  pivot_longer(-metric, names_to = "portfolio", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value)

raw_ct_comb <- left_join(raw_ct_mod, raw_ct_nonmod) |> select(-portfolio_description) |> select(portfolio, portfolio_name, everything())

## Create normalize table ---------
best <- ct_scales |> filter(value_type == "best") |> select(-value_type)
worst <- ct_scales |> filter(value_type == "worst") |> select(-value_type)
norm_ct <- raw_ct_comb |>
  mutate(across(c(mean_spawners,mean_phos, mean_juv:ind_pop), \(col) {
    metric <- cur_column()
    b <- best[[metric]]
    w <- worst[[metric]]
    (col - w) / (b - w)
  })) |> 
  mutate(across(c(max_decline, cat_decline), \(col) {
    metric <- cur_column()
    b <- best[[metric]]
    w <- worst[[metric]]
    1- (col /w)
  })) |> 
  mutate(mean_rear_prop = pmin(1, mean_rear_prop)) |> 
  pivot_longer(cols = -c(portfolio,portfolio_name),
               names_to = "metric", 
               values_to = "score") |> 
  # most non-modeled are based on a lookup table for constructued scale
  left_join(nonmodlookup |> select(-metric_description), by = c("metric", "score")) |> 
  mutate(score = if_else(!is.na(score_description), normalized_value, score)) |> 
  select(-normalized_value, -score_description) |> 
  pivot_wider(names_from = "metric", 
              values_from = score) |> 
  select(-timeliness_initiation, -timeliness_benefits ) |> 
  # timeliness normalization had to be copied and pasted directly
  left_join(timeliness, by = "portfolio") |> 
  relocate(cost_cost, .after = last_col())


## Create weighted tables ------- 
metric_cols <- names(weights[-1])

### Table 1 -------------------
scores <- map(1:nrow(weights), \(i) {
  w <- weights[i, metric_cols] |> as.numeric()
  norm_ct |>
    select(portfolio, portfolio_name, all_of(metric_cols)) |>
    mutate(
      weight_set = weights$weight_set[i],
      score = as.numeric(as.matrix(pick(all_of(metric_cols))) %*% w)
    ) 
}) |>
  list_rbind() |>
  select(weight_set, portfolio, portfolio_name, score)

## Tables for app ----------------------------


### Table 2 -----------------
scores_portfolio_metric <- function(weight_set_choice) {
  w <- weights |> 
    filter(weight_set == weight_set_choice) |> 
    select(-weight_set)
  
  metric_cols <- names(w)
  
  norm_ct |>
    mutate(across(all_of(metric_cols), \(col) col * w[[cur_column()]]))  |> 
    select(portfolio_name, everything()) |> 
    group_by(portfolio) |> 
    mutate(abundance = mean_spawners+mean_phos + max_decline + cat_decline,
           productivity = mean_juv+mean_jac + mean_crr,
           `diversity and fitness` = avg_annual_shannon_di_size + mean_phos_d,
           `number of populations` = mean_spawners_trib+mean_rear_prop + hab_access + ind_pop,
           natural = natural_natural,
           `other species` = species_fr + species_other,
           timeliness = timeliness_initiation + timeliness_benefits,
           cost = cost_cost,
           `Total Portfolio Value` = rowSums(across(mean_spawners:cost_cost))) |> 
    mutate(across(c(abundance:`Total Portfolio Value`), \(x) round(x,3))) |> 
    select(portfolio, abundance, productivity, `diversity and fitness`, 
           `number of populations`, natural, `other species`,
           timeliness, cost, `Total Portfolio Value`) |> 
    ungroup() 
}

# This one is in shiny app
results2 <- map(unique(weights$weight_set), \(ws) {
  scores_portfolio_metric(ws) |>
    mutate(weight_set = ws)
}) |>
  list_rbind() |>
  left_join(raw_ct_nonmod |> select(portfolio, portfolio_name)) |>
  select(weight_set, portfolio, portfolio_name, everything()) |>
  janitor::clean_names(case = "title")

### Table 3 ---------------------
scores_weight_metric <- function(portfolio_choice) {
  map(weights$weight_set, \(ws) {
    scores_portfolio_metric(ws) |>
      filter(portfolio == portfolio_choice) |>
      mutate(weight_set = ws)
  }) |>
    list_rbind() |> 
    select(weight_set, abundance, productivity, `diversity and fitness`,
            `number of populations`, natural, `other species`,
            timeliness, cost, `Total Portfolio Value`)
}
table3 <- scores_weight_metric("p2")  |> 
  janitor::clean_names(case = "title")

portfolios <- paste0("p", 1:14)

# this one is in shiny app
all_scores <- purrr::map_dfr(portfolios, \(p) {
  scores_weight_metric(p) |>
    mutate(portfolio = p)
}) |> janitor::clean_names(case = "title")


### Consequence table ------
# this looks bad but works for the shiny app
# raw_table <- raw_ct_comb |>
#   select(portfolio_name, everything()) |>
#   mutate(across(mean_spawners:cost_cost, ~format_metric(.))) |>
#   pivot_longer(-c(portfolio_name, portfolio), names_to = "metric", values_to = "value") |>
#   select(-portfolio) |>
#   left_join(obj_met |> select(metric, metric_display, objective_display)) |>
#   select(-metric) |>
#   pivot_wider(names_from = metric_display, values_from = value) |>
#   select(Portfolio = portfolio_name, Objective = objective_display, everything())

nonmodlookup_formatted <- nonmodlookup |> mutate(score = format_metric(score))
raw_table <- raw_ct_comb |> 
  select(portfolio_name, everything()) |>
  mutate(across(mean_spawners:cost_cost, ~format_metric(.))) |>
  pivot_longer(cols = -c(portfolio,portfolio_name),
               names_to = "metric", 
               values_to = "score") |> 
  left_join(nonmodlookup_formatted |> select(-metric_description), by = c("metric", "score")) |> 
  mutate(score = if_else(!is.na(score_description), score_description, as.character(score))) |> 
  select(-c(score_description, normalized_value)) |> 
  left_join(obj_met |> select(metric, metric_display, objective, objective_display)) |> 
  select(-metric) |>
  select( -objective) |>
  pivot_wider(names_from = metric_display, values_from = score) |>
  select(portfolio, Portfolio = portfolio_name, Objective = objective_display, everything())


# raw_table <- raw_ct_comb|>
#   select(portfolio_name, everything()) |>
#   mutate(across(mean_spawners:cost_cost, ~format_metric(.))) |>
#   pivot_longer(-c(portfolio_name, portfolio), names_to = "metric", values_to = "value") |>
#   select(-portfolio) |>
#   pivot_wider(names_from = portfolio_name, values_from = value) |> left_join(obj_met |> select(objective_display, metric, metric_display)) |>
#   select(Objective = objective_display, Metric= metric_display, everything())
  
### Table 1 ---------------
results1_portfolio_weightset <- scores |> 
  pivot_wider(names_from = weight_set, values_from = score) |> select(-portfolio) |> 
  mutate(across(c(A:K), \(x) round(x,3))) 


save(raw_table, results1_portfolio_weightset, results2, all_scores, file ="wr_sdm/consequence_tables/results_tables.Rdata")

# This output needs to be copied into winterRunDSM-shiny
save(raw_table, results1_portfolio_weightset, results2, all_scores, file ="../winterRunDSM-shiny/data/results_tables.Rdata")
