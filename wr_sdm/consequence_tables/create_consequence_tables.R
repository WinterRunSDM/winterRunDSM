library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load metrics results for portfolios
load("wr_sdm/portfolios/portfolio_performance_metrics.Rdata")
ct_scales <- read_csv("wr_sdm/consequence_tables/ct_scales.csv")
raw_ct_nonmod <- read_csv("wr_sdm/consequence_tables/nonmodeled_metrics.csv")
weights <- read_excel("wr_sdm/consequence_tables/weights.xlsx")

# Create consequence table
raw_ct_mod <- map(1:14, \(i) {
  get(paste0("p", i, "_metrics"))$metrics_table_raw |>
    select(metric, portfolio) |>
    rename("p{i}" := portfolio)
}) |>
  reduce(full_join, by = "metric") |> 
  pivot_longer(-metric, names_to = "portfolio", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value)

raw_ct_comb <- left_join(raw_ct_mod, raw_ct_nonmod) |> select(portfolio, portfolio_name, everything())

# Normalize 
best <- ct_scales |> filter(value_type == "best") |> select(-value_type)
worst <- ct_scales |> filter(value_type == "worst") |> select(-value_type)
norm_ct <- raw_ct_comb |>
  mutate(across(c(mean_spawners,mean_phos, mean_juv:ind_pop), \(col) {
    metric <- cur_column()
    b <- best[[metric]]
    w <- worst[[metric]]
    (col - w) / (b - w)
  })) |> 
  mutate(across(c(max_decline, cat_decline, natural_natural:timeliness_benefits), \(col) {
    metric <- cur_column()
    b <- best[[metric]]
    w <- worst[[metric]]
    1-(b) / (w-b)
  })) |> 
  mutate(mean_juv_rear_trib=pmin(1, mean_juv_rear_trib),
         cost_cost = case_when(cost_cost == 1 ~ 1,
                               cost_cost == 2 ~ 0.993328885923949,
                               cost_cost == 3 ~ 0.960640426951301,
                               cost_cost == 4 ~ 0.900600400266845,
                               cost_cost == 5 ~ 0.600400266844563,
                               cost_cost == 6 ~ 0)) 

# Multiply by weights  
metric_cols <- names(weights[-1])

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





### this is Results 1 for app
results_portfolio_weightset <- scores |> 
  pivot_wider(names_from = weight_set, values_from = score) |> select(-portfolio)


