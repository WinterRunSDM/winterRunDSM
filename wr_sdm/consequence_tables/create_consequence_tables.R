library(ggplot2)
library(dplyr)
library(tidyr)
library(readxl)

# Load metrics results for portfolios
load("wr_sdm/portfolios/portfolio_performance_metrics.Rdata")
ct_scales <- read_csv("wr_sdm/consequence_tables/ct_scales.csv")
raw_ct_nonmod <- read_csv("wr_sdm/consequence_tables/nonmodeled_metrics.csv")
weights <- read_excel("wr_sdm/consequence_tables/weights.xlsx")
obj_met <- read_excel("wr_sdm/documentation/objectives_metrics_v2.xlsx")

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

# Create consequence table
raw_ct_mod <- map(1:14, \(i) {
  get(paste0("p", i, "_metrics"))$metrics_table_raw |>
    select(metric, portfolio) |>
    rename("p{i}" := portfolio)
}) |>
  reduce(full_join, by = "metric") |> 
  pivot_longer(-metric, names_to = "portfolio", values_to = "value") |> 
  pivot_wider(names_from = metric, values_from = value)

raw_ct_comb <- left_join(raw_ct_mod, raw_ct_nonmod) 

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


## Tables for app ----------------------------

### this is Consequence table for app
raw_table <- raw_ct_comb|> 
  select(portfolio_name, everything()) |> 
  mutate(across(mean_spawners:cost_cost, ~format_metric(.))) |> 
  pivot_longer(-c(portfolio_name, portfolio), names_to = "metric", values_to = "value") |> 
  select(-portfolio) |> 
  pivot_wider(names_from = portfolio_name, values_from = value) |> left_join(obj_met |> select(metric, metric_display)) |> 
  select(metric_display, everything())
  
### this is Results 1 for app
results_portfolio_weightset <- scores |> 
  pivot_wider(names_from = weight_set, values_from = score) |> select(-portfolio)


