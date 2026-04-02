library(ggplot2)
source("wr_sdm/portfolios/calculate_portfolio_performance_metrics.R")


## Visualize
load("wr_sdm/portfolios/portfolio_params.Rdata")
load("wr_sdm/portfolios/portfolio_results.Rdata")

# Create metrics
p1_metrics <- calculate_performance_metrics(p1_results, p1_params)
p1_metrics$metrics_table

p2_metrics <- calculate_performance_metrics(p2_results, p2_params)
p2_metrics$metrics_table

p3_metrics <- calculate_performance_metrics(p3_results, p3_params)
p3_metrics$metrics_table

p4_metrics <- calculate_performance_metrics(p4_results, p4_params)
p4_metrics$metrics_table

p5_metrics <- calculate_performance_metrics(p5_results, p5_params)
p5_metrics$metrics_table

p6_metrics <- calculate_performance_metrics(p6_results, p6_params)
p6_metrics$metrics_table

p7_metrics <- calculate_performance_metrics(p7_results, p7_params)
p7_metrics$metrics_table

p8_metrics <- calculate_performance_metrics(p8_results, p8_params)
p8_metrics$metrics_table

p9_metrics <- calculate_performance_metrics(p9_results, p9_params)
p9_metrics$metrics_table

p10_metrics <- calculate_performance_metrics(p10_results, p10_params)
p10_metrics$metrics_table

p11_metrics <- calculate_performance_metrics(p11_results, p11_params)
p11_metrics$metrics_table

p12_metrics <- calculate_performance_metrics(p12_results, p12_params)
p12_metrics$metrics_table

p13_metrics <- calculate_performance_metrics(p13_results, p13_params)
p13_metrics$metrics_table

p14_metrics <- calculate_performance_metrics(p14_results, p14_params)
p14_metrics$metrics_table

save(p1_metrics, p2_metrics, p3_metrics, p4_metrics, p5_metrics, p6_metrics, p7_metrics, p8_metrics, p9_metrics, 
     p10_metrics, p11_metrics, p12_metrics, p13_metrics, p14_metrics, file = "wr_sdm/portfolios/portfolio_performance_metrics.Rdata")


