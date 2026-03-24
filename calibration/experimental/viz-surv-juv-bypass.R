library(winterRunDSM)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)

bp <- winterRunDSM::wr_sdm_baseline_params

cat("Fixed coefficients:\n")
cat("  .avg_temp_thresh =", bp$.surv_juv_bypass_avg_temp_thresh, "\n")
cat("  .high_predation =", bp$.surv_juv_bypass_high_predation, "\n")
cat("  .medium =", bp$.surv_juv_bypass_medium, "\n")
cat("  .large =", bp$.surv_juv_bypass_large, "\n")
cat("  .floodplain =", bp$.surv_juv_bypass_floodplain, "\n")

# score = x[6] + .floodplain + .avg_temp * avg_temp + .high_pred * high_pred
# survival = inv.logit(score)

n <- 200
df <- expand_grid(
  int = seq(-7, 7, length.out = n),
  avg_temp_thresh = seq(0, 1, length.out = 5)
) |>
  mutate(
    score = int + bp$.surv_juv_bypass_floodplain +
      bp$.surv_juv_bypass_avg_temp_thresh * avg_temp_thresh +
      bp$.surv_juv_bypass_high_predation * 0,
    survival = inv.logit(score),
    temp_label = factor(
      paste0("Avg Temp Thresh: ", round(avg_temp_thresh * 100), "%"),
      levels = paste0("Avg Temp Thresh: ", round(seq(0, 1, length.out = 5) * 100), "%")
    )
  )

p <- ggplot(df, aes(x = int, y = survival, color = temp_label)) +
  annotate("rect", xmin = -3.5, xmax = 3.5, ymin = 0, ymax = 1,
           fill = "grey80", alpha = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "surv_juv_bypass: Survival by Calibrated Intercept (x[6])",
    x = "Calibrated Intercept (x[6])",
    y = "Survival Probability (small size class)",
    color = "Avg Temp"
  ) +
  theme_minimal()

print(p)
