library(winterRunDSM)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)

bp <- winterRunDSM::wr_sdm_baseline_params

# Fixed coefficients
cat("Fixed coefficients:\n")
cat("  .migratory_temp =", bp$.adult_en_route_migratory_temp, "\n")
cat("  .bypass_overtopped =", bp$.adult_en_route_bypass_overtopped, "\n")

# Sweep inputs
n <- 200
df <- expand_grid(
  int = seq(-7, 7, length.out = n),
  migratory_temp = seq(0, 1, length.out = 5),
  bypass_overtopped = c(0, 1)
) |>
  mutate(
    survival = inv.logit(
      int +
      bp$.adult_en_route_migratory_temp * migratory_temp +
      bp$.adult_en_route_bypass_overtopped * bypass_overtopped
    ),
    bypass_label = ifelse(bypass_overtopped == 1, "Bypass Overtopped", "Bypass Not Overtopped"),
    temp_label = factor(
      paste0("Temp>20°C: ", round(migratory_temp * 100), "%"),
      levels = paste0("Temp>20°C: ", round(seq(0, 1, length.out = 5) * 100), "%")
    )
  )

p <- ggplot(df, aes(x = int, y = survival, color = temp_label)) +
  annotate("rect", xmin = 0, xmax = 3.5, ymin = 0, ymax = 1,
           fill = "grey80", alpha = 0.3) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~bypass_label) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "surv_adult_enroute: Survival Range Across Inputs",
    x = "Calibrated Intercept (x[1])",
    y = "Survival Probability",
    color = "Migratory Temp"
  ) +
  theme_minimal()

print(p)
