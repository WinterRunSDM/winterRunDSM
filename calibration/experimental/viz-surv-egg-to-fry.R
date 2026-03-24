library(winterRunDSM)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)

bp <- winterRunDSM::wr_sdm_baseline_params

cat("Fixed coefficients:\n")
cat("  .surv_egg_to_fry_int =", bp$.surv_egg_to_fry_int, "\n")
cat("  .proportion_natural =", bp$.surv_egg_to_fry_proportion_natural, "\n")
cat("  .scour =", bp$.surv_egg_to_fry_scour, "\n")

n <- 200
df <- expand_grid(
  temp_effect = seq(0.01, 1, length.out = n),
  proportion_natural = seq(0, 1, length.out = 5),
  scour = c(0, 0.05, 0.10, 0.20)
) |>
  mutate(
    base_surv = inv.logit(
      bp$.surv_egg_to_fry_int +
      bp$.surv_egg_to_fry_proportion_natural * proportion_natural +
      bp$.surv_egg_to_fry_scour * scour
    ),
    survival = base_surv * temp_effect,
    survival = pmax(survival, 0),
    scour_label = paste0("Scour Prob: ", scour * 100, "%"),
    nat_label = factor(
      paste0("Prop Natural: ", round(proportion_natural * 100), "%"),
      levels = paste0("Prop Natural: ", round(seq(0, 1, length.out = 5) * 100), "%")
    )
  )

p <- ggplot(df, aes(x = temp_effect, y = survival, color = nat_label)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~scour_label) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "surv_egg_to_fry: Survival Range Across Inputs",
    x = "Calibrated Temp Effect Multiplier (x[12])",
    y = "Egg-to-Fry Survival",
    color = "Proportion Natural"
  ) +
  theme_minimal()

print(p)
