library(winterRunDSM)
library(boot)
library(dplyr)
library(tidyr)
library(ggplot2)

bp <- winterRunDSM::wr_sdm_baseline_params

cat("Fixed coefficients:\n")
cat("  .avg_temp_thresh =", bp$.surv_juv_rear_avg_temp_thresh, "\n")
cat("  .high_predation =", bp$.surv_juv_rear_high_predation, "\n")
cat("  .surv_juv_rear_contact_points =", bp$.surv_juv_rear_contact_points, "\n")
cat("  .surv_juv_rear_prop_diversions =", bp$.surv_juv_rear_prop_diversions, "\n")
cat("  .surv_juv_rear_total_diversions =", bp$.surv_juv_rear_total_diversions, "\n")
cat("  .stranded =", bp$.surv_juv_rear_stranded, "\n")
cat("  .medium =", bp$.surv_juv_rear_medium, "\n")
cat("  .large =", bp$.surv_juv_rear_large, "\n")

# Inchannel base score (deterministic, small size class):
# score = x[2] + (.avg_temp * avg_temp) + (.high_pred * high_pred) +
#          (.contact * x[3] * contact * high_pred) +
#          (.prop_div * x[4] * prop_div) +
#          (.total_div * x[5] * total_div) +
#          (.stranded * stranded)
# survival = inv.logit(score)

n <- 200

# Fixed environmental scenario: moderate conditions
env <- list(
  avg_temp_thresh = 0.3,
  high_predation = 0.5,
  contact_points = 5,
  prop_diversions = 0.3,
  total_diversions = 0.3,
  stranded = 0.1,
  max_temp_thresh = 0
)

# Midpoints for calibrated params
mid <- list(int = 0, contact = 1.75, prop_div = 1.75, total_div = 1.75)

# Helper: compute inchannel survival (small size class, deterministic)
calc_surv <- function(int, contact, prop_div, total_div) {
  score <- int +
    bp$.surv_juv_rear_avg_temp_thresh * env$avg_temp_thresh +
    bp$.surv_juv_rear_high_predation * env$high_predation +
    bp$.surv_juv_rear_contact_points * contact * env$contact_points * env$high_predation +
    bp$.surv_juv_rear_prop_diversions * prop_div * env$prop_diversions +
    bp$.surv_juv_rear_total_diversions * total_div * env$total_diversions +
    bp$.surv_juv_rear_stranded * env$stranded
  inv.logit(score)
}

# --- Panel 1: Sweep x[2] intercept ---
df1 <- tibble(x = seq(-7, 7, length.out = n), param = "x[2] Intercept") |>
  mutate(survival = calc_surv(x, mid$contact, mid$prop_div, mid$total_div))

# --- Panel 2: Sweep x[3] contact points ---
df2 <- tibble(x = seq(-7, 7, length.out = n), param = "x[3] Contact Points") |>
  mutate(survival = calc_surv(mid$int, x, mid$prop_div, mid$total_div))

# --- Panel 3: Sweep x[4] prop diversions ---
df3 <- tibble(x = seq(-7, 7, length.out = n), param = "x[4] Prop Diversions") |>
  mutate(survival = calc_surv(mid$int, mid$contact, x, mid$total_div))

# --- Panel 4: Sweep x[5] total diversions ---
df4 <- tibble(x = seq(-7, 7, length.out = n), param = "x[5] Total Diversions") |>
  mutate(survival = calc_surv(mid$int, mid$contact, mid$prop_div, x))

bounds <- tibble(
  param = c("x[2] Intercept", "x[3] Contact Points", "x[4] Prop Diversions", "x[5] Total Diversions"),
  xmin = c(-3.5, 0, 0, 0),
  xmax = c(3.5, 3.5, 3.5, 3.5)
)

df <- bind_rows(df1, df2, df3, df4)

p <- ggplot(df, aes(x = x, y = survival)) +
  geom_rect(data = bounds, aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = 1),
            inherit.aes = FALSE, fill = "grey80", alpha = 0.3) +
  geom_line(linewidth = 0.8, color = "steelblue") +
  facet_wrap(~param, scales = "free_x") +
  scale_x_continuous(breaks = seq(-7, 7, by = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "surv_juv_rear: Inchannel Survival by Calibrated Parameter (others at midpoint)",
    subtitle = paste0("Env: avg_temp_thresh=", env$avg_temp_thresh,
                      ", high_pred=", env$high_predation,
                      ", contacts=", env$contact_points,
                      ", prop_div=", env$prop_diversions,
                      ", total_div=", env$total_diversions),
    x = "Calibrated Parameter Value",
    y = "Survival Probability (small size class)"
  ) +
  theme_minimal()

print(p)

# --- Combined sensitivity: sweep intercept across high_predation levels ---
df_pred <- expand_grid(
  int = seq(-7, 7, length.out = n),
  high_predation = c(0, 0.25, 0.5, 0.75, 1)
) |>
  mutate(
    score = int +
      bp$.surv_juv_rear_avg_temp_thresh * env$avg_temp_thresh +
      bp$.surv_juv_rear_high_predation * high_predation +
      bp$.surv_juv_rear_contact_points * mid$contact * env$contact_points * high_predation +
      bp$.surv_juv_rear_prop_diversions * mid$prop_div * env$prop_diversions +
      bp$.surv_juv_rear_total_diversions * mid$total_div * env$total_diversions +
      bp$.surv_juv_rear_stranded * env$stranded,
    survival = inv.logit(score),
    pred_label = factor(
      paste0("High Predation: ", round(high_predation * 100), "%"),
      levels = paste0("High Predation: ", round(seq(0, 1, length.out = 5) * 100), "%")
    )
  )

p2 <- ggplot(df_pred, aes(x = int, y = survival, color = pred_label)) +
  annotate("rect", xmin = -3.5, xmax = 3.5, ymin = 0, ymax = 1,
           fill = "grey80", alpha = 0.3) +
  geom_line(linewidth = 0.8) +
  scale_x_continuous(breaks = seq(-7, 7, by = 1)) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(
    title = "surv_juv_rear: Intercept (x[2]) Sensitivity by Predation Level",
    x = "Calibrated Intercept (x[2])",
    y = "Survival Probability (small size class)",
    color = "Predation"
  ) +
  theme_minimal()

print(p2)
