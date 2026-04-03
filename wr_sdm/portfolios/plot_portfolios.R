load("wr_sdm/portfolios/portfolio_performance_metrics.Rdata")

# Settings --------------
theme_plots <- 
  theme_bw() + 
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 13),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 13),
        legend.position = "top")

colors <- c("#93c47d", "#009E73")
colors2 <- c("goldenrod", "#D55E00")

# Table to includeat the top (use kbl or DT or whatever you want) --------------
p1_metrics$metrics_table

# Plots -----------------------
## Abundance --------------
# Spawners
ggplot(p1_metrics$spawners) + 
  geom_line(aes(sim_year, spawners, color = scenario)) +
  geom_point(aes(sim_year, spawners, color = scenario, shape = scenario), size = 3) +
  labs(title = "Spawners")+
  scale_color_manual(values = colors)+
  theme_plots

# pHOS
ggplot(p1_metrics$returns) + 
  geom_line(aes(sim_year, phos, color = scenario)) +
  geom_point(aes(sim_year, phos, color = scenario, shape = scenario), size = 3) +
  scale_color_manual(values = colors2)+
  labs(title = "pHOS",y = "pHOS") +
  theme_plots

## Productivity ---------------------
# Upper Sac Juveniles
ggplot(p1_metrics$juvs_us) +
  geom_line(aes(year, total_juv, color = scenario)) +
  geom_point(aes(year, total_juv, color = scenario, shape = scenario), size = 3) +
  labs(title = "Juveniles in Upper Sac", y = "Juveniles in Upper Sac") +
  scale_color_manual(values = colors)+
  theme_plots

# Juveniles at Chipps
ggplot(p1_metrics$juv_chipps) +
  geom_line(aes(year, jac, color = scenario)) +
  geom_point(aes(year, jac, color = scenario, shape = scenario), size = 3) +
  labs(title = "Juveniles at Chipps", y = "Juveniles at Chipps") +
  scale_color_manual(values = colors)+
  theme_plots

# Cohort Replacement Rate
ggplot(p1_metrics$crr) + 
  geom_line(aes(sim_year, crr, color = scenario)) +
  geom_point(aes(sim_year, crr, color = scenario, shape = scenario), size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed") + 
  labs(title = "Cohort Replacement Rate", y = "Cohort Replacement Rate") +
  scale_color_manual(values = colors2)+
  theme_plots

## Diversity ---------------
# Size Class Diversity
ggplot(p1_metrics$shannon_di_size) + 
  geom_line(aes(year, shannon_index, color = scenario)) +
  geom_point(aes(year, shannon_index, color = scenario, shape = scenario), size = 3) +
  labs(title = "Size Class Diversity", y = "Diversity Index")+
  scale_color_manual(values = colors)+
  theme_plots

ggplot(p1_metrics$juvenile_size_ocean_entry)+
  geom_col(aes(year, value, fill = factor(size_or_age)), position = "fill", color = "black") +
  labs(y = "proportion of juveniles", title = "Size Diversity") +
  facet_wrap(~scenario, nrow = 2) + 
  viridis::scale_fill_viridis(option= "mako", discrete = TRUE)  + 
  theme_bw()

## Number of tributaries supporting WRCS -----------------
ggplot(p1_metrics$spawners_tribs) + 
  geom_line(aes(sim_year, spawners, color = scenario)) +
  geom_point(aes(sim_year, spawners, color = scenario, shape = scenario), size = 3) +
  labs(title = "Spawners in Tributaries", y = "Spawners in Tributaries")+
  scale_color_manual(values = colors)+
  theme_plots

# Proportion rearing habitat -------------------------
ggplot(p1_metrics$rearing_prop) + 
  geom_line(aes(sim_year, mean_rear_prop, color = scenario)) +
  geom_point(aes(sim_year, mean_rear_prop, color = scenario, shape = scenario), size = 3) +
  labs(title = "Proportion of Rearing Habitat in Tributaries", y = "Proportion of Rearing Habitat in Tributaries")+
  scale_color_manual(values = colors)+
  theme_plots

# Spawning and rearing habitat above Shasta -------------------------
ggplot(p1_metrics$habitat_abv_shasta) +
  geom_line(aes(sim_year, habitat_score, color = scenario)) +
  geom_point(aes(sim_year, habitat_score, color = scenario, shape = scenario), size = 3) +
  labs(title = "Habitat access", y = "Habitat Access")+
  scale_color_manual(values = colors)+
  theme_plots

# Independent populations ------------------------
ggplot(p1_metrics$sub_area_ind_pop_combined) +
  geom_line(aes(sim_year, scenario, color = scenario)) +
  geom_point(aes(sim_year, scenario, color = scenario, shape = scenario), size = 3) +
  labs(title = "Independent Conditions", y = "Independent Conditions")+
  scale_color_manual(values = colors)+
  theme_plots
