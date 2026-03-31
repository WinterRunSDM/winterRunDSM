library(tidyverse)
library(readxl)

# battle creek
bc_raw <- read_xlsx("data-raw/habitat-data-docs/Tw_assessment_data_032626_DRAFT.xlsx",
                    sheet = "BattleCreek", skip = 3)
names(bc_raw) <- c("date", "ubc_avg", "ubc_max", "ubc_min", "ubc_dif",
                   "wildcat_avg", "wildcat_max", "wildcat_min", "wildcat_dif",
                   "colemandam_avg", "colemandam_max", "colemandam_min", "colemandam_dif")
bc_clean <- bc_raw |> 
  filter(!is.na(ubc_avg)) |> 
  pivot_longer(-date, 
               names_to = "loc_stat",
               values_to = "deg_f") |> 
  separate(loc_stat, 
           into = c("location", "statistic"), 
           sep = "_") |> 
  mutate(watershed = "battle creek")

# above shasta lake
asl_raw <- read_xlsx("data-raw/habitat-data-docs/Tw_assessment_data_032626_DRAFT.xlsx",
                    sheet = "AboveShastaLake", skip = 3)
names(asl_raw) <- c("date", "sacasl_avg", "sacasl_max", "sacasl_min", "sacasl_dif",
                   "mccloudasl_avg", "mccloudasl_max", "mccloudasl_min", "mccloudasl_dif",
                   "pitasl_avg", "pitasl_max", "pitasl_min", "pitasl_dif")
asl_clean <- asl_raw |> 
  pivot_longer(-date, 
               names_to = "loc_stat",
               values_to = "deg_f") |> 
  separate(loc_stat, 
           into = c("location", "statistic"), 
           sep = "_") |> 
  mutate(watershed = "above shasta lake")

# mccloud
mccloud_raw <- read_xlsx("data-raw/habitat-data-docs/Tw_assessment_data_032626_DRAFT.xlsx",
                     sheet = "McCloudRiver", skip = 3)
names(mccloud_raw) <- c("date", "MRA_avg", "MRA_max", "MRA_min", "MRA_dif",
                        "MRB_avg", "MRB_max", "MRB_min", "MRB_dif",
                        "MR3A_avg", "MR3A_max", "MR3A_min", "MR3A_dif",
                        "MR4A_avg", "MR4A_max", "MR4A_min", "MR4A_dif",
                        "MR5A_avg", "MR5A_max", "MR5A_min", "MR5A_dif",
                        "MR6A_avg", "MR6A_max", "MR6A_min", "MR6A_dif")
mccloud_clean <- mccloud_raw |> 
  pivot_longer(-date, 
               names_to = "loc_stat",
               values_to = "deg_f") |> 
  separate(loc_stat, 
           into = c("location", "statistic"), 
           sep = "_") |> 
  mutate(watershed = "mccloud river")

# pit
pit_raw <- read_xlsx("data-raw/habitat-data-docs/Tw_assessment_data_032626_DRAFT.xlsx",
                         sheet = "PitRiver", skip = 3)
names(pit_raw) <- c("date", "r1_avg", "r1_max", "r1_min", "r1_dif",
                        "pph5_avg", "pph5_max", "pph5_min", "pph5_dif")
pit_clean <- pit_raw |> 
  pivot_longer(-date, 
               names_to = "loc_stat",
               values_to = "deg_f") |> 
  separate(loc_stat, 
           into = c("location", "statistic"), 
           sep = "_") |> 
  mutate(watershed = "pit river")

# upper sacramento
little_sac_raw <- read_xlsx("data-raw/habitat-data-docs/Tw_assessment_data_032626_DRAFT.xlsx",
                     sheet = "SacramentoRiver", skip = 3)
names(little_sac_raw) <- c("date", "box_avg", "box_max", "box_min", "box_dif",
                           "cantara_avg", "cantara_max", "cantara_min", "cantara_dif",
                           "mossbraeup_avg", "mossbraeup_max", "mossbraeup_min", "mossbraeup_dif",
                           "mossbraebelow_avg", "mossbraebelow_max", "mossbraebelow_min", "mossbraebelow_dif",
                           "soda_avg", "soda_max", "soda_min", "soda_dif",
                           "riverside_avg", "riverside_max", "riverside_min", "riverside_dif",
                           "conant_avg", "conant_max", "conant_min", "conant_dif",
                           "sims_avg", "sims_max", "sims_min", "sims_dif",
                           "gibson_avg", "gibson_max", "gibson_min", "gibson_dif")
little_sac_clean <- little_sac_raw |> 
  pivot_longer(-date, 
               names_to = "loc_stat",
               values_to = "deg_f") |> 
  separate(loc_stat, 
           into = c("location", "statistic"), 
           sep = "_") |> 
  mutate(watershed = "little sacramento river")

all_temps <- bind_rows(bc_clean, asl_clean) |> 
  bind_rows(mccloud_clean) |> 
  bind_rows(pit_clean) |> 
  bind_rows(little_sac_clean) |> 
  glimpse()

all_temps |> 
  mutate(timeframe = ifelse(watershed %in% c("mccloud river", "pit river"), 
         "early", "late")) |> 
  filter(statistic == "avg") |> 
  ggplot(aes(x = date, y = deg_f, color = location)) + 
  geom_line() +
  geom_hline(aes(yintercept = 53.5),
             linetype = "dashed") +
  facet_wrap(~timeframe, scales = "free")

write_csv(all_temps, "data-raw/habitat-data-docs/mikedeas_temp_data_clean.csv")
