# Creates a data frame of portfolios to join with action descriptions
# Copied from create_portfolios.R
library(readxl)
library(here)

actions <- readxl::read_excel(here("wr_sdm/documentation/action_descriptions.xlsx"))
portfolio_names <- read_csv("wr_sdm/consequence_tables/nonmodeled_metrics.csv") |> select(portfolio, portfolio_name, portfolio_description)

h_actions <- c("H-1", "H-2a", "H-2b", "H-2c", "H-3")
sr_actions <- c("SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4a", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-9", "SR-10", "SR-11")
bc_actions <- c("BC-1", "BC-2", "BC-3", "BC-5", "BC-6", "BC-7", "BC-8", "BC-9")
o_actions <- c("O-1", "O-2", "O-3")

p1 <- data.frame(portfolio = "p1", 
                 action = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5c", "F-1",
                            h_actions, sr_actions, bc_actions, o_actions))
p2 <- data.frame(portfolio = "p2",
                 action = c("ASD-1", "ASD-2", "ASD-3", "ASD-4", "ASD-5a", "F-1",
                            h_actions, sr_actions, bc_actions, o_actions))
p3 <- data.frame(portfolio = "p3",
                 action = c("ASD-2", "ASD-6", "ASD-7", "ASD-8", "F-1",
                            h_actions, sr_actions, bc_actions, o_actions))
p4 <- data.frame(portfolio = "p4",
                 action = c(h_actions, sr_actions, bc_actions, o_actions, "F-1"))
p5 <- data.frame(portfolio = "p5",
                 action = c(h_actions, bc_actions))
p6 <- data.frame(portfolio = "p6",
                 action = c(h_actions, sr_actions, o_actions, "F-1"))
p7 <- data.frame(portfolio = "p7",
                 action = c("ASD-1", "ASD-2", 
                            h_actions, 
                            "SR-1", "SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-4b", "SR-5", "SR-6", "SR-8", "SR-10", "SR-11",
                            "BC-1", "BC-2", "BC-3", "BC-5", "BC-7", "BC-8", "BC-9",
                            "O-2", "O-3"))
p8 <- data.frame(portfolio = "p8",
                 action = c("ASD-1", "ASD-2", "ASD-3", "ASD-4",
                            "H-1", "H-2c",
                            "SR-2b", "SR-4b","SR-8", "SR-11",
                            "BC-1", "BC-3", "BC-5", "BC-8", 
                            "O-2", "O-3"))
p9 <- data.frame(portfolio = "p9",
                 action = c("ASD-3", "ASD-4",
                            "H-2a", "H-2c",
                            "SR-2b",  "SR-4b", "SR-11",
                            "O-3"))
p10 <- data.frame(portfolio = "p10", 
                 action = c("H-3", bc_actions))
p11 <- data.frame(portfolio = "p11", 
                  action = c("ASD-3", "ASD-4", "ASD-5a",
                             "H-2a", "H-2c",
                             "SR-2b",  "SR-4b", "SR-10", "SR-11",
                             "O-3"))
p12 <- data.frame(portfolio = "p12", 
                  action = c("ASD-1", "ASD-3", "ASD-4", "ASD-5a", 
                             "SR-3", "SR-4a", "SR-10", "SR-11",
                             "BC-7", "BC-8"))
p13 <- data.frame(portfolio = "p13", 
                  action = c("ASD-1", h_actions, 
                             "SR-11", 
                             "BC-8"))
p14 <- data.frame(portfolio = "p14", 
                  action = c("ASD-5a", 
                             "SR-1","SR-2a", "SR-2b", "SR-2c", "SR-3", "SR-10",
                             "BC-2", "BC-5", "BC-6", "BC-7"))
p_actions <- list(p1, p2, p3, p4, p5, p6, p7,
                  p8,p9,p10,p11,p12,p13,p14)


portfolios <- data.table::rbindlist(p_actions, fill = TRUE) |> left_join(actions) |> 
  select(portfolio:long_description) |> left_join(portfolio_names) |> select(portfolio, portfolio_name, portfolio_description, everything())
saveRDS(portfolios, file ="../winterRunDSM-shiny/data/portfolio_table.Rdata")
