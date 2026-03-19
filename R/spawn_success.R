#' @title Spawning Success
#' @description Calculates the annual reproductive success.
#' @param escapement The number of returning adults
#' @param adult_prespawn_survival The adult prespawn survival rate
#' @param egg_to_fry_survival The egg to fry survival rate
#' @param prob_scour The probability of nest scouring
#' @param spawn_habitat The available spawning habitat in square meters
#' @param sex_ratio The female to male spawning ratio
#' @param redd_size The size of redds including defensible space
#' @param fecundity The number of eggs per female
#' @param stochastic \code{TRUE} \code{FALSE} value indicating if model is being run stochastically
#' @source IP-117068
#' @export

spawn_success <- function(escapement,
                          proportion_natural, # R2R ADDS NEW PARAM
                          hatchery_age_distribution, # R2R ADDS NEW PARAM
                          natural_age_distribution, # R2R ADDS NEW PARAM
                          fecundity_lookup, # R2R ADDS NEW PARAM
                          adult_prespawn_survival,
                          adult_prespawn_survival_abv_dam, # ADDS NEW PARAM for abv dam
                          abv_dam_spawn_proportion, # ADDS NEW PARAM for abv dam
                          abv_dam_spawn_habitat_proportion, # WR DSM adds param for calculating spawing habitat potential
                          harvest_rate_abv_dam, # WR SDM addition
                          egg_to_fry_survival,
                          egg_to_fry_survival_abv_dam, # WR SDM addition
                          prob_scour, spawn_habitat,
                          stochastic,
                          sex_ratio,
                          redd_size,
                          fecundity){


# spawn capacity -------------------------------------------------------
    # spawner potential below dam
    # will represent all habitat if no above dam habitat
    spawn_habitat_blw_dam <- spawn_habitat * (1 - abv_dam_spawn_habitat_proportion)
    capacity_blw_dam <- spawn_habitat_blw_dam / redd_size
    spawner_potential_blw_dam <- if(stochastic) {
      rbinom(31, round(escapement), ((1 - abv_dam_spawn_proportion) * adult_prespawn_survival * sex_ratio))
    } else {
      # R2R Adds abv dam logic, WR SDM adds harvest rate logic
      # round(escapement * harvest_rate_abv_dam * adult_prespawn_survival * sex_ratio)
      round((1 - abv_dam_spawn_proportion) * escapement * adult_prespawn_survival * sex_ratio, 0)
    }
    spawner_capacity_blw_dam <- pmin(spawner_potential_blw_dam, capacity_blw_dam)
    if(capacity_blw_dam["Upper Sacramento River"] < spawner_potential_blw_dam["Upper Sacramento River"]) {
      print("Below dam is spawning habitat limited")
    } else {
      print("Below dam is spawner capacity limited (escapement, prespawn survival, sex ratio)")
    }

    # spawner potential above dam
    # will be 0 if there is no above dam habitat
    spawn_habitat_abv_dam <- spawn_habitat - spawn_habitat_blw_dam
    capacity_abv_dam <- spawn_habitat_abv_dam / redd_size
    spawner_potential_abv_dam <- if(stochastic) {
      rbinom(31, round(escapement), (abv_dam_spawn_proportion * adult_prespawn_survival_abv_dam * sex_ratio))
    } else {
      round(abv_dam_spawn_proportion * escapement * (1 - harvest_rate_abv_dam) * adult_prespawn_survival_abv_dam * sex_ratio, 0)
    }
    spawner_capacity_abv_dam <- pmin(spawner_potential_abv_dam, capacity_abv_dam)
    if(capacity_abv_dam["Upper Sacramento River"] < spawner_potential_abv_dam["Upper Sacramento River"]) {
      print("Above dam is spawning habitat limited")
    } else {
      print("Above dam is spawner capacity limited (escapement, prespawn survival, sex ratio)")
      print(paste("Spawning capacity abv:", capacity_abv_dam["Upper Sacramento River"]))
      print(paste("Spawning capacity blw:", capacity_blw_dam["Upper Sacramento River"]))
    }

    # full spawner capacity
    spawner_potential <- spawner_potential_abv_dam + spawner_potential_blw_dam
    capacity <- spawn_habitat / redd_size
    spawner_capacity <- pmin(spawner_potential, capacity) # previously "spawners

    # get fecundities for hatchery vs natural
    fecundity_natural <- fecundity_lookup |>
      filter(origin == "Wild") |>
      pull(fecundity)

    fecundity_hatch <- fecundity_lookup |>
      filter(origin == "Hatchery") |>
      pull(fecundity)

# below dam -------------------------------------------------------------

    # natural fry
    nat_spawn_blw_dam <- tibble(watershed = winterRunDSM::watershed_labels,
                                spawners = round(spawner_capacity_blw_dam * proportion_natural)) |>
      left_join(natural_age_distribution, by = c("watershed")) |>
      mutate(age_2_spawners = round(spawners * prop_2),
             age_3_spawners = round(spawners * prop_3),
             age_4_spawners = round(spawners * prop_4),
             age_5_spawmers = round(spawners * prop_5)) |>
      select(-c(prop_2, prop_3, prop_4, prop_5, spawners, watershed)) |>
      as.matrix()
    dimnames(nat_spawn_blw_dam) <- list(c(winterRunDSM::watershed_labels), c("2", "3", "4", "5"))

    natural_fry_blw_dam <- suppressWarnings(rowSums(sweep(nat_spawn_blw_dam *
                                                            (1 - prob_scour), 2, fecundity_natural, "*") *
                                                      egg_to_fry_survival))
    # hatchery fry
    hatch_spawn_blw_dam <- tibble(watershed = winterRunDSM::watershed_labels,
                                spawners = round(spawner_capacity_blw_dam * (1 - proportion_natural))) |>
      left_join(hatchery_age_distribution, by = c("watershed" = "watershed")) |>
      mutate(age_2_spawners = round(spawners * prop_2),
             age_3_spawners = round(spawners * prop_3),
             age_4_spawners = round(spawners * prop_4),
             age_5_spawners = round(spawners * prop_5)) |>
      select(-c(prop_2, prop_3, prop_4, prop_5, spawners, watershed)) |>
      as.matrix()
    dimnames(hatch_spawn_blw_dam) <- list(c(winterRunDSM::watershed_labels), c("2", "3", "4", "5"))

    # calculate hatchery fry
    hatchery_fry_blw_dam <- suppressWarnings(rowSums(sweep(hatch_spawn_blw_dam *
                                                             (1 - prob_scour), 2, fecundity_hatch, "*") *
                                                       egg_to_fry_survival))

    # combine
    fry_blw_dam <- natural_fry_blw_dam + hatchery_fry_blw_dam

    fry_blw_dam <- if(stochastic) {
      pmax(round(rnorm(31, fry_blw_dam, (sqrt(fry_blw_dam) / 2))), 0)
    } else {
      round(fry_blw_dam)
    }

    zeros <- matrix(0, nrow = length(escapement), ncol = 3)

    fry_blw_dam_final <- cbind(fry_blw_dam, zeros)


# above dam ------------------------------------------------------------

    # this is hard-coded for Upper Sac here
    if(abv_dam_spawn_habitat_proportion["Upper Sacramento River"] == 0) {
      # if no above dam action, return the below dam fry as total amount
      return(list("total_fry" = fry_blw_dam_final,
                  "prop_abv_dam" = rep(0, 31)))
    } else {
      # natural fry
      nat_spawn_abv_dam <- tibble(watershed = winterRunDSM::watershed_labels,
                                  spawners = round(spawner_capacity_abv_dam * proportion_natural)) |>
        left_join(natural_age_distribution, by = c("watershed")) |>
        mutate(age_2_spawners = round(spawners * prop_2),
               age_3_spawners = round(spawners * prop_3),
               age_4_spawners = round(spawners * prop_4),
               age_5_spawmers = round(spawners * prop_5)) |>
        select(-c(prop_2, prop_3, prop_4, prop_5, spawners, watershed)) |>
        as.matrix()
      dimnames(nat_spawn_abv_dam) <- list(c(winterRunDSM::watershed_labels), c("2", "3", "4", "5"))

      natural_fry_abv_dam <- suppressWarnings(rowSums(sweep(nat_spawn_abv_dam *
                                                              (1 - prob_scour), 2, fecundity_natural, "*") *
                                                        egg_to_fry_survival_abv_dam))
      # hatchery fry
      hatch_spawn_abv_dam <- tibble(watershed = winterRunDSM::watershed_labels,
                                    spawners = round(spawner_capacity_abv_dam * (1 - proportion_natural))) |>
        left_join(hatchery_age_distribution, by = c("watershed" = "watershed")) |>
        mutate(age_2_spawners = round(spawners * prop_2),
               age_3_spawners = round(spawners * prop_3),
               age_4_spawners = round(spawners * prop_4),
               age_5_spawners = round(spawners * prop_5)) |>
        select(-c(prop_2, prop_3, prop_4, prop_5, spawners, watershed)) |>
        as.matrix()
      dimnames(hatch_spawn_abv_dam) <- list(c(winterRunDSM::watershed_labels), c("2", "3", "4", "5"))


      # calculate hatchery fry
      hatchery_fry_abv_dam <- suppressWarnings(rowSums(sweep(hatch_spawn_abv_dam *
                                                               (1 - prob_scour), 2, fecundity_hatch, "*") *
                                                         egg_to_fry_survival_abv_dam))

      # combine
      fry_abv_dam <- natural_fry_abv_dam + hatchery_fry_abv_dam

      fry_abv_dam <- if(stochastic) {
        pmax(round(rnorm(31, fry_abv_dam, (sqrt(fry_abv_dam) / 2))), 0)
      } else {
        round(fry_abv_dam)
      }

      zeros <- matrix(0, nrow = length(escapement), ncol = 3)

      fry_abv_dam_final <- cbind(fry_abv_dam, zeros)
      
      total_fry_final <- matrix(0, nrow = length(escapement), ncol = 4)
      total_fry_final[, 1] <- fry_blw_dam
      # add above dam fish to a bigger size class
      total_fry_final[, 2] <- fry_abv_dam

      prop_abv_dam <- fry_abv_dam / (fry_abv_dam + fry_blw_dam)
      prop_abv_dam[is.nan(prop_abv_dam)] <- 0

      return(list("total_fry" = total_fry_final,
                  "prop_abv_dam" = prop_abv_dam))
    }

}