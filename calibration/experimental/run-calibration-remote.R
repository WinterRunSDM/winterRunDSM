library(winterRunDSM)
library(GA)
library(dplyr)
library(readr)
library(jsonlite)
library(DSMCalibrationData)

source("calibration/fitness.R")
source("calibration/update-params.R")

# Read config
config_path <- Sys.getenv("CONFIG_PATH", "/config.json")
if (!file.exists(config_path)) {
  stop("Config file not found: ", config_path)
}
cfg <- jsonlite::fromJSON(config_path)

# Defaults
description <- cfg$description %||% "calibration"
pop_size    <- cfg$pop_size %||% 100L
iter        <- cfg$max_iter %||% 5000L
seed        <- cfg$seed %||% 1234L
n_cores     <- cfg$cores %||% 4L
ga_run      <- cfg$ga_run %||% 100L
pmutation   <- cfg$pmutation %||% 0.4
keep        <- cfg$keep %||% c(1L)
comp_years  <- cfg$comparison_years %||% c(6L, 20L)
s3_bucket   <- cfg$s3_bucket %||% ""

# Bounds: use config if provided, otherwise derive from LTO mapping
if (!is.null(cfg$lower_bounds) && !is.null(cfg$upper_bounds)) {
  lower_bounds <- cfg$lower_bounds
  upper_bounds <- cfg$upper_bounds
} else {
  map_params <- tibble::tibble(
    "LTO_index" = c(1:16, 13),
    "R2R_index" = c(2, 6, 7, 10,
                    NA, NA, NA, NA,
                    NA, 11, 1, 12,
                    3, 4, 5, 9, 8)) |>
    dplyr::mutate(
      "LTO_mins" = c(rep(-3.5, 10), 0, -3.5, rep(0, 4), 0),
      "LTO_maxes" = c(rep(3.5, 9), -1, rep(3.5, 6), 3.5)) |>
    dplyr::arrange(R2R_index) |>
    dplyr::filter(!is.na(R2R_index))
  lower_bounds <- map_params$LTO_mins
  upper_bounds <- map_params$LTO_maxes
  lower_bounds[12] <- 0.01
  upper_bounds[12] <- 1
}

# Suggestions matrix
suggestions <- if (!is.null(cfg$suggestions)) {
  matrix(cfg$suggestions, nrow = 1)
} else {
  NULL
}

cat("=== Calibration Config ===\n")
cat("Description:      ", description, "\n")
cat("Pop size:         ", pop_size, "\n")
cat("Max iter:         ", iter, "\n")
cat("GA run:           ", ga_run, "\n")
cat("Seed:             ", seed, "\n")
cat("Cores:            ", n_cores, "\n")
cat("Mutation rate:    ", pmutation, "\n")
cat("Watersheds (keep):", keep, "\n")
cat("Comparison years: ", comp_years[1], "to", comp_years[2], "\n")
cat("Lower bounds:     ", lower_bounds, "\n")
cat("Upper bounds:     ", upper_bounds, "\n")
cat("Suggestions:      ", if (is.null(suggestions)) "(none)" else "provided", "\n")
cat("S3 bucket:        ", ifelse(s3_bucket == "", "(none - local only)", s3_bucket), "\n")
cat("==========================\n\n")

params <- DSMCalibrationData::set_synth_years(winterRunDSM::wr_sdm_baseline_params)
grandtab_observed <- DSMCalibrationData::grandtab_observed

set.seed(seed)

cat("Starting GA calibration...\n")
start_time <- Sys.time()

res <- ga(type = "real-valued",
          fitness =
            function(x) -winter_run_fitness(
              known_adults = grandtab_observed$winter,
              seeds = DSMCalibrationData::grandtab_imputed$winter,
              params = params,
              x[1], x[2], x[3], x[4], x[5], x[6], x[7], x[8], x[9], x[10],
              x[11], x[12],
              keep = keep,
              comp_years = comp_years
            ),
          lower = lower_bounds,
          upper = upper_bounds,
          suggestions = suggestions,
          popSize = pop_size,
          maxiter = iter,
          run = ga_run,
          parallel = n_cores,
          pmutation = pmutation)

elapsed <- difftime(Sys.time(), start_time, units = "hours")
cat(sprintf("\nCalibration complete in %.2f hours\n", as.numeric(elapsed)))
cat("Best fitness:", res@fitnessValue, "\n")

# Save results and config together
timestamp <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
filename <- paste0(description, "_", timestamp, ".rds")
local_path <- paste0("calibration/", filename)

output <- list(
  ga_result = res,
  config = cfg,
  elapsed_hours = as.numeric(elapsed),
  timestamp = timestamp
)
readr::write_rds(output, local_path)
cat("Results saved locally:", local_path, "\n")

# Upload to S3 if configured
if (s3_bucket != "") {
  s3_prefix <- paste0("s3://", s3_bucket, "/calibration-results/")

  # Upload results
  s3_rds <- paste0(s3_prefix, filename)
  cat("Uploading results to S3:", s3_rds, "\n")
  status <- system(paste("aws s3 cp", local_path, s3_rds))

  # Upload config for reference
  s3_cfg <- paste0(s3_prefix, description, "_", timestamp, "_config.json")
  cat("Uploading config to S3:", s3_cfg, "\n")
  system(paste("aws s3 cp", config_path, s3_cfg))

  if (status == 0) {
    cat("Upload complete\n")
  } else {
    cat("WARNING: S3 upload failed with status", status, "\n")
  }
}
