#' Launch a calibration run on a remote EC2 spot instance.
#'
#' @param description Character. Name for this calibration run.
#' @param seed Integer. Random seed for the GA.
#' @param pop_size Integer. GA population size.
#' @param max_iter Integer. Maximum GA iterations.
#' @param ga_run Integer. Consecutive generations without improvement before stopping.
#' @param cores Integer. Number of parallel cores.
#' @param pmutation Numeric. GA mutation rate.
#' @param keep Integer vector. Watershed indices to calibrate on.
#' @param comparison_years Integer vector of length 2. Column range in grandtab data.
#' @param lower_bounds Numeric vector of length 12. Lower bounds for parameters.
#' @param upper_bounds Numeric vector of length 12. Upper bounds for parameters.
#' @param suggestions Numeric vector of length 12 or NULL. Starting suggestion for GA.
#' @param instance_type Character. EC2 instance type.
#' @param use_spot Logical. Use spot instance.
#' @param profile Character. AWS CLI profile name.
#' @param region Character. AWS region.
#' @param bucket Character. S3 bucket name.
#' @param ecr_image Character. ECR image URI.
#' @param ami Character. EC2 AMI ID.
#' @param security_group Character. EC2 security group ID.
#' @param iam_profile Character. IAM instance profile name.
#' @return The EC2 instance ID (invisibly).
calibrate_remote <- function(
  description = "calibration",
  seed = 1234,
  pop_size = 100,
  max_iter = 5000,
  ga_run = 100,
  cores = 4,
  pmutation = 0.4,
  keep = c(1),
  comparison_years = c(6, 20),
  lower_bounds = c(0, -3.5, 0, 0, 0, -3.5, -3.5, 0, -3.5, -3.5, -3.5, 0.01),
  upper_bounds = c(3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, -1, 3.5, 3.5, 1),
  suggestions = NULL,
  instance_type = "c5.xlarge",
  use_spot = TRUE,
  profile = Sys.getenv("AWS_PROFILE"),
  region = Sys.getenv("AWS_REGION", "us-west-2"),
  bucket = Sys.getenv("CALIBRATION_S3_BUCKET", "winter-run-dsm-calibration"),
  ecr_image = Sys.getenv("CALIBRATION_ECR_IMAGE"),
  ami = Sys.getenv("CALIBRATION_AMI"),
  security_group = Sys.getenv("CALIBRATION_SG"),
  iam_profile = Sys.getenv("CALIBRATION_IAM_PROFILE", "winter-run-dsm-calibration-ec2")
) {

  # Validate required params
  required <- c(profile = profile, ecr_image = ecr_image, ami = ami, security_group = security_group)
  missing <- names(required)[required == ""]
  if (length(missing) > 0) {
    cli::cli_abort("Missing required parameters: {.val {missing}}.
                    Set via function args or environment variables.")
  }

  # Build config

  config <- list(
    description = description,
    seed = seed,
    pop_size = pop_size,
    max_iter = max_iter,
    ga_run = ga_run,
    cores = cores,
    pmutation = pmutation,
    keep = keep,
    comparison_years = comparison_years,
    lower_bounds = lower_bounds,
    upper_bounds = upper_bounds,
    suggestions = suggestions,
    s3_bucket = bucket
  )

  config_json <- jsonlite::toJSON(config, pretty = TRUE, auto_unbox = TRUE)
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  config_key <- paste0("configs/", description, "_", timestamp, ".json")
  s3_config_path <- paste0("s3://", bucket, "/", config_key)

  # Upload config to S3
  cli::cli_progress_step("Uploading config to {.url {s3_config_path}}")
  tmp <- tempfile(fileext = ".json")
  writeLines(config_json, tmp)
  system2("aws", c(
    "--profile", profile, "--region", region,
    "s3", "cp", tmp, s3_config_path
  ), stdout = TRUE, stderr = TRUE)
  unlink(tmp)

  # Build user data script
  user_data <- paste0(
    "#!/bin/bash\n",
    "$(aws ecr get-login-password --region ", region,
    " | docker login --username AWS --password-stdin ",
    sub("/[^/]+$", "", ecr_image), ")\n",
    "docker pull ", ecr_image, "\n",
    "docker run --rm",
    " -e S3_CONFIG=", s3_config_path,
    " -e AWS_DEFAULT_REGION=", region,
    " ", ecr_image, "\n",
    "shutdown -h now\n"
  )
  user_data_b64 <- base64enc::base64encode(charToRaw(user_data))

  # Build launch command
  tags <- paste0(
    "ResourceType=instance,Tags=[",
    "{Key=Name,Value=calibration-", description, "},",
    "{Key=Project,Value=winter-run-dsm},",
    "{Key=CalibrationDesc,Value=", description, "}]"
  )

  launch_args <- c(
    "--profile", profile, "--region", region,
    "ec2", "run-instances",
    "--image-id", ami,
    "--instance-type", instance_type,
    "--count", "1",
    "--security-group-ids", security_group,
    "--iam-instance-profile", paste0("Name=", iam_profile),
    "--user-data", user_data_b64,
    "--tag-specifications", tags,
    "--instance-initiated-shutdown-behavior", "terminate"
  )

  if (use_spot) {
    launch_args <- c(launch_args,
      "--instance-market-options",
      "MarketType=spot,SpotOptions={SpotInstanceType=one-time,InstanceInterruptionBehavior=terminate}"
    )
  }

  spot_label <- if (use_spot) "spot" else "on-demand"
  cli::cli_progress_step("Launching {.val {instance_type}} ({spot_label}) instance")
  result <- system2("aws", launch_args, stdout = TRUE, stderr = TRUE)
  parsed <- jsonlite::fromJSON(paste(result, collapse = "\n"))
  instance_id <- parsed$Instances$InstanceId

  cli::cli_alert_success("Instance launched: {.val {instance_id}}")
  cli::cli_bullets(c(
    " " = "Description: {.val {description}}",
    " " = "Config: {.url {s3_config_path}}",
    "i" = "Check status with {.fn calibrate_check_results}"
  ))

  invisible(instance_id)
}


#' Check running calibrations and completed results.
#'
#' @param profile Character. AWS CLI profile name.
#' @param region Character. AWS region.
#' @param bucket Character. S3 bucket name.
#' @return A list with `running` and `completed` data frames (invisibly).
calibrate_check_results <- function(
  profile = Sys.getenv("AWS_PROFILE"),
  region = Sys.getenv("AWS_REGION", "us-west-2"),
  bucket = Sys.getenv("CALIBRATION_S3_BUCKET", "winter-run-dsm-calibration")
) {

  # Check running instances
  cli::cli_h2("Running Calibrations")
  instances_json <- system2("aws", c(
    "--profile", profile, "--region", region,
    "ec2", "describe-instances",
    "--filters",
    "Name=tag:Project,Values=winter-run-dsm",
    "Name=instance-state-name,Values=pending,running",
    "--output", "json"
  ), stdout = TRUE, stderr = TRUE)

  parsed <- jsonlite::fromJSON(paste(instances_json, collapse = "\n"))

  running <- data.frame()
  if (length(parsed$Reservations) > 0) {
    rows <- list()
    for (res in seq_along(parsed$Reservations$Instances)) {
      insts <- parsed$Reservations$Instances[[res]]
      for (j in seq_len(nrow(insts))) {
        inst <- insts[j, ]
        tags <- inst$Tags[[1]]
        desc <- ""
        if (!is.null(tags)) {
          desc_match <- tags$Value[tags$Key == "CalibrationDesc"]
          if (length(desc_match) > 0) desc <- desc_match[1]
        }
        rows[[length(rows) + 1]] <- data.frame(
          InstanceId = inst$InstanceId,
          Type = inst$InstanceType,
          State = inst$State$Name,
          LaunchTime = as.character(inst$LaunchTime),
          Description = desc,
          stringsAsFactors = FALSE
        )
      }
    }
    if (length(rows) > 0) running <- do.call(rbind, rows)
  }

  if (nrow(running) == 0) {
    cli::cli_alert_info("No running instances.")
  } else {
    print(running, row.names = FALSE)
  }

  # Check completed results in S3
  cli::cli_h2("Completed Results (S3)")
  results_json <- system2("aws", c(
    "--profile", profile, "--region", region,
    "s3api", "list-objects-v2",
    "--bucket", bucket,
    "--prefix", "calibration-results/",
    "--output", "json"
  ), stdout = TRUE, stderr = TRUE)

  parsed_s3 <- jsonlite::fromJSON(paste(results_json, collapse = "\n"))

  completed <- data.frame()
  if (!is.null(parsed_s3$Contents)) {
    completed <- parsed_s3$Contents[grepl("\\.rds$", parsed_s3$Contents$Key), ]
    if (nrow(completed) > 0) {
      completed <- data.frame(
        Key = basename(completed$Key),
        Size = paste0(round(completed$Size / 1024, 1), " KB"),
        LastModified = as.character(completed$LastModified),
        stringsAsFactors = FALSE
      )
    }
  }

  if (nrow(completed) == 0) {
    cli::cli_alert_info("No results yet.")
  } else {
    print(completed, row.names = FALSE)
  }

  cli::cli_rule()
  cli::cli_alert_info("Download a result with {.fn calibrate_download_result}")

  invisible(list(running = running, completed = completed))
}


#' Terminate a running calibration instance.
#'
#' @param instance_id Character. The EC2 instance ID to terminate.
#'   If NULL, lists running instances and prompts for selection.
#' @param profile Character. AWS CLI profile name.
#' @param region Character. AWS region.
#' @return The terminated instance ID (invisibly).
calibrate_stop <- function(
  instance_id = NULL,
  profile = Sys.getenv("AWS_PROFILE"),
  region = Sys.getenv("AWS_REGION", "us-west-2")
) {

  if (is.null(instance_id)) {
    # List running instances for selection
    instances_json <- system2("aws", c(
      "--profile", profile, "--region", region,
      "ec2", "describe-instances",
      "--filters",
      "Name=tag:Project,Values=winter-run-dsm",
      "Name=instance-state-name,Values=pending,running",
      "--output", "json"
    ), stdout = TRUE, stderr = TRUE)

    parsed <- jsonlite::fromJSON(paste(instances_json, collapse = "\n"))

    running <- data.frame()
    if (length(parsed$Reservations) > 0) {
      rows <- list()
      for (res in seq_along(parsed$Reservations$Instances)) {
        insts <- parsed$Reservations$Instances[[res]]
        for (j in seq_len(nrow(insts))) {
          inst <- insts[j, ]
          tags <- inst$Tags[[1]]
          desc <- ""
          if (!is.null(tags)) {
            desc_match <- tags$Value[tags$Key == "CalibrationDesc"]
            if (length(desc_match) > 0) desc <- desc_match[1]
          }
          rows[[length(rows) + 1]] <- data.frame(
            InstanceId = inst$InstanceId,
            Description = desc,
            LaunchTime = as.character(inst$LaunchTime),
            stringsAsFactors = FALSE
          )
        }
      }
      if (length(rows) > 0) running <- do.call(rbind, rows)
    }

    if (nrow(running) == 0) {
      cli::cli_alert_info("No running calibration instances to stop.")
      return(invisible(NULL))
    }

    cli::cli_h2("Running Calibrations")
    for (i in seq_len(nrow(running))) {
      cli::cli_bullets(c(" " = "{.val {i}}: {running$InstanceId[i]} - {running$Description[i]} (launched {running$LaunchTime[i]})"))
    }

    choice <- as.integer(readline(prompt = "Enter number to terminate (0 to cancel): "))
    if (is.na(choice) || choice == 0 || choice > nrow(running)) {
      cli::cli_alert_info("Cancelled.")
      return(invisible(NULL))
    }

    instance_id <- running$InstanceId[choice]
  }

  cli::cli_progress_step("Terminating instance {.val {instance_id}}")
  system2("aws", c(
    "--profile", profile, "--region", region,
    "ec2", "terminate-instances",
    "--instance-ids", instance_id
  ), stdout = TRUE, stderr = TRUE)

  cli::cli_alert_success("Instance {.val {instance_id}} terminated.")
  invisible(instance_id)
}


#' Download a calibration result from S3.
#'
#' @param filename Character. The .rds filename to download.
#' @param dest Character. Local directory to save to.
#' @param profile Character. AWS CLI profile name.
#' @param region Character. AWS region.
#' @param bucket Character. S3 bucket name.
#' @return The loaded R object.
calibrate_download_result <- function(
  filename,
  dest = "calibration",
  profile = Sys.getenv("AWS_PROFILE"),
  region = Sys.getenv("AWS_REGION", "us-west-2"),
  bucket = Sys.getenv("CALIBRATION_S3_BUCKET", "winter-run-dsm-calibration")
) {

  s3_path <- paste0("s3://", bucket, "/calibration-results/", filename)
  local_path <- file.path(dest, filename)

  cli::cli_progress_step("Downloading {.file {filename}}")
  system2("aws", c(
    "--profile", profile, "--region", region,
    "s3", "cp", s3_path, local_path
  ), stdout = TRUE, stderr = TRUE)

  cli::cli_alert_success("Saved to {.file {local_path}}")
  readr::read_rds(local_path)
}
