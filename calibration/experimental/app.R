library(shiny)
library(paws)
library(jsonlite)
library(DT)

# -- Config -------------------------------------------------------------------
S3_BUCKET    <- Sys.getenv("CALIBRATION_S3_BUCKET", "winter-run-calibration")
S3_PREFIX    <- "calibration-results/"
CONFIG_PREFIX <- "configs/"
ECR_IMAGE    <- Sys.getenv("CALIBRATION_ECR_IMAGE", "")
EC2_INSTANCE_TYPE <- "c5.xlarge"
EC2_AMI      <- Sys.getenv("CALIBRATION_AMI", "")
EC2_KEY_NAME <- Sys.getenv("CALIBRATION_KEY_NAME", "")
EC2_SECURITY_GROUP <- Sys.getenv("CALIBRATION_SG", "")
EC2_IAM_PROFILE <- Sys.getenv("CALIBRATION_IAM_PROFILE", "")

# Parameter metadata for bounds UI
param_labels <- c(
  "surv_adult_enroute_int",
  "surv_juv_rear_int",
  "surv_juv_rear_contact_points",
  "surv_juv_rear_prop_diversions",
  "surv_juv_rear_total_diversions",
  "surv_juv_bypass_int",
  "surv_juv_delta_int",
  "surv_juv_delta_contact_points",
  "surv_juv_delta_total_diverted",
  "surv_juv_outmigration_sj_int",
  "ocean_entry_success_int",
  "surv_egg_to_fry_temp_effect"
)

default_lower <- c(0, -3.5, 0, 0, 0, -3.5, -3.5, 0, -3.5, -3.5, -3.5, 0.01)
default_upper <- c(3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, -1, 3.5, 3.5, 1)

# -- UI -----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Winter Run DSM - Calibration Launcher"),

  tabsetPanel(
    # Tab 1: Configure & Launch
    tabPanel("Launch",
      fluidRow(
        column(4,
          wellPanel(
            h4("General"),
            textInput("description", "Description", value = "calibration"),
            numericInput("seed", "Seed", value = 1234, min = 1),
            numericInput("pop_size", "Population Size", value = 100, min = 10),
            numericInput("max_iter", "Max Iterations", value = 5000, min = 100),
            numericInput("ga_run", "GA Run (convergence)", value = 100, min = 10),
            numericInput("cores", "Cores", value = 4, min = 1),
            numericInput("pmutation", "Mutation Rate", value = 0.4, min = 0, max = 1, step = 0.05),
            textInput("keep", "Watersheds (comma-separated)", value = "1"),
            textInput("comp_years", "Comparison Years (start, end)", value = "6, 20"),
            selectInput("instance_type", "EC2 Instance Type",
                        choices = c("c5.xlarge", "c5.2xlarge", "c5.4xlarge"),
                        selected = "c5.xlarge"),
            checkboxInput("use_spot", "Use Spot Instance (60-70% cheaper)", value = TRUE)
          )
        ),
        column(4,
          wellPanel(
            h4("Parameter Bounds"),
            tags$div(style = "max-height: 500px; overflow-y: auto;",
              lapply(seq_along(param_labels), function(i) {
                fluidRow(
                  column(12, tags$strong(param_labels[i])),
                  column(6, numericInput(
                    paste0("lower_", i), "Lower", value = default_lower[i], step = 0.1
                  )),
                  column(6, numericInput(
                    paste0("upper_", i), "Upper", value = default_upper[i], step = 0.1
                  ))
                )
              })
            )
          )
        ),
        column(4,
          wellPanel(
            h4("Actions"),
            actionButton("launch", "Launch Calibration", class = "btn-primary btn-lg",
                         style = "width: 100%; margin-bottom: 15px;"),
            hr(),
            h4("Config Preview"),
            verbatimTextOutput("config_preview"),
            hr(),
            downloadButton("download_config", "Download Config JSON"),
            br(), br(),
            fileInput("upload_config", "Load Config JSON", accept = ".json")
          )
        )
      )
    ),

    # Tab 2: Monitor
    tabPanel("Monitor",
      fluidRow(
        column(12,
          br(),
          actionButton("refresh", "Refresh", class = "btn-info"),
          br(), br(),
          h4("Running Instances"),
          DTOutput("running_table"),
          br(),
          h4("Completed Results (S3)"),
          DTOutput("results_table")
        )
      )
    )
  )
)

# -- Server -------------------------------------------------------------------
server <- function(input, output, session) {

  s3 <- paws::s3()
  ec2 <- paws::ec2()

  # Build config list from inputs
  build_config <- reactive({
    lower <- sapply(seq_along(param_labels), function(i) input[[paste0("lower_", i)]])
    upper <- sapply(seq_along(param_labels), function(i) input[[paste0("upper_", i)]])
    keep <- as.integer(trimws(strsplit(input$keep, ",")[[1]]))
    comp_years <- as.integer(trimws(strsplit(input$comp_years, ",")[[1]]))

    list(
      description = input$description,
      seed = input$seed,
      pop_size = input$pop_size,
      max_iter = input$max_iter,
      ga_run = input$ga_run,
      cores = input$cores,
      pmutation = input$pmutation,
      keep = keep,
      comparison_years = comp_years,
      lower_bounds = lower,
      upper_bounds = upper,
      suggestions = NULL,
      s3_bucket = S3_BUCKET
    )
  })

  # Config preview
  output$config_preview <- renderText({
    jsonlite::toJSON(build_config(), pretty = TRUE, auto_unbox = TRUE)
  })

  # Download config
  output$download_config <- downloadHandler(
    filename = function() paste0(input$description, ".json"),
    content = function(file) {
      jsonlite::write_json(build_config(), file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  # Upload config
  observeEvent(input$upload_config, {
    req(input$upload_config)
    cfg <- jsonlite::fromJSON(input$upload_config$datapath)

    updateTextInput(session, "description", value = cfg$description %||% "calibration")
    updateNumericInput(session, "seed", value = cfg$seed %||% 1234)
    updateNumericInput(session, "pop_size", value = cfg$pop_size %||% 100)
    updateNumericInput(session, "max_iter", value = cfg$max_iter %||% 5000)
    updateNumericInput(session, "ga_run", value = cfg$ga_run %||% 100)
    updateNumericInput(session, "cores", value = cfg$cores %||% 4)
    updateNumericInput(session, "pmutation", value = cfg$pmutation %||% 0.4)
    updateTextInput(session, "keep", value = paste(cfg$keep %||% 1, collapse = ", "))
    updateTextInput(session, "comp_years", value = paste(cfg$comparison_years %||% c(6, 20), collapse = ", "))

    if (!is.null(cfg$lower_bounds)) {
      for (i in seq_along(cfg$lower_bounds)) {
        updateNumericInput(session, paste0("lower_", i), value = cfg$lower_bounds[i])
      }
    }
    if (!is.null(cfg$upper_bounds)) {
      for (i in seq_along(cfg$upper_bounds)) {
        updateNumericInput(session, paste0("upper_", i), value = cfg$upper_bounds[i])
      }
    }
  })

  # Launch calibration
  observeEvent(input$launch, {
    cfg <- build_config()
    config_json <- jsonlite::toJSON(cfg, pretty = TRUE, auto_unbox = TRUE)
    config_key <- paste0(CONFIG_PREFIX, cfg$description, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")

    # Upload config to S3
    tryCatch({
      s3$put_object(
        Bucket = S3_BUCKET,
        Key = config_key,
        Body = charToRaw(as.character(config_json))
      )

      s3_config_path <- paste0("s3://", S3_BUCKET, "/", config_key)

      # EC2 user data script
      user_data <- paste0(
        "#!/bin/bash\n",
        "docker pull ", ECR_IMAGE, "\n",
        "docker run --rm ",
        "-e S3_CONFIG=", s3_config_path, " ",
        "-e AWS_DEFAULT_REGION=us-west-2 ",
        ECR_IMAGE, "\n",
        "shutdown -h now\n"
      )

      # Launch EC2 instance
      launch_args <- list(
        ImageId = EC2_AMI,
        InstanceType = input$instance_type,
        MinCount = 1,
        MaxCount = 1,
        SecurityGroupIds = list(EC2_SECURITY_GROUP),
        IamInstanceProfile = list(Name = EC2_IAM_PROFILE),
        UserData = base64enc::base64encode(charToRaw(user_data)),
        TagSpecifications = list(
          list(
            ResourceType = "instance",
            Tags = list(
              list(Key = "Name", Value = paste0("calibration-", cfg$description)),
              list(Key = "Project", Value = "winter-run-dsm"),
              list(Key = "CalibrationDesc", Value = cfg$description)
            )
          )
        ),
        InstanceInitiatedShutdownBehavior = "terminate"
      )

      if (input$use_spot) {
        launch_args$InstanceMarketOptions <- list(
          MarketType = "spot",
          SpotOptions = list(
            SpotInstanceType = "one-time",
            InstanceInterruptionBehavior = "terminate"
          )
        )
      }

      result <- do.call(ec2$run_instances, launch_args)

      instance_id <- result$Instances[[1]]$InstanceId
      showNotification(
        paste("Launched instance", instance_id, "for", cfg$description),
        type = "message", duration = 10
      )
    },
    error = function(e) {
      showNotification(paste("Launch failed:", e$message), type = "error", duration = 15)
    })
  })

  # Monitor: running instances
  running_data <- reactiveVal(data.frame())
  results_data <- reactiveVal(data.frame())

  refresh_data <- function() {
    # Get running calibration instances
    tryCatch({
      instances <- ec2$describe_instances(
        Filters = list(
          list(Name = "tag:Project", Values = list("winter-run-dsm")),
          list(Name = "instance-state-name", Values = list("pending", "running"))
        )
      )

      rows <- list()
      for (reservation in instances$Reservations) {
        for (inst in reservation$Instances) {
          name_tag <- ""
          desc_tag <- ""
          for (tag in inst$Tags) {
            if (tag$Key == "Name") name_tag <- tag$Value
            if (tag$Key == "CalibrationDesc") desc_tag <- tag$Value
          }
          rows[[length(rows) + 1]] <- data.frame(
            InstanceId = inst$InstanceId,
            Name = name_tag,
            Description = desc_tag,
            State = inst$State$Name,
            Type = inst$InstanceType,
            LaunchTime = as.character(inst$LaunchTime),
            stringsAsFactors = FALSE
          )
        }
      }

      if (length(rows) > 0) {
        running_data(do.call(rbind, rows))
      } else {
        running_data(data.frame(
          InstanceId = character(), Name = character(), Description = character(),
          State = character(), Type = character(), LaunchTime = character()
        ))
      }
    }, error = function(e) {
      showNotification(paste("EC2 refresh failed:", e$message), type = "warning")
    })

    # Get completed results from S3
    tryCatch({
      objects <- s3$list_objects_v2(Bucket = S3_BUCKET, Prefix = S3_PREFIX)
      if (length(objects$Contents) > 0) {
        rows <- lapply(objects$Contents, function(obj) {
          data.frame(
            Key = obj$Key,
            Size = paste0(round(obj$Size / 1024, 1), " KB"),
            LastModified = as.character(obj$LastModified),
            stringsAsFactors = FALSE
          )
        })
        results_data(do.call(rbind, rows))
      } else {
        results_data(data.frame(Key = character(), Size = character(), LastModified = character()))
      }
    }, error = function(e) {
      showNotification(paste("S3 refresh failed:", e$message), type = "warning")
    })
  }

  observeEvent(input$refresh, refresh_data())

  output$running_table <- renderDT({
    datatable(running_data(), options = list(pageLength = 10), rownames = FALSE)
  })

  output$results_table <- renderDT({
    datatable(results_data(), options = list(pageLength = 10, order = list(list(2, "desc"))),
              rownames = FALSE)
  })
}

shinyApp(ui, server)
