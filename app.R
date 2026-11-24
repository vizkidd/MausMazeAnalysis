# Biostatistical Maze Project - Shiny dashboard
# Run from this directory with: shiny::runApp()

# Permit larger local multi-file session uploads; increase this only when needed.
options(shiny.maxRequestSize = 100 * 1024^2)

suppressPackageStartupMessages({
  library(shiny)
  library(shinyFiles)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

app_dir <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)
source(file.path(app_dir, "R", "maze_helpers.R"), local = TRUE)

app_roots <- c(
  Home = normalizePath(path.expand("~"), winslash = "/", mustWork = FALSE),
  "App directory" = app_dir
)
if (.Platform$OS.type == "windows") {
  available_drives <- paste0(LETTERS, ":/")
  available_drives <- available_drives[dir.exists(available_drives)]
  if (length(available_drives)) {
    drive_labels <- paste("Drive", substr(available_drives, 1, 1))
    app_roots <- c(app_roots, stats::setNames(available_drives, drive_labels))
  }
} else {
  app_roots <- c(app_roots, "File system" = "/")
}

metric_choices <- list(
  "Path, checkpoints and completion" = c(
    "Phase A path deviation (completed four-checkpoint phase)" = "path_deviation",
    "Full-task path deviation (four checkpoints + exit)" = "full_task_deviation",
    "Phase A movement steps" = "phase_a_steps",
    "Phase B movement steps" = "phase_b_steps",
    "Phase B steps to exit (reconciled endpoint)" = "phase_b_steps_to_exit",
    "Total movement steps" = "total_steps",
    "Full-task steps (including exit endpoint)" = "full_task_steps",
    "Checkpoint completion (%)" = "checkpoint_completion_pct",
    "Game score" = "score",
    "Completion time (s)" = "completion_time_sec"
  ),
  "Navigation and movement behaviour" = c(
    "Order efficiency" = "orderEfficiency",
    "Execution efficiency" = "executionEfficiency",
    "Total Phase A efficiency" = "totalPhaseAEfficiency",
    "Destination efficiency" = "destinationEfficiency",
    "Turn count" = "turnCount",
    "Excess turns" = "excessTurns",
    "Action regret" = "actionRegret",
    "Repeated cells" = "repeatedCellCount",
    "Repeated edges" = "repeatedEdgeCount",
    "Repeated-cell rate" = "repeatCellRate",
    "Immediate backtracks" = "immediateBacktracks",
    "Backtrack rate" = "backtrackRate",
    "Dead-end entries" = "deadEndEntries"
  ),
  "Response, motor and control measures" = c(
    "Mean junction latency (player summary, ms)" = "response_latency_ms",
    "Mean junction decision latency (raw events, ms)" = "junction_latency_mean_ms",
    "Median junction decision latency (raw events, ms)" = "junction_latency_median_ms",
    "Junction decision count" = "junction_decision_count",
    "Junctions encountered" = "junctionsEncountered",
    "Junction latency variability (CV)" = "junctionLatencyVariabilityCV",
    "Wall collisions (player summary)" = "wallCollisionsCount",
    "Wall collision events" = "wall_collision_count",
    "Revisit events" = "revisit_event_count",
    "Rapid input bursts (player summary)" = "rciBurstCount",
    "Rapid input burst events" = "rapid_input_burst_count",
    "Rapid input frequency" = "rciFrequency",
    "Motor accuracy" = "motorAccuracy",
    "Blocked input rate" = "blockedInputRate",
    "Redundant input rate" = "redundantInputRate",
    "Barrier adaptation recovery (s)" = "barrierAdaptationRecoverySec",
    "Barrier perseverative errors" = "barrierPerseverativeErrors"
  ),
  "Cognitive domain scores" = c(
    "Executive function score" = "executiveFunctionScore",
    "Executive function T-score" = "executiveFunctionTScore",
    "Complex attention score" = "complexAttentionScore",
    "Complex attention T-score" = "complexAttentionTScore",
    "Learning and memory score" = "learningMemoryScore",
    "Learning and memory T-score" = "learningMemoryTScore",
    "Perceptual-motor score" = "perceptualMotorScore",
    "Perceptual-motor T-score" = "perceptualMotorTScore"
  )
)

ui <- fluidPage(
  titlePanel("Maze Trial Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Data source"),
      radioButtons(
        "source_type", "Load data from:",
        choices = c("Bundled sample JSONs" = "examples",
                    "A local folder" = "folder",
                    "Upload JSON files" = "upload"),
        selected = if (dir.exists(file.path(app_dir, "example_data"))) "examples" else "folder"
      ),
      conditionalPanel(
        condition = "input.source_type == 'folder'",
        shinyDirButton("data_dir", "Choose JSON folder", "Select a folder containing session JSON files", class = "btn-default"),
        verbatimTextOutput("dir_path_display", placeholder = TRUE)
      ),
      conditionalPanel(
        condition = "input.source_type == 'upload'",
        fileInput("json_files", "Upload session JSON files", multiple = TRUE, accept = c(".json", "application/json"))
      ),
      actionButton("run_analysis", "Load and analyse", icon = icon("play"), class = "btn-primary", width = "100%"),
      textOutput("data_status"),
      downloadButton("download_players", "Export player metrics"),
      downloadButton("download_visits", "Export tile visits"),
      downloadButton("download_quality", "Export data-quality log"),
      hr(),
      h4("Maze explorer"),
      uiOutput("maze_selector_ui"),
      selectInput("player_filter", "Player path:",
                  choices = c("Both players" = "ALL", "Player 1" = "PLAYER_1", "Player 2" = "PLAYER_2"),
                  selected = "ALL"),
      selectInput("phase_filter", "Movement phase:",
                  choices = c("Both phases" = "BOTH", "Checkpoint collection (A)" = "PHASE_A_CHECKPOINTS",
                              "Exit race (B)" = "PHASE_B_EXIT_RACE"), selected = "BOTH"),
      checkboxInput("show_actual_paths", "Overlay player trajectories", TRUE),
      checkboxInput("show_optimal_a", "Show optimal four-checkpoint route", TRUE),
      checkboxInput("show_optimal_full", "Show global optimum including exit", FALSE),
      hr(),
      h4("Comparative testing"),
      selectInput("test_var", "Metric:", choices = metric_choices, selected = "path_deviation"),
      uiOutput("eval_model_ui"),
      conditionalPanel(
        condition = "input.eval_model == 'one_sample'",
        selectInput("one_sample_population", "Player records for the baseline test:",
                    choices = c("Human-controlled players" = "Human",
                                "AI-controlled agents" = "AI",
                                "All player types / unknown" = "ALL"),
                    selected = "Human"),
        numericInput("reference_value", "One-sample reference value:", value = 0, step = 0.1)
      ),
      selectInput("test_method", "Test selection:",
                  choices = c("Automatic (normality diagnostic)" = "auto",
                              "Parametric test" = "parametric",
                              "Non-parametric test" = "nonparametric"),
                  selected = "auto"),
      checkboxInput("signed_log", "Apply signed log transform", FALSE),
      checkboxInput("z_score", "Apply z-score standardisation", FALSE),
      helpText("One-sample tests default to Human records so AI agents are not mixed into the cognitive-assessment sample. Choose AI or All explicitly when that is the intended analysis."),
      helpText("Choose a scientifically justified reference for one-sample tests; zero is not automatically appropriate for every metric."),
      helpText("Signed log preserves zero and the sign of deviations / paired differences. Z-scoring adjusts the one-sample reference value as well. Repeated trials from one participant are not independent observations.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Overview",
          br(),
          uiOutput("summary_cards"),
          fluidRow(
            column(6, plotOutput("age_plot", height = "280px")),
            column(6, plotOutput("deviation_plot", height = "280px"))
          ),
          h4("Player-level trial summary"),
          tableOutput("player_summary_table")
        ),
        tabPanel("Maze and heatmap",
          br(),
          uiOutput("maze_caption"),
          plotOutput("maze_map_plot", height = "720px"),
          p("Heatmap counts include the initial tile, successful MOVE destinations, and a distinct EXIT_REACHED tile when it is not the last recorded MOVE. EXIT_REVEALED is a phase marker, not an extra visit.")
        ),
        tabPanel("Population plots",
          br(),
          h3("Pooled coordinate occupancy across mazes"),
          p("Each panel pools recognised Human or AI visits across all PvAI trials on their shared zero-based coordinate frame. Values are normalised as visits per 1,000 recorded tile arrivals. Maze walls are intentionally omitted because each trial has a different generated layout."),
          plotOutput("pooled_heatmap_plot", height = "470px"),
          hr(),
          h3("Complete movement distribution vs optimal baselines"),
          p("For each recognised PvAI player record that completed all four checkpoints and reached the exit, the observed full-task movement total is overlaid with the wall-aware global optimum for that same maze. The optimum is repeated once per eligible player record, so each panel compares matched maze-specific baselines with observed complete journeys."),
          plotOutput("actual_optimal_plot", height = "430px"),
          h4("Completed-journey summary"),
          tableOutput("actual_optimal_summary_table"),
          p("Incomplete journeys are excluded from this comparison. These are descriptive trial-level distributions; repeated trials from the same participant are not independent observations.")
        ),
        tabPanel("Comparative testing strategy",
          br(),
          h4("Step 1: Normality diagnostics"),
          plotOutput("normality_plots", height = "330px"),
          h4("Steps 2-4: Transformation and hypothesis test"),
          verbatimTextOutput("stats_results"),
          h4("Observations included in the test"),
          tableOutput("analysis_pairs")
        ),
        tabPanel("Response times and events",
          br(),
          fluidRow(
            column(6, selectInput("latency_player_filter", "Player:",
                                 choices = c("Both players" = "ALL", "Player 1" = "PLAYER_1", "Player 2" = "PLAYER_2"),
                                 selected = "ALL")),
            column(6, selectInput("latency_phase_filter", "Phase:",
                                 choices = c("Both phases" = "BOTH", "Checkpoint collection (A)" = "PHASE_A_CHECKPOINTS",
                                             "Exit race (B)" = "PHASE_B_EXIT_RACE"), selected = "BOTH"))
          ),
          p("Decision-latency plots use JUNCTION_DECISION events only; other event types may carry timing values for different purposes."),
          plotOutput("latency_plot", height = "360px"),
          h4("Response-latency summary by trial, player and phase"),
          tableOutput("latency_summary_table"),
          h4("Event counts by player"),
          tableOutput("event_summary_table")
        ),
        tabPanel("Data quality", 
          br(),
          p("Checks include maze topology, checkpoint count, game-recorded versus recomputed optimal distances, movement counts, event-to-trial mapping, and duplicate exports."),
          tableOutput("quality_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  shinyDirChoose(input, "data_dir", roots = app_roots, session = session)

  output$dir_path_display <- renderPrint({
    if (is.null(input$data_dir) || is.integer(input$data_dir)) {
      cat("No folder selected.")
    } else {
      cat(parseDirPath(app_roots, input$data_dir))
    }
  })

  parsed_data <- eventReactive(input$run_analysis, {
    paths <- character(0)
    if (identical(input$source_type, "examples")) {
      example_dir <- file.path(app_dir, "example_data")
      paths <- list.files(example_dir, pattern = "\\.json$", full.names = TRUE, ignore.case = TRUE)
    } else if (identical(input$source_type, "folder")) {
      if (is.null(input$data_dir) || is.integer(input$data_dir)) {
        showNotification("Choose a folder before loading data.", type = "error")
        return(NULL)
      }
      target_dir <- parseDirPath(app_roots, input$data_dir)
      paths <- list.files(target_dir, pattern = "\\.json$", full.names = TRUE, ignore.case = TRUE)
    } else if (identical(input$source_type, "upload")) {
      if (is.null(input$json_files) || !nrow(input$json_files)) {
        showNotification("Upload at least one JSON file before loading data.", type = "error")
        return(NULL)
      }
      paths <- input$json_files$datapath
    }
    if (!length(paths)) {
      showNotification("No JSON files were found in the selected source.", type = "warning")
      return(NULL)
    }
    withProgress(message = "Reading maze sessions", value = 0, {
      incProgress(0.15, detail = paste("Found", length(paths), "JSON file(s)"))
      result <- tryCatch(parse_maze_files(paths), error = function(e) {
        showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = NULL)
        NULL
      })
      incProgress(0.85, detail = "Path reconstruction and data checks complete")
      result
    })
  }, ignoreInit = TRUE)

  # Use only explicitly recognised Human-vs-AI records for the population plots.
  # Unknown strategy labels and PvP trials are not silently folded into either group.
  pvai_player_records <- reactive({
    data <- parsed_data()
    req(data)
    if (is.null(data$players) || !nrow(data$players)) return(data.frame())
    data$players %>%
      filter(matchup == "PvAI", player_type %in% c("Human", "AI"))
  })

  complete_pvai_journeys <- reactive({
    players <- pvai_player_records()
    if (!nrow(players)) return(data.frame())
    required <- c("checkpoint_goal_valid", "phase_a_completed", "phase_b_completed",
                  "full_task_steps", "optimal_full_task_steps")
    if (!all(required %in% names(players))) return(data.frame())
    players %>%
      filter(checkpoint_goal_valid %in% TRUE,
             phase_a_completed %in% TRUE,
             phase_b_completed %in% TRUE,
             is.finite(full_task_steps),
             is.finite(optimal_full_task_steps)) %>%
      mutate(excess_steps = full_task_steps - optimal_full_task_steps,
             actual_optimal_ratio = ifelse(optimal_full_task_steps > 0,
                                           full_task_steps / optimal_full_task_steps, NA_real_))
  })

  output$data_status <- renderText({
    data <- parsed_data()
    if (is.null(data)) return("No data loaded")
    visit_count <- if (nrow(data$trajectory) && "counts_as_visit" %in% names(data$trajectory)) {
      sum(data$trajectory$counts_as_visit %in% TRUE)
    } else 0L
    paste0(nrow(data$trials), " unique trial(s), ", nrow(data$players),
           " player record(s), ", visit_count, " tile visit(s)")
  })

  output$maze_selector_ui <- renderUI({
    data <- parsed_data()
    req(data, nrow(data$trials) > 0)
    trials <- data$trials
    labels <- paste0("Trial ", trials$trial_number,
                     " | Seed ", trials$maze_seed,
                     " | ", trials$maze_width, "×", trials$maze_height,
                     " | ", trials$session_timestamp)
    choices <- stats::setNames(trials$maze_key, labels)
    selectInput("selected_maze", "Select one generated maze:", choices = choices,
                selected = unname(trials$maze_key[1]))
  })

  output$summary_cards <- renderUI({
    data <- parsed_data()
    req(data)
    trials <- data$trials
    players <- data$players
    human_players <- if (nrow(players) && "player_type" %in% names(players)) {
      players %>% filter(player_type == "Human")
    } else data.frame()
    valid_objective <- if (nrow(human_players) && "checkpoint_goal_valid" %in% names(human_players)) human_players$checkpoint_goal_valid %in% TRUE else rep(FALSE, nrow(human_players))
    completion_rate <- if (any(valid_objective)) 100 * mean(human_players$phase_a_completed[valid_objective], na.rm = TRUE) else NA_real_
    completed_dev <- if (nrow(human_players) && "path_deviation" %in% names(human_players)) human_players$path_deviation[is.finite(human_players$path_deviation)] else numeric(0)
    mean_dev <- if (length(completed_dev)) mean(completed_dev) else NA_real_
    card <- function(label, value, detail = "") {
      column(3, wellPanel(h5(label), h3(value), tags$small(detail)))
    }
    fluidRow(
      card("Unique mazes", format(nrow(trials), big.mark = ","), "After duplicate-trial removal"),
      card("Player records", format(nrow(players), big.mark = ","), "One record per player per trial"),
      card("Human checkpoint completion", ifelse(is.finite(completion_rate), paste0(round(completion_rate, 1), "%"), "NA"), "Human-controlled records; valid four-checkpoint objectives only"),
      card("Mean Human Phase A deviation", ifelse(is.finite(mean_dev), sprintf("%.1f steps", mean_dev), "NA"), "Completed human checkpoint phases only")
    )
  })

  output$age_plot <- renderPlot({
    data <- parsed_data(); req(data)
    if (!nrow(data$players) || !("participant_age" %in% names(data$players))) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Age data not available") + theme_void())
    }
    age_df <- data$players %>%
      filter(is.finite(participant_age)) %>%
      distinct(participant_id, participant_age)
    if (!nrow(age_df)) return(ggplot() + annotate("text", x = 1, y = 1, label = "Age data not available") + theme_void())
    ggplot(age_df, aes(x = participant_age)) +
      geom_histogram(bins = 12, fill = "#4C78A8", colour = "white") +
      theme_minimal(base_size = 12) + labs(title = "Participant age distribution", x = "Age", y = "Participants")
  })

  output$deviation_plot <- renderPlot({
    data <- parsed_data(); req(data)
    if (!nrow(data$players) || !("path_deviation" %in% names(data$players))) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "No completed Phase A paths to compare") + theme_void())
    }
    dev <- data$players %>% filter(is.finite(path_deviation))
    if (!nrow(dev)) return(ggplot() + annotate("text", x = 1, y = 1, label = "No completed Phase A paths to compare") + theme_void())
    dev <- dev %>% mutate(player_type = ifelse(is.na(player_type), "Unknown", player_type))
    ggplot(dev, aes(x = path_deviation)) +
      geom_histogram(bins = 15, fill = "#E45756", colour = "white") +
      geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.8) +
      facet_wrap(~player_type, scales = "free_y") +
      theme_minimal(base_size = 12) +
      labs(title = "Completed Phase A path deviation by player type", x = "Actual steps − optimal four-checkpoint steps", y = "Records")
  })

  output$player_summary_table <- renderTable({
    data <- parsed_data(); req(data)
    if (!nrow(data$players)) return(data.frame(Message = "No player records are available in the loaded JSON files."))
    data$players %>%
      select(participant_id, session_timestamp, trial_number, maze_seed, matchup, player_id = agent,
             player_type, match_outcome, checkpoints_collected, checkpoints_required,
             checkpoint_goal_valid, checkpoint_sequence, phase_a_completed, phase_b_completed,
             phase_a_steps, phase_b_steps, phase_b_endpoint_extra_step, phase_b_steps_to_exit,
             total_steps, full_task_steps, optimal_phase_a_steps, observed_order_optimal_steps,
             path_deviation, full_task_deviation, response_latency_ms,
             junction_decision_count, wall_collision_count, revisit_event_count,
             rapid_input_burst_count) %>%
      arrange(session_timestamp, trial_number, player_id)
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")

  output$maze_caption <- renderUI({
    data <- parsed_data(); req(data, input$selected_maze)
    tr <- data$trials %>% filter(maze_key == input$selected_maze) %>% slice(1)
    req(nrow(tr) == 1)
    tags$div(
      h4(paste("Maze seed", tr$maze_seed, "- Trial", tr$trial_number)),
      p(paste0("Grid: ", tr$maze_width, " × ", tr$maze_height,
               " | Checkpoints: ", tr$checkpoint_count,
               " | Optimal checkpoint route: ", tr$phase_a_optimal_steps,
               " steps | Global optimum including exit: ", tr$full_task_optimal_steps,
               " steps | Wall density: ", ifelse(is.finite(tr$wall_density_pct), paste0(round(tr$wall_density_pct, 1), "%"), "NA")))
    )
  })

  make_wall_segments <- function(cells) {
    rows <- list()
    add <- function(x, y, xend, yend) {
      rows[[length(rows) + 1L]] <<- data.frame(x = x, y = y, xend = xend, yend = yend)
    }
    if (!nrow(cells)) return(data.frame())
    max_x <- max(cells$x, na.rm = TRUE)
    max_y <- max(cells$y, na.rm = TRUE)
    for (i in seq_len(nrow(cells))) {
      cell <- cells[i, ]
      # Draw each internal wall once, using the lower/left cell's North/East flag.
      if (cell$y < max_y && isTRUE(cell$wallNorth)) add(cell$x - 0.5, cell$y + 0.5, cell$x + 0.5, cell$y + 0.5)
      if (cell$x < max_x && isTRUE(cell$wallEast)) add(cell$x + 0.5, cell$y - 0.5, cell$x + 0.5, cell$y + 0.5)
    }
    # The outer maze boundary is closed even if a boundary flag is absent in an export.
    add(-0.5, -0.5, max_x + 0.5, -0.5)
    add(-0.5, max_y + 0.5, max_x + 0.5, max_y + 0.5)
    add(-0.5, -0.5, -0.5, max_y + 0.5)
    add(max_x + 0.5, -0.5, max_x + 0.5, max_y + 0.5)
    dplyr::bind_rows(rows)
  }

  output$maze_map_plot <- renderPlot({
    data <- parsed_data(); req(data, input$selected_maze)
    cells <- data$cells %>% filter(maze_key == input$selected_maze)
    if (!nrow(cells)) return(ggplot() + annotate("text", x = 1, y = 1, label = "Maze cell data unavailable") + theme_void())
    tr <- data$trials %>% filter(maze_key == input$selected_maze) %>% slice(1)
    if (!nrow(tr) || !is.finite(tr$maze_width) || !is.finite(tr$maze_height) || tr$maze_width < 1 || tr$maze_height < 1) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Maze dimensions are unavailable") + theme_void())
    }
    trajectory <- data$trajectory
    if (nrow(trajectory) && all(c("maze_key", "counts_as_visit", "agent", "phase", "x", "y", "step") %in% names(trajectory))) {
      visits <- trajectory %>% filter(maze_key == input$selected_maze, counts_as_visit)
    } else {
      visits <- data.frame(x = integer(0), y = integer(0), agent = character(0), phase = character(0), step = integer(0), counts_as_visit = logical(0))
    }
    if (input$player_filter != "ALL") visits <- visits %>% filter(agent == input$player_filter)
    if (input$phase_filter == "PHASE_A_CHECKPOINTS") visits <- visits %>% filter(phase == "PHASE_A_CHECKPOINTS")
    if (input$phase_filter == "PHASE_B_EXIT_RACE") visits <- visits %>% filter(phase == "PHASE_B_EXIT_RACE")
    visit_counts <- if (nrow(visits)) visits %>% count(x, y, name = "visits") else data.frame(x = integer(0), y = integer(0), visits = integer(0))
    grid <- cells %>% left_join(visit_counts, by = c("x", "y")) %>% mutate(visits = tidyr::replace_na(visits, 0L))
    walls <- make_wall_segments(cells)
    p <- ggplot(grid, aes(x = x, y = y)) +
      geom_tile(aes(fill = visits), colour = "#d9e0e8", linewidth = 0.15, width = 0.96, height = 0.96) +
      scale_fill_gradient(low = "#F7FBFF", high = "#D62728", name = "Tile visits") +
      coord_fixed() +
      scale_x_continuous(breaks = 0:(as.integer(tr$maze_width) - 1L), limits = c(-0.5, tr$maze_width - 0.5), expand = c(0, 0)) +
      scale_y_continuous(breaks = 0:(as.integer(tr$maze_height) - 1L), limits = c(-0.5, tr$maze_height - 0.5), expand = c(0, 0)) +
      theme_minimal(base_size = 10) +
      theme(panel.grid = element_blank(), axis.title = element_blank(), legend.position = "right")
    if (nrow(walls)) {
      p <- p + geom_segment(data = walls, aes(x = x, y = y, xend = xend, yend = yend),
                            inherit.aes = FALSE, colour = "#253247", linewidth = 0.35)
    }
    if (isTRUE(input$show_actual_paths)) {
      paths <- if (nrow(trajectory) && all(c("maze_key", "agent", "phase", "step", "x", "y") %in% names(trajectory))) {
        trajectory %>% filter(maze_key == input$selected_maze)
      } else data.frame()
      if (nrow(paths) && input$player_filter != "ALL") paths <- paths %>% filter(agent == input$player_filter)
      if (nrow(paths) && input$phase_filter == "PHASE_A_CHECKPOINTS") paths <- paths %>% filter(phase == "PHASE_A_CHECKPOINTS")
      if (nrow(paths) && input$phase_filter == "PHASE_B_EXIT_RACE") paths <- paths %>% filter(phase == "PHASE_B_EXIT_RACE")
      if (nrow(paths)) {
        p <- p + geom_path(data = paths %>% arrange(agent, phase, step),
                           aes(x = x, y = y, group = interaction(agent, phase), colour = agent),
                           inherit.aes = FALSE, linewidth = 0.75, alpha = 0.85, lineend = "round")
      }
    }
    opt <- if (nrow(data$routes) && all(c("maze_key", "route_type", "x", "y", "step") %in% names(data$routes))) {
      data$routes %>% filter(maze_key == input$selected_maze)
    } else data.frame()
    if (nrow(opt) && isTRUE(input$show_optimal_a) && nrow(opt %>% filter(route_type == "Checkpoint optimum"))) {
      p <- p + geom_path(data = opt %>% filter(route_type == "Checkpoint optimum"),
                         aes(x = x, y = y, group = route_type), inherit.aes = FALSE,
                         colour = "#00A6A6", linewidth = 1.15, linetype = "dashed")
    }
    if (nrow(opt) && isTRUE(input$show_optimal_full) && nrow(opt %>% filter(route_type == "Full task optimum"))) {
      p <- p + geom_path(data = opt %>% filter(route_type == "Full task optimum"),
                         aes(x = x, y = y, group = route_type), inherit.aes = FALSE,
                         colour = "#7B2CBF", linewidth = 1.15, linetype = "dotdash")
    }
    cp <- cells %>% filter(isCheckpoint)
    if (nrow(cp)) {
      cp$label <- paste0("C", cp$checkpointIndex + 1L)
      p <- p + geom_point(data = cp, aes(x = x, y = y), inherit.aes = FALSE,
                          shape = 21, size = 4, fill = "#FFD166", colour = "#5A4300", stroke = 0.6) +
        geom_text(data = cp, aes(x = x, y = y, label = label), inherit.aes = FALSE,
                  size = 2.5, fontface = "bold", vjust = -1.0)
    }
    if (is.finite(tr$start_x) && is.finite(tr$start_y)) {
      p <- p + geom_point(data = data.frame(x = tr$start_x, y = tr$start_y),
                          aes(x = x, y = y), inherit.aes = FALSE, shape = 21, size = 4.5,
                          fill = "#2CA25F", colour = "white", stroke = 0.7)
    }
    if (is.finite(tr$exit_x) && is.finite(tr$exit_y)) {
      p <- p + geom_point(data = data.frame(x = tr$exit_x, y = tr$exit_y),
                          aes(x = x, y = y), inherit.aes = FALSE, shape = 22, size = 4.5,
                          fill = "#343A40", colour = "white", stroke = 0.7)
    }
    p <- p + labs(title = "Tile visitation and reconstructed routes",
           subtitle = "Dashed teal = shortest checkpoint route; purple dot-dash = global optimum including exit")
    p
  })

  output$eval_model_ui <- renderUI({
    data <- parsed_data()
    choices <- c("One-sample: metric vs reference" = "one_sample")
    has_pvai <- !is.null(data) && nrow(data$players) && any(data$players$matchup == "PvAI", na.rm = TRUE)
    has_pvp <- !is.null(data) && nrow(data$players) && any(data$players$matchup == "PvP", na.rm = TRUE)
    if (has_pvai) choices <- c(choices, "Paired PvAI: Human vs AI" = "paired_1p")
    if (has_pvp) choices <- c(choices, "Paired PvP: Winner vs Loser" = "paired_2p")
    note <- if (is.null(data) || !nrow(data$players)) {
      "Load session data to enable paired PvAI/PvP comparisons."
    } else if (!has_pvp && !has_pvai) {
      "No PvAI or PvP trials were identified from the aiStrategy metadata; only the one-sample design is available."
    } else if (!has_pvp) {
      "No PvP-labelled trials were found in the loaded data; the Winner-Loser design will appear when PvP data are loaded."
    } else if (!has_pvai) {
      "No PvAI-labelled trials were found in the loaded data; the Human-AI design is unavailable for this dataset."
    } else "Paired analyses require one valid record in each comparison group within the same trial."
    tagList(
      selectInput("eval_model", "Evaluation design:", choices = choices, selected = unname(choices[1])),
      helpText(note)
    )
  })

  prepare_eval_set <- reactive({
    data <- parsed_data(); req(data, input$test_var, input$eval_model)
    df <- data$players
    if (!(input$test_var %in% names(df))) return(list(status = paste("Metric not present:", input$test_var)))
    df$.value <- suppressWarnings(as.numeric(df[[input$test_var]]))
    df <- df %>% filter(is.finite(.value))
    if (input$eval_model == "one_sample") {
      population <- input$one_sample_population %||% "Human"
      if (!identical(population, "ALL")) df <- df %>% filter(player_type == population)
      if (!nrow(df)) {
        return(list(status = paste0("No usable records for the selected one-sample population (", population,
                                    "). Select another player population or load more data.")))
      }
      target <- suppressWarnings(as.numeric(input$reference_value))
      if (length(target) != 1L || !is.finite(target)) target <- 0
      return(list(status = "OK", kind = "one_sample", values = df$.value,
                  target = target, rows = df, group1 = paste(population, "records")))
    }
    if (input$eval_model == "paired_1p") {
      df <- df %>% filter(matchup == "PvAI", player_type %in% c("Human", "AI"))
      pair_df <- pair_trial_records(df, group = "player_type", first = "Human", second = "AI")
      if (!nrow(pair_df)) return(list(status = "No complete, unique Human-AI pairs with non-missing selected metrics were found within the same trial and maze."))
      return(list(status = "OK", kind = "paired", x = pair_df$group1_value, y = pair_df$group2_value,
                  values = pair_df$difference, rows = pair_df, group1 = "Human", group2 = "AI",
                  difference_label = "Human − AI"))
    }
    if (input$eval_model == "paired_2p") {
      df <- df %>% filter(matchup == "PvP")
      pair_df <- pair_trial_records(df, group = "match_outcome", first = "Winner", second = "Loser")
      if (!nrow(pair_df)) return(list(status = "No complete, unique Winner-Loser pairs from PvP trials were found. Check the aiStrategy metadata or load PvP sessions."))
      return(list(status = "OK", kind = "paired", x = pair_df$group1_value, y = pair_df$group2_value,
                  values = pair_df$difference, rows = pair_df, group1 = "Winner", group2 = "Loser",
                  difference_label = "Winner − Loser"))
    }
    list(status = "Unsupported evaluation design.")
  })

  eval_result <- reactive({
    prepared <- prepare_eval_set()
    if (!identical(prepared$status, "OK")) return(prepared)
    transform_values <- function(x) {
      x <- as.numeric(x)
      if (isTRUE(input$signed_log)) x <- sign(x) * log1p(abs(x))
      x
    }
    if (prepared$kind == "one_sample") {
      x <- transform_values(prepared$values)
      y <- NULL
      target <- if (isTRUE(input$signed_log)) sign(prepared$target) * log1p(abs(prepared$target)) else prepared$target
      values <- x
    } else {
      x <- prepared$x
      y <- prepared$y
      target <- 0
      values <- transform_values(prepared$values)
    }
    standardisation <- NULL
    if (isTRUE(input$z_score)) {
      # Standardise the actual analysis vector: observed metric for one-sample
      # tests, or within-trial differences for paired tests.
      pool <- values
      center <- mean(pool, na.rm = TRUE)
      spread <- stats::sd(pool, na.rm = TRUE)
      if (is.finite(spread) && spread > 0) {
        values <- (values - center) / spread
        target <- (target - center) / spread
        if (prepared$kind == "one_sample") x <- (x - center) / spread
        standardisation <- list(center = center, sd = spread)
      } else {
        standardisation <- list(center = center, sd = spread, skipped = TRUE)
      }
    }
    residuals <- if (prepared$kind == "one_sample") values - target else values
    list(status = "OK", kind = prepared$kind, values = values, residuals = residuals,
         x = x, y = if (prepared$kind == "paired") y else NULL, target = target,
         rows = prepared$rows, group1 = prepared$group1,
         group2 = prepared$group2 %||% NULL, difference_label = prepared$difference_label %||% NULL,
         standardisation = standardisation, n = length(residuals))
  })

  safe_shapiro <- function(x) {
    x <- x[is.finite(x)]
    if (length(x) < 3L) return(list(statistic = NA_real_, p.value = NA_real_, note = "Shapiro-Wilk requires at least 3 observations."))
    if (length(unique(x)) < 2L || !is.finite(stats::sd(x)) || stats::sd(x) == 0) {
      return(list(statistic = NA_real_, p.value = NA_real_, note = "Normality test is undefined for a constant sample."))
    }
    sampled <- FALSE
    if (length(x) > 5000L) {
      keep <- unique(as.integer(round(seq(1, length(x), length.out = 5000L))))
      x <- x[keep]
      sampled <- TRUE
    }
    result <- tryCatch(stats::shapiro.test(x), error = function(e) NULL)
    if (is.null(result)) return(list(statistic = NA_real_, p.value = NA_real_, note = "Shapiro-Wilk test could not be computed."))
    list(statistic = unname(result$statistic), p.value = result$p.value,
         note = if (sampled) "Test calculated on a reproducible sample of 5,000." else "")
  }

  output$pooled_heatmap_plot <- renderPlot({
    data <- parsed_data(); req(data)
    pvai_players <- pvai_player_records()
    if (!nrow(pvai_players)) {
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = "No explicitly classified Human-vs-AI records are available") + theme_void())
    }
    if (is.null(data$trajectory) || !nrow(data$trajectory)) {
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = "No reconstructed tile visits are available") + theme_void())
    }

    eligible_trials <- unique(pvai_players$trial_key)
    visits <- data$trajectory %>%
      filter(trial_key %in% eligible_trials,
             player_type %in% c("Human", "AI"),
             counts_as_visit %in% TRUE,
             is.finite(x), is.finite(y)) %>%
      mutate(x = as.integer(x), y = as.integer(y),
             player_type = factor(player_type, levels = c("Human", "AI")))
    if (!nrow(visits)) {
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = "No valid Human/AI tile-arrival records are available") + theme_void())
    }

    # Match the companion analysis: coordinate counts are divided by all
    # recorded arrivals within each player type, then expressed per 1,000.
    totals <- visits %>% count(player_type, name = "total_visits")
    coordinate_counts <- visits %>%
      count(player_type, x, y, name = "coordinate_visits") %>%
      left_join(totals, by = "player_type") %>%
      mutate(visits_per_1000 = 1000 * coordinate_visits / total_visits)

    max_x <- if (nrow(data$cells)) max(data$cells$x, na.rm = TRUE) else max(visits$x, na.rm = TRUE)
    max_y <- if (nrow(data$cells)) max(data$cells$y, na.rm = TRUE) else max(visits$y, na.rm = TRUE)
    if (!is.finite(max_x) || !is.finite(max_y)) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = "Maze coordinate bounds are unavailable") + theme_void())
    }
    grid <- tidyr::expand_grid(
      player_type = factor(c("Human", "AI"), levels = c("Human", "AI")),
      x = seq.int(0L, as.integer(max_x)),
      y = seq.int(0L, as.integer(max_y))
    ) %>%
      left_join(coordinate_counts %>% select(player_type, x, y, visits_per_1000),
                by = c("player_type", "x", "y")) %>%
      mutate(visits_per_1000 = tidyr::replace_na(visits_per_1000, 0))

    p <- ggplot(grid, aes(x = x, y = y)) +
      geom_tile(aes(fill = visits_per_1000), colour = "white", linewidth = 0.12) +
      facet_wrap(~player_type, nrow = 1) +
      scale_fill_gradient(low = "#FFF7BC", high = "#B10026", name = "Visits per\n1,000 arrivals") +
      scale_x_continuous(breaks = seq(0, as.integer(max_x), by = 5),
                         limits = c(-0.5, max_x + 0.5), expand = c(0, 0)) +
      scale_y_continuous(breaks = seq(0, as.integer(max_y), by = 5),
                         limits = c(-0.5, max_y + 0.5), expand = c(0, 0)) +
      coord_fixed() +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank(),
            strip.text = element_text(face = "bold", size = 13),
            axis.title = element_text(face = "bold"),
            legend.position = "right") +
      labs(title = "Pooled coordinate occupancy across recognised PvAI mazes",
           subtitle = paste0(length(unique(visits$trial_key)), " PvAI mazes · ",
                             format(nrow(visits), big.mark = ","), " tile arrivals pooled across both groups"),
           x = "Maze x coordinate", y = "Maze y coordinate")

    # Mark the start only when every included maze uses the same start coordinate.
    maze_keys <- unique(pvai_players$maze_key)
    starts <- data$trials %>%
      filter(maze_key %in% maze_keys, is.finite(start_x), is.finite(start_y)) %>%
      distinct(start_x, start_y)
    if (nrow(starts) == 1L) {
      p <- p + geom_point(data = starts, aes(x = start_x, y = start_y),
                           inherit.aes = FALSE, shape = 8, size = 4.2,
                           colour = "#17283F", stroke = 1.1)
    }
    p
  })

  output$actual_optimal_plot <- renderPlot({
    completed <- complete_pvai_journeys()
    if (!nrow(completed)) {
      return(ggplot() + annotate("text", x = 1, y = 1,
                                 label = "No complete Human/AI full-task journeys with a valid maze optimum") + theme_void())
    }

    movement_long <- completed %>%
      select(player_type, trial_key, full_task_steps, optimal_full_task_steps) %>%
      pivot_longer(cols = c(full_task_steps, optimal_full_task_steps),
                   names_to = "distribution", values_to = "movement_steps") %>%
      mutate(distribution = dplyr::recode(
        distribution,
        full_task_steps = "Observed full-task steps",
        optimal_full_task_steps = "Optimal full-task baseline"
      ))

    # Use common integer-step bins for both distributions and both player facets.
    movement_range <- range(movement_long$movement_steps, finite = TRUE)
    bin_width <- max(1, ceiling(diff(movement_range) / 18))
    bin_boundary <- floor(movement_range[1] / bin_width) * bin_width

    # Median markers make the separation between observed routes and their
    # maze-specific optima clear.
    median_lines <- completed %>%
      group_by(player_type) %>%
      summarise(
        `Observed full-task steps` = median(full_task_steps, na.rm = TRUE),
        `Optimal full-task baseline` = median(optimal_full_task_steps, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(`Observed full-task steps`, `Optimal full-task baseline`),
                   names_to = "distribution", values_to = "median_steps")

    ggplot(movement_long, aes(x = movement_steps)) +
      geom_histogram(
        data = movement_long %>% filter(distribution == "Observed full-task steps"),
        aes(y = after_stat(density)), binwidth = bin_width, boundary = bin_boundary,
        position = "identity", fill = "#E56B5D", colour = "#D85A4A",
        alpha = 0.34, linewidth = 0.65
      ) +
      geom_histogram(
        data = movement_long %>% filter(distribution == "Optimal full-task baseline"),
        aes(y = after_stat(density)), binwidth = bin_width, boundary = bin_boundary,
        position = "identity", fill = NA, colour = "#263B53",
        linetype = "dashed", linewidth = 1.0
      ) +
      geom_vline(data = median_lines,
                 aes(xintercept = median_steps, colour = distribution,
                     linetype = distribution),
                 inherit.aes = FALSE, linewidth = 1.0) +
      facet_wrap(~player_type, nrow = 1, scales = "fixed") +
      scale_colour_manual(values = c("Observed full-task steps" = "#D85A4A",
                                     "Optimal full-task baseline" = "#263B53")) +
      scale_linetype_manual(values = c("Observed full-task steps" = "solid",
                                       "Optimal full-task baseline" = "dashed")) +
      theme_minimal(base_size = 12) +
      theme(panel.grid.minor = element_blank(),
            strip.text = element_text(face = "bold", size = 13),
            legend.position = "bottom") +
      guides(linetype = "none",
             colour = guide_legend(title = NULL, override.aes = list(linewidth = 2.2))) +
      labs(title = "Observed complete journeys vs maze-specific optimal baselines",
           subtitle = paste0(nrow(completed), " completed player-trial records; each baseline is recomputed for that record's maze"),
           x = "Movement steps: start → four checkpoints → exit",
           y = "Density (each distribution integrates to 1)")
  })

  output$actual_optimal_summary_table <- renderTable({
    completed <- complete_pvai_journeys()
    if (!nrow(completed)) return(data.frame(Message = "No completed journeys are available for this comparison."))
    completed %>%
      group_by(player_type) %>%
      summarise(
        completed_journeys = n(),
        median_observed_steps = median(full_task_steps, na.rm = TRUE),
        median_optimal_steps = median(optimal_full_task_steps, na.rm = TRUE),
        median_excess_steps = median(excess_steps, na.rm = TRUE),
        median_observed_to_optimal_ratio = median(actual_optimal_ratio, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(factor(player_type, levels = c("Human", "AI")))
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")

  output$normality_plots <- renderPlot({
    result <- eval_result()
    if (!identical(result$status, "OK")) {
      return(ggplot() + annotate("text", x = 1, y = 1, label = result$status, size = 5) + theme_void())
    }
    x <- result$residuals[is.finite(result$residuals)]
    if (length(x) < 2L) return(ggplot() + annotate("text", x = 1, y = 1, label = "At least two observations are required for these plots.") + theme_void())
    old <- par(no.readonly = TRUE)
    on.exit(par(old), add = TRUE)
    par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
    if (length(unique(x)) < 2L || !is.finite(sd(x)) || sd(x) == 0) {
      plot.new(); title("Distribution of residuals / paired differences")
      text(0.5, 0.5, "Constant values: no distribution shape to display")
      plot.new(); title("Normal Q-Q plot")
      text(0.5, 0.5, "Normality is undefined for a constant sample")
    } else {
      hist(x, breaks = "FD", probability = TRUE, col = "#8FB9D8", border = "white",
           main = "Distribution of residuals / paired differences", xlab = if (is.null(result$difference_label)) "Observed − reference" else result$difference_label)
      lines(stats::density(x), lwd = 2, col = "#B23A48")
      abline(v = 0, lty = 2, col = "#444444")
      stats::qqnorm(x, main = "Normal Q-Q plot", pch = 19, col = "#4C78A8")
      stats::qqline(x, col = "#B23A48", lwd = 2)
    }
  })

  output$stats_results <- renderPrint({
    result <- eval_result()
    if (!identical(result$status, "OK")) {
      cat(result$status, "\n")
      return(invisible(NULL))
    }
    x <- result$residuals[is.finite(result$residuals)]
    cat("Metric:", input$test_var, "\n")
    cat("Design:", input$eval_model, "\n")
    if (result$kind == "one_sample") cat("Player population:", input$one_sample_population %||% "Human", "\n")
    cat("n:", length(x), "\n")
    if (length(x)) {
      cat("Mean difference / residual:", round(mean(x), 4), "\n")
      cat("SD:", round(stats::sd(x), 4), " | Median:", round(stats::median(x), 4), "\n")
    }
    if (!is.null(result$standardisation)) {
      if (isTRUE(result$standardisation$skipped)) cat("Z-score standardisation skipped (zero or undefined SD).\n")
      else cat("Z-score: center =", round(result$standardisation$center, 4), "; SD =", round(result$standardisation$sd, 4), "\n")
    }
    sh <- safe_shapiro(x)
    if (is.finite(sh$p.value)) {
      cat("Shapiro-Wilk W =", round(sh$statistic, 4), "; p =", signif(sh$p.value, 4), "\n")
      cat("Normality diagnostic:", if (sh$p.value > 0.05) "no evidence against normality at alpha 0.05" else "evidence against normality at alpha 0.05", "\n")
    } else cat("Shapiro-Wilk:", sh$note, "\n")
    if (length(x) < 2L) {
      cat("\nInsufficient observations for inferential testing (at least 2 are required here).\n")
      return(invisible(NULL))
    }
    use_parametric <- if (input$test_method == "parametric") TRUE else if (input$test_method == "nonparametric") FALSE else is.finite(sh$p.value) && sh$p.value > 0.05
    if (length(unique(x)) < 2L || !is.finite(sd(x)) || sd(x) == 0) {
      cat("\nNo variability is present; a test statistic is not informative.\n")
      return(invisible(NULL))
    }
    cat("\nSelected test:", if (use_parametric) "one-sample / paired t-test on residuals" else "Wilcoxon signed-rank test on residuals", "\n")
    if (!use_parametric) cat("Note: Wilcoxon signed-rank inference concerns a location shift / pseudomedian and relies on distributional assumptions; it is not a test of the arithmetic mean.\n")
    test <- tryCatch({
      if (use_parametric) stats::t.test(x, mu = 0) else stats::wilcox.test(x, mu = 0, exact = FALSE)
    }, error = function(e) e)
    if (inherits(test, "error")) {
      cat("Test could not be calculated:", conditionMessage(test), "\n")
    } else {
      print(test)
      cat("\nInterpretation direction:", if (result$kind == "paired") result$difference_label else "Observed value − reference", "\n")
      cat("Positive values indicate a larger metric in the first group / relative to the reference. For path deviation, positive values indicate more steps than the optimum.\n")
    }
  })

  output$analysis_pairs <- renderTable({
    result <- eval_result()
    req(identical(result$status, "OK"))
    if (result$kind == "one_sample") {
      out <- result$rows %>% select(participant_id, trial_number, maze_seed, agent, player_type, match_outcome, value = .value)
      return(head(out, 100))
    }
    head(result$rows, 100)
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")

  output$latency_plot <- renderPlot({
    data <- parsed_data(); req(data)
    if (is.null(data$events) || !nrow(data$events)) return(ggplot() + annotate("text", x = 1, y = 1, label = "No raw events available") + theme_void())
    latency_events <- data$events %>% filter(event_type == "JUNCTION_DECISION", is.finite(decision_latency_ms))
    latency_player <- input$latency_player_filter %||% "ALL"
    latency_phase <- input$latency_phase_filter %||% "BOTH"
    if (latency_player != "ALL") latency_events <- latency_events %>% filter(agent == latency_player)
    if (latency_phase != "BOTH") latency_events <- latency_events %>% filter(phase == latency_phase)
    if (!nrow(latency_events)) return(ggplot() + annotate("text", x = 1, y = 1, label = "No decision-latency records match the selected filters") + theme_void())
    ggplot(latency_events, aes(x = decision_latency_ms, fill = agent)) +
      geom_histogram(bins = 25, alpha = 0.60, position = "identity") +
      facet_wrap(~phase, scales = "free_y") +
      theme_minimal(base_size = 12) +
      labs(title = "Junction decision latency by phase and player", x = "Decision latency (ms)", y = "Events", fill = "Player")
  })

  output$latency_summary_table <- renderTable({
    data <- parsed_data(); req(data)
    if (is.null(data$events) || !nrow(data$events)) return(data.frame(Message = "No event stream available"))
    latency_events <- data$events %>%
      filter(event_type == "JUNCTION_DECISION", is.finite(decision_latency_ms))
    latency_player <- input$latency_player_filter %||% "ALL"
    latency_phase <- input$latency_phase_filter %||% "BOTH"
    if (latency_player != "ALL") latency_events <- latency_events %>% filter(agent == latency_player)
    if (latency_phase != "BOTH") latency_events <- latency_events %>% filter(phase == latency_phase)
    if (!nrow(latency_events)) return(data.frame(Message = "No decision-latency records match the selected filters"))
    latency_events %>% group_by(trial_key, agent, phase) %>%
      summarise(n = n(), mean_ms = mean(decision_latency_ms), median_ms = median(decision_latency_ms),
                IQR_ms = IQR(decision_latency_ms), p90_ms = as.numeric(quantile(decision_latency_ms, 0.90)),
                .groups = "drop") %>% arrange(trial_key, agent, phase)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$event_summary_table <- renderTable({
    data <- parsed_data(); req(data)
    if (is.null(data$events) || !nrow(data$events)) return(data.frame(Message = "No event stream available"))
    data$events %>% filter(agent %in% c("PLAYER_1", "PLAYER_2")) %>%
      count(trial_key, agent, event_type, name = "events") %>%
      arrange(trial_key, agent, desc(events)) %>% head(100)
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$quality_table <- renderTable({
    data <- parsed_data(); req(data)
    if (!nrow(data$quality)) return(data.frame(Status = "No data quality issues were detected by the configured checks."))
    data$quality %>% arrange(factor(severity, levels = c("Error", "Warning", "Info")), file, trial) %>% head(250)
  }, striped = TRUE, bordered = TRUE, spacing = "s", na = "—")

  output$download_players <- downloadHandler(
    filename = function() paste0("maze_player_metrics_", Sys.Date(), ".csv"),
    content = function(file) {
      data <- parsed_data(); req(data)
      utils::write.csv(data$players, file, row.names = FALSE, na = "")
    }
  )
  output$download_visits <- downloadHandler(
    filename = function() paste0("maze_tile_visits_", Sys.Date(), ".csv"),
    content = function(file) {
      data <- parsed_data(); req(data)
      utils::write.csv(data$trajectory, file, row.names = FALSE, na = "")
    }
  )
  output$download_quality <- downloadHandler(
    filename = function() paste0("maze_data_quality_", Sys.Date(), ".csv"),
    content = function(file) {
      data <- parsed_data(); req(data)
      utils::write.csv(data$quality, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)
