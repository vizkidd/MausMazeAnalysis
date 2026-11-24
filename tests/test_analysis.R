# Regression checks for the maze parser and exact wall-aware route solver.
# Run from the project directory with: Rscript tests/test_analysis.R

project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
source(file.path(project_dir, "R", "maze_helpers.R"), local = TRUE)

example_files <- list.files(file.path(project_dir, "example_data"),
                            pattern = "\\.json$", full.names = TRUE)
stopifnot(length(example_files) >= 2L)
parsed <- parse_maze_files(example_files)

# Two exports overlap on Trial 1; the same participant/session/trial/seed is
# counted once, not twice.
stopifnot(nrow(parsed$trials) == 2L)
stopifnot(nrow(parsed$players) == 4L)
stopifnot(any(parsed$quality$code == "duplicate_trial_skipped"))

trial_order <- order(parsed$trials$trial_number)
trials <- parsed$trials[trial_order, , drop = FALSE]
stopifnot(identical(as.numeric(trials$phase_a_optimal_steps), c(63, 49)))
stopifnot(identical(as.numeric(trials$full_task_optimal_steps), c(86, 75)))
stopifnot(all(trials$wall_mismatches == 0))
stopifnot(all(trials$boundary_wall_violations == 0))
stopifnot(!any(parsed$quality$code %in% c("move_coordinate_outside_maze", "exit_revealed_coordinate_invalid", "player_event_stream_missing")))
stopifnot(sum(parsed$quality$code == "junction_decision_count_mismatch") == 2L)
stopifnot(!any(parsed$quality$code %in% c("wall_collision_count_mismatch", "revisit_count_mismatch", "rci_burst_count_mismatch", "junction_latency_summary_mismatch")))

# Movement-event counts reconstruct the stored per-phase movement measures.
for (i in seq_len(nrow(parsed$players))) {
  p <- parsed$players[i, ]
  ev <- parsed$events[parsed$events$trial_key == p$trial_key &
                        parsed$events$agent == p$agent &
                        parsed$events$event_type == "MOVE", , drop = FALSE]
  a <- sum(ev$phase == "PHASE_A_CHECKPOINTS")
  b <- sum(ev$phase == "PHASE_B_EXIT_RACE")
  stopifnot(a == p$phase_a_steps, b == p$phase_b_steps)
}
stopifnot(!any(parsed$quality$code %in% c("phase_a_move_count_mismatch", "phase_b_move_count_mismatch")))

# Player-level event metrics are derived from that player's own trial event stream.
for (i in seq_len(nrow(parsed$players))) {
  p <- parsed$players[i, ]
  ev <- parsed$events[parsed$events$trial_key == p$trial_key & parsed$events$agent == p$agent, , drop = FALSE]
  stopifnot(p$wall_collision_count == sum(ev$event_type == "WALL_COLLISION"))
  stopifnot(p$revisit_event_count == sum(ev$event_type == "REVISIT"))
  stopifnot(p$junction_decision_count == sum(ev$event_type == "JUNCTION_DECISION"))
  stopifnot(p$rapid_input_burst_count == sum(ev$event_type == "RCI_BURST"))
}

# Incomplete checkpoint collection is not assigned a full Phase A deviation.
p1_t1 <- parsed$players[parsed$players$trial_number == 1 & parsed$players$agent == "PLAYER_1", ]
p2_t1 <- parsed$players[parsed$players$trial_number == 1 & parsed$players$agent == "PLAYER_2", ]
p1_t2 <- parsed$players[parsed$players$trial_number == 2 & parsed$players$agent == "PLAYER_1", ]
p2_t2 <- parsed$players[parsed$players$trial_number == 2 & parsed$players$agent == "PLAYER_2", ]
stopifnot(p1_t1$path_deviation == 214)
stopifnot(is.na(p2_t1$path_deviation), p2_t1$checkpoints_collected == 1)
stopifnot(p1_t2$path_deviation == 94)
stopifnot(p2_t2$path_deviation == 114)
stopifnot(is.na(p2_t2$full_task_deviation))
stopifnot(p1_t1$phase_b_endpoint_extra_step == 1, p1_t1$phase_b_steps_to_exit == 50)
stopifnot(p1_t2$phase_b_endpoint_extra_step == 1, p1_t2$phase_b_steps_to_exit == 43)
stopifnot(p1_t1$full_task_steps == 327, p1_t2$full_task_steps == 186)
stopifnot(p1_t1$full_task_deviation == 241)
stopifnot(p1_t2$full_task_deviation == 111)
stopifnot(sum(parsed$quality$code == "exit_transition_missing_from_move_stream") == 2L)
stopifnot(any(parsed$quality$code == "observed_order_mismatch"))

# Visitation includes one initial tile per represented player plus every MOVE
# destination. REVISIT is metadata, not a second tile-arrival event.
expected_visits <- nrow(parsed$players) + sum(parsed$events$event_type == "MOVE")
actual_visits <- sum(parsed$trajectory$counts_as_visit)
stopifnot(actual_visits == 905L)
stopifnot(actual_visits >= expected_visits)
stopifnot(sum(parsed$trajectory$counts_as_move) == sum(parsed$events$event_type == "MOVE"))
stopifnot(all(parsed$trajectory$x >= 0 & parsed$trajectory$y >= 0))

# Simple 3x1 corridor checks 0-based coordinates, wall directions, checkpoint
# optimum, full task optimum, and a disconnected exit edge.
make_corridor <- function(close_exit_edge = FALSE, close_checkpoint_edge = FALSE) {
  cells <- list()
  for (x in 0:2) {
    cells[[x + 1L]] <- list(
      x = x, y = 0,
      wallNorth = TRUE, wallEast = if (x == 2L || (x == 1L && close_exit_edge) || (x == 0L && close_checkpoint_edge)) TRUE else FALSE,
      wallSouth = TRUE, wallWest = if (x == 0L || (x == 1L && close_checkpoint_edge) || (x == 2L && close_exit_edge)) TRUE else FALSE,
      isCheckpoint = x == 1L, checkpointIndex = if (x == 1L) 0L else -1L,
      isStart = x == 0L, isExit = x == 2L
    )
  }
  list(width = 3, height = 1, startX = 0, startY = 0,
       exitX = 2, exitY = 0, cells = cells)
}

corridor <- analyse_maze(make_corridor())
stopifnot(isTRUE(corridor$solver_ok))
stopifnot(corridor$phase_a_length == 1, corridor$full_length == 2)
stopifnot(nrow(corridor$phase_a_route) == 2L, nrow(corridor$full_route) == 3L)

disconnected_exit <- analyse_maze(make_corridor(close_exit_edge = TRUE))
stopifnot(isTRUE(disconnected_exit$solver_ok))
stopifnot(disconnected_exit$phase_a_length == 1)
stopifnot(is.na(disconnected_exit$full_length))

# Grid fingerprints ignore JSON cell ordering but change when maze geometry changes.
corridor_cells_reversed <- make_corridor()
corridor_cells_reversed$cells <- rev(corridor_cells_reversed$cells)
stopifnot(identical(maze_fingerprint(make_corridor()), maze_fingerprint(corridor_cells_reversed)))
changed_corridor <- make_corridor()
changed_corridor$cells[[1]]$wallNorth <- FALSE
stopifnot(!identical(maze_fingerprint(make_corridor()), maze_fingerprint(changed_corridor)))
bad_outer_boundary <- analyse_maze(changed_corridor)
stopifnot(!isTRUE(bad_outer_boundary$solver_ok))
stopifnot(bad_outer_boundary$boundary_wall_violations == 1L)

unreachable_checkpoint <- analyse_maze(make_corridor(close_checkpoint_edge = TRUE))
stopifnot(!isTRUE(unreachable_checkpoint$solver_ok))
stopifnot(is.na(unreachable_checkpoint$phase_a_length))


# Event segmentation must not attach an ambiguous remainder to the wrong trial.
ambiguous_events <- list(
  list(eventType = "TRIAL_START", elapsedSeconds = 0),
  list(eventType = "MOVE", elapsedSeconds = 1),
  list(eventType = "MOVE", elapsedSeconds = 2)
)
ambiguous_chunks <- split_trial_events(ambiguous_events, 2L)
stopifnot(length(ambiguous_chunks) == 2L, all(lengths(ambiguous_chunks) == 0L))
reset_events <- list(
  list(eventType = "MOVE", elapsedSeconds = 0),
  list(eventType = "MOVE", elapsedSeconds = 1),
  list(eventType = "MOVE", elapsedSeconds = 0),
  list(eventType = "MOVE", elapsedSeconds = 1)
)
reset_chunks <- split_trial_events(reset_events, 2L)
stopifnot(identical(as.integer(lengths(reset_chunks)), c(2L, 2L)))

# Paired comparisons must match exactly one observation per side within trial.
pair_data <- data.frame(
  trial_key = c("t1", "t1", "t2", "t2", "t2", "t3"),
  player_type = c("Human", "AI", "Human", "AI", "AI", "Human"),
  .value = c(10, 8, 7, 6, 5, 4)
)
paired <- pair_trial_records(pair_data, group = "player_type", first = "Human", second = "AI")
stopifnot(nrow(paired) == 1L, paired$trial_key == "t1", paired$difference == 2)

# Unknown/NA boolean representations retain the explicit conservative default.
stopifnot(isTRUE(scalar_bool(NA, TRUE)))
stopifnot(isTRUE(scalar_bool("unknown", TRUE)))
stopifnot(identical(scalar_bool("false", TRUE), FALSE))

cat("All maze project regression checks passed.\n")
