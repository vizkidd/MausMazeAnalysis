# Helpers for the Biostatistical Maze Project Shiny dashboard.
# Coordinates follow the supplied game schema: x increases East and y increases North.

`%||%` <- function(x, y) if (is.null(x)) y else x

scalar_num <- function(x, default = NA_real_) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- suppressWarnings(as.numeric(unlist(x, use.names = FALSE)[1]))
  if (length(value) == 0 || !is.finite(value)) default else value
}

scalar_chr <- function(x, default = NA_character_) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- as.character(unlist(x, use.names = FALSE)[1])
  if (is.na(value) || !nzchar(value)) default else value
}

scalar_bool <- function(x, default = FALSE) {
  if (is.null(x) || length(x) == 0) return(default)
  value <- unlist(x, use.names = FALSE)[1]
  if (length(value) == 0 || is.na(value)) return(default)
  if (is.logical(value)) return(value)
  if (is.numeric(value)) return(if (is.finite(value)) value != 0 else default)
  text <- tolower(trimws(as.character(value)))
  if (text %in% c("true", "1", "yes", "y")) return(TRUE)
  if (text %in% c("false", "0", "no", "n")) return(FALSE)
  default
}

event_field <- function(event, field) {
  if (is.list(event)) event[[field]] else NULL
}

valid_bool_scalar <- function(x) {
  if (is.null(x) || length(x) != 1L) return(FALSE)
  value <- unlist(x, use.names = FALSE)
  if (length(value) != 1L || is.na(value)) return(FALSE)
  if (is.logical(value)) return(TRUE)
  if (is.numeric(value)) return(is.finite(value) && value %in% c(0, 1))
  tolower(trimws(as.character(value))) %in% c("true", "false", "1", "0", "yes", "no", "y", "n")
}

coord_key <- function(x, y) paste(as.integer(x), as.integer(y), sep = ",")

quality_row <- function(severity, code, file, trial = NA_integer_, scope = "file", message) {
  data.frame(
    severity = as.character(severity), code = as.character(code),
    file = as.character(file), trial = as.integer(trial),
    scope = as.character(scope), message = as.character(message),
    stringsAsFactors = FALSE
  )
}

read_maze_json <- function(path) {
  tryCatch({
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
    if (!length(lines)) stop("File is empty")
    txt <- paste(lines, collapse = "\n")
    if (startsWith(txt, "\ufeff")) txt <- substring(txt, 2L)
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) structure(list(message = conditionMessage(e)), class = "maze_json_error"))
}

# Segment top-level rawEvents into trials. The supplied schema has no trialNumber
# on individual events; TRIAL_START markers define the boundaries. If marker
# counts are wrong, elapsed-time resets are accepted only when they yield an
# exact, unambiguous number of trial chunks. Otherwise events remain unmapped.
split_trial_events <- function(events, n_trials) {
  out <- vector("list", n_trials)
  if (!length(events) || n_trials < 1L) return(out)
  types <- vapply(events, function(e) scalar_chr(event_field(e, "eventType"), ""), character(1))
  starts <- which(types == "TRIAL_START")

  if (length(starts) == n_trials) {
    for (i in seq_len(n_trials)) {
      end <- if (i < length(starts)) starts[i + 1L] - 1L else length(events)
      if (starts[i] <= end) out[[i]] <- events[starts[i]:end]
    }
    return(out)
  }

  elapsed <- vapply(events, function(e) scalar_num(event_field(e, "elapsedSeconds")), numeric(1))
  valid_elapsed <- is.finite(elapsed)
  resets <- if (sum(valid_elapsed) == length(elapsed)) which(diff(elapsed) < -1e-8) + 1L else integer(0)
  boundaries <- c(1L, resets, length(events) + 1L)
  chunks <- lapply(seq_len(length(boundaries) - 1L), function(i) {
    events[boundaries[i]:(boundaries[i + 1L] - 1L)]
  })
  if (length(chunks) == n_trials) return(chunks)
  out
}

# Build complete within-trial pairs for an independent or paired comparison.
# Ambiguous groups (zero or multiple records per side) are intentionally omitted.
pair_trial_records <- function(data, trial_key = "trial_key", group = "player_type",
                               value = ".value", first = "Human", second = "AI") {
  required <- c(trial_key, group, value)
  if (!is.data.frame(data) || !all(required %in% names(data)) || !nrow(data)) return(data.frame())
  keep <- !is.na(data[[trial_key]]) & !is.na(data[[group]]) & is.finite(suppressWarnings(as.numeric(data[[value]])))
  data <- data[keep, , drop = FALSE]
  if (!nrow(data)) return(data.frame())
  groups <- split(data, as.character(data[[trial_key]]), drop = TRUE)
  pairs <- lapply(names(groups), function(key) {
    g <- groups[[key]]
    a <- suppressWarnings(as.numeric(g[[value]][g[[group]] == first]))
    b <- suppressWarnings(as.numeric(g[[value]][g[[group]] == second]))
    if (length(a) != 1L || length(b) != 1L) return(NULL)
    data.frame(trial_key = key, group1_value = a, group2_value = b,
               difference = a - b, stringsAsFactors = FALSE)
  })
  pairs <- Filter(Negate(is.null), pairs)
  if (!length(pairs)) data.frame() else dplyr::bind_rows(pairs)
}

maze_cells_frame <- function(maze) {
  if (!is.list(maze)) return(data.frame())
  raw_cells <- event_field(maze, "cells")
  if (is.null(raw_cells) || !length(raw_cells)) return(data.frame())
  rows <- lapply(raw_cells, function(cell) {
    data.frame(
      x = scalar_num(event_field(cell, "x")), y = scalar_num(event_field(cell, "y")),
      wallNorth = scalar_bool(event_field(cell, "wallNorth"), TRUE),
      wallEast = scalar_bool(event_field(cell, "wallEast"), TRUE),
      wallSouth = scalar_bool(event_field(cell, "wallSouth"), TRUE),
      wallWest = scalar_bool(event_field(cell, "wallWest"), TRUE),
      isCheckpoint = scalar_bool(event_field(cell, "isCheckpoint"), FALSE),
      checkpointIndex = scalar_num(event_field(cell, "checkpointIndex"), -1),
      isStart = scalar_bool(event_field(cell, "isStart"), FALSE),
      isExit = scalar_bool(event_field(cell, "isExit"), FALSE),
      stringsAsFactors = FALSE
    )
  })
  cells <- do.call(rbind, rows)
  cells <- cells[is.finite(cells$x) & is.finite(cells$y), , drop = FALSE]
  cells
}

# Stable fingerprint of maze geometry. Sorting cells makes the signature
# independent of JSON cell-array order, while wall/checkpoint/start/exit data
# distinguish layouts that happen to reuse a seed or trial number.
maze_fingerprint <- function(maze) {
  cells <- maze_cells_frame(maze)
  if (nrow(cells)) {
    cells <- cells[order(cells$x, cells$y), , drop = FALSE]
    fields <- c("x", "y", "wallNorth", "wallEast", "wallSouth", "wallWest",
                "isCheckpoint", "checkpointIndex", "isStart", "isExit")
    cell_tokens <- apply(cells[, fields, drop = FALSE], 1, function(row) {
      paste(ifelse(is.na(row), "NA", as.character(row)), collapse = ",")
    })
  } else {
    cell_tokens <- character(0)
  }
  metadata <- c(
    scalar_chr(maze$width, "NA"), scalar_chr(maze$height, "NA"),
    scalar_chr(maze$startX, "NA"), scalar_chr(maze$startY, "NA"),
    scalar_chr(maze$exitX, "NA"), scalar_chr(maze$exitY, "NA"),
    paste0("n=", length(event_field(maze, "cells") %||% list())),
    cell_tokens
  )
  payload <- enc2utf8(paste(metadata, collapse = "|"))
  tmp <- tempfile("maze-fingerprint-")
  con <- file(tmp, open = "wb")
  writeBin(charToRaw(payload), con)
  close(con)
  signature <- unname(tools::md5sum(tmp))
  unlink(tmp)
  as.character(signature)
}

# Validate the reciprocal wall representation. The correct y convention is
# confirmed by MOVE events: y + 1 is North, y - 1 is South.
wall_consistency <- function(cells) {
  if (!nrow(cells)) return(list(count = NA_integer_, examples = character(0)))
  index <- setNames(seq_len(nrow(cells)), coord_key(cells$x, cells$y))
  mismatches <- character(0)
  for (i in seq_len(nrow(cells))) {
    p <- cells[i, ]
    for (d in list(
      list(dx = 0L, dy = 1L, a = "wallNorth", b = "wallSouth"),
      list(dx = 1L, dy = 0L, a = "wallEast", b = "wallWest")
    )) {
      k <- coord_key(p$x + d$dx, p$y + d$dy)
      j <- unname(index[k])
      if (length(j) && !is.na(j) && isTRUE(p[[d$a]] != cells[[d$b]][j])) {
        mismatches <- c(mismatches, paste0("(", p$x, ",", p$y, ") / (", cells$x[j], ",", cells$y[j], ")"))
      }
    }
  }
  list(count = length(mismatches), examples = head(mismatches, 5))
}

# Breadth-first search on traversable cells. An edge is open only when both
# corresponding wall flags are FALSE; inconsistent edges are handled conservatively.
maze_bfs <- function(cells, start) {
  n <- nrow(cells)
  if (!n || length(start) != 2L || any(!is.finite(start))) {
    return(list(distance = numeric(0), parent = integer(0), cells = cells, index = character(0)))
  }
  keys <- coord_key(cells$x, cells$y)
  index <- setNames(seq_len(n), keys)
  start_key <- coord_key(start[1], start[2])
  start_idx <- unname(index[start_key])
  if (!length(start_idx) || is.na(start_idx)) {
    return(list(distance = rep(Inf, n), parent = rep(NA_integer_, n), cells = cells, index = index))
  }

  directions <- list(
    list(dx = 0L, dy = 1L, wall = "wallNorth", opposite = "wallSouth"),
    list(dx = 1L, dy = 0L, wall = "wallEast", opposite = "wallWest"),
    list(dx = 0L, dy = -1L, wall = "wallSouth", opposite = "wallNorth"),
    list(dx = -1L, dy = 0L, wall = "wallWest", opposite = "wallEast")
  )
  distance <- rep(Inf, n)
  parent <- rep(NA_integer_, n)
  queue <- integer(n)
  head <- 1L; tail <- 1L
  queue[tail] <- start_idx
  distance[start_idx] <- 0

  while (head <= tail) {
    current <- queue[head]
    head <- head + 1L
    cell <- cells[current, , drop = FALSE]
    for (d in directions) {
      next_key <- coord_key(cell$x + d$dx, cell$y + d$dy)
      next_idx <- unname(index[next_key])
      if (!length(next_idx) || is.na(next_idx) || is.finite(distance[next_idx])) next
      open_from <- !isTRUE(cell[[d$wall]])
      open_to <- !isTRUE(cells[[d$opposite]][next_idx])
      if (open_from && open_to) {
        distance[next_idx] <- distance[current] + 1
        parent[next_idx] <- current
        tail <- tail + 1L
        queue[tail] <- next_idx
      }
    }
  }
  names(distance) <- keys
  list(distance = distance, parent = parent, cells = cells, index = index, start_idx = start_idx)
}

bfs_path_to <- function(bfs, target) {
  if (is.null(bfs$start_idx) || length(target) != 2L) return(data.frame(x = integer(0), y = integer(0)))
  target_key <- coord_key(target[1], target[2])
  target_idx <- unname(bfs$index[target_key])
  if (!length(target_idx) || is.na(target_idx) || !is.finite(bfs$distance[target_idx])) {
    return(data.frame(x = integer(0), y = integer(0)))
  }
  ids <- target_idx
  while (ids[1] != bfs$start_idx) {
    prev <- bfs$parent[ids[1]]
    if (is.na(prev)) return(data.frame(x = integer(0), y = integer(0)))
    ids <- c(prev, ids)
  }
  data.frame(x = bfs$cells$x[ids], y = bfs$cells$y[ids])
}

all_permutations <- function(x) {
  if (!length(x)) return(list(integer(0)))
  out <- list()
  for (i in seq_along(x)) {
    rest <- x[-i]
    tails <- all_permutations(rest)
    for (tail in tails) out[[length(out) + 1L]] <- c(x[i], tail)
  }
  out
}

analyse_maze <- function(maze) {
  if (!is.list(maze)) maze <- list()
  cells <- maze_cells_frame(maze)
  width <- scalar_num(maze$width)
  height <- scalar_num(maze$height)
  start <- c(scalar_num(maze$startX), scalar_num(maze$startY))
  exit <- c(scalar_num(maze$exitX), scalar_num(maze$exitY))
  result <- list(
    cells = cells, width = width, height = height, start = start, exit = exit,
    checkpoints = data.frame(), phase_a_length = NA_real_, full_length = NA_real_,
    phase_a_route = data.frame(), full_route = data.frame(),
    wall_mismatches = NA_integer_, boundary_wall_violations = NA_integer_,
    missing_wall_cells = NA_integer_, wall_density_pct = NA_real_, total_cells = NA_integer_, solver_ok = FALSE, solver_message = "Maze metadata is incomplete.",
    distance_matrix = matrix(numeric(0), 0, 0), node_coords = matrix(numeric(0), 0, 2)
  )
  if (!nrow(cells) || !is.finite(width) || !is.finite(height) || any(!is.finite(c(start, exit)))) return(result)
  if (width < 1 || height < 1 || width != floor(width) || height != floor(height)) {
    result$solver_message <- "Maze width and height must be positive whole numbers."
    return(result)
  }
  if (any(cells$x != floor(cells$x) | cells$y != floor(cells$y))) {
    result$solver_message <- "Cell coordinates must be whole numbers."
    return(result)
  }
  if (any(cells$checkpointIndex[cells$isCheckpoint] != floor(cells$checkpointIndex[cells$isCheckpoint]))) {
    result$solver_message <- "Checkpoint indices must be whole numbers."
    return(result)
  }
  if (any(c(start, exit) != floor(c(start, exit)))) {
    result$solver_message <- "Start and exit coordinates must be whole numbers."
    return(result)
  }
  cells$x <- as.integer(cells$x)
  cells$y <- as.integer(cells$y)
  cells$checkpointIndex <- as.integer(cells$checkpointIndex)
  result$cells <- cells
  wall_fields <- c("wallNorth", "wallEast", "wallSouth", "wallWest")
  raw_cells <- event_field(maze, "cells")
  missing_wall_cells <- sum(vapply(raw_cells, function(cell) {
    !is.list(cell) || any(!vapply(wall_fields, function(field) valid_bool_scalar(event_field(cell, field)), logical(1)))
  }, logical(1)))
  result$missing_wall_cells <- missing_wall_cells
  if (missing_wall_cells > 0L) {
    result$solver_message <- paste0("Wall flags are missing or invalid for ", missing_wall_cells, " cell(s); exact pathfinding is unavailable.")
    return(result)
  }
  if (anyDuplicated(coord_key(cells$x, cells$y))) {
    result$solver_message <- "Duplicate cell coordinates prevent reliable pathfinding."
    return(result)
  }
  if (any(cells$x < 0 | cells$x >= width | cells$y < 0 | cells$y >= height)) {
    result$solver_message <- "Cell coordinates fall outside the declared maze dimensions."
    return(result)
  }
  expected_n <- as.integer(width * height)
  if (nrow(cells) != expected_n) {
    result$solver_message <- paste0("Grid has ", nrow(cells), " cells; expected ", expected_n, ".")
    return(result)
  }
  if (!all(coord_key(0:(as.integer(width) - 1L), rep(0, as.integer(width))) %in% coord_key(cells$x, cells$y))) {
    # This quick check is only a guard; the count and bounds checks above are primary.
    result$solver_message <- "Grid coordinates are incomplete."
    return(result)
  }
  boundary_violations <- sum(!cells$wallWest[cells$x == 0]) +
    sum(!cells$wallEast[cells$x == width - 1]) +
    sum(!cells$wallSouth[cells$y == 0]) +
    sum(!cells$wallNorth[cells$y == height - 1])
  result$boundary_wall_violations <- as.integer(boundary_violations)
  if (boundary_violations > 0L) {
    result$solver_message <- paste0("Maze has ", boundary_violations,
                                    " open wall flag(s) on the outer boundary; exact pathfinding is disabled.")
    return(result)
  }
  index <- setNames(seq_len(nrow(cells)), coord_key(cells$x, cells$y))
  if (!(coord_key(start[1], start[2]) %in% names(index)) || !(coord_key(exit[1], exit[2]) %in% names(index))) {
    result$solver_message <- "Start or exit coordinate is absent from the grid."
    return(result)
  }
  cp <- cells[cells$isCheckpoint, c("x", "y", "checkpointIndex"), drop = FALSE]
  cp <- cp[order(cp$checkpointIndex, cp$x, cp$y), , drop = FALSE]
  if (nrow(cp) > 8L) {
    result$solver_message <- "More than eight checkpoints were found; exact route enumeration is disabled to avoid factorial runtime."
    return(result)
  }
  if (anyDuplicated(cp$checkpointIndex) || anyDuplicated(coord_key(cp$x, cp$y))) {
    result$solver_message <- "Checkpoint indices or coordinates are duplicated."
    return(result)
  }
  result$checkpoints <- cp
  consistency <- wall_consistency(cells)
  result$wall_mismatches <- consistency$count
  result$total_cells <- nrow(cells)
  internal_edges <- max(0, (width - 1) * height + (height - 1) * width)
  closed_edges <- sum(cells$wallEast[cells$x < width - 1], na.rm = TRUE) +
    sum(cells$wallNorth[cells$y < height - 1], na.rm = TRUE)
  result$wall_density_pct <- if (internal_edges > 0) 100 * closed_edges / internal_edges else NA_real_
  if (consistency$count > 0) {
    result$solver_message <- paste0("Maze contains ", consistency$count, " non-reciprocal wall edges; path lengths are conservatively treated as unavailable.")
    return(result)
  }

  node_coords <- rbind(matrix(start, nrow = 1), as.matrix(cp[, c("x", "y"), drop = FALSE]), matrix(exit, nrow = 1))
  rownames(node_coords) <- c("START", if (nrow(cp)) paste0("CP", cp$checkpointIndex) else character(0), "EXIT")
  bfs_results <- lapply(seq_len(nrow(node_coords)), function(i) maze_bfs(cells, node_coords[i, ]))
  dist_mat <- matrix(Inf, nrow(node_coords), nrow(node_coords), dimnames = list(rownames(node_coords), rownames(node_coords)))
  for (i in seq_len(nrow(node_coords))) {
    for (j in seq_len(nrow(node_coords))) {
      dist_mat[i, j] <- bfs_results[[i]]$distance[coord_key(node_coords[j, 1], node_coords[j, 2])]
    }
  }
  result$distance_matrix <- dist_mat
  result$node_coords <- node_coords

  cp_ids <- if (nrow(cp)) 2:(nrow(cp) + 1L) else integer(0)
  exit_id <- nrow(node_coords)
  perms <- all_permutations(seq_along(cp_ids))

  best_a <- Inf; best_a_route <- integer(0)
  best_full <- Inf; best_full_route <- integer(0)
  for (perm in perms) {
    route_a <- c(1L, cp_ids[perm])
    d_a <- if (length(route_a) > 1L) sum(vapply(seq_len(length(route_a) - 1L), function(i) dist_mat[route_a[i], route_a[i + 1L]], numeric(1))) else 0
    if (is.finite(d_a) && d_a < best_a) { best_a <- d_a; best_a_route <- route_a }
    route_full <- c(route_a, exit_id)
    d_full <- if (length(route_full) > 1L) sum(vapply(seq_len(length(route_full) - 1L), function(i) dist_mat[route_full[i], route_full[i + 1L]], numeric(1))) else 0
    if (is.finite(d_full) && d_full < best_full) { best_full <- d_full; best_full_route <- route_full }
  }

  stitch_route <- function(route_ids) {
    if (!length(route_ids)) return(data.frame(x = integer(0), y = integer(0), step = integer(0)))
    path <- data.frame(x = node_coords[route_ids[1], 1], y = node_coords[route_ids[1], 2])
    if (length(route_ids) > 1L) {
      for (i in seq_len(length(route_ids) - 1L)) {
        segment <- bfs_path_to(bfs_results[[route_ids[i]]], node_coords[route_ids[i + 1L], ])
        if (!nrow(segment)) return(data.frame(x = integer(0), y = integer(0), step = integer(0)))
        path <- rbind(path, segment[-1, , drop = FALSE])
      }
    }
    path$step <- seq_len(nrow(path)) - 1L
    path
  }
  if (is.finite(best_a)) {
    result$phase_a_length <- best_a
    result$phase_a_route <- stitch_route(best_a_route)
  }
  if (is.finite(best_full)) {
    result$full_length <- best_full
    result$full_route <- stitch_route(best_full_route)
  }
  result$solver_ok <- is.finite(result$phase_a_length) && (nrow(cp) == 0L || nrow(result$phase_a_route) > 0L)
  result$solver_message <- if (result$solver_ok) "OK" else "At least one required route is unreachable in this maze."
  result
}

route_distance_for_sequence <- function(maze_result, checkpoint_indices) {
  if (!isTRUE(maze_result$solver_ok)) return(NA_real_)
  cp <- maze_result$checkpoints
  if (!length(checkpoint_indices)) return(if (nrow(cp) == 0L) 0 else NA_real_)
  checkpoint_indices <- suppressWarnings(as.integer(checkpoint_indices))
  if (length(checkpoint_indices) != nrow(cp) || anyNA(checkpoint_indices) ||
      anyDuplicated(checkpoint_indices) || !setequal(checkpoint_indices, cp$checkpointIndex)) return(NA_real_)
  cp_rows <- match(checkpoint_indices, cp$checkpointIndex)
  if (anyNA(cp_rows)) return(NA_real_)
  ids <- c(1L, cp_rows + 1L)
  dm <- maze_result$distance_matrix
  if (length(ids) < 2L) return(0)
  sum(vapply(seq_len(length(ids) - 1L), function(i) dm[ids[i], ids[i + 1L]], numeric(1)))
}

shortest_distance_between <- function(cells, from, to) {
  bfs <- maze_bfs(cells, from)
  if (is.null(bfs$index) || !length(bfs$index)) return(NA_real_)
  value <- bfs$distance[coord_key(to[1], to[2])]
  if (!length(value) || !is.finite(value)) NA_real_ else as.numeric(value)
}

maze_step_is_open <- function(cells, from, to) {
  if (!nrow(cells) || length(from) != 2L || length(to) != 2L ||
      any(!is.finite(c(from, to)))) return(FALSE)
  dx <- as.integer(to[1] - from[1])
  dy <- as.integer(to[2] - from[2])
  if (abs(dx) + abs(dy) != 1L) return(FALSE)
  keys <- coord_key(cells$x, cells$y)
  i <- match(coord_key(from[1], from[2]), keys)
  j <- match(coord_key(to[1], to[2]), keys)
  if (is.na(i) || is.na(j)) return(FALSE)
  direction <- if (dx == 1L) c("wallEast", "wallWest") else if (dx == -1L) c("wallWest", "wallEast") else if (dy == 1L) c("wallNorth", "wallSouth") else c("wallSouth", "wallNorth")
  !isTRUE(cells[[direction[1]]][i]) && !isTRUE(cells[[direction[2]]][j])
}

classify_matchup <- function(ai_strategy, player_slot) {
  s <- tolower(ifelse(is.na(ai_strategy), "", ai_strategy))
  slot <- as.integer(player_slot)
  if (grepl("human\\s*player\\s*1", s)) {
    return(list(matchup = "PvAI", player_type = if (slot == 1L) "Human" else "AI"))
  }
  if (grepl("human\\s*player\\s*2", s)) {
    return(list(matchup = "PvAI", player_type = if (slot == 2L) "Human" else "AI"))
  }
  if (grepl("pvp|player\\s*vs\\s*player|human\\s*vs\\s*human", s)) {
    return(list(matchup = "PvP", player_type = "Human"))
  }
  if (grepl("human", s) && grepl("ai|computer", s)) {
    return(list(matchup = "PvAI", player_type = NA_character_))
  }
  list(matchup = "Unknown", player_type = NA_character_)
}

flatten_player_fields <- function(player) {
  out <- list()
  if (is.null(player) || !is.list(player)) return(out)
  for (nm in names(player)) {
    value <- player[[nm]]
    if (is.null(value)) {
      out[[nm]] <- NA
    } else if (length(value) > 1L || is.list(value)) {
      out[[nm]] <- paste(unlist(value, use.names = FALSE), collapse = ",")
    } else {
      out[[nm]] <- unlist(value, use.names = FALSE)[1]
    }
  }
  out
}

parse_maze_files <- function(paths) {
  if (is.null(paths) || !length(paths)) stop("No JSON files were supplied.")
  all_trials <- list(); all_players <- list(); all_cells <- list()
  all_trajectory <- list(); all_events <- list(); all_routes <- list(); all_quality <- list()
  seen_keys <- character(0)

  add_quality <- function(...) all_quality[[length(all_quality) + 1L]] <<- quality_row(...)

  for (path in paths) {
    file_label <- basename(path)
    doc <- read_maze_json(path)
    if (inherits(doc, "maze_json_error")) {
      add_quality("Error", "invalid_json", file_label, NA_integer_, "file", doc$message)
      next
    }
    if (!is.list(doc)) {
      add_quality("Error", "invalid_json_root", file_label, NA_integer_, "file",
                  "JSON root must be an object containing participant/session metadata and trials.")
      next
    }
    participant_id <- scalar_chr(doc$participantId, tools::file_path_sans_ext(file_label))
    participant_age <- scalar_num(doc$participantAge)
    session_timestamp <- scalar_chr(doc$sessionTimestamp, file_label)
    session_id <- paste(participant_id, session_timestamp, sep = "|")
    trials <- doc$trials
    if (is.null(trials) || !length(trials)) {
      if (!is.null(doc$mazeGrid)) {
        trials <- list(list(trialNumber = 1, mazeGrid = doc$mazeGrid))
        add_quality("Warning", "legacy_top_level_maze", file_label, 1L, "file", "No trials array was found; parsed the legacy top-level mazeGrid as one trial.")
      } else {
        add_quality("Error", "missing_trials", file_label, NA_integer_, "file", "JSON contains neither a trials array nor a top-level mazeGrid.")
        next
      }
    }
    raw_events <- doc$rawEvents %||% list()
    if (!is.list(raw_events)) raw_events <- list()
    invalid_events <- sum(!vapply(raw_events, is.list, logical(1)))
    if (invalid_events > 0L) {
      add_quality("Warning", "invalid_event_record", file_label, NA_integer_, "events",
                  paste0("Skipped ", invalid_events, " raw event record(s) that were not JSON objects."))
      raw_events <- Filter(is.list, raw_events)
    }
    chunks <- split_trial_events(raw_events, length(trials))
    starts_count <- sum(vapply(raw_events, function(e) identical(scalar_chr(event_field(e, "eventType"), ""), "TRIAL_START"), logical(1)))
    if (length(raw_events) && starts_count != length(trials)) {
      add_quality("Warning", "event_trial_marker_count", file_label, NA_integer_, "file",
                  paste0("Found ", starts_count, " TRIAL_START markers for ", length(trials), " trial records. Event chunks were assigned only if elapsed-time resets yielded an exact trial count; otherwise raw events were left unmapped."))
    }
    if (length(raw_events) && any(lengths(chunks) == 0L)) {
      add_quality("Warning", "event_trial_chunk_assignment_failed", file_label, NA_integer_, "file",
                  paste0(sum(lengths(chunks) == 0L), " trial event chunk(s) could not be assigned unambiguously; spatial and event-derived measures for those trials may be unavailable."))
    }

    for (i in seq_along(trials)) {
      trial <- trials[[i]]
      if (!is.list(trial)) {
        add_quality("Error", "invalid_trial_record", file_label, i, "trial", "Trial entry is not a JSON object and was skipped.")
        next
      }
      trial_number <- as.integer(scalar_num(trial$trialNumber, i))
      maze_seed <- scalar_chr(trial$mazeSeed, "NA")
      maze <- trial$mazeGrid %||% if (length(trials) == 1L) doc$mazeGrid else NULL
      if (is.null(maze)) {
        add_quality("Error", "missing_trial_maze", file_label, trial_number, "trial", "Trial has no mazeGrid; pathfinding and spatial analysis are unavailable.")
        maze <- list()
      }
      maze_signature <- maze_fingerprint(maze)
      trial_key <- paste(session_id, trial_number, maze_seed, maze_signature, sep = "|")
      if (trial_key %in% seen_keys) {
        add_quality("Info", "duplicate_trial_skipped", file_label, trial_number, "trial",
                    "This session/trial/seed/maze-geometry combination was already loaded from another JSON export; duplicate row and events were skipped.")
        next
      }
      seen_keys <- c(seen_keys, trial_key)

      maze_result <- analyse_maze(maze)
      if (!isTRUE(maze_result$solver_ok)) {
        add_quality("Error", "path_solver_failed", file_label, trial_number, "maze", maze_result$solver_message)
      }
      if (!is.na(maze_result$missing_wall_cells) && maze_result$missing_wall_cells > 0) {
        add_quality("Error", "missing_wall_flags", file_label, trial_number, "maze",
                    paste0("Wall flags are missing or invalid for ", maze_result$missing_wall_cells, " cell(s); exact route calculations were disabled."))
      }
      if (!is.na(maze_result$wall_mismatches) && maze_result$wall_mismatches > 0) {
        add_quality("Warning", "nonreciprocal_walls", file_label, trial_number, "maze",
                    paste0("Found ", maze_result$wall_mismatches, " inconsistent shared wall edges."))
      }
      if (!is.na(maze_result$boundary_wall_violations) && maze_result$boundary_wall_violations > 0) {
        add_quality("Error", "open_outer_boundary", file_label, trial_number, "maze",
                    paste0("Found ", maze_result$boundary_wall_violations,
                           " outer-boundary wall flags marked open; route calculations were disabled."))
      }
      if (isTRUE(maze_result$solver_ok) && !is.finite(maze_result$full_length)) {
        add_quality("Warning", "full_task_exit_unreachable", file_label, trial_number, "maze",
                    "The start and all checkpoints are mutually reachable, but the maze exit is not reachable after checkpoint collection.")
      }
      if (nrow(maze_result$checkpoints) == 4L && !setequal(maze_result$checkpoints$checkpointIndex, 0:3)) {
        add_quality("Warning", "checkpoint_index_convention", file_label, trial_number, "maze",
                    "Four checkpoints were found, but their indices are not exactly 0, 1, 2 and 3. Recorded checkpointSequence values may not map correctly.")
      }
      if (nrow(maze_result$checkpoints) != 4L) {
        add_quality("Warning", "checkpoint_count", file_label, trial_number, "maze",
                    paste0("Found ", nrow(maze_result$checkpoints), " checkpoint tiles; the study task is expected to contain four."))
      }

      maze_key <- paste0("M", length(all_trials) + 1L, "_", gsub("[^A-Za-z0-9_-]", "", maze_seed))
      ev_chunk <- chunks[[i]] %||% list()
      ev_rows <- lapply(seq_along(ev_chunk), function(j) {
        e <- ev_chunk[[j]]
        data.frame(
          trial_key = trial_key, maze_key = maze_key, file_name = file_label,
          event_index = j, elapsed_seconds = scalar_num(event_field(e, "elapsedSeconds")),
          event_type = scalar_chr(event_field(e, "eventType"), ""), agent = scalar_chr(event_field(e, "agent"), ""),
          x = scalar_num(event_field(e, "posX")), y = scalar_num(event_field(e, "posY")),
          direction = scalar_chr(event_field(e, "direction"), ""), phase = scalar_chr(event_field(e, "phase"), ""),
          decision_latency_ms = scalar_num(event_field(e, "decisionLatencyMs")),
          details = scalar_chr(event_field(e, "details"), ""), stringsAsFactors = FALSE
        )
      })
      ev_df <- if (length(ev_rows)) do.call(rbind, ev_rows) else data.frame()
      if (nrow(ev_df)) all_events[[length(all_events) + 1L]] <- ev_df

      if (nrow(maze_result$cells)) {
        cells_df <- maze_result$cells
        cells_df$maze_key <- maze_key
        all_cells[[length(all_cells) + 1L]] <- cells_df
      }
      if (nrow(maze_result$phase_a_route)) {
        route <- maze_result$phase_a_route; route$maze_key <- maze_key; route$route_type <- "Checkpoint optimum"
        all_routes[[length(all_routes) + 1L]] <- route
      }
      if (nrow(maze_result$full_route)) {
        route <- maze_result$full_route; route$maze_key <- maze_key; route$route_type <- "Full task optimum"
        all_routes[[length(all_routes) + 1L]] <- route
      }

      stored_opt <- scalar_num(trial$player1$optimalTSPPathLength, scalar_num(trial$player2$optimalTSPPathLength))
      if (is.finite(stored_opt) && is.finite(maze_result$phase_a_length) && stored_opt != maze_result$phase_a_length) {
        add_quality("Warning", "optimal_length_mismatch", file_label, trial_number, "maze",
                    paste0("Game-recorded checkpoint optimum = ", stored_opt, "; recomputed wall-aware optimum = ", maze_result$phase_a_length, "."))
      }

      maze_row <- data.frame(
        trial_key = trial_key, maze_key = maze_key, session_id = session_id,
        participant_id = participant_id, participant_age = participant_age,
        session_timestamp = session_timestamp, file_name = file_label,
        trial_number = trial_number, maze_seed = maze_seed, maze_fingerprint = maze_signature,
        mode_code = scalar_num(trial$mode), ai_strategy = scalar_chr(trial$aiStrategy, "Unknown"),
        maze_width = maze_result$width, maze_height = maze_result$height,
        start_x = maze_result$start[1], start_y = maze_result$start[2],
        exit_x = maze_result$exit[1], exit_y = maze_result$exit[2],
        checkpoint_count = nrow(maze_result$checkpoints),
        total_cells = maze_result$total_cells, wall_density_pct = maze_result$wall_density_pct,
        phase_a_optimal_steps = maze_result$phase_a_length,
        full_task_optimal_steps = maze_result$full_length,
        stored_optimal_steps = stored_opt, solver_ok = maze_result$solver_ok,
        wall_mismatches = maze_result$wall_mismatches,
        boundary_wall_violations = maze_result$boundary_wall_violations,
        stringsAsFactors = FALSE
      )
      all_trials[[length(all_trials) + 1L]] <- maze_row

      for (slot in 1:2) {
        player_name <- paste0("player", slot)
        player <- trial[[player_name]]
        if (is.null(player) || !is.list(player)) next
        agent <- scalar_chr(player$playerId, paste0("PLAYER_", slot))
        matchup_info <- classify_matchup(maze_row$ai_strategy, slot)
        cp_collected <- scalar_num(player$checkpointsCollected, NA_real_)
        cp_required <- scalar_num(player$totalCheckpointsRequired, nrow(maze_result$checkpoints))
        checkpoint_goal_valid <- nrow(maze_result$checkpoints) == 4L && is.finite(cp_required) && cp_required == 4
        if (is.finite(cp_required) && cp_required != 4) {
          add_quality("Warning", "checkpoint_requirement_mismatch", file_label, trial_number, agent,
                      paste0("Player record expects ", cp_required, " checkpoints; the study objective requires four."))
        }
        if (is.finite(cp_collected) && (!is.finite(cp_required) || cp_collected < 0 ||
                                        cp_collected > cp_required || cp_collected != floor(cp_collected))) {
          add_quality("Warning", "invalid_checkpoint_collection_count", file_label, trial_number, agent,
                      paste0("checkpointsCollected = ", cp_collected,
                             " is outside the valid integer range 0..", cp_required, "."))
        }
        phase_a_complete <- cp_required > 0 && is.finite(cp_collected) && cp_collected >= cp_required
        if (!is.null(player$phaseACompleted) && scalar_bool(player$phaseACompleted) != phase_a_complete) {
          add_quality("Warning", "phase_a_flag_mismatch", file_label, trial_number, agent,
                      paste0("phaseACompleted flag disagrees with checkpointsCollected (", cp_collected, "/", cp_required, "). Completion is derived from collected checkpoint count."))
        }
        agent_events <- if (nrow(ev_df)) ev_df[ev_df$agent == agent, , drop = FALSE] else data.frame()
        has_agent_event_stream <- nrow(agent_events) > 0L
        if (nrow(ev_df) > 0L && !has_agent_event_stream) {
          add_quality("Warning", "player_event_stream_missing", file_label, trial_number, agent,
                      "Raw events exist for this trial, but none match this player's playerId; movement heatmap and event-derived completion may be incomplete.")
        }
        has_exit_reached <- has_agent_event_stream && any(agent_events$event_type == "EXIT_REACHED")
        reported_phase_b_complete <- if (is.null(player$phaseBCompleted)) NA else scalar_bool(player$phaseBCompleted, FALSE)
        # Prefer an explicit exit event whenever this player's event stream exists;
        # fall back to the player summary only when no per-player events were exported.
        phase_b_complete <- if (has_agent_event_stream) has_exit_reached else if (is.na(reported_phase_b_complete)) FALSE else reported_phase_b_complete
        if (!is.na(reported_phase_b_complete) && has_agent_event_stream && reported_phase_b_complete != has_exit_reached) {
          add_quality("Warning", "phase_b_flag_mismatch", file_label, trial_number, agent,
                      paste0("phaseBCompleted flag is ", reported_phase_b_complete,
                             " while EXIT_REACHED event presence is ", has_exit_reached, "."))
        }
        moves <- if (nrow(agent_events)) agent_events[agent_events$event_type == "MOVE", , drop = FALSE] else data.frame()
        phase_a_moves <- if (nrow(moves)) moves[moves$phase == "PHASE_A_CHECKPOINTS", , drop = FALSE] else data.frame()
        phase_b_moves <- if (nrow(moves)) moves[moves$phase == "PHASE_B_EXIT_RACE", , drop = FALSE] else data.frame()
        valid_move_positions <- if (nrow(moves)) {
          ok <- is.finite(moves$x) & is.finite(moves$y) & moves$x == floor(moves$x) & moves$y == floor(moves$y)
          ok[is.na(ok)] <- FALSE
          if (nrow(maze_result$cells)) {
            ok <- ok & coord_key(moves$x, moves$y) %in% coord_key(maze_result$cells$x, maze_result$cells$y)
          } else {
            ok[] <- FALSE
          }
          ok[is.na(ok)] <- FALSE
          ok
        } else logical(0)
        if (any(!valid_move_positions)) {
          add_quality("Warning", "move_coordinate_outside_maze", file_label, trial_number, agent,
                      paste0(sum(!valid_move_positions), " MOVE event(s) have missing, non-integer, or off-grid coordinates. They remain in raw movement counts but are excluded from the spatial heatmap/route overlay."))
        }
        spatial_moves <- if (nrow(moves)) moves[valid_move_positions, , drop = FALSE] else data.frame()
        reported_a <- scalar_num(player$actualDistancePhaseA)
        reported_b <- scalar_num(player$actualDistancePhaseB)
        steps_a <- if (nrow(phase_a_moves) || is.finite(reported_a) && reported_a == 0) nrow(phase_a_moves) else reported_a
        steps_b <- if (nrow(phase_b_moves) || is.finite(reported_b) && reported_b == 0) nrow(phase_b_moves) else reported_b
        if (is.finite(reported_a) && nrow(phase_a_moves) != reported_a) {
          add_quality("Warning", "phase_a_move_count_mismatch", file_label, trial_number, agent,
                      paste0("MOVE events in Phase A = ", nrow(phase_a_moves), "; player metric actualDistancePhaseA = ", reported_a, "."))
        }
        if (is.finite(reported_b) && nrow(phase_b_moves) != reported_b) {
          add_quality("Warning", "phase_b_move_count_mismatch", file_label, trial_number, agent,
                      paste0("MOVE events in Phase B = ", nrow(phase_b_moves), "; player metric actualDistancePhaseB = ", reported_b, "."))
        }

        sequence_raw <- player$checkpointSequence %||% integer(0)
        if (is.character(sequence_raw) && length(sequence_raw) == 1L && grepl(",", sequence_raw, fixed = TRUE)) {
          sequence_raw <- trimws(strsplit(sequence_raw, ",", fixed = TRUE)[[1]])
        }
        checkpoint_sequence <- suppressWarnings(as.integer(unlist(sequence_raw, use.names = FALSE)))
        observed_route_steps <- if (checkpoint_goal_valid && phase_a_complete && length(checkpoint_sequence) == nrow(maze_result$checkpoints)) {
          route_distance_for_sequence(maze_result, checkpoint_sequence)
        } else NA_real_
        reported_observed <- scalar_num(player$observedOrderOptimalPathLength)
        if (is.finite(observed_route_steps) && is.finite(reported_observed) && observed_route_steps != reported_observed) {
          add_quality("Warning", "observed_order_mismatch", file_label, trial_number, agent,
                      paste0("Recomputed shortest path for the recorded four-checkpoint order = ", observed_route_steps,
                             "; game-recorded observedOrderOptimalPathLength = ", reported_observed, "."))
        }

        reveal <- if (nrow(agent_events)) agent_events[agent_events$event_type == "EXIT_REVEALED", , drop = FALSE] else data.frame()
        exits <- if (nrow(agent_events)) agent_events[agent_events$event_type == "EXIT_REACHED", , drop = FALSE] else data.frame()
        last_phase_b_move <- if (nrow(phase_b_moves)) tail(phase_b_moves, 1) else data.frame()
        last_phase_b_xy <- if (nrow(last_phase_b_move)) c(last_phase_b_move$x[1], last_phase_b_move$y[1]) else if (nrow(reveal)) c(reveal$x[nrow(reveal)], reveal$y[nrow(reveal)]) else c(NA_real_, NA_real_)
        exit_xy <- if (nrow(exits)) c(exits$x[nrow(exits)], exits$y[nrow(exits)]) else c(NA_real_, NA_real_)
        exit_endpoint_extra_step <- NA_real_
        exit_event_matches_maze <- nrow(exits) > 0L && all(is.finite(c(exit_xy, maze_result$exit))) && all(exit_xy == maze_result$exit)
        if (nrow(exits) && !exit_event_matches_maze) {
          add_quality("Warning", "exit_event_coordinate_mismatch", file_label, trial_number, agent,
                      paste0("EXIT_REACHED event coordinate (", exit_xy[1], ",", exit_xy[2],
                             ") differs from mazeGrid exit (", maze_result$exit[1], ",", maze_result$exit[2], ")."))
        }
        if (exit_event_matches_maze && all(is.finite(last_phase_b_xy))) {
          if (all(last_phase_b_xy == exit_xy)) {
            exit_endpoint_extra_step <- 0
          } else if (maze_step_is_open(maze_result$cells, last_phase_b_xy, exit_xy)) {
            exit_endpoint_extra_step <- 1
            add_quality("Warning", "exit_transition_missing_from_move_stream", file_label, trial_number, agent,
                        "EXIT_REACHED is one traversable tile beyond the last Phase B MOVE/EXIT_REVEALED position. The stored Phase B move count matches MOVE events but omits this final endpoint transition; the derived full-task step count adds one step.")
          } else {
            add_quality("Warning", "exit_transition_unverified", file_label, trial_number, agent,
                        "EXIT_REACHED does not match the last Phase B position and is not one verified open maze edge away; full-task path length is not calculated.")
          }
        }
        phase_b_steps_to_exit <- if (phase_b_complete && is.finite(steps_b) && is.finite(exit_endpoint_extra_step)) steps_b + exit_endpoint_extra_step else NA_real_
        phase_b_opt <- NA_real_
        if (nrow(reveal) && all(is.finite(c(reveal$x[nrow(reveal)], reveal$y[nrow(reveal)], maze_result$exit)))) {
          phase_b_opt <- shortest_distance_between(maze_result$cells,
                                                    c(reveal$x[nrow(reveal)], reveal$y[nrow(reveal)]),
                                                    maze_result$exit)
        }
        reported_phase_b_opt <- scalar_num(player$optimalDistancePhaseB)
        if (is.finite(phase_b_opt) && is.finite(reported_phase_b_opt) && phase_b_opt != reported_phase_b_opt) {
          add_quality("Warning", "phase_b_optimal_mismatch", file_label, trial_number, agent,
                      paste0("Recomputed reveal-to-exit distance = ", phase_b_opt,
                             "; game-recorded optimalDistancePhaseB = ", reported_phase_b_opt, "."))
        }

        winner_value <- if (is.null(player$isWinner)) NA else scalar_bool(player$isWinner, FALSE)
        winner_flag <- isTRUE(winner_value)
        outcome_value <- if (is.na(winner_value)) NA_character_ else if (winner_flag) "Winner" else "Loser"
        event_count <- function(type) {
          if (!has_agent_event_stream) return(NA_integer_)
          as.integer(sum(agent_events$event_type == type, na.rm = TRUE))
        }
        junction_latency_values <- if (has_agent_event_stream) {
          z <- agent_events$decision_latency_ms[agent_events$event_type == "JUNCTION_DECISION"]
          z[is.finite(z)]
        } else numeric(0)
        event_metric_checks <- list(
          list(summary_field = "wallCollisionsCount", event_type = "WALL_COLLISION", code = "wall_collision_count_mismatch"),
          list(summary_field = "repeatedCellCount", event_type = "REVISIT", code = "revisit_count_mismatch"),
          list(summary_field = "rciBurstCount", event_type = "RCI_BURST", code = "rci_burst_count_mismatch"),
          list(summary_field = "junctionsEncountered", event_type = "JUNCTION_DECISION", code = "junction_decision_count_mismatch")
        )
        for (check in event_metric_checks) {
          reported_count <- scalar_num(player[[check$summary_field]])
          observed_count <- event_count(check$event_type)
          if (is.finite(reported_count) && is.finite(observed_count) && reported_count != observed_count) {
            add_quality("Warning", check$code, file_label, trial_number, agent,
                        paste0("Player summary ", check$summary_field, " = ", reported_count,
                               "; raw ", check$event_type, " event count = ", observed_count,
                               ". The two measures are preserved separately."))
          }
        }
        reported_latency <- scalar_num(player$avgJunctionLatencyMs)
        observed_latency <- if (length(junction_latency_values)) mean(junction_latency_values) else NA_real_
        latency_tolerance <- max(0.001, abs(reported_latency) * 1e-6, na.rm = TRUE)
        if (is.finite(reported_latency) && is.finite(observed_latency) &&
            abs(reported_latency - observed_latency) > latency_tolerance) {
          add_quality("Warning", "junction_latency_summary_mismatch", file_label, trial_number, agent,
                      paste0("Player summary avgJunctionLatencyMs = ", reported_latency,
                             "; mean JUNCTION_DECISION event latency = ", observed_latency, " ms."))
        }
        player_fields <- flatten_player_fields(player)
        player_row <- c(list(
          trial_key = trial_key, maze_key = maze_key, session_id = session_id,
          participant_id = participant_id, participant_age = participant_age,
          session_timestamp = session_timestamp, file_name = file_label,
          trial_number = trial_number, maze_seed = maze_seed, mode_code = scalar_num(trial$mode),
          ai_strategy = maze_row$ai_strategy, matchup = matchup_info$matchup,
          player_slot = slot, agent = agent, player_type = matchup_info$player_type,
          match_outcome = outcome_value, is_winner = winner_value,
          checkpoints_collected = cp_collected, checkpoints_required = cp_required,
          checkpoint_goal_valid = checkpoint_goal_valid,
          checkpoint_sequence = paste(checkpoint_sequence, collapse = ","),
          checkpoint_completion_pct = if (is.finite(cp_required) && cp_required > 0) 100 * cp_collected / cp_required else NA_real_,
          phase_a_completed = phase_a_complete, phase_b_completed = phase_b_complete,
          phase_a_steps = steps_a, phase_b_steps = steps_b,
          phase_b_endpoint_extra_step = exit_endpoint_extra_step,
          phase_b_steps_to_exit = phase_b_steps_to_exit,
          total_steps = if (is.finite(steps_a) && is.finite(steps_b)) steps_a + steps_b else NA_real_,
          full_task_steps = if (is.finite(steps_a) && is.finite(phase_b_steps_to_exit)) steps_a + phase_b_steps_to_exit else NA_real_,
          optimal_phase_a_steps = maze_result$phase_a_length,
          optimal_full_task_steps = maze_result$full_length,
          observed_order_optimal_steps = reported_observed,
          phase_b_optimal_steps = phase_b_opt,
          path_deviation = if (checkpoint_goal_valid && phase_a_complete && is.finite(steps_a) && is.finite(maze_result$phase_a_length)) steps_a - maze_result$phase_a_length else NA_real_,
          full_task_deviation = if (checkpoint_goal_valid && phase_a_complete && phase_b_complete && is.finite(steps_a) && is.finite(phase_b_steps_to_exit) && is.finite(maze_result$full_length)) steps_a + phase_b_steps_to_exit - maze_result$full_length else NA_real_,
          response_latency_ms = scalar_num(player$avgJunctionLatencyMs),
          junction_decision_count = event_count("JUNCTION_DECISION"),
          junction_latency_mean_ms = if (length(junction_latency_values)) mean(junction_latency_values) else NA_real_,
          junction_latency_median_ms = if (length(junction_latency_values)) stats::median(junction_latency_values) else NA_real_,
          wall_collision_count = event_count("WALL_COLLISION"),
          revisit_event_count = event_count("REVISIT"),
          rapid_input_burst_count = event_count("RCI_BURST"),
          completion_time_sec = scalar_num(player$completionTime),
          score = scalar_num(player$gameScore)
        ), player_fields)
        # Avoid collisions between canonical snake_case columns and raw camelCase fields.
        player_row <- player_row[!duplicated(names(player_row), fromLast = TRUE)]
        all_players[[length(all_players) + 1L]] <- as.data.frame(player_row, stringsAsFactors = FALSE, optional = TRUE)

        # Start tile + accepted MOVE destinations form the visited-tile stream.
        start_on_grid <- nrow(maze_result$cells) > 0L && all(is.finite(maze_result$start)) &&
          all(maze_result$start == floor(maze_result$start)) &&
          coord_key(maze_result$start[1], maze_result$start[2]) %in% coord_key(maze_result$cells$x, maze_result$cells$y)
        if (start_on_grid) {
          start_visit <- data.frame(
            trial_key = trial_key, maze_key = maze_key, agent = agent, player_slot = slot,
            player_type = matchup_info$player_type, phase = "PHASE_A_CHECKPOINTS", event_type = "START",
            x = maze_result$start[1], y = maze_result$start[2], elapsed_seconds = 0,
            step = -1, counts_as_move = FALSE, counts_as_visit = TRUE, stringsAsFactors = FALSE
          )
          all_trajectory[[length(all_trajectory) + 1L]] <- start_visit
        }
        if (nrow(spatial_moves)) {
          move_visits <- data.frame(
            trial_key = trial_key, maze_key = maze_key, agent = agent, player_slot = slot,
            player_type = matchup_info$player_type, phase = spatial_moves$phase, event_type = "MOVE",
            x = spatial_moves$x, y = spatial_moves$y, elapsed_seconds = spatial_moves$elapsed_seconds,
            step = spatial_moves$event_index, counts_as_move = TRUE, counts_as_visit = TRUE, stringsAsFactors = FALSE
          )
          all_trajectory[[length(all_trajectory) + 1L]] <- move_visits
        }
        # EXIT_REVEALED identifies the starting point of Phase B. It is included
        # in route drawing but not counted as a second tile visit.
        reveal_xy <- if (nrow(reveal)) c(reveal$x[nrow(reveal)], reveal$y[nrow(reveal)]) else c(NA_real_, NA_real_)
        reveal_on_grid <- nrow(maze_result$cells) > 0L && all(is.finite(reveal_xy)) &&
          all(reveal_xy == floor(reveal_xy)) && coord_key(reveal_xy[1], reveal_xy[2]) %in% coord_key(maze_result$cells$x, maze_result$cells$y)
        if (nrow(reveal) && !reveal_on_grid) {
          add_quality("Warning", "exit_revealed_coordinate_invalid", file_label, trial_number, agent,
                      "EXIT_REVEALED has a missing, non-integer, or off-grid coordinate; it is excluded from the spatial route overlay.")
        }
        if (nrow(reveal) && reveal_on_grid) {
          reveal_visit <- data.frame(
            trial_key = trial_key, maze_key = maze_key, agent = agent, player_slot = slot,
            player_type = matchup_info$player_type, phase = "PHASE_B_EXIT_RACE", event_type = "EXIT_REVEALED",
            x = reveal$x[nrow(reveal)], y = reveal$y[nrow(reveal)],
            elapsed_seconds = reveal$elapsed_seconds[nrow(reveal)], step = reveal$event_index[nrow(reveal)] - 0.1,
            counts_as_move = FALSE, counts_as_visit = FALSE, stringsAsFactors = FALSE
          )
          all_trajectory[[length(all_trajectory) + 1L]] <- reveal_visit
        }
        exit_event_on_grid <- nrow(maze_result$cells) > 0L && all(is.finite(exit_xy)) &&
          all(exit_xy == floor(exit_xy)) && coord_key(exit_xy[1], exit_xy[2]) %in% coord_key(maze_result$cells$x, maze_result$cells$y)
        if (nrow(exits) && exit_event_matches_maze && exit_event_on_grid) {
          differs <- !all(is.finite(last_phase_b_xy)) || any(last_phase_b_xy != exit_xy)
          if (differs) {
            exit_visit <- data.frame(
              trial_key = trial_key, maze_key = maze_key, agent = agent, player_slot = slot,
              player_type = matchup_info$player_type, phase = "PHASE_B_EXIT_RACE", event_type = "EXIT_REACHED",
              x = exit_xy[1], y = exit_xy[2], elapsed_seconds = exits$elapsed_seconds[nrow(exits)],
              step = exits$event_index[nrow(exits)] + 0.1,
              counts_as_move = FALSE, counts_as_visit = TRUE, stringsAsFactors = FALSE
            )
            all_trajectory[[length(all_trajectory) + 1L]] <- exit_visit
          }
          if (all(is.finite(last_phase_b_xy)) && sum(abs(last_phase_b_xy - exit_xy)) > 1) {
            add_quality("Warning", "trajectory_exit_gap", file_label, trial_number, agent,
                        "Last MOVE coordinate is more than one tile from EXIT_REACHED; inspect the event stream before using the final segment as a continuous trajectory.")
          }
        }
      }
    }
  }

  bind_or_empty <- function(x) if (length(x)) dplyr::bind_rows(x) else data.frame()
  trials_df <- bind_or_empty(all_trials)
  players_df <- bind_or_empty(all_players)
  cells_df <- bind_or_empty(all_cells)
  trajectory_df <- bind_or_empty(all_trajectory)
  events_df <- bind_or_empty(all_events)
  routes_df <- bind_or_empty(all_routes)
  quality_df <- bind_or_empty(all_quality)
  if (nrow(trajectory_df)) {
    trajectory_df <- trajectory_df[!is.na(trajectory_df$x) & !is.na(trajectory_df$y), , drop = FALSE]
    trajectory_df <- trajectory_df[order(trajectory_df$maze_key, trajectory_df$agent, trajectory_df$step), , drop = FALSE]
  }
  list(trials = trials_df, players = players_df, cells = cells_df,
       trajectory = trajectory_df, events = events_df, routes = routes_df,
       quality = quality_df)
}
