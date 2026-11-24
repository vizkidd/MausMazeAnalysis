# Biostatistical Maze Project — revised Shiny dashboard

This revision replaces the original dashboard's top-level JSON assumptions and greedy Manhattan route estimate with a trial-aware parser, wall-aware maze solver, reconstructed movement/visit stream, and trial-matched comparative analysis.

## Run the dashboard

Use R 4.1 or later and ggplot2 3.4.0 or later. Install the packages below once:

```r
install.packages(c(
  "shiny", "shinyFiles", "jsonlite", "dplyr", "tidyr", "ggplot2"
))
```

Open this project directory as the working directory, then run:

```r
shiny::runApp()
```

The source selector defaults to the two bundled sample JSON exports; click **Load and analyse** to parse them. You can instead select a local folder containing JSON files or upload multiple JSON files from the sidebar. The folder picker exposes available drive roots on Windows (and the file-system root on Unix-like systems); uploads are configured for up to 100 MB per request. CSV export controls are provided for player metrics, tile visits, and the data-quality log. The metric selector includes player-summary measures and event-derived junction decisions, wall collisions, revisits, rapid-input bursts, and junction latency summaries.

`app.R` is the canonical Shiny entry point.

## How the game data is interpreted

The supplied exports contain top-level session metadata, a `trials` array, and a top-level `rawEvents` array. The authoritative maze and player metrics are nested within each trial. The parser uses each trial's own `mazeGrid`; it does not reuse the top-level maze grid for every trial.

The event schema does not carry a `trialNumber` on each event. `TRIAL_START` markers split the chronological `rawEvents` stream into trial chunks. If the marker count is not exact, elapsed-time resets are used only when they yield exactly the expected number of chunks. If the mapping is still ambiguous, events are left unmapped and a data-quality warning is raised rather than assigning events to the wrong trial.

Maze cell coordinates are zero-based. `x + 1` is East and `y + 1` is North. A passage is traversable only when both cells' reciprocal wall flags say the shared edge is open. The solver uses breadth-first search for pairwise maze distances and enumerates the checkpoint orders to find the exact minimum route.

Two optimal routes are reported separately:

- **Phase A optimum:** start tile through all four checkpoints, with no exit leg. This is the quantity represented by the game's `optimalTSPPathLength` field.
- **Full-task global optimum:** start tile through all four checkpoints and then to the exit. This is a separate theoretical baseline and is not substituted for the game's Phase A value.

The full-task route is calculated over shortest-path distances in the maze graph. This theoretical baseline assumes the exit tile and all passages represented in `mazeGrid` are traversable in the same graph before the exit is revealed; confirm that this matches the game's collision/exit logic. If the exit is physically blocked until Phase A completes, the solver must model that phase-specific restriction instead. Incomplete players are not assigned a full-task path deviation. Phase A `path_deviation` is also missing unless the maze and player record represent the required four-checkpoint objective and the player collected all four checkpoints.

## Tile visits and heatmap

The reconstructed tile-visit stream includes:

1. one initial start-tile visit for each player record;
2. every valid `MOVE` destination;
3. an `EXIT_REACHED` tile when it differs from the last Phase B position.

`REVISIT` is a metadata event describing an arrival already represented by a `MOVE`; it is not counted as another arrival. `EXIT_REVEALED` is a phase boundary and a trajectory marker, not another visit. The heatmap counts each represented tile arrival, including revisits through separate MOVE events. The maze map uses the correct zero-based x/y coordinates and draws the outer boundary independently of the per-cell boundary flags.

In the supplied sample, two `EXIT_REACHED` events identify a traversable final tile transition that is absent from the `MOVE` stream. The raw `actualDistancePhaseB` fields equal the count of Phase B `MOVE` events and omit that endpoint transition. The app preserves the recorded count, exposes the extra endpoint step separately, and uses it only for the event-reconciled full-task count/deviation. The quality log marks this so the distinction is visible.

## Population-level plots

The **Population plots** tab adds two cross-trial views:

- **Pooled coordinate occupancy:** uses the parser-reconstructed visit stream for explicitly classified PvAI trials only. Human and AI are shown in separate facets on the common zero-based x/y coordinate frame. Each tile is expressed as visits per 1,000 arrivals within that player type. The input visit stream counts the initial start tile, each valid `MOVE` destination, and a distinct `EXIT_REACHED` endpoint; `REVISIT` is not added a second time and `EXIT_REVEALED` is not a visit. Maze walls are omitted because the plots pool different generated layouts; the same coordinate does not imply the same corridor or physical maze feature. The common start is marked only when all included trials share one start coordinate.
- **Complete movement vs optimal baseline:** overlays the observed full-task movement-step distribution with the global wall-aware optimum for the same maze, in separate Human and AI facets. Only recognised PvAI records with a valid four-checkpoint objective, all four checkpoints collected, an `EXIT_REACHED` completion, a reconciled full-task movement count, and a finite maze optimum are included. The full-task count includes the reconciled final exit endpoint. Median lines and a summary table report observed steps, optimum, excess, and observed/optimal ratio.

The distributions are descriptive player-trial distributions, not participant-level distributions. A maze-specific optimum is repeated for each eligible player record; if both players finish the same trial, both records contribute their own observed path against the same maze optimum.

## Comparative analysis

The overview's completion-rate and mean Phase A deviation cards are Human-only, and the deviation histogram is split by Human/AI player type. The player-level table still exposes both types. The comparative analysis dashboard offers the roadmap's three designs:

- **One-sample baseline:** compare a selected metric with a user-entered reference. The population selector defaults to Human-controlled records so AI agents are not silently mixed into the cognitive-assessment sample; AI-only and all-record analyses are also available. For Phase A path deviation, zero is the natural reference (`actual steps - optimal four-checkpoint steps`). Other metrics require a scientifically justified reference; zero should not be assumed.
- **Paired PvAI:** compare Human and AI records inside the same `trial_key` (same participant/session/trial/maze), not by row position.
- **Paired PvP:** compare Winner and Loser records inside the same trial. This is shown only when PvP is inferred from the session strategy metadata and a unique winner/loser pair is available.

The app tests normality on one-sample residuals or within-trial paired differences. Pair construction is strict: each side must have exactly one usable observation within the same trial/maze; ambiguous duplicate sides are excluded and tested in the helper regression suite. It supports the one-sample/paired t-test and Wilcoxon signed-rank test. Signed-log transformation is applied to the analysis variable (or paired difference), and z-scoring is applied to that analysis vector with the one-sample reference transformed consistently. Shapiro–Wilk is not attempted for fewer than three observations or for constant vectors; the inferential test is withheld for fewer than two usable values or no variability.

Player-level counts for `WALL_COLLISION`, `REVISIT`, `JUNCTION_DECISION`, and `RCI_BURST` are derived from that player's event stream; missing player event streams remain `NA` rather than being treated as zero. These are exploratory per-metric tests, not a full repeated-measures model. Repeated trials from the same participant are not independent. For a final cohort analysis, consider participant-level mixed-effects models (and maze/session clustering where appropriate), multiplicity control across metrics, effect sizes and confidence intervals, and an explicit plan for incomplete trials and missingness.

## Data-quality behaviour

The parser deduplicates repeated exports using participant, session timestamp, trial number, maze seed, and a canonical fingerprint of the maze geometry (cell-array order does not affect the fingerprint). This prevents different layouts from collapsing together when a seed or trial label is reused. It records issues for invalid JSON, missing trial mazes, missing/invalid wall flags, open outer boundaries, inconsistent reciprocal walls, disconnected exits, non-four-checkpoint mazes, unexpected checkpoint indices, event/trial marker mismatches, unmapped player event streams, invalid/off-grid spatial events, invalid checkpoint collection counts, player completion-flag inconsistencies, stored-versus-recomputed optimal distances, observed-order mismatches, move-count mismatches, and exit-coordinate/trajectory inconsistencies.

Unknown `aiStrategy` values remain `Unknown`; the numeric `mode` value is retained but not assigned an undocumented meaning. The supplied sample uses `aiStrategy = "Human Player 2"`, so Player 2 is classified as Human and Player 1 as AI. No PvP sample is included.

## Regression checks

Run the included checks from the project directory:

```sh
Rscript tests/test_analysis.R
```

The test script validates duplicate handling, optimal paths, player phase counts, incomplete checkpoint handling, event-derived visits and event-summary consistency, Phase B exit reconciliation, cell-order-independent maze fingerprints, and synthetic connected/disconnected/boundary-invalid mazes.
