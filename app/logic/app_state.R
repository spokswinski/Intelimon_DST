# app/logic/app_state.R
# ---------------------------------------------------------------------------
# Shared, reactive application state. One store is created per session in
# app/main.R and passed into every view module's server(). This replaces the
# global `<<-` data.tables (scan_calls, metrics, ...) and the manual revision
# counters (metrics_rev, treatments_rev, scan_calls_rev) from the original
# single-file app: because each slot is a reactiveVal, downstream reactives
# re-run automatically when it changes.
#
# Write from a module:   state$metrics(new_dt)
# Read from a module:    state$metrics()
#
# `plots` is a plain (non-reactive) data.table: it is loaded once at startup
# and never changes.
# ---------------------------------------------------------------------------

box::use(
  shiny[reactiveVal],
  data.table[data.table],
  app/logic/api_client[load_plots],
)

#' Empty scan_calls skeleton (five-column layout, scanner_id retained for
#' downstream calls).
empty_scan_calls <- function() {
  data.table(
    site_name  = character(),
    plot       = character(),
    date_code  = character(),
    scan_name  = character(),
    scanner_id = character()
  )
}

#' Construct the per-session state store. Loads the plot inventory once
#' (wrapped so a network failure yields an empty table rather than crashing
#' the whole app; the map will simply show no plots).
new_app_state <- function() {
  plots <- tryCatch(
    load_plots(),
    error = function(e) {
      warning("Failed to load plot inventory: ", conditionMessage(e),
              call. = FALSE)
      data.table(
        site_name = character(), plot = character(),
        Longitude = numeric(),   Latitude = numeric()
      )
    }
  )

  list(
    # constant
    plots = plots,

    # reactive slots (were globals in the single-file app)
    scan_calls             = reactiveVal(empty_scan_calls()),
    metrics                = reactiveVal(data.table()),
    tree_inventory         = reactiveVal(data.table()),
    additional_models      = reactiveVal(data.table()),
    additional_models_wide = reactiveVal(data.table()),
    treatment_dates        = reactiveVal(character()),
    aoi_polygon            = reactiveVal(NULL),

    # Surface fuel bed submitted from the Fuel tool tab (NULL until the user
    # presses "Submit fuel values"). Consumed by the rothRmel tab when its
    # fuel source is set to "Fuel tool values". See app/view/fuel_tool.R.
    fuel_tool_values       = reactiveVal(NULL)
  )
}
