# app/logic/series.R
# ---------------------------------------------------------------------------
# Time-series construction and treatment-date parsing. Pure functions on
# data.tables; no Shiny.
# ---------------------------------------------------------------------------

box::use(
  data.table[data.table, as.data.table, copy, dcast, setorder],
  stats[sd],
)

#' Parse a comma-separated string of YYYYMMDD dates; returns list(ok, bad).
parse_treatment_dates <- function(txt) {
  parts <- trimws(unlist(strsplit(txt, ",")))
  parts <- parts[nzchar(parts)]
  if (length(parts) == 0) return(list(ok = character(), bad = character()))

  valid <- grepl("^\\d{8}$", parts) & !is.na(as.Date(parts, format = "%Y%m%d"))
  list(ok = parts[valid], bad = parts[!valid])
}

#' Assign each scan to a time-step group for one metric column, returning the
#' LONG per-scan rows (not aggregated). This is the shared basis for all three
#' plot modes.
#'
#' Scans are clustered into time steps by walking the unique scan dates in
#' order. A new time step starts at a date when any of:
#'   1. a treatment date falls between the previous date and this one
#'      (highest priority - treatments always split a step),
#'   2. any site/plot combo scanned on this date was already scanned in the
#'      current step (a repeat visit signals a new sampling campaign;
#'      all scans on that date move to the new step together), or
#'   3. the gap from the previous scan date exceeds 30 days.
#' Plots appearing only once ("odd plots out") stay lumped with the step that
#' was open when they were scanned. `transform` is applied to the raw values
#' (e.g. gap fraction = 1 - canopyCover).
#'
#' Returns a data.table: site_name | plot | combo | label | scan_date | value
#' | grp. `combo` (site||plot) is the internal key used for repeat-visit
#' detection and per-plot grouping; `label` ("site / plot") is for legends.
assign_time_steps <- function(metrics_dt, metric_col, treat_dates,
                              transform = identity) {
  df <- data.table(
    site_name = as.character(metrics_dt$site_name),
    plot      = as.character(metrics_dt$plot),
    scan_date = as.Date(metrics_dt$date_code, format = "%Y%m%d"),
    value     = transform(
      suppressWarnings(as.numeric(metrics_dt[[metric_col]]))
    )
  )
  df[, combo := paste(site_name, plot, sep = "||")]
  df[, label := paste(site_name, plot, sep = " / ")]
  df <- df[!is.na(scan_date) & !is.na(value)]
  if (nrow(df) == 0) return(df)
  setorder(df, scan_date)

  tvec <- sort(as.Date(treat_dates, format = "%Y%m%d"))
  tvec <- tvec[!is.na(tvec)]

  # Walk unique dates in order, assigning each date to a time-step group
  dates    <- sort(unique(df$scan_date))
  date_grp <- integer(length(dates))
  seen     <- character()   # site/plot combos already in the current step
  g        <- 1L

  for (k in seq_along(dates)) {
    combos_today <- unique(df[scan_date == dates[k], combo])

    if (k > 1) {
      treat_between <- length(tvec) > 0 &&
        any(tvec > dates[k - 1] & tvec <= dates[k])
      repeat_visit  <- any(combos_today %in% seen)
      gap_days      <- as.numeric(dates[k] - dates[k - 1])

      if (treat_between || repeat_visit || gap_days > 30) {
        g    <- g + 1L
        seen <- character()
      }
    }

    date_grp[k] <- g
    seen <- union(seen, combos_today)
  }

  df[, grp := date_grp[match(scan_date, dates)]]
  df[]
}

#' Aggregate assign_time_steps() output to one row per time step:
#' t (mean date), mean, sd, n. Used by the default "time series" plot mode.
build_metric_series <- function(metrics_dt, metric_col, treat_dates,
                                transform = identity) {
  df <- assign_time_steps(metrics_dt, metric_col, treat_dates, transform)
  if (nrow(df) == 0) return(df)

  df[, .(
    t    = mean(scan_date),
    mean = mean(value),
    sd   = sd(value),      # NA when a step holds a single scan
    n    = .N
  ), by = grp][order(t)]
}

#' Reshape the long additional_models table (one row per model per scan) into
#' a metrics-like wide table: one row per scan, identifying columns first,
#' then one column per model holding model_metric_value. Model script names
#' are site-specific (e.g. onehrmod_MSGBR.rda), so the "_SITE.rda" suffix is
#' stripped to a generic model name (onehrmod) so the same model lines up
#' across sites.
reshape_models_wide <- function(models_dt) {
  if (nrow(models_dt) == 0) return(data.table())

  dt <- copy(models_dt)

  # Strip "_{site}.rda" (fall back to just ".rda") from the script name
  dt[, model_name := mapply(
    function(nm, site) sub(paste0("_", site, "\\.rda$"), "", nm),
    model_script_name, site_name
  )]
  dt[, model_name := sub("\\.rda$", "", model_name)]

  dt[, model_metric_value := suppressWarnings(as.numeric(model_metric_value))]

  dcast(
    dt,
    site_name + plot + date_code + scanner_id ~ model_name,
    value.var = "model_metric_value",
    fun.aggregate = mean   # collapses accidental duplicates
  )
}
