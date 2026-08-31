# app/logic/api_client.R
# ---------------------------------------------------------------------------
# IntELiMon REST API v2 client (USGS EROS). Pure functions returning
# data.tables / data.frames; no Shiny. `base_url` is the API root and is
# exported so callers can build ad-hoc endpoints (e.g. the future
# points2pano call).
# ---------------------------------------------------------------------------

box::use(
  jsonlite[fromJSON],
  data.table[data.table, as.data.table, rbindlist, setcolorder],
)

#' API root
base_url <- "https://edcintl.cr.usgs.gov/geoengine5/rest/intelimon/v2"

#' Load the full plot inventory from /plots and return a data.table with
#' site_name | plot | Longitude | Latitude (WGS84). Coordinates arrive as
#' EPSG:3857 POINT() strings and are reprojected to 4326 here.
load_plots <- function() {
  request_url <- paste0(base_url, "/plots")
  raw <- fromJSON(request_url, flatten = TRUE)

  plots <- data.frame(
    site_name = raw$site_name,
    plot      = raw$plot,
    location  = raw$location,
    stringsAsFactors = FALSE
  )

  plots$location <- gsub("[POINT()]", "", plots$location)
  plots <- tidyr::separate(
    plots, col = location,
    into = c("Longitude", "Latitude"), sep = " "
  )

  pts <- sf::st_as_sf(
    plots,
    coords = c("Longitude", "Latitude"),
    crs = 3857
  )
  pts_wgs84 <- sf::st_transform(pts, 4326)
  coords <- sf::st_coordinates(pts_wgs84)

  plots$Longitude <- coords[, 1]
  plots$Latitude  <- coords[, 2]

  as.data.table(plots)
}

#' Pull the first matching field name out of a response data frame; errors
#' with the actual field names so a schema mismatch is easy to diagnose.
pick_field <- function(df, candidates) {
  hit <- intersect(candidates, names(df))
  if (length(hit) == 0) {
    stop("None of [", paste(candidates, collapse = ", "),
         "] found in /scans response. Fields returned: ",
         paste(names(df), collapse = ", "))
  }
  df[[hit[1]]]
}

#' Query /scans for one site/plot combo and return a data.table:
#'   site_name | plot | date_code | scanner_id | scan_name
#' One row per scan record. scan_name = site_plot_date_scannerid,
#' e.g. ALKCR_0073_20231003_2. Only status == "Available" records are kept.
fetch_scans_for_plot <- function(site, plot) {
  url <- paste0(
    base_url, "/scans",
    "?site=", utils::URLencode(site, reserved = TRUE),
    "&plot=", utils::URLencode(plot, reserved = TRUE)
  )

  raw <- fromJSON(url, flatten = TRUE)

  # Unwrap if records are nested in a list element (e.g. $data or $scans)
  if (!is.data.frame(raw)) {
    if (is.list(raw)) {
      dfs <- Filter(is.data.frame, raw)
      if (length(dfs) > 0) raw <- dfs[[1]]
    }
    if (!is.data.frame(raw)) return(NULL)   # no scans for this combo
  }
  if (nrow(raw) == 0) return(NULL)

  # Keep only scans marked Available
  if ("status" %in% names(raw)) {
    raw <- raw[raw$status == "Available", , drop = FALSE]
    if (nrow(raw) == 0) return(NULL)
  }

  date_code  <- as.character(pick_field(raw, c("date", "scan_date", "date_code")))
  scanner_id <- as.character(pick_field(raw, c("scanner_id", "scannerId",
                                               "scanner", "scanner_num")))

  # Normalize dates to YYYYMMDD in case they arrive as YYYY-MM-DD
  date_code <- gsub("-", "", date_code)

  out <- data.table(
    site_name  = site,
    plot       = plot,
    date_code  = date_code,
    scanner_id = scanner_id
  )
  out[, scan_name := paste(site_name, plot, date_code, scanner_id, sep = "_")]
  out
}

#' Query /scans for every clicked site/plot combo and stack the results.
#' Combos with multiple scan records naturally expand into multiple rows.
#' `clicked` is a data.frame/data.table with site_name + plot columns.
populate_scan_calls <- function(clicked) {
  results <- vector("list", nrow(clicked))

  for (i in seq_len(nrow(clicked))) {
    results[[i]] <- tryCatch(
      fetch_scans_for_plot(clicked$site_name[i], clicked$plot[i]),
      error = function(e) {
        warning("Scan query failed for ", clicked$site_name[i], "/",
                clicked$plot[i], ": ", conditionMessage(e), call. = FALSE)
        NULL
      }
    )
  }

  out <- rbindlist(Filter(Negate(is.null), results), use.names = TRUE)
  if (nrow(out) == 0) return(out)

  setcolorder(out, c("site_name", "plot", "date_code", "scan_name", "scanner_id"))
  out[]
}

#' Query /scan/metrics for one scan; returns a data.table with the scan's
#' identifying columns prepended.
fetch_metrics_for_scan <- function(site, plot, date_code, scanner_id) {
  url <- paste0(
    base_url, "/scan/metrics",
    "?site=",       utils::URLencode(site, reserved = TRUE),
    "&plot=",       utils::URLencode(plot, reserved = TRUE),
    "&date=",       utils::URLencode(date_code, reserved = TRUE),
    "&scanner_id=", utils::URLencode(scanner_id, reserved = TRUE)
  )

  raw <- fromJSON(url, flatten = TRUE)

  # Normalize to a data frame: records arrays come back as data frames,
  # single records as named lists
  if (!is.data.frame(raw)) {
    if (is.list(raw)) {
      dfs <- Filter(is.data.frame, raw)
      if (length(dfs) > 0) {
        raw <- dfs[[1]]
      } else if (length(raw) > 0) {
        raw <- as.data.frame(raw[lengths(raw) == 1], stringsAsFactors = FALSE)
      }
    }
    if (!is.data.frame(raw) || nrow(raw) == 0) return(NULL)
  }
  if (nrow(raw) == 0) return(NULL)

  out <- as.data.table(raw)
  out[, `:=`(site_name  = site,
             plot       = plot,
             date_code  = date_code,
             scanner_id = scanner_id)]
  setcolorder(out, c("site_name", "plot", "date_code", "scanner_id"))
  out
}

#' Query /scan/tree_inventory (json) for one scan; one row per tree with
#' identifying columns prepended. Response fields: TreeID, X, Y, Radius,
#' Error, H, h_radius, DBH, BasalA (strings -> numeric here).
fetch_tree_inventory_for_scan <- function(site, plot, date_code, scanner_id) {
  url <- paste0(
    base_url, "/scan/tree_inventory",
    "?site=",       utils::URLencode(site, reserved = TRUE),
    "&plot=",       utils::URLencode(plot, reserved = TRUE),
    "&date=",       utils::URLencode(date_code, reserved = TRUE),
    "&scanner_id=", utils::URLencode(scanner_id, reserved = TRUE),
    "&format=json"
  )

  raw <- fromJSON(url, flatten = TRUE)

  if (!is.data.frame(raw) || nrow(raw) == 0) return(NULL)

  # Drop the unnamed row-index column the endpoint prepends
  raw <- raw[, nzchar(names(raw)), drop = FALSE]

  # Values arrive as strings; convert numeric-looking columns
  raw <- utils::type.convert(raw, as.is = TRUE)

  out <- as.data.table(raw)
  out[, `:=`(site_name  = site,
             plot       = plot,
             date_code  = date_code,
             scanner_id = scanner_id)]
  setcolorder(out, c("site_name", "plot", "date_code", "scanner_id"))
  out
}

#' Query /scan/additional_models_metrics for one scan; one row per prediction
#' model with identifying columns prepended. The response nests a models array
#' (model_script_name, model_metric_value, model_last_updated) inside each scan
#' record; it is unnested here.
fetch_models_for_scan <- function(site, plot, date_code, scanner_id) {
  url <- paste0(
    base_url, "/scan/additional_models_metrics",
    "?site=",       utils::URLencode(site, reserved = TRUE),
    "&plot=",       utils::URLencode(plot, reserved = TRUE),
    "&date=",       utils::URLencode(date_code, reserved = TRUE),
    "&scanner_id=", utils::URLencode(scanner_id, reserved = TRUE)
  )

  raw <- fromJSON(url)

  if (!is.data.frame(raw) || nrow(raw) == 0) return(NULL)
  if (!"models" %in% names(raw)) return(NULL)

  # Unnest: one output row per model, carrying the scan-level fields
  rows <- lapply(seq_len(nrow(raw)), function(i) {
    m <- raw$models[[i]]
    if (!is.data.frame(m) || nrow(m) == 0) return(NULL)
    scan_cols <- raw[i, setdiff(names(raw), "models"), drop = FALSE]
    cbind(scan_cols[rep(1, nrow(m)), , drop = FALSE], m)
  })

  out <- rbindlist(Filter(Negate(is.null), rows), use.names = TRUE, fill = TRUE)
  if (nrow(out) == 0) return(NULL)

  out[, `:=`(site_name  = site,
             plot       = plot,
             date_code  = date_code,
             scanner_id = scanner_id)]
  setcolorder(out, c("site_name", "plot", "date_code", "scanner_id"))
  out
}
