# app/logic/plotting.R
# ---------------------------------------------------------------------------
# Shared metric plot renderer. Used by the Direct outputs, Predictive models
# and rothRmel tabs, so it lives in the logic layer. `treatment_dates` is
# passed in explicitly.
#
# Four plot modes (chosen from the dropdown at the top of each tab's sidebar):
#   "timeseries" - aggregated mean +/- sd per time step, connected line
#   "individual" - one colored line/point series per site/plot, legend at side
#   "boxplot"    - box & whisker per time step (distribution across plots),
#                  time on the X axis
#   "bar"        - mean per time step as bars, time on the X axis
#
# Plots render with a transparent background and light text so they sit on the
# Aurora Glass cards; see aurora_theme(). Data points are enlarged diamonds,
# treatment lines are thick coral verticals.
#
# NOTE: shiny::validate / shiny::need are called with an explicit shiny::
# prefix. jsonlite also exports validate(); the explicit namespace guarantees
# Shiny's version is used regardless of attach order.
# ---------------------------------------------------------------------------

box::use(
  shiny[div, tags, plotOutput, downloadLink, downloadHandler, outputOptions],
  ggplot2[...],
  stats[approx],
  grid[unit],
  data.table[fwrite, setnames],
  app/logic/series[build_metric_series, assign_time_steps],
  app/logic/constants[DERIVED_METRICS],
)

# Two palettes for the same plots. SCREEN is the Aurora Glass palette (kept in
# sync with app/static/styles.css): light ink on a transparent ground, made to
# sit on the dark glass cards. EXPORT is its light-ground counterpart, used for
# the SVG/PNG downloads - those land on a white page where the screen palette's
# pale cyan/cream would be invisible, so every colour is darkened to read on
# white. `metric_series_plot(light = TRUE)` selects it.
SCREEN_PAL <- list(
  accent     = "#8ff0e2",
  treat      = "#ff6b7d",
  point      = "#ffe9c9",
  txt        = "#eaf1ff",
  txt_dim    = "#aeb9d6",
  boxfill    = "#ffffff26",
  grid_major = "#ffffff2e",
  grid_minor = "#ffffff18",
  axis_text  = "#e6edff",
  axis_title = "#d3ddf5",
  title      = "#ffffff",
  bg         = "transparent",
  stroke     = "#0a0a0a"
)

EXPORT_PAL <- list(
  accent     = "#0b7285",
  treat      = "#c92a2a",
  point      = "#f59f00",
  txt        = "#1a1a1a",
  txt_dim    = "#495057",
  boxfill    = "#00000010",
  grid_major = "#00000026",
  grid_minor = "#00000014",
  axis_text  = "#212529",
  axis_title = "#343a40",
  title      = "#000000",
  bg         = "white",
  stroke     = "#1a1a1a"
)

TREAT_LW <- 1.4
POINT_SZ <- 3.4
DIAMOND  <- 23

# Transparent, light-on-dark theme so ggplot output blends into the glass card.
# Pair with renderPlot(..., bg = "transparent") in the view modules.
# Plot font. R's default device sans renders thin and jagged at small sizes on
# some systems; a condensed/humanist face reads better in narrow card axes.
# The first entry that actually exists on the machine is used - unavailable
# families silently fall back to "sans", so this is safe to leave as-is.
# Change this one string to restyle every axis in the app. Any family the
# graphics device cannot find is substituted automatically, so an unavailable
# name degrades to the device default rather than erroring.
PLOT_FONT <- "sans"

#' Axis label formatter: keeps only the decimals the data actually needs.
#' A near-flat series (e.g. 141.73 -> 141.81) otherwise prints six-character
#' labels at every break and crowds a narrow axis.
#' @export
axis_fmt <- function(x) {
  fin <- x[is.finite(x)]
  if (length(fin) == 0) return(format(x))
  rng <- diff(range(fin))
  step <- if (length(fin) > 1) min(diff(sort(unique(fin)))) else rng
  digits <- if (!is.finite(step) || step <= 0) 0 else
    max(0, min(2, ceiling(-log10(step))))
  out <- formatC(x, format = "f", digits = digits, big.mark = ",")
  trimws(out)
}

#' Continuous y scale used by every card: few breaks, tidy labels.
#' @export
y_scale <- function() {
  scale_y_continuous(n.breaks = 5, labels = axis_fmt,
                     expand = expansion(mult = 0.08))
}

aurora_theme <- function(pal = SCREEN_PAL) {
  theme_minimal(base_size = 13, base_family = PLOT_FONT) +
    theme(
      plot.background   = element_rect(fill = pal$bg, color = NA),
      panel.background  = element_rect(fill = pal$bg, color = NA),
      legend.background = element_rect(fill = pal$bg, color = NA),
      legend.key        = element_rect(fill = pal$bg, color = NA),
      panel.grid.major  = element_line(color = pal$grid_major, linewidth = 0.4),
      panel.grid.minor  = element_line(color = pal$grid_minor, linewidth = 0.3),
      text              = element_text(color = pal$txt, family = PLOT_FONT,
                                       face = "plain"),
      axis.text         = element_text(color = pal$axis_text, size = 10.5,
                                       face = "plain", lineheight = 0.9),
      axis.text.y       = element_text(margin = margin(r = 4), hjust = 1),
      axis.text.x       = element_text(margin = margin(t = 3)),
      axis.title        = element_text(color = pal$axis_title, size = 11.5),
      axis.title.y      = element_text(margin = margin(r = 6), angle = 90),
      axis.title.x      = element_text(margin = margin(t = 5)),
      plot.title        = element_text(color = pal$title, face = "bold",
                                       size = 13.5, hjust = 0.5,
                                       margin = margin(b = 7)),
      plot.margin       = margin(t = 6, r = 10, b = 4, l = 4),
      legend.text       = element_text(color = pal$axis_text, size = 9.5),
      legend.title      = element_text(color = pal$axis_title, size = 10)
    )
}

.treat_vec <- function(treat_dates) {
  if (length(treat_dates) == 0) return(as.Date(character()))
  tv <- as.Date(treat_dates, format = "%Y%m%d")
  tv[!is.na(tv)]
}

# Add a chronological time-step factor (labelled by the step's mean date).
.add_step_factor <- function(long) {
  long[, gdate := mean(scan_date), by = grp]
  labs_chr <- format(long$gdate, "%Y-%m-%d")
  long[, grp_lab := factor(labs_chr,
                           levels = unique(labs_chr[order(long$gdate)]))]
  long
}

# Interpolate treatment dates onto a discrete (factor) time axis.
.treat_positions <- function(dates_sorted, tvec) {
  if (length(tvec) == 0 || length(dates_sorted) == 0) return(numeric())
  idx  <- seq_along(dates_sorted)
  xpos <- approx(as.numeric(dates_sorted), idx,
                 xout = as.numeric(tvec), rule = 1)$y
  xpos[!is.na(xpos)]
}

# Per-scan table carried on every plot for the CSV download. Always the RAW
# observations (one row per scan, as the "individual" mode plots them) rather
# than whatever the current mode happens to aggregate to - the mean/sd of a
# time series can be recomputed from these rows, but not the reverse. The
# value column is named for the metric so the file says what it holds.
.export_table <- function(long, y_label) {
  if (nrow(long) == 0) return(long)
  out <- long[, list(site_name, plot, scan_date, time_step = grp, value)]
  setnames(out, "value", y_label)
  out[]
}

#' Build a metric plot for one metric column of `data_dt`.
#'
#' The returned ggplot carries the raw per-scan rows behind it as the
#' "imn_raw" attribute; `register_plot_download()` writes that as the CSV.
#'
#' @param mode "timeseries" | "individual" | "boxplot" | "bar"
#' @param light TRUE swaps the on-screen Aurora palette for the light-ground
#'   EXPORT palette used by the SVG/PNG downloads.
#' @export
metric_series_plot <- function(metric, y_label, data_dt, treat_dates,
                               errorbars_on, treatlines_on,
                               mode = "timeseries", light = FALSE) {

  pal <- if (isTRUE(light)) EXPORT_PAL else SCREEN_PAL

  if (metric %in% names(DERIVED_METRICS)) {
    source_col <- DERIVED_METRICS[[metric]]$source
    transform  <- DERIVED_METRICS[[metric]]$transform
  } else {
    source_col <- metric
    transform  <- identity
  }

  shiny::validate(
    shiny::need(nrow(data_dt) > 0,
                "No data loaded - press Get Data on the Selection Map tab."),
    shiny::need(source_col %in% names(data_dt),
                paste0("Metric '", source_col, "' not found in the data table."))
  )

  tvec <- .treat_vec(treat_dates)
  show_treat <- treatlines_on == "on" && length(tvec) > 0

  # Raw rows, computed once: the CSV payload for every mode, and the plotting
  # frame for the two modes that draw individual observations.
  raw <- assign_time_steps(data_dt, source_col, treat_dates, transform)
  export_dt <- .export_table(raw, y_label)

  # ---- Individual plot time series --------------------------------------
  plt <- if (mode == "individual") {
    long <- raw
    shiny::validate(shiny::need(
      nrow(long) > 0, "No valid values for this metric in the loaded scans."))

    dr <- as.numeric(diff(range(long$scan_date)))
    jw <- max(1, dr / 120)

    p <- ggplot(long, aes(x = scan_date, y = value,
                          color = label, group = label))
    if (show_treat) {
      p <- p + geom_vline(xintercept = tvec, color = pal$treat,
                          linetype = "solid", linewidth = TREAT_LW)
    }
    p +
      geom_line(linewidth = 0.7, na.rm = TRUE) +
      geom_point(shape = DIAMOND, size = POINT_SZ, stroke = 0.5,
                 color = pal$stroke, aes(fill = label),
                 position = position_jitter(width = jw, height = 0, seed = 42),
                 na.rm = TRUE) +
      scale_x_date(expand = expansion(mult = 0.05)) +
      y_scale() +
      labs(x = "Scan date", y = y_label, title = y_label,
           color = "Site / Plot", fill = "Site / Plot") +
      aurora_theme(pal) +
      theme(legend.position = "right", legend.key.size = unit(0.9, "lines"))

  # ---- Box & whisker per time step (time on X) --------------------------
  } else if (mode == "boxplot") {
    long <- raw
    shiny::validate(shiny::need(
      nrow(long) > 0, "No valid values for this metric in the loaded scans."))
    .add_step_factor(long)

    p <- ggplot(long, aes(x = grp_lab, y = value))
    if (show_treat) {
      xpos <- .treat_positions(sort(unique(long$gdate)), tvec)
      if (length(xpos) > 0) {
        p <- p + geom_vline(xintercept = xpos, color = pal$treat,
                            linetype = "solid", linewidth = TREAT_LW)
      }
    }
    p +
      geom_boxplot(fill = pal$boxfill, color = pal$txt_dim, width = 0.6,
                   outlier.shape = NA, na.rm = TRUE) +
      geom_point(shape = DIAMOND, size = POINT_SZ - 0.9, stroke = 0.4,
                 color = pal$stroke, fill = pal$point,
                 position = position_jitter(width = 0.12, height = 0, seed = 42),
                 na.rm = TRUE) +
      y_scale() +
      labs(x = "Scan date (time step)", y = y_label, title = y_label) +
      aurora_theme(pal) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))

  # ---- Bar: mean per time step (time on X) ------------------------------
  } else if (mode == "bar") {
    smry <- build_metric_series(data_dt, source_col, treat_dates, transform)
    shiny::validate(shiny::need(
      nrow(smry) > 0, "No valid values for this metric in the loaded scans."))
    labs_chr <- format(smry$t, "%Y-%m-%d")
    smry[, lab := factor(labs_chr, levels = unique(labs_chr[order(smry$t)]))]

    p <- ggplot(smry, aes(x = lab, y = mean))
    if (show_treat) {
      xpos <- .treat_positions(sort(smry$t), tvec)
      if (length(xpos) > 0) {
        p <- p + geom_vline(xintercept = xpos, color = pal$treat,
                            linetype = "solid", linewidth = TREAT_LW)
      }
    }
    p <- p + geom_col(fill = pal$accent, width = 0.7, alpha = 0.85)
    if (errorbars_on == "on") {
      p <- p + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                             width = 0.3, color = pal$txt_dim, na.rm = TRUE)
    }
    p +
      y_scale() +
      labs(x = "Scan date (time step)", y = y_label, title = y_label) +
      aurora_theme(pal) +
      theme(axis.text.x = element_text(angle = 35, hjust = 1))

  # ---- Time series (default) --------------------------------------------
  } else {
    smry <- build_metric_series(data_dt, source_col, treat_dates, transform)
    shiny::validate(shiny::need(
      nrow(smry) > 0, "No valid values for this metric in the loaded scans."))

    p <- ggplot(smry, aes(x = t, y = mean))
    if (show_treat) {
      p <- p + geom_vline(xintercept = tvec, color = pal$treat,
                          linetype = "solid", linewidth = TREAT_LW)
    }
    p <- p + geom_line(color = pal$accent, linewidth = 0.9)
    if (errorbars_on == "on") {
      p <- p + geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd),
                             width = 5, color = pal$txt_dim, na.rm = TRUE)
    }
    p +
      geom_point(shape = DIAMOND, size = POINT_SZ, stroke = 0.6,
                 color = pal$stroke, fill = pal$point, na.rm = TRUE) +
      scale_x_date(limits = range(smry$t), expand = expansion(mult = 0.05)) +
      y_scale() +
      labs(x = "Scan date", y = y_label, title = y_label) +
      aurora_theme(pal)
  }

  attr(plt, "imn_raw") <- export_dt
  plt
}

#' Wrap a `metric_series_plot()` card with a "Download" menu pinned to its
#' lower-right corner (CSV of the underlying data, or an SVG/PNG of the
#' rendered plot). Pair with `register_plot_download()` in the module server.
#' @export
plot_card_ui <- function(ns, id, height = "100%") {
  div(
    class = "imn-plot-wrap",
    plotOutput(ns(id), height = height),
    div(
      class = "dropdown imn-plot-dl",
      tags$button(
        class = "btn btn-sm dropdown-toggle", type = "button",
        `data-bs-toggle` = "dropdown", `aria-expanded` = "false",
        "Download"
      ),
      tags$ul(
        class = "dropdown-menu dropdown-menu-end",
        tags$li(downloadLink(ns(paste0(id, "_dl_csv")), "CSV data",
                             class = "dropdown-item")),
        tags$li(downloadLink(ns(paste0(id, "_dl_svg")), "SVG image",
                             class = "dropdown-item")),
        tags$li(downloadLink(ns(paste0(id, "_dl_png")), "PNG image",
                             class = "dropdown-item"))
      )
    )
  )
}

#' Register the three download handlers behind `plot_card_ui()`'s menu.
#'
#' `plot_fn` is a function of one argument, `light`, returning the ggplot from
#' `metric_series_plot()`. The image handlers call it with `light = TRUE` so
#' the download is rebuilt on the light-ground EXPORT palette - the on-screen
#' Aurora colours are near-invisible on the white page an SVG/PNG lands on.
#' The CSV comes from the plot's "imn_raw" attribute: the raw per-scan rows,
#' regardless of which mode is on screen.
#' @export
register_plot_download <- function(output, id, plot_fn, filename_prefix) {
  output[[paste0(id, "_dl_csv")]] <- downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".csv"),
    content  = function(file) fwrite(attr(plot_fn(), "imn_raw"), file)
  )
  output[[paste0(id, "_dl_svg")]] <- downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".svg"),
    content  = function(file) {
      ggsave(file, plot = plot_fn(light = TRUE), device = "svg",
             width = 9, height = 5.5, bg = "white")
    }
  )
  output[[paste0(id, "_dl_png")]] <- downloadHandler(
    filename = function() paste0(filename_prefix, "_", Sys.Date(), ".png"),
    content  = function(file) {
      ggsave(file, plot = plot_fn(light = TRUE), device = "png",
             width = 9, height = 5.5, dpi = 200, bg = "white")
    }
  )

  # These links live inside a collapsed Bootstrap dropdown, so Shiny sees them
  # as hidden and suspends them - the download URL never reaches the client and
  # every item renders permanently disabled. Opening the menu does not trigger
  # Shiny's visibility recalculation (that fires for tabs, not dropdowns), so
  # the suspension has to be turned off outright.
  for (suffix in c("_dl_csv", "_dl_svg", "_dl_png")) {
    outputOptions(output, paste0(id, suffix), suspendWhenHidden = FALSE)
  }
}
