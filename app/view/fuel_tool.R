# app/view/fuel_tool.R
# ---------------------------------------------------------------------------
# Fuel tool tab. Surface / canopy / time-lag fuel cards prefilled from the
# loaded scans, plus an AOI draw map. Reads metrics + additional_models_wide
# from shared state and writes the drawn polygon to state$aoi_polygon.
# Dynamic input ids built inside renderUI are namespaced with session$ns.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
  gridlayout[grid_container, grid_card],
  data.table[...],
  leaflet[...],
  leaflet.extras[...],
  stats[setNames],
)
box::use(
  app/logic/fuel[
    SURFACE_FUEL_MODELS, COVER_CLASS_MODELS, TIMELAG_FUEL_MODELS,
    CANOPY_FUEL_ROWS, BROWN_CLASSES, brown_class_load
  ],
  app/logic/fuel_models[
    fuel_model_lookup, fuel_model_choices, fuel_model_bed
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c("IntELiMonDSS fuelSpace"),
    row_sizes = c("1fr"),
    col_sizes = c("264px", "1fr"),
    gap_size = "10px",
    grid_card(
      area = "IntELiMonDSS",
      card_header("Fuel data source"),
      card_body(
        radioButtons(ns("fuel_source"), label = NULL,
          choices = list("Modeled/User defined" = "modeled",
                         "LANDFIRE derived"      = "landfire"),
          selected = "modeled", width = "100%"),
        radioButtons(ns("fuel_agg"), "Plot aggregation",
          choices = list("Most recent per plot" = "recent",
                         "Mean of all scans"    = "all"),
          selected = "recent", width = "100%"),
        selectInput(ns("cover_class"), "Surface fuel cover class",
          choices = setNames(
            names(COVER_CLASS_MODELS),
            vapply(COVER_CLASS_MODELS, `[[`, character(1), "label")
          )),

        # LANDFIRE fuel-model picker. Rendered server-side rather than with a
        # conditionalPanel so it does not depend on a namespaced id resolving
        # inside a JS expression.
        uiOutput(ns("fbfm_block")),

        tags$hr(style = "margin:6px 0;"),
        tags$strong("Send to rothRmel"),
        actionButton(ns("submit_fuels"), "\u2191  Submit fuel values",
                     width = "100%", class = "btn-primary"),
        uiOutput(ns("submit_status"))
      )
    ),
    grid_card(
      area = "fuelSpace",
      card_body(
        grid_container(
          layout = c(
            "surfaceFuel canopyFuel",
            "timelagFuel emptyCard "
          ),
          row_sizes = c("1fr", "1fr"),
          col_sizes = c("1fr", "1fr"),
          gap_size = "10px",
          grid_card(area = "surfaceFuel", class = "fuel-card",
                    card_header("Surface fuel models"),
                    card_body(uiOutput(ns("surface_fuel_ui")))),
          grid_card(area = "canopyFuel", class = "fuel-card",
                    card_header("Canopy fuels"),
                    card_body(uiOutput(ns("canopy_fuel_ui")))),
          grid_card(area = "timelagFuel", class = "fuel-card",
                    card_header("Time lag fuels"),
                    card_body(uiOutput(ns("timelag_fuel_ui")))),
          grid_card(area = "emptyCard", class = "fuel-card",
            card_body(
              div(style = "display:flex; height:100%; gap:10px;",
                div(style = "flex:1; min-width:0;",
                    leafletOutput(ns("aoi_map"), height = "100%")),
                div(style = "width:150px; flex:none; display:flex;
                             flex-direction:column; gap:6px;",
                    actionButton(ns("btn_fuel_dud1"), "Download LCP", width = "100%"),
                    actionButton(ns("btn_fuel_dud2"), "Download FastFuels", width = "100%"),
                    actionButton(ns("btn_fuel_dud3"), "Download fuel report", width = "100%"),
                    actionButton(ns("btn_fuel_dud4"), "Download FCCS", width = "100%")
                )
              )
            )
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, state) {
  moduleServer(id, function(input, output, session) {

    # Aggregate a source table to one row per column, honoring the plot-
    # aggregation radio (most recent scan per site/plot, or all scans).
    aggregate_scans <- function(src) {
      if (nrow(src) == 0) return(data.table())
      m <- as.data.table(copy(src))
      if (input$fuel_agg == "recent") {
        m[, .d := as.Date(date_code, format = "%Y%m%d")]
        setorder(m, site_name, plot, -.d)
        m <- m[, .SD[1], by = .(site_name, plot)]
        m[, .d := NULL]
      }
      m
    }

    fuel_scans  <- reactive({ aggregate_scans(state$metrics()) })
    fuel_models <- reactive({ aggregate_scans(state$additional_models_wide()) })

    # Mean of a column, looked up in whichever table holds it. Direct scan
    # metrics (CBH, canopyCover, LF_*) live in `metrics`; the fuel/cover/time-
    # lag MODELS (MFBDmod, onehrmod, Grassmod, ...) live in the wide models
    # table. Falls back across both so column placement doesn't matter.
    fuel_mean <- function(col) {
      for (m in list(fuel_scans(), fuel_models())) {
        if (nrow(m) > 0 && col %in% names(m)) {
          v <- suppressWarnings(as.numeric(m[[col]]))
          if (!all(is.na(v))) return(mean(v, na.rm = TRUE))
        }
      }
      NA_real_
    }

    # One label + editable numeric box row (dynamic id -> namespaced).
    fuel_input_row <- function(id_suffix, label, value) {
      ns  <- session$ns
      val <- if (is.na(value)) NA else round(value, 3)
      div(class = "fuel-row",
          tags$span(label, class = "fuel-label"),
          div(style = "width:76px; flex:none;",
              tags$div(class = "compact-num",
                       numericInput(ns(id_suffix), label = NULL, value = val)))
      )
    }

    # ---- LANDFIRE fuel model resolution -----------------------------------
    # The scan metrics carry the LANDFIRE fire behavior fuel model codes:
    #   LF_FBFM13 -> Anderson 13, LF_FBFM40 -> Scott & Burgan 40.
    # Take the modal value across the aggregated scans.
    lf_fbfm_code <- reactive({
      col <- if (identical(input$fbfm_system, "FBFM13")) "LF_FBFM13" else "LF_FBFM40"
      for (m in list(fuel_scans(), fuel_models())) {
        if (nrow(m) > 0 && col %in% names(m)) {
          v <- m[[col]]
          v <- v[!is.na(v) & nzchar(as.character(v))]
          if (length(v) > 0) {
            tt <- sort(table(as.character(v)), decreasing = TRUE)
            return(names(tt)[1])
          }
        }
      }
      NA_character_
    })

    # The resolved model: the scan's LANDFIRE code unless the user overrides it
    active_fuel_model <- reactive({
      sys <- if (identical(input$fbfm_system, "FBFM13")) "FBFM13" else "FBFM40"
      ov  <- input$fbfm_override
      valid <- unlist(fuel_model_choices(sys), use.names = FALSE)
      key <- if (!is.null(ov) && nzchar(ov) && ov %in% valid) ov else lf_fbfm_code()
      fuel_model_lookup(key, sys)
    })

    # Sidebar block shown only in LANDFIRE mode: which classification to use
    # (Anderson 13 and Scott & Burgan 40 are separate systems and are NOT
    # interchangeable, so the choice is explicit), the code resolved from the
    # scan, and an override.
    output$fbfm_block <- renderUI({
      ns <- session$ns
      if (!identical(input$fuel_source, "landfire")) return(NULL)

      sys  <- if (identical(input$fbfm_system, "FBFM13")) "FBFM13" else "FBFM40"
      code <- lf_fbfm_code()

      tagList(
        tags$hr(style = "margin:6px 0;"),
        tags$strong("LANDFIRE fuel model"),
        radioButtons(ns("fbfm_system"), NULL,
          choices = list("Scott & Burgan 40 (LF_FBFM40)" = "FBFM40",
                         "Anderson 13 (LF_FBFM13)" = "FBFM13"),
          selected = sys, width = "100%"),
        div(class = "imn-fnote",
            "The two systems are separate classifications - pick the one you",
            " intend to model with; they are not interchangeable."),
        div(class = "imn-fread",
            div(tags$span("From scan"),
                tags$b(if (is.na(code)) "not reported" else as.character(code)))),
        selectInput(ns("fbfm_override"), "Override",
                    choices = c("Use scan value" = "",
                                fuel_model_choices(sys)),
                    # a code from the other system is not valid here, so an
                    # override only survives a system switch if it exists in
                    # the newly selected classification
                    selected = {
                      cur <- input$fbfm_override
                      valid <- unlist(fuel_model_choices(sys), use.names = FALSE)
                      if (is.null(cur) || !nzchar(cur) || !(cur %in% valid)) ""
                      else cur
                    },
                    width = "100%")
      )
    })

    # ---- Surface fuel models card ----
    # In LANDFIRE mode the modelled surface-fuel rows are replaced by the
    # standard fire behavior fuel model resolved from LF_FBFM13/LF_FBFM40.
    output$surface_fuel_ui <- renderUI({
      landfire <- input$fuel_source == "landfire"

      if (landfire) {
        fm <- active_fuel_model()
        if (is.null(fm)) {
          return(div(class = "imn-fnote",
                     paste("No LANDFIRE fire behavior fuel model available for",
                           "the loaded scans. Pick one from the sidebar, or",
                           "switch to Modeled/User defined.")))
        }
        bed <- fuel_model_bed(fm)
        lr <- function(k, v) div(class = "fuel-row",
                                 tags$span(k, class = "fuel-label"),
                                 tags$span(v, class = "imn-fval"))
        return(tagList(
          div(class = "imn-fbfm-head",
              tags$b(paste0(fm$code, " \u2014 ", fm$name)),
              tags$span(class = "imn-fbfm-sys",
                        if (identical(input$fbfm_system, "FBFM13"))
                          "Anderson 13" else "Scott & Burgan 40")),
          lr("1-hour load (t/ac)",   sprintf("%.2f", bed$load_tonsac[["d1"]])),
          lr("10-hour load (t/ac)",  sprintf("%.2f", bed$load_tonsac[["d10"]])),
          lr("100-hour load (t/ac)", sprintf("%.2f", bed$load_tonsac[["d100"]])),
          lr("Live herb load (t/ac)", sprintf("%.2f", bed$load_tonsac[["herb"]])),
          lr("Live woody load (t/ac)", sprintf("%.2f", bed$load_tonsac[["woody"]])),
          lr("Fuel bed depth (ft)",  sprintf("%.2f", bed$depth_ft)),
          lr("Dead Mx (%)",          sprintf("%.0f", bed$mx_dead_pct)),
          if (isTRUE(bed$non_burnable))
            div(class = "imn-sim-warn", tags$b("Non-burnable model."),
                " This LANDFIRE class carries no fuel; rothRmel will return zero spread.")
        ))
      }

      cover_key <- input$cover_class
      cover_lab <- if (!is.null(cover_key) && nzchar(cover_key))
        COVER_CLASS_MODELS[[cover_key]]$label else "Fine fuels cover %"
      cover_val <- if (is.null(cover_key) || !nzchar(cover_key)) NA
        else fuel_mean(COVER_CLASS_MODELS[[cover_key]]$col)

      rows <- list(fuel_input_row("sf_cover", cover_lab, cover_val))

      for (k in names(SURFACE_FUEL_MODELS)) {
        m   <- SURFACE_FUEL_MODELS[[k]]
        rows <- c(rows, list(fuel_input_row(paste0("sf_", k), m$label,
                                            fuel_mean(m$col))))
      }
      tagList(rows)
    })

    # ---- Time lag fuels card (counts -> Brown tons/acre) ----
    output$timelag_fuel_ui <- renderUI({
      ns <- session$ns
      landfire <- input$fuel_source == "landfire"

      rows <- lapply(names(TIMELAG_FUEL_MODELS), function(k) {
        m   <- TIMELAG_FUEL_MODELS[[k]]
        val <- if (landfire) NA else fuel_mean(m$col)
        cnt <- if (is.na(val)) NA else round(val)   # whole intercepts
        div(class = "fuel-row",
            tags$span(m$label, class = "fuel-label"),
            div(style = "width:58px; flex:none;",
                tags$div(class = "compact-num",
                         numericInput(ns(paste0("tl_", k)), label = NULL, value = cnt))),
            div(style = "width:58px; flex:none; text-align:right; font-size:12px;",
                textOutput(ns(paste0("tl_out_", k)), inline = TRUE))
        )
      })

      tagList(div(class = "timelag-card",
        div(class = "fuel-row", style = "font-weight:bold;",
            tags$span("", class = "fuel-label"),
            tags$span("Count", style = "width:58px; font-size:11px;"),
            tags$span("Tons/acre", style = "width:58px; text-align:right; font-size:11px;")),
        rows,
        hr(style = "margin:2px 0;"),
        div(class = "fuel-row", style = "font-weight:bold;",
            tags$span("Total", class = "fuel-label"),
            tags$span("", style = "width:58px;"),
            div(style = "width:58px; text-align:right; font-size:12px;",
                textOutput(ns("tl_out_total"), inline = TRUE)))
      ))
    })

    # Per-class tons/acre outputs (gray boxes) + total
    local({
      for (k in names(TIMELAG_FUEL_MODELS)) {
        local({
          key <- k
          output[[paste0("tl_out_", key)]] <- renderText({
            load <- brown_class_load(input[[paste0("tl_", key)]],
                                     BROWN_CLASSES[[key]])
            if (is.na(load)) "-" else sprintf("%.3f", load)
          })
        })
      }
    })

    output$tl_out_total <- renderText({
      total <- sum(vapply(names(TIMELAG_FUEL_MODELS), function(k) {
        load <- brown_class_load(input[[paste0("tl_", k)]], BROWN_CLASSES[[k]])
        if (is.na(load)) 0 else load
      }, numeric(1)))
      sprintf("%.3f", total)
    })

    # ---- Canopy fuels card ----
    output$canopy_fuel_ui <- renderUI({
      landfire <- input$fuel_source == "landfire"

      rows <- lapply(names(CANOPY_FUEL_ROWS), function(k) {
        r <- CANOPY_FUEL_ROWS[[k]]
        # In LANDFIRE mode, CBD comes from LF_CBD; other canopy rows have no
        # LANDFIRE equivalent -> blank/editable.
        val <- if (landfire && r$col != "LF_CBD") NA else fuel_mean(r$col)
        # canopyCover is a 0-1 ratio -> show as percent
        if (r$col == "canopyCover" && !is.na(val)) val <- val * 100
        fuel_input_row(paste0("cf_", k), r$label, val)
      })
      tagList(rows)
    })

    # ---- Submit fuel values to the rothRmel tab ---------------------------
    # Assembles the surface fuel bed currently shown (LANDFIRE standard model,
    # or the modelled/user-edited numeric boxes) plus the canopy values, and
    # writes it to shared state. The rothRmel tab picks it up when its fuel
    # source is set to "Fuel tool values".
    observeEvent(input$submit_fuels, {
      landfire <- identical(input$fuel_source, "landfire")

      num <- function(id, default = NA_real_) {
        v <- suppressWarnings(as.numeric(input[[id]]))
        if (length(v) != 1 || is.na(v)) default else v
      }

      if (landfire) {
        fm <- active_fuel_model()
        if (is.null(fm)) {
          showNotification(
            "No LANDFIRE fuel model resolved - pick one before submitting.",
            type = "warning", duration = 6)
          return()
        }
        bed <- fuel_model_bed(fm)
        bed$source <- "landfire"
        bed$system <- if (identical(input$fbfm_system, "FBFM13"))
          "Anderson 13" else "Scott & Burgan 40"
        bed$label <- paste0(fm$code, " - ", fm$name)
      } else {
        # user-edited / modelled: time-lag counts -> Brown loads, depth in cm
        d1  <- brown_class_load(num("tl_onehr"), BROWN_CLASSES$onehr)
        d10 <- brown_class_load(num("tl_tenhr"), BROWN_CLASSES$tenhr)
        d100 <- brown_class_load(num("tl_hunhr"), BROWN_CLASSES$hunhr)
        depth_cm <- num("sf_mfbd")
        bed <- list(
          load_tonsac = c(
            d1    = if (is.na(d1)) 0 else d1,
            d10   = if (is.na(d10)) 0 else d10,
            d100  = if (is.na(d100)) 0 else d100,
            herb  = 0, woody = 0
          ),
          depth_ft = depth_cm * 0.0328084,
          mx_dead_pct = NULL,     # rothRmel sidebar value is used
          sav = NULL,             # standard SAV set
          source = "modeled",
          system = "Modeled / user defined",
          label = sprintf("user values (depth %.1f cm)", depth_cm),
          non_burnable = FALSE
        )
      }

      # canopy values from the canopy card travel with the bed
      cc <- num("cf_cc")
      bed$cbh_m <- num("cf_cbh")
      # the canopy card shows LF_CBD in its stored form (kg/m^3 x 100);
      # fire_behavior expects kg/m^3, so scale on the way out
      cbd_raw <- num("cf_cbd")
      bed$cbd <- if (is.na(cbd_raw)) NA_real_ else cbd_raw / 100
      bed$canopy_cover_pct <- cc
      bed$aggregation <- if (identical(input$fuel_agg, "recent"))
        "Most recent per plot" else "Mean of all scans"
      bed$submitted_at <- Sys.time()

      if (is.na(bed$depth_ft) || bed$depth_ft <= 0) {
        showNotification(
          "Fuel bed depth is missing or zero - rothRmel cannot spread fire without it.",
          type = "warning", duration = 6)
      }

      state$fuel_tool_values(bed)
      showNotification(
        sprintf("Submitted to rothRmel: %s (%s).", bed$label, bed$aggregation),
        type = "message", duration = 5)
    })

    output$submit_status <- renderUI({
      b <- state$fuel_tool_values()
      if (is.null(b)) {
        return(div(class = "imn-fnote",
                   "Nothing submitted yet. rothRmel will use scan-level fuels."))
      }
      div(class = "imn-okbox",
          tags$b("Submitted. "),
          sprintf("%s \u00B7 %s \u00B7 %s", b$label, b$system,
                  format(b$submitted_at, "%H:%M:%S")))
    })

    # ---- AOI draw map ----
    output$aoi_map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(providers$Esri.WorldImagery) |>
        fitBounds(-125, 24, -66, 50) |>          # CONUS default
        addDrawToolbar(
          targetGroup       = "aoi",
          polygonOptions    = drawPolygonOptions(),
          rectangleOptions  = drawRectangleOptions(),
          polylineOptions   = FALSE,
          circleOptions     = FALSE,
          markerOptions     = FALSE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions(edit = TRUE, remove = TRUE)
        )
    })

    # Center the AOI map on the selected plots when metrics load
    observeEvent(state$metrics(), {
      m <- fuel_scans()
      if (nrow(m) == 0) return()
      coords <- as.data.table(state$plots)[
        paste(site_name, plot) %in% unique(paste(m$site_name, m$plot)),
        .(Longitude, Latitude)
      ]
      if (nrow(coords) == 0) return()
      leafletProxy("aoi_map", session) |>
        fitBounds(min(coords$Longitude), min(coords$Latitude),
                  max(coords$Longitude), max(coords$Latitude))
    })

    # Capture drawn/edited features and store as an sf polygon in state.
    store_aoi_feature <- function(feat) {
      if (is.null(feat)) { state$aoi_polygon(NULL); return(invisible()) }

      coords <- feat$geometry$coordinates[[1]]
      ring   <- do.call(rbind, lapply(coords, function(p) c(p[[1]], p[[2]])))
      if (!identical(ring[1, ], ring[nrow(ring), ])) ring <- rbind(ring, ring[1, ])

      poly <- sf::st_sf(
        geometry = sf::st_sfc(sf::st_polygon(list(ring)), crs = 4326)
      )
      state$aoi_polygon(poly)

      area_ha <- as.numeric(sf::st_area(sf::st_transform(poly, 5070))) / 1e4
      showNotification(
        sprintf("AOI stored (%d vertices, %.1f ha).", nrow(ring) - 1, area_ha),
        type = "message", duration = 4
      )
    }

    observeEvent(input$aoi_map_draw_new_feature, {
      store_aoi_feature(input$aoi_map_draw_new_feature)
    })
    observeEvent(input$aoi_map_draw_edited_features, {
      f <- input$aoi_map_draw_edited_features
      if (!is.null(f$features) && length(f$features) > 0) {
        store_aoi_feature(f$features[[length(f$features)]])
      }
    })
    observeEvent(input$aoi_map_draw_deleted_features, {
      state$aoi_polygon(NULL)
      showNotification("AOI cleared.", type = "message", duration = 3)
    })
  })
}
