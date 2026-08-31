# app/view/selection_map.R
# ---------------------------------------------------------------------------
# Selection Map tab. Owns plot selection on the Leaflet map and the two data
# pulls (Get Scans, Get Data). Writes results into the shared `state` store
# (scan_calls, metrics, tree_inventory, additional_models,
# additional_models_wide, treatment_dates); other tabs read from it.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
  gridlayout[grid_container, grid_card],
  leaflet[...],
  data.table[...],
)
box::use(
  app/logic/api_client[
    populate_scan_calls, fetch_metrics_for_scan,
    fetch_tree_inventory_for_scan, fetch_models_for_scan
  ],
  app/logic/series[parse_treatment_dates, reshape_models_wide],
  app/logic/constants[LABEL_THRESHOLD, MIN_ZOOM_LABELS],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c("IntELiMonDSS leaflet_map"),
    row_sizes = c("1fr"),
    col_sizes = c("288px", "1fr"),
    gap_size = "10px",
    grid_card(
      area = "IntELiMonDSS",
      card_header("Select Scans"),
      card_body(
        p(strong("Select up to 10 plots"),
          "by clicking them on the map."),
        div(
          class = "plot-counter",
          textOutput(ns("plot_count"), inline = TRUE)
        ),
        actionButton(ns("btn_clear"), "\u2715  Clear Plots", width = "100%"),
        actionButton(ns("btn_get_scans"), "\u2630  Get Scans", width = "100%"),
        dateRangeInput(
          inputId = ns("daterange"),
          label   = "Select date range",
          start   = "2021-01-01",
          end     = Sys.Date(),
          min     = "2021-01-01",
          max     = Sys.Date(),
          format  = "yyyy-mm-dd"
        ),
        actionButton(ns("btn_get_data"), "\u2913  Get Data", width = "100%"),
        textInput(
          inputId = ns("treatment_dates_text"),
          label   = "Treatment dates",
          value   = "",
          placeholder = "YYYYMMDD, YYYYMMDD..."
        ),
        actionButton(ns("btn_submit_dates"), "Submit treatments", width = "100%")
      )
    ),
    grid_card(
      area = "leaflet_map",
      full_screen = TRUE,
      card_header("IntELiMon Plot Locations"),
      card_body(
        padding = 0,
        div(
          style = "position: relative; height: 100%;",
          leafletOutput(ns("map"), height = "100%"),
          div(
            class = "map-btn-container",
            style = "top: 80px;",
            actionButton(ns("btn_reset"), "\u27F3  Refresh Zoom", class = "btn")
          )
        )
      )
    )
  )
}

#' @export
server <- function(id, state) {
  moduleServer(id, function(input, output, session) {

    plots <- state$plots

    # -- Selection counter ---------------------------------------------------
    n_selected <- reactive({
      uniqueN(state$scan_calls(), by = c("site_name", "plot"))
    })

    output$plot_count <- renderText({
      sprintf("%d / 10 plots selected", n_selected())
    })

    # -- Date range filter ---------------------------------------------------
    # Filters scan_calls down to scans whose date_code (YYYYMMDD) falls inside
    # the selected range. Rows not yet populated (empty date_code) are kept so
    # freshly clicked plots are never hidden.
    filtered_scan_calls <- reactive({
      df <- copy(state$scan_calls())
      if (nrow(df) == 0) return(df)

      df[, .scan_date := as.Date(as.character(date_code), format = "%Y%m%d")]
      df <- df[is.na(.scan_date) |
                 (.scan_date >= input$daterange[1] &
                    .scan_date <= input$daterange[2])]
      df[, .scan_date := NULL]
      df[]
    })

    # -- Treatment dates -----------------------------------------------------
    apply_treatment_text <- function(txt) {
      parsed <- parse_treatment_dates(txt)

      if (length(parsed$bad) > 0) {
        showNotification(
          paste("Ignoring invalid date(s):",
                paste(parsed$bad, collapse = ", "), "- use YYYYMMDD."),
          type = "warning", duration = 6
        )
      }

      state$treatment_dates(parsed$ok)

      if (length(parsed$ok) > 0) {
        showNotification(
          sprintf("Stored %d treatment date(s): %s",
                  length(parsed$ok), paste(parsed$ok, collapse = ", ")),
          type = "message", duration = 5
        )
      } else if (length(parsed$bad) == 0) {
        showNotification("Treatment dates cleared.", type = "message",
                         duration = 4)
      }
    }

    observeEvent(input$btn_submit_dates, {
      apply_treatment_text(input$treatment_dates_text)
    })

    # -- Base map (rendered once) -------------------------------------------
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(preferCanvas = TRUE)) |>
        addProviderTiles(
          providers$Esri.WorldImagery,
          group = "Satellite",
          options = providerTileOptions(maxZoom = 20)
        ) |>
        addTiles(
          urlTemplate = "https://basemaps.cartocdn.com/rastertiles/voyager/{z}/{x}/{y}.png?key=cb1_2nr5_1_392ce3984694e7a58625372e",
          group = "Political map",
          options = tileOptions(maxZoom = 20),
          attribution = paste(
            '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors',
            '&copy; <a href="https://carto.com/attributions">CARTO</a>'
          )
        ) |>
        addLayersControl(
          baseGroups = c("Satellite", "Political map"),
          options    = layersControlOptions(collapsed = FALSE),
          position   = "topright"
        ) |>
        fitBounds(lng1 = -179.5, lat1 = 24.0, lng2 = -66.9, lat2 = 71.5)
    })

    # Plots visible in the current map bounds
    visible_plots <- reactive({
      bounds <- input$map_bounds
      if (is.null(bounds)) return(plots[0])
      plots[
        Latitude  >= bounds$south & Latitude  <= bounds$north &
          Longitude >= bounds$west & Longitude <= bounds$east
      ]
    })

    # Redraw markers whenever the view changes
    observeEvent(input$map_bounds, {
      vp <- visible_plots()
      show_labels <- nrow(vp) <= LABEL_THRESHOLD &&
        !is.null(input$map_zoom) &&
        input$map_zoom >= MIN_ZOOM_LABELS

      proxy <- leafletProxy("map", session) |>
        clearGroup("plot-labels") |>
        clearGroup("plot-circles")

      if (nrow(vp) == 0) return()

      if (show_labels) {
        proxy |>
          addCircleMarkers(
            data = vp, lng = ~Longitude, lat = ~Latitude,
            group = "plot-labels", radius = 0,
            color = "transparent", fillColor = "transparent",
            fillOpacity = 0, opacity = 0,
            label = as.character(vp$plot),
            labelOptions = labelOptions(
              noHide = TRUE, direction = "center", textOnly = TRUE,
              className = "plot-label", offset = c(0, 0)
            )
          ) |>
          addCircleMarkers(
            data = vp, lng = ~Longitude, lat = ~Latitude,
            group = "plot-circles",
            layerId = ~paste(site_name, plot, sep = "||"),
            radius = 8, color = "black", weight = 1,
            fillColor = "red", fillOpacity = 0.85, opacity = 1
          )
      } else {
        proxy |>
          addCircleMarkers(
            data = vp, lng = ~Longitude, lat = ~Latitude,
            group = "plot-circles",
            layerId = ~paste(site_name, plot, sep = "||"),
            radius = 8, color = "black", weight = 1,
            fillColor = "red", fillOpacity = 0.85, opacity = 1
          )
      }
    })

    # Handle circle clicks: append unique rows to scan_calls (max 10 plots)
    observeEvent(input$map_marker_click, {
      click <- input$map_marker_click
      if (is.null(click$id)) return()

      parts <- strsplit(click$id, "\\|\\|")[[1]]
      if (length(parts) != 2) return()

      sc <- state$scan_calls()

      already_exists <- nrow(sc[site_name == parts[1] & plot == parts[2]]) > 0
      if (already_exists) return()

      if (uniqueN(sc, by = c("site_name", "plot")) >= 10) {
        showNotification("Plot limit reached (10). Clear plots to select others.",
                         type = "warning", duration = 4)
        return()
      }

      new_row <- data.table(
        site_name  = parts[1],
        plot       = parts[2],
        date_code  = character(1),
        scan_name  = character(1),
        scanner_id = character(1)
      )
      state$scan_calls(rbindlist(list(sc, new_row), use.names = TRUE))
    })

    # Refresh Zoom: fly back to CONUS + Alaska
    observeEvent(input$btn_reset, {
      leafletProxy("map", session) |>
        fitBounds(lng1 = -179.5, lat1 = 24.0, lng2 = -66.9, lat2 = 71.5)
    })

    # Clear Plots: reset scan_calls to empty
    observeEvent(input$btn_clear, {
      state$scan_calls(data.table(
        site_name  = character(),
        plot       = character(),
        date_code  = character(),
        scan_name  = character(),
        scanner_id = character()
      ))
      showNotification("Cleared selected plots.", type = "message", duration = 3)
    })

    # Get Scans: query /scans?site=&plot= for each clicked combo and populate
    # date_code + scan_name. Combos with multiple scans expand into rows.
    observeEvent(input$btn_get_scans, {
      sc <- state$scan_calls()
      if (nrow(sc) == 0) {
        showNotification("No plots selected - click plots on the map first.",
                         type = "warning", duration = 4)
        return()
      }

      clicked <- unique(sc[, .(site_name, plot)])

      withProgress(message = "Querying IntELiMon scans...", value = 0, {
        incProgress(0.1, detail = sprintf("Querying %d plot(s)", nrow(clicked)))
        populated <- populate_scan_calls(clicked)
        incProgress(0.8, detail = "Updating scan_calls")

        if (nrow(populated) == 0) {
          showNotification("No scans found for the selected plots.",
                           type = "warning", duration = 6)
          return()
        }

        state$scan_calls(populated)

        n_missing <- nrow(clicked) -
          uniqueN(populated, by = c("site_name", "plot"))

        msg <- sprintf("Found %d scan(s) across %d plot(s).",
                       nrow(populated),
                       uniqueN(populated, by = c("site_name", "plot")))
        if (n_missing > 0) {
          msg <- paste(msg, sprintf("%d plot(s) had no scans.", n_missing))
        }
        showNotification(msg, type = "message", duration = 6)
      })
    })

    # Get Data: filter scan_calls by date range, then make the per-scan API
    # calls and populate the shared metrics / tree_inventory / models tables.
    observeEvent(input$btn_get_data, {
      df <- filtered_scan_calls()
      df <- df[nzchar(date_code)]   # only Get-Scans-populated rows are queryable

      if (nrow(df) == 0) {
        showNotification(
          "No populated scans in the selected date range - press Get Scans first.",
          type = "warning", duration = 5
        )
        return()
      }

      withProgress(message = "Retrieving scan data...", value = 0, {
        metrics_list <- vector("list", nrow(df))
        trees_list   <- vector("list", nrow(df))
        models_list  <- vector("list", nrow(df))
        step <- 1 / nrow(df)

        for (i in seq_len(nrow(df))) {
          incProgress(step, detail = df$scan_name[i])

          metrics_list[[i]] <- tryCatch(
            fetch_metrics_for_scan(df$site_name[i], df$plot[i],
                                   df$date_code[i], df$scanner_id[i]),
            error = function(e) {
              warning("Metrics query failed for ", df$scan_name[i], ": ",
                      conditionMessage(e), call. = FALSE)
              NULL
            }
          )

          trees_list[[i]] <- tryCatch(
            fetch_tree_inventory_for_scan(df$site_name[i], df$plot[i],
                                          df$date_code[i], df$scanner_id[i]),
            error = function(e) {
              warning("Tree inventory query failed for ", df$scan_name[i], ": ",
                      conditionMessage(e), call. = FALSE)
              NULL
            }
          )

          models_list[[i]] <- tryCatch(
            fetch_models_for_scan(df$site_name[i], df$plot[i],
                                  df$date_code[i], df$scanner_id[i]),
            error = function(e) {
              warning("Models query failed for ", df$scan_name[i], ": ",
                      conditionMessage(e), call. = FALSE)
              NULL
            }
          )

          # -- Call 4: points2pano url (future step) -------------------------
          # pano <- fromJSON(paste0(base_url, "/scan/points2pano?site=", ...))
        }

        metrics <- rbindlist(Filter(Negate(is.null), metrics_list),
                             use.names = TRUE, fill = TRUE)
        tree_inventory <- rbindlist(Filter(Negate(is.null), trees_list),
                                    use.names = TRUE, fill = TRUE)
        additional_models <- rbindlist(Filter(Negate(is.null), models_list),
                                       use.names = TRUE, fill = TRUE)

        state$metrics(metrics)
        state$tree_inventory(tree_inventory)
        state$additional_models(additional_models)
        state$additional_models_wide(reshape_models_wide(additional_models))

        n_ok <- function(x) sum(!vapply(x, is.null, logical(1)))
        showNotification(
          sprintf(paste("Of %d scan(s) in range: metrics for %d,",
                        "tree inventories for %d, model sets for %d."),
                  nrow(df), n_ok(metrics_list), n_ok(trees_list),
                  n_ok(models_list)),
          type = if (n_ok(metrics_list) > 0) "message" else "warning",
          duration = 8
        )
      })
    })

    # Expose the date-range-filtered scans in case other modules ever need it
    filtered_scan_calls
  })
}
