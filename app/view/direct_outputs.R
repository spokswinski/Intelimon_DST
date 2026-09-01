# app/view/direct_outputs.R
# ---------------------------------------------------------------------------
# Direct outputs tab. Three metric time-series cards (tree / volume / canopy)
# plus the Points2Pano iframe viewer. Reads metrics + treatment_dates +
# scan_calls from the shared state. The "Revise treatments" control also lives
# here and writes back to state$treatment_dates.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
  gridlayout[grid_container, grid_card],
  data.table[...],
)
box::use(
  app/logic/plotting[metric_series_plot, plot_card_ui, register_plot_download],
  app/logic/series[parse_treatment_dates],
  app/logic/constants[
    TREE_METRIC_LABELS, VOLUME_METRIC_LABELS, CANOPY_METRIC_LABELS,
    PANO_CROP_TOP, PANO_CROP_BOTTOM, PANO_CROP_LEFT, PANO_CROP_RIGHT
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c("IntELiMonDSS directOutputs"),
    row_sizes = c("1fr"),
    col_sizes = c("258px", "1fr"),
    gap_size = "10px",
    grid_card(
      area = "IntELiMonDSS",
      card_header("Select Statistics"),
      card_body(
        selectInput(ns("plot_mode"), "Plot type",
                    choices = list(
                      "Time series"                 = "timeseries",
                      "Time series individual plot"  = "individual",
                      "Box and Whisker"             = "boxplot",
                      "Bar"                          = "bar"),
                    selected = "timeseries", width = "100%"),
        selectInput(ns("data_type"), "Data type",
                    choices = list(
                      "Raw data"       = "raw",
                      "Percent change" = "percent"),
                    selected = "raw", width = "100%"),
        radioButtons(ns("show_treatments"), "Treatment date lines",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "on", inline = TRUE, width = "100%"),
        radioButtons(ns("show_errorbars"), "Error bars",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "on", inline = TRUE, width = "100%"),
        selectInput(ns("treeStatistics"), "Tree statistics",
          choices = list(
            "Basal area"          = "Basalarea",
            "Mean DBH"            = "MDBH",
            "Stems per acre"      = "StemsPacre",
            "Number of trees"     = "TreesN",
            "Mean tree height"    = "MeanTH",
            "Maximum tree height" = "MaxTH"
          ),
          selected = "Basalarea"),
        selectInput(ns("volumeStatistics"), "Volume statistics",
          choices = list(
            "Ground cover volume" = "mGCvol",
            "Understory volume"   = "mUSvol",
            "Midstory volume"     = "mMSvol",
            "Overstory volume"    = "mOSvol"
          ),
          selected = "mGCvol"),
        selectInput(ns("canopyStatistics"), "Canopy statistics",
          choices = list(
            "Canopy base height" = "CBH",
            "Canopy cover"       = "canopyCover",
            "Gap fraction"       = "gapFraction",
            "Leaf area index"    = "LAI",
            "Overstory LAI"      = "OLAI",
            "Midstory LAI"       = "MLAI",
            "Understory LAI"     = "ULAI"
          ),
          selected = "CBH"),
        textInput(ns("treatment_revision_text"), "Revise treatment dates",
                  value = "", placeholder = "YYYYMMDD, YYYYMMDD..."),
        actionButton(ns("btn_revise_treatments"), "Revise treatments",
                     width = "100%")
      )
    ),
    grid_card(
      area = "directOutputs",
      card_body(
        grid_container(
          layout = c(
            "treeStats   volumeStats",
            "canopyStats panoViewer "
          ),
          row_sizes = c("1fr", "1fr"),
          col_sizes = c("1fr", "1fr"),
          gap_size = "10px",
          grid_card(area = "treeStats", full_screen = TRUE,
                    card_body(plot_card_ui(ns, "treeStats"))),
          grid_card(area = "canopyStats", full_screen = TRUE,
                    card_body(plot_card_ui(ns, "canopyStats"))),
          grid_card(area = "volumeStats", full_screen = TRUE,
                    card_body(plot_card_ui(ns, "volumeStats"))),
          grid_card(
            area = "panoViewer",
            full_screen = TRUE,
            card_header(
              class = "d-flex justify-content-between align-items-center",
              span("Points2Pano"),
              div(
                class = "d-flex align-items-center gap-2",
                actionButton(ns("btn_pano_prev"), "\u25C0", class = "btn-sm"),
                div(class = "pano-info",
                    textOutput(ns("pano_label"), inline = TRUE)),
                actionButton(ns("btn_pano_next"), "\u25B6", class = "btn-sm")
              )
            ),
            card_body(
              padding = 0,
              uiOutput(ns("pano_frame"), style = "height: 100%;")
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

    # -- Revise treatments (writes shared state) -----------------------------
    observeEvent(input$btn_revise_treatments, {
      parsed <- parse_treatment_dates(input$treatment_revision_text)
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
    })

    # -- Metric time-series cards -------------------------------------------
    # Each card is a builder taking `light`: the screen render uses the Aurora
    # palette, the SVG/PNG downloads re-run it light for a white page.
    metric_card <- function(id, input_id, labels, prefix) {
      plot_fn <- function(light = FALSE) {
        key <- input[[input_id]]
        metric_series_plot(key, labels[[key]], state$metrics(),
                           state$treatment_dates(),
                           input$show_errorbars, input$show_treatments,
                           input$plot_mode, input$data_type, light = light)
      }
      output[[id]] <- renderPlot(plot_fn(), bg = "transparent", res = 110)
      register_plot_download(output, id, plot_fn, prefix)
    }

    metric_card("treeStats",   "treeStatistics",   TREE_METRIC_LABELS,   "tree_stats")
    metric_card("volumeStats", "volumeStatistics", VOLUME_METRIC_LABELS, "volume_stats")
    metric_card("canopyStats", "canopyStatistics", CANOPY_METRIC_LABELS, "canopy_stats")

    # -- Points2Pano viewer --------------------------------------------------
    pano_idx <- reactiveVal(1)

    # Populated scans available to the viewer (in scan_calls order)
    pano_scans <- reactive({
      sc <- state$scan_calls()
      sc[nzchar(date_code)]
    })

    # Reset to the first record whenever scan_calls changes
    observeEvent(state$scan_calls(), {
      pano_idx(1)
    })

    observeEvent(input$btn_pano_prev, {
      n <- nrow(pano_scans())
      if (n == 0) return()
      pano_idx(if (pano_idx() <= 1) n else pano_idx() - 1)   # wrap
    })

    observeEvent(input$btn_pano_next, {
      n <- nrow(pano_scans())
      if (n == 0) return()
      pano_idx(if (pano_idx() >= n) 1 else pano_idx() + 1)   # wrap
    })

    output$pano_label <- renderText({
      df <- pano_scans()
      if (nrow(df) == 0) return("No scans loaded")

      row <- df[min(pano_idx(), nrow(df))]
      date_fmt <- format(as.Date(row$date_code, format = "%Y%m%d"), "%m-%d-%Y")

      sprintf("Site: %s | Plot: %s | %s | Scanner: %s",
              row$site_name, row$plot, date_fmt, row$scanner_id)
    })

    output$pano_frame <- renderUI({
      df <- pano_scans()

      if (nrow(df) == 0) {
        return(div(
          style = "display:flex; align-items:center; justify-content:center;
                   height:100%; color:#888; text-align:center; padding:20px;",
          "No scans loaded - select plots on the Selection Map tab and press Get Scans."
        ))
      }

      idx <- min(pano_idx(), nrow(df))
      row <- df[idx]

      pano_url <- sprintf(
        "https://burnpro3d.sdsc.edu/points2pano/?plot=%s_%s&ts=%s&m=Basalarea",
        row$site_name, row$plot, row$date_code
      )

      div(
        style = "width:100%; height:100%; overflow:hidden; position:relative;",
        tags$iframe(
          src = pano_url,
          style = sprintf(
            "position:absolute; top:-%dpx; left:-%dpx;
             width:calc(100%% + %dpx); height:calc(100%% + %dpx); border:none;",
            PANO_CROP_TOP, PANO_CROP_LEFT,
            PANO_CROP_LEFT + PANO_CROP_RIGHT,
            PANO_CROP_TOP + PANO_CROP_BOTTOM
          ),
          title = paste("Points2Pano:", row$scan_name)
        )
      )
    })
  })
}
