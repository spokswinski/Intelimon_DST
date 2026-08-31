# app/view/rothrmel.R
# ---------------------------------------------------------------------------
# rothRmel tab. Enter winds / fuel moistures / canopy assumptions in the
# sidebar; three Direct-outputs-style cards plot a chosen fire-behavior metric
# over scan date, and the fourth card is the Points2Pano viewer. Fire behavior
# (Rothermel surface + Van Wagner crown) is computed per scan from the shared
# metrics + additional_models_wide, so treatment-driven changes in fuel and
# canopy structure show up as trends over time.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
  gridlayout[grid_container, grid_card],
  data.table[...],
)
box::use(
  app/logic/plotting[metric_series_plot],
  app/logic/fire_behavior[scan_fire_behavior, FIRE_METRIC_LABELS],
  app/logic/constants[
    PANO_CROP_TOP, PANO_CROP_BOTTOM, PANO_CROP_LEFT, PANO_CROP_RIGHT
  ],
)

# Choices for the three metric dropdowns, grouped surface vs crown.
metric_choices <- list(
  "Surface fire" = list(
    "Rate of spread (ch/hr)"           = "ros_ch_hr",
    "Rate of spread (m/min)"           = "ros_m_min",
    "Fireline intensity (kW/m)"        = "fli_kw_m",
    "Flame length (ft)"                = "flame_ft",
    "Flame length (m)"                 = "flame_m",
    "Reaction intensity (BTU/ft2/min)" = "rxn_int",
    "Heat per unit area (BTU/ft2)"     = "hpa"
  ),
  "Crown fire" = list(
    "Crown-initiation intensity Io (kW/m)" = "crown_Io",
    "Critical active-crown ROS Ro (m/min)" = "crown_Ro",
    "Torching index (mph)"                 = "torching_idx",
    "Crowning index (mph)"                 = "crowning_idx",
    "Crown fire type (0/1/2)"              = "fire_type_num"
  )
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c("IntELiMonDSS rothSpace"),
    row_sizes = c("1fr"),
    col_sizes = c("264px", "1fr"),
    gap_size = "10px",
    grid_card(
      area = "IntELiMonDSS",
      card_header("Fire behavior inputs"),
      card_body(
        style = "overflow-y: auto;",
        radioButtons(ns("fuel_src"), "Surface fuel source",
                     choices = list("Scan level fuels" = "scan",
                                    "Fuel tool values" = "tool"),
                     selected = "scan", width = "100%"),
        uiOutput(ns("fuel_src_note")),
        tags$hr(style = "margin:6px 0;"),

        selectInput(ns("plot_mode"), "Plot type",
                    choices = list(
                      "Time series"                 = "timeseries",
                      "Time series individual plot"  = "individual",
                      "Box and Whisker"             = "boxplot",
                      "Bar"                          = "bar"),
                    selected = "timeseries", width = "100%"),
        radioButtons(ns("show_treatments"), "Treatment date lines",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "on", inline = TRUE, width = "100%"),
        radioButtons(ns("show_errorbars"), "Error bars",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "off", inline = TRUE, width = "100%"),
        tags$hr(style = "margin:6px 0;"),

        selectInput(ns("metric_1"), "Card 1 metric",
                    choices = metric_choices, selected = "ros_ch_hr"),
        selectInput(ns("metric_2"), "Card 2 metric",
                    choices = metric_choices, selected = "fli_kw_m"),
        selectInput(ns("metric_3"), "Card 3 metric",
                    choices = metric_choices, selected = "torching_idx"),
        tags$hr(style = "margin:6px 0;"),

        tags$strong("Wind & slope"),
        numericInput(ns("wind_mph"), "20-ft wind speed (mi/h)",
                     value = 10, min = 0, max = 100, step = 1),
        numericInput(ns("waf"), "Wind adjustment factor (midflame)",
                     value = 0.3, min = 0.05, max = 1, step = 0.05),
        numericInput(ns("slope_pct"), "Slope (%)",
                     value = 0, min = 0, max = 200, step = 5),
        tags$hr(style = "margin:6px 0;"),

        tags$strong("Dead fuel moisture (%)"),
        numericInput(ns("m1"), "1-hour", value = 6, min = 1, max = 60, step = 1),
        numericInput(ns("m10"), "10-hour", value = 7, min = 1, max = 60, step = 1),
        numericInput(ns("m100"), "100-hour", value = 8, min = 1, max = 60, step = 1),
        numericInput(ns("mx_dead"), "Dead moisture of extinction",
                     value = 25, min = 10, max = 60, step = 1),
        tags$hr(style = "margin:6px 0;"),

        tags$strong("Live fuel"),
        numericInput(ns("m_herb"), "Herbaceous moisture (%)",
                     value = 90, min = 30, max = 300, step = 10),
        numericInput(ns("m_woody"), "Woody moisture (%)",
                     value = 90, min = 30, max = 300, step = 10),
        numericInput(ns("live_herb_load"), "Herbaceous load (tons/acre)",
                     value = 0, min = 0, max = 10, step = 0.1),
        numericInput(ns("live_woody_load"), "Woody load (tons/acre)",
                     value = 0, min = 0, max = 10, step = 0.1),
        tags$hr(style = "margin:6px 0;"),

        tags$strong("Canopy"),
        numericInput(ns("fmc"), "Foliar moisture content (%)",
                     value = 100, min = 60, max = 200, step = 10),
        helpText(
          style = "font-size:11px;",
          "Surface: Rothermel (1972) + Byram. Crown: Van Wagner (1977) with ",
          "Rothermel (1991) crown spread. Dead loads and fuel-bed depth come ",
          "from each scan; live loads and weather are held constant across ",
          "scans. Validate against BehavePlus before operational use."
        )
      )
    ),
    grid_card(
      area = "rothSpace",
      card_body(
        grid_container(
          layout = c(
            "card1 card2",
            "card3 panoViewer"
          ),
          row_sizes = c("1fr", "1fr"),
          col_sizes = c("1fr", "1fr"),
          gap_size = "10px",
          grid_card(area = "card1",
                    card_body(plotOutput(ns("card1"), height = "100%"))),
          grid_card(area = "card2",
                    card_body(plotOutput(ns("card2"), height = "100%"))),
          grid_card(area = "card3",
                    card_body(plotOutput(ns("card3"), height = "100%"))),
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

    # -- Fire behavior table (recomputes when data or any input changes) -----
    env <- reactive({
      bed <- if (identical(input$fuel_src, "tool"))
        state$fuel_tool_values() else NULL
      list(
        bed             = bed,
        wind_mph        = input$wind_mph,
        waf             = input$waf,
        slope_pct       = input$slope_pct,
        m1              = input$m1,
        m10             = input$m10,
        m100            = input$m100,
        mx_dead         = input$mx_dead,
        m_herb          = input$m_herb,
        m_woody         = input$m_woody,
        live_herb_load  = input$live_herb_load,
        live_woody_load = input$live_woody_load,
        fmc             = input$fmc
      )
    })

    fire_behavior <- reactive({
      shiny::validate(shiny::need(
        nrow(state$metrics()) > 0,
        "No data loaded - press Get Data on the Selection Map tab."
      ))
      if (identical(input$fuel_src, "tool")) {
        shiny::validate(shiny::need(
          !is.null(state$fuel_tool_values()),
          paste("No fuel values submitted yet - set them on the Fuel tool tab",
                "and press Submit fuel values, or switch back to scan level fuels.")
        ))
      }
      scan_fire_behavior(state$metrics(), state$additional_models_wide(), env())
    })

    output$fuel_src_note <- renderUI({
      b <- state$fuel_tool_values()
      if (identical(input$fuel_src, "scan")) {
        return(div(class = "imn-fnote",
                   paste("Surface fuels come from each scan's Brown time-lag",
                         "loads and fuel bed depth.")))
      }
      if (is.null(b)) {
        return(div(class = "imn-sim-warn", tags$b("Nothing submitted. "),
                   "Set values on the Fuel tool tab and press Submit fuel values."))
      }
      div(class = "imn-okbox",
          tags$b(b$label), tags$br(),
          sprintf("%s \u00B7 %s", b$system, b$aggregation), tags$br(),
          tags$span(style = "color:var(--imn-dim)",
                    "Surface fuels held constant across scans; canopy still per-scan."))
    })

    # One card renderer bound to a metric-dropdown input id
    fire_card <- function(input_id) {
      renderPlot({
        key <- input[[input_id]]
        metric_series_plot(key, FIRE_METRIC_LABELS[[key]], fire_behavior(),
                           state$treatment_dates(),
                           input$show_errorbars, input$show_treatments, input$plot_mode)
      }, bg = "transparent", res = 110)
    }

    output$card1 <- fire_card("metric_1")
    output$card2 <- fire_card("metric_2")
    output$card3 <- fire_card("metric_3")

    # -- Points2Pano viewer (same behavior as the Direct outputs tab) --------
    pano_idx <- reactiveVal(1)

    pano_scans <- reactive({
      sc <- state$scan_calls()
      sc[nzchar(date_code)]
    })

    observeEvent(state$scan_calls(), { pano_idx(1) })

    observeEvent(input$btn_pano_prev, {
      n <- nrow(pano_scans()); if (n == 0) return()
      pano_idx(if (pano_idx() <= 1) n else pano_idx() - 1)
    })
    observeEvent(input$btn_pano_next, {
      n <- nrow(pano_scans()); if (n == 0) return()
      pano_idx(if (pano_idx() >= n) 1 else pano_idx() + 1)
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
