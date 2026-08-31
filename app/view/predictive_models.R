# app/view/predictive_models.R
# ---------------------------------------------------------------------------
# Predictive models tab. Three model cards (Windows A-C), each plotting the
# model chosen in its dropdown from additional_models_wide, plus a Points2Pano
# viewer in the fourth cell (same behaviour as the Direct outputs tab). The
# A-C dropdowns are populated here by observing the shared state.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
  gridlayout[grid_container, grid_card],
  data.table[...],
)
box::use(
  app/logic/plotting[metric_series_plot],
  app/logic/constants[
    PANO_CROP_TOP, PANO_CROP_BOTTOM, PANO_CROP_LEFT, PANO_CROP_RIGHT
  ],
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  grid_container(
    layout = c("IntELiMonDSS modelSpace"),
    row_sizes = c("1fr"),
    col_sizes = c("264px", "1fr"),
    gap_size = "10px",
    grid_card(
      area = "IntELiMonDSS",
      card_header("Select Models"),
      card_body(
        selectInput(ns("plot_mode_pm"), "Plot type",
                    choices = list(
                      "Time series"                 = "timeseries",
                      "Time series individual plot"  = "individual",
                      "Box and Whisker"             = "boxplot",
                      "Bar"                          = "bar"),
                    selected = "timeseries", width = "100%"),
        radioButtons(ns("show_treatments_pm"), "Treatment date lines",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "on", inline = TRUE, width = "100%"),
        radioButtons(ns("show_errorbars_pm"), "Error bars",
                     choices = list("On" = "on", "Off" = "off"),
                     selected = "on", inline = TRUE, width = "100%"),
        selectInput(ns("model_a"), "Window A available models",
                    choices = list("Load data first" = "")),
        selectInput(ns("model_b"), "Window B available models",
                    choices = list("Load data first" = "")),
        selectInput(ns("model_c"), "Window C available models",
                    choices = list("Load data first" = ""))
      )
    ),
    grid_card(
      area = "modelSpace",
      card_body(
        grid_container(
          layout = c(
            "modelA modelB",
            "modelC panoViewer"
          ),
          row_sizes = c("1fr", "1fr"),
          col_sizes = c("1fr", "1fr"),
          gap_size = "10px",
          grid_card(area = "modelA",
                    card_body(plotOutput(ns("modelA"), height = "100%"))),
          grid_card(area = "modelB",
                    card_body(plotOutput(ns("modelB"), height = "100%"))),
          grid_card(area = "modelC",
                    card_body(plotOutput(ns("modelC"), height = "100%"))),
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

    # Populate the Window A-C dropdowns whenever the wide models table changes,
    # staggering defaults so the three cards start on different models.
    observeEvent(state$additional_models_wide(), {
      wide <- state$additional_models_wide()
      model_choices <- setdiff(
        names(wide),
        c("site_name", "plot", "date_code", "scanner_id")
      )
      model_choices <- sort(model_choices)
      if (length(model_choices) == 0) return()

      pick <- function(k) model_choices[min(k, length(model_choices))]
      updateSelectInput(session, "model_a", choices = model_choices, selected = pick(1))
      updateSelectInput(session, "model_b", choices = model_choices, selected = pick(2))
      updateSelectInput(session, "model_c", choices = model_choices, selected = pick(3))
    })

    # One card renderer bound to a dropdown input id
    model_card <- function(input_id) {
      renderPlot({
        key <- input[[input_id]]
        shiny::validate(shiny::need(
          nzchar(key),
          "No models loaded - press Get Data on the Selection Map tab."
        ))
        metric_series_plot(key, key, state$additional_models_wide(),
                           state$treatment_dates(),
                           input$show_errorbars_pm, input$show_treatments_pm,
                           input$plot_mode_pm)
      }, bg = "transparent", res = 110)
    }

    output$modelA <- model_card("model_a")
    output$modelB <- model_card("model_b")
    output$modelC <- model_card("model_c")

    # -- Points2Pano viewer (same behaviour as the Direct outputs tab) -------
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
