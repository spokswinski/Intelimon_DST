# app/view/help.R
# ---------------------------------------------------------------------------
# Help tab. A styled overview page (Aurora Glass): what the tool does, a quick-
# start, a guide to each tab and the plot types, and data sources. The hex logo
# is the embedded brand asset from app/logic/brand.R.
# ---------------------------------------------------------------------------

box::use(
  shiny[...],
  bslib[card, card_header, card_body],
)
box::use(
  app/logic/brand[logo_uri],
)

# One quick-start step tile.
.step <- function(n, title, body) {
  div(class = "imn-step",
      tags$b(paste0(n, "  \u00B7  ", title)),
      tags$p(body))
}

# One label / description row (for the tab guide and plot-type guide).
.row <- function(name, desc) {
  div(class = "imn-tabrow",
      tags$span(class = "imn-tabname", name),
      tags$span(class = "imn-tabdesc", desc))
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  div(
    class = "imn-help",

    card(
      full_screen = FALSE, fill = FALSE,
      card_header("About this tool"),
      card_body(
        fillable = FALSE,
        img(class = "imn-hexlogo", src = logo_uri, alt = "IntELiMon logo"),
        tags$h2("Track how treatments change forest structure and fire behavior over time."),
        tags$p(
          "The IntELiMon Decision Support Tool pulls lidar-derived vegetation, ",
          "fuels, and fire-behavior metrics for monitoring plots across the ",
          "country and shows how they shift before and after fuel treatments. ",
          "Pick plots on the map, pull their scans once, and every tab reads ",
          "from the same shared set \u2014 so a treatment you enter on the ",
          "Selection Map marks up every chart in the app."
        )
      )
    ),

    card(
      full_screen = FALSE, fill = FALSE,
      card_header("Quick start"),
      card_body(
        fillable = FALSE,
        div(
          class = "imn-steps",
          .step("1", "Select",
                "On the Selection Map, click up to ten plots and set a date range."),
          .step("2", "Retrieve",
                "Press Get Scans, then Get Data to load metrics, tree inventories, and models."),
          .step("3", "Add treatments",
                "Enter treatment dates as YYYYMMDD (comma-separated) and Submit; a red line marks each on every chart."),
          .step("4", "Explore & export",
                "Use the plot-type dropdown to compare metrics over time, and build or download fuel inputs from the Fuel tool.")
        )
      )
    ),

    card(
      full_screen = FALSE, fill = FALSE,
      card_header("The tabs"),
      card_body(
        fillable = FALSE,
        .row("Selection Map",
             "Pick plots on the map, pull their scans, and set the date range and treatment dates."),
        .row("Direct outputs",
             "Tree, volume, and canopy metrics over time, plus the Points2Pano scan viewer."),
        .row("Predictive models",
             "Model outputs (Windows A\u2013C) over time, plus a Points2Pano viewer."),
        .row("Fuel tool",
             "Surface, canopy, and time-lag fuels for the loaded scans; area of interest and exports."),
        .row("rothRmel",
             "Rothermel surface + Van Wagner crown fire behavior computed per scan, tracked over time.")
      )
    ),

    card(
      full_screen = FALSE, fill = FALSE,
      card_header("Plot types"),
      card_body(
        fillable = FALSE,
        tags$p(class = "imn-help-intro",
               "The dropdown at the top of the Direct outputs, Predictive models, and rothRmel sidebars switches how each metric is drawn:"),
        .row("Time series",
             "Mean per time step, connected \u2014 the combined trend across plots."),
        .row("Time series individual plot",
             "One colored line per site/plot, tracking each plot's own change."),
        .row("Box and Whisker",
             "The distribution across plots at each time step."),
        .row("Bar",
             "Mean per time step, drawn as bars.")
      )
    ),

    card(
      full_screen = FALSE, fill = FALSE,
      card_header("Data & sources"),
      card_body(
        fillable = FALSE,
        tags$p(
          "Data sources: IntELiMon REST API v2 \u00B7 LANDFIRE Product Services ",
          "\u00B7 USGS EROS scan imagery."
        ),
        tags$p(
          class = "imn-help-note",
          "Fire-behavior estimates (Rothermel 1972, Byram 1959, Van Wagner ",
          "1977) are intended for relative comparison over time; validate ",
          "against BehavePlus before operational use."
        )
      )
    )
  )
}

#' @export
server <- function(id, state) {
  moduleServer(id, function(input, output, session) {
    # Static content - nothing reactive to wire.
  })
}
