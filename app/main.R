# app/main.R
# ---------------------------------------------------------------------------
# Application entrypoint (called by rhino::app()). Assembles the page_navbar,
# creates the shared reactive state store once per session, and wires each tab
# module to it. To add a tab: create app/view/<name>.R exporting ui()/server(),
# import it below, add a nav_panel to `ui`, and call its server in `server`.
#
# Branding: the navbar title is the IntELiMon logo + "Decision Support Tool",
# and the page footer carries the federal sponsor plate. Both pull their
# embedded (base64) assets from app/logic/brand.R. Aurora Glass styling is in
# app/static/styles.css, attached through the header slot.
# ---------------------------------------------------------------------------

box::use(
  shiny[NS, moduleServer, tags, includeCSS, div, span, img],
  bslib[page_navbar, nav_panel, bs_theme],
)
box::use(
  app/logic/app_state[new_app_state],
  app/logic/brand[logo_uri, sponsors],
  app/view/selection_map,
  app/view/direct_outputs,
  app/view/predictive_models,
  app/view/fuel_tool,
  app/view/forestry_tool,
  app/view/rothrmel,
  app/view/help,
)

# Navbar brand: logo + two-line wordmark.
brand_title <- function() {
  div(
    class = "imn-brand",
    img(class = "imn-logo", src = logo_uri, alt = "IntELiMon logo"),
    div(
      class = "imn-brand-text",
      span(class = "imn-brand-title", "IntELiMon"),
      span(class = "imn-brand-sub", "Decision Support Tool")
    )
  )
}

# Footer sponsor plate: frosted white pane holding the six agency logos.
sponsor_footer <- function() {
  imgs <- lapply(sponsors, function(s) {
    img(class = paste("imn-sponsor", s$cls), src = s$uri, alt = s$alt)
  })
  div(
    class = "imn-sponsorbar",
    div(class = "imn-sponsor-plate", imgs)
  )
}

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_navbar(
    title       = brand_title(),
    window_title = "IntELiMon Decision Support Tool",
    selected    = "Selection Map",
    collapsible = TRUE,
    theme       = bs_theme(),
    # Styles are attached here rather than via app/styles/main.scss: page_navbar
    # builds a complete page and Rhino's separate stylesheet link does not
    # reliably merge into it, so the header slot is the dependable place.
    header      = tags$head(includeCSS("app/static/styles.css")),
    footer      = sponsor_footer(),
    nav_panel("Selection Map",     selection_map$ui(ns("selection_map"))),
    nav_panel("Direct outputs",    direct_outputs$ui(ns("direct_outputs"))),
    nav_panel("Predictive models", predictive_models$ui(ns("predictive_models"))),
    nav_panel("Fuel tool",         fuel_tool$ui(ns("fuel_tool"))),
    nav_panel("Forestry tool",     forestry_tool$ui(ns("forestry_tool"))),
    nav_panel("rothRmel",          rothrmel$ui(ns("rothrmel"))),
    nav_panel("Help",              help$ui(ns("help")))
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Graphics device for plot rendering, best available first.
    #
    # The default bitmap device rasterises text poorly at card sizes: glyph
    # counters (the hole in "e", "a", "o") fill in solid and strokes go jagged.
    # That is a rasterisation problem, not a font problem - changing typeface
    # does not fix it. ragg has the best text rendering and font handling;
    # Cairo is a solid fallback. Both are probed, so a machine with neither
    # simply keeps the default behaviour.
    if (requireNamespace("ragg", quietly = TRUE)) {
      options(shiny.useragg = TRUE)
    } else if (isTRUE(capabilities("cairo"))) {
      options(shiny.usecairo = TRUE)
    }

    # One shared state store for the whole session
    state <- new_app_state()

    selection_map$server("selection_map", state)
    direct_outputs$server("direct_outputs", state)
    predictive_models$server("predictive_models", state)
    fuel_tool$server("fuel_tool", state)
    forestry_tool$server("forestry_tool", state)
    rothrmel$server("rothrmel", state)
    help$server("help", state)
  })
}
