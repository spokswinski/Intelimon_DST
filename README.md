# IntELiMon Decision Support Tool

Rhino conversion of the Shiny app for the USGS
IntELiMon lidar vegetation/fuels monitoring program. Each tab is an
independent [`box`](https://klmr.me/box/) module under `app/view/`, shared
logic lives under `app/logic/`, and cross-tab state flows through one reactive
store instead of global `<<-` assignment.


## Theme

The app ships with the **Aurora Glass** theme: a blurred aurora background with frosted translucent cards. Styling lives in `app/static/styles.css` (attached via the navbar header slot); plots render with a transparent background and light text (`aurora_theme()` in `app/logic/plotting.R`) so they sit on the glass. The navbar shows the IntELiMon logo and title, and the page footer carries the federal sponsor plate (USGS, FWS, USDA Forest Service, BIA, NPS, SERDP/ESTCP) — all embedded as base64 in `app/logic/brand.R`.

Direct outputs, Predictive models, and rothRmel each have a **Plot type** dropdown at the top of the sidebar with four modes: Time series, Time series individual plot, Box and Whisker, and Bar. Predictive models shows Windows A-C plus a Points2Pano viewer. The Forestry tab is temporarily removed.


## Forestry tool

Three workflows share one stem map, driven by the per-scan tree inventory:
inventory (stand metrics), prescription (mark stems, see the residual stand),
and FVS export. A second mode simulates a stem map across a landscape AOI.

**Units.** The tree inventory reports **DBH in inches** (converted upstream) and
X / Y / H in metres. Per-stem basal area uses the forester's constant,
`BA_ft2 = 0.005454 * DBH_in^2` — the inventory's own `BasalA` column carries
exactly that value and is used directly when present, with the constant as a
fallback. QMD comes straight from the inch diameters; no conversion.

**Occlusion scaling.** The scan plot is a circle of the given radius, but TLS
occlusion means stems are only findable over part of it. The metrics response
reports `nonocarea` (m²) per scan, and **every per-area value divides by
nonocarea, not by πr²** — density, basal area, SDI, and the FVS `TreeCount`
expansion factor. Using the full plot area understates them by the occluded
fraction. The full-area result is shown only as a comparison column. QMD is a
ratio of stem sizes and is unaffected. A residual size bias remains after the
area correction (small stems behind large boles go missing first); the Size
bias tab estimates it for display but does not apply it.

**Species.** Lidar does not measure species, so composition is entered as a
ratio (code + percent). The assignment rule decides which stems get which code
— random, by size class, or spatially clustered. Stand proportions match the
ratio under all three, but per-tree identity drives FVS growth equations.

**FVS.** DBH, height, and crown ratio ((H − CBH)/H, measured rather than
imputed) plus the occlusion-corrected expansion factor come from the scan.
Exports `FVS_TreeInit`, `FVS_StandInit`, and a `.key` file; the prescription
can be written as a thinning keyword. Keyword spellings and field widths vary
between FVS versions and variants — **verify the generated files against the
FVS documentation for your variant before relying on a run.**

**Generative AOI map.** Fits a Weibull diameter distribution and
occlusion-corrected density to the loaded scans, then simulates stems across a
rectangular AOI. Output is **simulated, not measured** — validate generated
canopy cover against independent ALS/LANDFIRE before use; exports carry a note
column saying so.

## Running it

First time, from the project root (in R):

```r
# 1. Bootstrap the project library (renv activates via .Rprofile on startup)
renv::install()     # installs the packages declared in dependencies.R
renv::snapshot()    # writes a complete renv.lock for your environment

# 2. Launch
shiny::runApp()     # or open app.R in RStudio and click "Run App"
```

The shipped `renv.lock` only pins R + renv itself (a bootstrap seed).
`renv/settings.json` uses renv's **implicit** snapshot type (the Rhino
default): renv scans the project's R files for `library()` calls. That is why
`dependencies.R` exists — renv cannot parse `box::use()` syntax, so every
package the app needs is declared there with a plain `library()` line.
`renv::snapshot()` records the full, hash-locked dependency set for your
machine. Run it once and commit the result. On another machine,
`renv::restore()` then reproduces that library exactly.

Do not set the snapshot type to `explicit`: that mode reads dependencies from
a `DESCRIPTION` file, which a Rhino app does not have, and `renv::install()`
will fail with "cannot find .../DESCRIPTION".

`gridlayout` is not on CRAN. If `renv::install()` reports it unavailable, run
`renv::install("rstudio/gridlayout")` and then re-run `renv::install()`.

Thereafter just `shiny::runApp()`.

## Structure

```
intelimon/
├── app.R                       # entrypoint: rhino::app()
├── config.yml                  # Rhino config
├── rhino.yml                   # sass: r  (compiles SCSS via the R sass pkg, no Node)
├── dependencies.R              # package declarations for renv
├── renv.lock / .Rprofile / renv/   # renv bootstrap
└── app/
    ├── main.R                  # page_navbar + module wiring + shared state
    ├── logic/                  # pure, testable, no Shiny UI
    │   ├── api_client.R        # IntELiMon REST calls + plots loader
    │   ├── series.R            # time-step clustering, treatment parsing, wide reshape
    │   ├── plotting.R          # shared metric_series_plot()
    │   ├── fuel.R              # Brown 1974 loads + fuel-tab column mappings
    │   ├── fire_behavior.R     # Rothermel/Byram/Van Wagner per-scan fire model
    │   ├── forestry.R          # occlusion-corrected stand metrics, thinning, simulation
    │   ├── fvs.R               # FVS TreeInit/StandInit + keyword export
    │   ├── brand.R             # embedded logo + sponsor logos (base64)
    │   ├── constants.R         # metric labels, derived metrics, map/pano tuning
    │   └── app_state.R        # new_app_state(): the shared reactive store
    ├── view/                   # one box module (ui + server) per tab
    │   ├── selection_map.R
    │   ├── direct_outputs.R
    │   ├── predictive_models.R
    │   ├── fuel_tool.R
    │   ├── forestry_tool.R     # stem map, prescription, FVS export, AOI simulation
    │   ├── rothrmel.R          # fire behavior (Rothermel/Van Wagner) over time
    │   └── help.R              # placeholder
    ├── styles/main.scss        # stub; see app/static/styles.css (below)
    ├── static/styles.css       # app styles, attached via main.R header slot
    ├── js/index.js             # auto-included; empty
    └── static/                 # static assets
```

## State management: module returns via a shared reactive store

The original app used global `<<-` tables (`scan_calls`, `metrics`,
`tree_inventory`, `additional_models`, `additional_models_wide`,
`treatment_dates`, `aoi_polygon`) plus manual revision counters
(`metrics_rev`, `treatments_rev`, `scan_calls_rev`) to force redraws.

`app/logic/app_state.R` replaces all of that with one `state` list of
`reactiveVal`s, created once in `main.R` and passed into every module's
`server(id, state)`. Modules **write** with `state$metrics(new_dt)` and
**read** with `state$metrics()`; because each slot is reactive, downstream
plots re-run automatically — the revision counters are gone. `state$plots` is
a plain (constant) data.table loaded once at startup.

Ownership:

- **selection_map** writes `scan_calls`, `metrics`, `tree_inventory`,
  `additional_models`, `additional_models_wide`, and (on Submit) `treatment_dates`.
- **direct_outputs** reads `metrics`, `treatment_dates`, `scan_calls`, and (on
  Revise) writes `treatment_dates`.
- **predictive_models** reads `additional_models_wide` + `treatment_dates`, and
  populates its own A–D dropdowns by observing the store (this removes the
  cross-namespace `updateSelectInput` the single-file app relied on).
- **fuel_tool** reads `metrics` + `additional_models_wide` + `plots`, writes
  `aoi_polygon`.

## Adding a tab (for multiple developers)

1. Create `app/view/my_tab.R` exporting `ui(id)` and `server(id, state)`.
2. Import it in `app/main.R`: add `app/view/my_tab` to the `box::use(...)` block.
3. Add `nav_panel("My tab", my_tab$ui(ns("my_tab")))` to `main.R`'s `ui`.
4. Add `my_tab$server("my_tab", state)` to `main.R`'s `server`.

Because each tab is namespaced and talks to the rest of the app only through
`state`, two developers can work on different tabs without colliding.

## Conversion notes / things to know

- **`box` modules do not attach R's default packages.** A module sees `base`
  plus whatever it imports — `stats`, `utils`, `graphics`, `grDevices` and
  `methods` are *not* available implicitly the way they are in a plain script.
  This bites on innocuous-looking base-ish functions: `setNames` and `sd` are
  `stats`, `URLencode` and `type.convert` are `utils`. Either import them
  (`box::use(stats[sd])`) or call them prefixed (`utils::URLencode()`). Symptom
  is a runtime `could not find function "..."` rather than a load-time error,
  so it surfaces only when that code path runs.
- **Never import data.table's special symbols through `box`.** `box::use`
  cannot parse `:=` in an attach list — its parser reads `:=` as a character
  literal and aborts at load time with `expected a name, got character literal
  ":="`. The same applies conceptually to `.N`, `.SD`, `.I`, `.GRP`, `.BY`.
  None of them need importing: inside a `dt[i, j, by]` call they are resolved
  by data.table's own `[` via non-standard evaluation, whether written as
  `dt[, x := y]` or `dt[, `:=`(a = 1, b = 2)]` or `dt[, .(n = .N), by = g]`.
  So import only ordinary data.table functions (`data.table`, `as.data.table`,
  `rbindlist`, `setorder`, `dcast`, ...) and use `:=`/`.N`/`.SD` freely in the
  code without listing them. Wildcard `data.table[...]` is fine — box attaches
  exports at load time without parsing `:=` as a token.
- **Input/output IDs are namespaced.** Every `inputId`/`outputId` inside a
  module is wrapped in `ns()`. Dynamic IDs built inside `renderUI`
  (the fuel cards) use `session$ns`.
- **`grid_card_plot()` was expanded.** gridlayout's `grid_card_plot(area=…)`
  hard-codes the plot's `outputId` to the area name, which can't be namespaced.
  Each was rewritten as `grid_card(area=…, card_body(plotOutput(ns(…))))`.
- **`jsonlite::validate` masking is handled structurally.** `api_client.R`
  imports `jsonlite[fromJSON]` only (never wildcard), and validation calls keep
  the explicit `shiny::validate(shiny::need(...))` form, so Shiny's `validate`
  is always the one used.
- **Imports use `box::use(pkg[...])` wildcards** for the large UI/data packages
  (shiny, bslib, gridlayout, leaflet, leaflet.extras, ggplot2, data.table) to
  keep the ported code bodies close to the original. `rhino::lint_r()` will flag
  these; tighten to explicit function lists per module when convenient.
- **Styles load from `app/static/styles.css`, not SCSS.** They are attached in
  `app/main.R` through `page_navbar`'s `header` slot
  (`tags$head(includeCSS(...))`). bslib's `page_navbar()` renders a complete
  HTML page, and Rhino's auto-included compiled stylesheet
  (`app/styles/main.scss`) does not reliably merge into a full bslib page, so
  styles placed in `main.scss` silently fail to load. Edit `styles.css`; the
  `main.scss` file is a stub explaining this. (`rhino.yml` still sets
  `sass: r` so no Node toolchain is needed if SCSS is reintroduced later.)
- **Points2Pano call #4 and the fuel export buttons remain stubs**, exactly as
  in the source. `lcp_builder.R` / `brown_fuel_load.R` are not yet wired in;
  `fuel.R` already holds the Brown planar-intercept logic for the Fuel tool.
```
