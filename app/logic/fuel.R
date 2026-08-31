# app/logic/fuel.R
# ---------------------------------------------------------------------------
# Fuel Tool support: Brown (1974) planar-intercept downed woody fuel loads
# and the model-name -> metrics-column mappings that fill the fuel cards.
# Pure functions / data; no Shiny.
# ---------------------------------------------------------------------------

# ---- Brown 1974 downed woody fuel load (40 m transects) -------------------
# Planar-intercept load (tons/acre) = 11.64 * n * d2 * s * a * c / (N * L_ft)
# No decay-class or slope correction (c = 1). L fixed at 40 m -> feet.
BROWN_UNIT_CONSTANT <- 11.64
BROWN_M_TO_FT       <- 3.280839895
BROWN_TRANSECT_M    <- 40

BROWN_CLASSES <- list(
  onehr  = list(d2 = 0.0151, s = 0.48, a = 1.13),  # 1-hr   0-0.25 in
  tenhr  = list(d2 = 0.289,  s = 0.48, a = 1.13),  # 10-hr  0.25-1 in
  hunhr  = list(d2 = 2.76,   s = 0.40, a = 1.13),  # 100-hr 1-3 in
  thohr  = list(d2 = 22.30,  s = 0.40, a = 1.00)   # 1000-hr >3 in sound
)

#' Load (tons/acre) for one class given an intercept count over a single
#' 40 m plane.
brown_class_load <- function(count, cls, n_planes = 1,
                             transect_m = BROWN_TRANSECT_M) {
  count <- suppressWarnings(as.numeric(count))
  if (is.na(count) || count < 0) return(NA_real_)
  length_ft <- transect_m * BROWN_M_TO_FT
  (BROWN_UNIT_CONSTANT * count * cls$d2 * cls$s * cls$a) /
    (n_planes * length_ft)
}

# ---- Fuel export tab: model-name -> metrics column mappings ---------------
# Surface fuel model rows (white boxes filled from these model columns in
# Modeled/User defined mode; cm units for the depth rows)
SURFACE_FUEL_MODELS <- list(
  mfbd = list(label = "Mean fuel bed depth (cm)", col = "MFBDmod"),
  mld  = list(label = "Mean litter depth (cm)",   col = "MLDmod"),
  mdd  = list(label = "Mean duff depth (cm)",     col = "MDDmod"),
  bg   = list(label = "Mean bare ground",         col = "BGmod")
)

# Cover-class models feeding the single "Fine fuels cover %" dropdown
COVER_CLASS_MODELS <- list(
  Flitt  = list(label = "Fine litter % cover", col = "Flittmod"),
  Grass  = list(label = "Grass % cover",       col = "Grassmod"),
  Forbs  = list(label = "Forbs % cover",       col = "Forbsmod"),
  Woody  = list(label = "Woody % cover",       col = "Woodymod")
)

# Time-lag fuel models -> intercept COUNTS per 40 m (feed the Brown calc)
TIMELAG_FUEL_MODELS <- list(
  onehr = list(label = "1-hour fuels /40m",    col = "onehrmod"),
  tenhr = list(label = "10-hour fuels /40m",   col = "tenhrmod"),
  hunhr = list(label = "100-hour fuels /40m",  col = "hunhrmod"),
  thohr = list(label = "1000-hour fuels /40m", col = "thohrmod")
)

# Canopy fuel rows -> metrics columns (Modeled/User defined mode)
CANOPY_FUEL_ROWS <- list(
  cbh    = list(label = "Mean canopy base height (m)", col = "CBH"),
  meanth = list(label = "Mean tree height (m)",        col = "MeanTH"),
  maxth  = list(label = "Average max tree height (m)", col = "MaxTH"),
  cc     = list(label = "Canopy cover (%)",            col = "canopyCover"),
  cbd    = list(label = "LANDFIRE canopy bulk density", col = "LF_CBD")
)
