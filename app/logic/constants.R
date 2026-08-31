# app/logic/constants.R
# ---------------------------------------------------------------------------
# Static lookup tables shared across the view modules. Pure data, no Shiny.
# Fuel-specific constants live in app/logic/fuel.R; API-specific constants
# (base_url) live in app/logic/api_client.R.
# ---------------------------------------------------------------------------

# Y-axis labels for the volume metrics
VOLUME_METRIC_LABELS <- c(
  mGCvol = "Ground cover volume (mGCvol)",
  mUSvol = "Understory volume (mUSvol)",
  mMSvol = "Midstory volume (mMSvol)",
  mOSvol = "Overstory volume (mOSvol)"
)

# Y-axis labels for the tree metrics
TREE_METRIC_LABELS <- c(
  Basalarea  = "Basal area (Basalarea)",
  MDBH       = "Mean DBH (MDBH)",
  StemsPacre = "Stems per acre (StemsPacre)",
  TreesN     = "Number of trees (TreesN)",
  MeanTH     = "Mean tree height (MeanTH)",
  MaxTH      = "Maximum tree height (MaxTH)"
)

# Y-axis labels for the canopy metrics
CANOPY_METRIC_LABELS <- c(
  CBH         = "Canopy base height (CBH)",
  canopyCover = "Canopy cover (canopyCover)",
  gapFraction = "Gap fraction (1 - canopyCover)",
  LAI         = "Leaf area index (LAI)",
  OLAI        = "Overstory LAI (OLAI)",
  MLAI        = "Midstory LAI (MLAI)",
  ULAI        = "Understory LAI (ULAI)"
)

# Derived metrics: dropdown key -> the metrics column it is computed from
# and the transform applied to it
DERIVED_METRICS <- list(
  gapFraction = list(source = "canopyCover", transform = function(x) 1 - x)
)

# Label threshold: only show plot labels when this many or fewer are in view
LABEL_THRESHOLD <- 1500
MIN_ZOOM_LABELS <- 11

# Points2Pano iframe crop (pixels). The burnpro3d page is cross-origin, so
# its own UI (header, bottom nav bar, side arrows) can't be restyled from
# this app; instead the iframe is oversized and shifted so those strips are
# clipped out of view. Set all to 0 for the full page.
PANO_CROP_TOP    <- 70   # px of the pano page's top header to hide
PANO_CROP_BOTTOM <- 90   # px of the pano page's bottom nav bar to hide
PANO_CROP_LEFT   <- 60   # px of the left edge (side arrow) to hide
PANO_CROP_RIGHT  <- 60   # px of the right edge (side arrow) to hide
