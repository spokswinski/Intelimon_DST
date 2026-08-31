# app/logic/fuel_models.R
# ---------------------------------------------------------------------------
# Standard fire behavior fuel model (FBFM) parameter tables.
#
#   ANDERSON_13     - Anderson (1982), Aids to Determining Fuel Models for
#                     Estimating Fire Behavior, USDA FS GTR INT-122.
#                     LANDFIRE layer: LF_FBFM13
#   SCOTT_BURGAN_40 - Scott & Burgan (2005), Standard Fire Behavior Fuel
#                     Models, USDA FS RMRS-GTR-153.
#                     LANDFIRE layer: LF_FBFM40
#
# Units, matching the Rothermel implementation in fire_behavior.R:
#   load_*    oven-dry fuel load, TONS/ACRE (1h, 10h, 100h, live herb, live woody)
#   depth_ft  fuel bed depth, FEET
#   mx_dead   dead fuel moisture of extinction, PERCENT
#   sav_*     surface-area-to-volume ratio, ft^2/ft^3
#
# !! VERIFY BEFORE OPERATIONAL USE !!
# These tables are transcribed from the published fuel model definitions and
# have NOT been validated against BehavePlus or the source documents in this
# environment. A mistyped load or depth changes predicted fire behavior
# substantially. Check the models you actually use against Anderson (1982)
# Table 1 / Scott & Burgan (2005) Appendix A before relying on any output.
#
# Non-burnable models (NB1/NB2/NB3/NB8/NB9, codes 91-99) carry no fuel and
# return zero spread; they are listed so LANDFIRE codes resolve rather than
# silently falling through.
# ---------------------------------------------------------------------------

box::use(
  data.table[data.table, rbindlist],
  stats[setNames],
)

# helper: one fuel model row
.fm <- function(code, number, name, d1, d10, d100, herb, woody,
                depth_ft, mx_dead, sav_d1 = 2000, sav_herb = 1600,
                sav_woody = 1600, dynamic = FALSE, group = NA_character_) {
  data.table(
    code = code, number = number, name = name,
    load_d1 = d1, load_d10 = d10, load_d100 = d100,
    load_herb = herb, load_woody = woody,
    depth_ft = depth_ft, mx_dead = mx_dead,
    sav_d1 = sav_d1, sav_herb = sav_herb, sav_woody = sav_woody,
    dynamic = dynamic, group = group
  )
}

#' Anderson (1982) 13 standard fire behavior fuel models.
#' @export
ANDERSON_13 <- rbindlist(list(
  .fm("FM1",  1,  "Short grass (1 ft)",            0.74,  0.00,  0.00, 0.00, 0.00, 1.0, 12, 3500, group = "Grass"),
  .fm("FM2",  2,  "Timber grass and understory",   2.00,  1.00,  0.50, 0.50, 0.00, 1.0, 15, 3000, group = "Grass"),
  .fm("FM3",  3,  "Tall grass (2.5 ft)",           3.01,  0.00,  0.00, 0.00, 0.00, 2.5, 25, 1500, group = "Grass"),
  .fm("FM4",  4,  "Chaparral (6 ft)",              5.01,  4.01,  2.00, 0.00, 5.01, 6.0, 20, 2000, group = "Shrub"),
  .fm("FM5",  5,  "Brush (2 ft)",                  1.00,  0.50,  0.00, 0.00, 2.00, 2.0, 20, 2000, group = "Shrub"),
  .fm("FM6",  6,  "Dormant brush, hardwood slash", 1.50,  2.50,  2.00, 0.00, 0.00, 2.5, 25, 1750, group = "Shrub"),
  .fm("FM7",  7,  "Southern rough",                1.13,  1.87,  1.50, 0.00, 0.37, 2.5, 40, 1750, group = "Shrub"),
  .fm("FM8",  8,  "Closed timber litter",          1.50,  1.00,  2.50, 0.00, 0.00, 0.2, 30, 2000, group = "Timber litter"),
  .fm("FM9",  9,  "Hardwood litter",               2.92,  0.41,  0.15, 0.00, 0.00, 0.2, 25, 2500, group = "Timber litter"),
  .fm("FM10", 10, "Timber litter and understory",  3.01,  2.00,  5.01, 0.00, 2.00, 1.0, 25, 2000, group = "Timber litter"),
  .fm("FM11", 11, "Light logging slash",           1.50,  4.51,  5.51, 0.00, 0.00, 1.0, 15, 1500, group = "Slash"),
  .fm("FM12", 12, "Medium logging slash",          4.01, 14.03, 16.53, 0.00, 0.00, 2.3, 20, 1500, group = "Slash"),
  .fm("FM13", 13, "Heavy logging slash",           7.01, 23.04, 28.05, 0.00, 0.00, 3.0, 25, 1500, group = "Slash")
), use.names = TRUE)

#' Scott & Burgan (2005) 40 standard fire behavior fuel models.
#' `dynamic = TRUE` models transfer live herbaceous load to dead as the
#' herbaceous fuel cures; the transfer is not implemented here (loads are
#' used as tabulated), which is conservative for cured-grass conditions.
#' @export
SCOTT_BURGAN_40 <- rbindlist(list(
  # --- non-burnable -------------------------------------------------------
  .fm("NB1", 91, "Urban/developed",      0, 0, 0, 0, 0, 0.0,  0, group = "Non-burnable"),
  .fm("NB2", 92, "Snow/ice",             0, 0, 0, 0, 0, 0.0,  0, group = "Non-burnable"),
  .fm("NB3", 93, "Agricultural",         0, 0, 0, 0, 0, 0.0,  0, group = "Non-burnable"),
  .fm("NB8", 98, "Open water",           0, 0, 0, 0, 0, 0.0,  0, group = "Non-burnable"),
  .fm("NB9", 99, "Bare ground",          0, 0, 0, 0, 0, 0.0,  0, group = "Non-burnable"),
  # --- grass (GR) ---------------------------------------------------------
  .fm("GR1", 101, "Short, sparse dry climate grass", 0.10, 0.00, 0.00, 0.30, 0.00, 0.4, 15, 2200, 2000, 1500, TRUE, "Grass"),
  .fm("GR2", 102, "Low load dry climate grass",      0.10, 0.00, 0.00, 1.00, 0.00, 1.0, 15, 2000, 1800, 1500, TRUE, "Grass"),
  .fm("GR3", 103, "Low load very coarse grass",      0.10, 0.40, 0.00, 1.50, 0.00, 2.0, 30, 1500, 1300, 1500, TRUE, "Grass"),
  .fm("GR4", 104, "Moderate load dry climate grass", 0.25, 0.00, 0.00, 1.90, 0.00, 2.0, 15, 2000, 1800, 1500, TRUE, "Grass"),
  .fm("GR5", 105, "Low load humid climate grass",    0.40, 0.00, 0.00, 2.50, 0.00, 1.5, 40, 1800, 1600, 1500, TRUE, "Grass"),
  .fm("GR6", 106, "Moderate load humid climate grass", 0.10, 0.00, 0.00, 3.40, 0.00, 1.5, 40, 2200, 2000, 1500, TRUE, "Grass"),
  .fm("GR7", 107, "High load dry climate grass",     1.00, 0.00, 0.00, 5.40, 0.00, 3.0, 15, 2000, 1800, 1500, TRUE, "Grass"),
  .fm("GR8", 108, "High load very coarse grass",     0.50, 1.00, 0.00, 7.30, 0.00, 4.0, 30, 1500, 1300, 1500, TRUE, "Grass"),
  .fm("GR9", 109, "Very high load humid climate grass", 1.00, 1.00, 0.00, 9.00, 0.00, 5.0, 40, 1800, 1600, 1500, TRUE, "Grass"),
  # --- grass-shrub (GS) ---------------------------------------------------
  .fm("GS1", 121, "Low load dry climate grass-shrub",      0.20, 0.00, 0.00, 0.50, 0.65, 0.9, 15, 2000, 1800, 1800, TRUE, "Grass-shrub"),
  .fm("GS2", 122, "Moderate load dry climate grass-shrub", 0.50, 0.50, 0.00, 0.60, 1.00, 1.5, 15, 2000, 1800, 1800, TRUE, "Grass-shrub"),
  .fm("GS3", 123, "Moderate load humid climate grass-shrub", 0.30, 0.25, 0.00, 1.45, 1.25, 1.8, 40, 1800, 1600, 1600, TRUE, "Grass-shrub"),
  .fm("GS4", 124, "High load humid climate grass-shrub",   1.90, 0.30, 0.10, 3.40, 7.10, 2.1, 40, 1800, 1600, 1600, TRUE, "Grass-shrub"),
  # --- shrub (SH) ---------------------------------------------------------
  .fm("SH1", 141, "Low load dry climate shrub",       0.25, 0.25, 0.00, 0.15, 1.30, 1.0, 15, 2000, 1800, 1600, TRUE, "Shrub"),
  .fm("SH2", 142, "Moderate load dry climate shrub",  1.35, 2.40, 0.75, 0.00, 3.85, 1.0, 15, 2000, 1800, 1600, FALSE, "Shrub"),
  .fm("SH3", 143, "Moderate load humid climate shrub", 0.45, 3.00, 0.00, 0.00, 6.20, 2.4, 40, 1600, 1800, 1400, FALSE, "Shrub"),
  .fm("SH4", 144, "Low load humid climate timber-shrub", 0.85, 1.15, 0.20, 0.00, 2.55, 3.0, 30, 2000, 1800, 1600, FALSE, "Shrub"),
  .fm("SH5", 145, "High load dry climate shrub",      3.60, 2.10, 0.00, 0.00, 2.90, 6.0, 15,  750, 1800, 1600, FALSE, "Shrub"),
  .fm("SH6", 146, "Low load humid climate shrub",     2.90, 1.45, 0.00, 0.00, 1.40, 2.0, 30,  750, 1800, 1600, FALSE, "Shrub"),
  .fm("SH7", 147, "Very high load dry climate shrub", 3.50, 5.30, 2.20, 0.00, 3.40, 6.0, 15,  750, 1800, 1600, FALSE, "Shrub"),
  .fm("SH8", 148, "High load humid climate shrub",    2.05, 3.40, 0.85, 0.00, 4.35, 3.0, 40,  750, 1800, 1600, FALSE, "Shrub"),
  .fm("SH9", 149, "Very high load humid climate shrub", 4.50, 2.45, 0.00, 1.55, 7.03, 4.4, 40,  750, 1800, 1500, TRUE,  "Shrub"),
  # --- timber-understory (TU) ---------------------------------------------
  .fm("TU1", 161, "Light load dry climate timber-grass-shrub", 0.20, 0.90, 1.50, 0.20, 0.90, 0.6, 20, 2000, 1800, 1600, TRUE,  "Timber-understory"),
  .fm("TU2", 162, "Moderate load humid climate timber-shrub",  0.95, 1.80, 1.25, 0.00, 0.20, 1.0, 30, 2000, 1800, 1600, FALSE, "Timber-understory"),
  .fm("TU3", 163, "Moderate load humid climate timber-grass-shrub", 1.10, 0.15, 0.25, 0.65, 1.10, 1.3, 30, 1800, 1600, 1400, TRUE, "Timber-understory"),
  .fm("TU4", 164, "Dwarf conifer with understory",             4.50, 0.00, 0.00, 0.00, 2.00, 0.5, 12, 2300, 1800, 2000, FALSE, "Timber-understory"),
  .fm("TU5", 165, "Very high load dry climate timber-shrub",   4.00, 4.00, 3.00, 0.00, 3.00, 1.0, 25, 1500, 1800, 750,  FALSE, "Timber-understory"),
  # --- timber litter (TL) -------------------------------------------------
  .fm("TL1", 181, "Low load compact conifer litter",  1.00, 2.20, 3.60, 0.00, 0.00, 0.2, 30, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL2", 182, "Low load broadleaf litter",        1.40, 2.30, 2.20, 0.00, 0.00, 0.2, 25, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL3", 183, "Moderate load confier litter",     0.50, 2.20, 2.80, 0.00, 0.00, 0.3, 20, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL4", 184, "Small downed logs",                0.50, 1.50, 4.20, 0.00, 0.00, 0.4, 25, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL5", 185, "High load conifer litter",         1.15, 2.50, 4.40, 0.00, 0.00, 0.6, 25, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL6", 186, "Moderate load broadleaf litter",   2.40, 1.20, 1.20, 0.00, 0.00, 0.3, 25, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL7", 187, "Large downed logs",                0.30, 1.40, 8.10, 0.00, 0.00, 0.4, 25, 2000, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL8", 188, "Long-needle litter",               5.80, 1.40, 1.10, 0.00, 0.00, 0.3, 35, 1800, 1800, 1600, FALSE, "Timber litter"),
  .fm("TL9", 189, "Very high load broadleaf litter",  6.65, 3.30, 4.15, 0.00, 0.00, 0.6, 35, 1800, 1800, 1600, FALSE, "Timber litter"),
  # --- slash-blowdown (SB) ------------------------------------------------
  .fm("SB1", 201, "Low load activity fuel",            1.50, 3.00, 11.00, 0.00, 0.00, 1.0, 25, 2000, 1800, 1600, FALSE, "Slash-blowdown"),
  .fm("SB2", 202, "Moderate load activity/low blowdown", 4.50, 4.25, 4.00, 0.00, 0.00, 1.0, 25, 2000, 1800, 1600, FALSE, "Slash-blowdown"),
  .fm("SB3", 203, "High load activity/moderate blowdown", 5.50, 2.75, 3.00, 0.00, 0.00, 1.2, 25, 2000, 1800, 1600, FALSE, "Slash-blowdown"),
  .fm("SB4", 204, "High blowdown",                     5.25, 3.50, 5.25, 0.00, 0.00, 2.7, 25, 2000, 1800, 1600, FALSE, "Slash-blowdown")
), use.names = TRUE)

#' Look up a fuel model by code (e.g. "TL2") or LANDFIRE number (e.g. 182).
#'
#' @param key character code or numeric LANDFIRE value
#' @param system "FBFM40" (Scott & Burgan) or "FBFM13" (Anderson)
#' @return one-row data.table, or NULL when unmatched
#' @export
fuel_model_lookup <- function(key, system = c("FBFM40", "FBFM13")) {
  system <- match.arg(system)
  tab <- if (system == "FBFM13") ANDERSON_13 else SCOTT_BURGAN_40
  if (length(key) == 0 || is.na(key[1])) return(NULL)

  k <- key[1]
  num <- suppressWarnings(as.numeric(k))
  hit <- if (!is.na(num)) tab[number == num] else tab[toupper(code) == toupper(trimws(as.character(k)))]
  if (nrow(hit) == 0) return(NULL)
  hit[1]
}

#' Named choices for a fuel model dropdown: "TL2 - Low load broadleaf litter".
#' @export
fuel_model_choices <- function(system = c("FBFM40", "FBFM13")) {
  system <- match.arg(system)
  tab <- if (system == "FBFM13") ANDERSON_13 else SCOTT_BURGAN_40
  setNames(as.list(tab$code), paste0(tab$code, " \u2014 ", tab$name))
}

#' Convert a fuel model row into the surface fuel bed shape used by
#' fire_behavior.R: loads (tons/acre), depth (ft), Mx (%), SAV (ft^2/ft^3).
#' @export
fuel_model_bed <- function(fm) {
  if (is.null(fm) || nrow(fm) == 0) return(NULL)
  list(
    fbfm_code = fm$code,
    fbfm_name = fm$name,
    load_tonsac = c(d1 = fm$load_d1, d10 = fm$load_d10, d100 = fm$load_d100,
                    herb = fm$load_herb, woody = fm$load_woody),
    depth_ft = fm$depth_ft,
    mx_dead_pct = fm$mx_dead,
    sav = c(d1 = fm$sav_d1, d10 = 109, d100 = 30,
            herb = fm$sav_herb, woody = fm$sav_woody),
    non_burnable = identical(fm$group, "Non-burnable")
  )
}
