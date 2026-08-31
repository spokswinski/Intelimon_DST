# This file declares the R packages the app depends on so that
# `renv::snapshot()` / `renv::install()` can discover them. It is not sourced
# at runtime. Add a library() line here whenever a new package is introduced.
library(rhino)

library(shiny)
library(bslib)
library(gridlayout)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(data.table)
library(tidyr)
library(sf)
library(jsonlite)

# Graphics device with high-quality text rasterisation for plot output.
library(ragg)
