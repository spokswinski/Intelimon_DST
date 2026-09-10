source("renv/activate.R")

# Resolve `app/...` box modules from the project root. `rhino::app()` sets this
# itself at runtime, but `rhino::lint_r()` and box.lsp do not, so without it
# box.linters looks for modules relative to each source file's own directory.
options(box.path = getwd())
