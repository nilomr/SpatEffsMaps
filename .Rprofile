source("renv/activate.R")

options(
    languageserver.formatting_style = function(options) {
        style <- styler::tidyverse_style(indent_by = options$tabSize)
        style$token$force_assignment_op <- NULL
        style
    },
    width = 80
)
