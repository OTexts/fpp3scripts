library(fpp3, quietly = TRUE)
library(patchwork)

set.seed(1967)

html <- knitr::is_html_output()

# Figure and chunk defaults are set by the otexts format (_extensions/).
# The rest of the setup shared by the OTexts books is in the otexts package:
# plot fonts (BookMath for TeX() labels), printing options, viridis and
# Okabe-Ito colour scales, the ggplot2 theme, conflicts resolved, html_math(),
# and TeX() and theme_void() defined so chunks can call them.
library(otexts)
setup_book(
  prefer = c(
    VAR = "fable",
    select = "dplyr",
    filter = "dplyr",
    vars = "dplyr",
    invoke = "purrr"
  )
)
