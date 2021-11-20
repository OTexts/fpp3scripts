set.seed(1967)

knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  echo = TRUE,
  cache = TRUE,
  out.width = "100%",
  fig.align = 'center',
  fig.width = 7,
  fig.asp = 0.618  # 1 / phi
)

library(fpp3, quietly=TRUE)
library(patchwork)

html <- knitr::is_html_output()

# Set some defaults
# Colours to be viridis for continuous scales and Okabe for discrete scales
options(
  digits = 4,
  width=58 + html*20,
  ggplot2.continuous.colour="viridis",
  ggplot2.continuous.fill = "viridis",
  ggplot2.discrete.colour = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442"),
  ggplot2.discrete.fill = c("#D55E00", "#0072B2","#009E73", "#CC79A7", "#E69F00", "#56B4E9", "#F0E442")
)

# Avoid some conflicts
conflicted::conflict_prefer("VAR","fable")
conflicted::conflict_prefer("select","dplyr")
conflicted::conflict_prefer("filter","dplyr")
conflicted::conflict_prefer("vars","dplyr")
conflicted::conflict_prefer("invoke","purrr")

