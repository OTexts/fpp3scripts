source("before-each-chapter.R")

## ----HierTree, echo=FALSE, fig.cap="A two level hierarchical tree diagram.", message=FALSE, warning=FALSE, fig.show = "hold",fig.height=6.2,fig.width=11, out.width="40%"----
if (html) {
  knitr::include_graphics("figs/hts.png")
} else {
  g <- igraph::graph_from_literal(Total - -A:B, A - -AA:AB:AC, B - -BA:BB)
  layout <- igraph::layout_as_tree(g, root = "Total")
  igraph::V(g)$color <- c(
    "Thistle", "GreenYellow", "LightBlue",
    rep("GreenYellow", 3), rep("LightBlue", 2)
  )
  igraph::V(g)$label.cex <- 2
  plot(g, layout = layout, vertex.size = 40)
}

## ----aus-states-tab, echo=FALSE---------------------------------------------------------------------------
tab <- data.frame(
    State = c("Australian Capital Territory", "New South Wales", "Northern Territory", "Queensland", "South Australia", "Tasmania", "Victoria", "Western Australia"),
    Region = c("Canberra", "Blue Mountains, Capital Country, Central Coast, Central NSW, Hunter, New England North West, North Coast NSW, Outback NSW, Riverina, Snowy Mountains, South Coast, Sydney, The Murray.", "Alice Springs, Barkly, Darwin, Kakadu Arnhem, Katherine Daly, Lasseter, MacDonnell.", "Brisbane, Bundaberg, Central Queensland, Darling Downs, Fraser Coast, Gold Coast, Mackay, Northern Outback, Sunshine Coast, Tropical North Queensland, Whitsundays.", "Adelaide, Adelaide Hills, Barossa, Clare Valley, Eyre Peninsula, Fleurieu Peninsula, Flinders Ranges and Outback, Kangaroo Island, Limestone Coast, Murraylands, Riverland, Yorke Peninsula.", "East Coast, Hobart and the South, Launceston Tamar and the North, North West, Wilderness West.", "Ballarat, Bendigo Loddon, Central Highlands, Central Murray, Geelong and the Bellarine, Gippsland, Goulburn, Great Ocean Road, High Country, Lakes, Macedon, Mallee, Melbourne, Melbourne East, Murray East, Peninsula, Phillip Island, Spa Country, Upper Yarra, Western Grampians, Wimmera.", "Australia's Coral Coast, Australia's Golden Outback, Australia's North West, Australia's South West, Experience Perth.")
  ) %>%
  knitr::kable(caption = "Australian tourism regions.",
               booktabs = TRUE,
               longtable = FALSE,
               align = if(html){c("l","l")}else{c("l","p{8.5cm}")},
               format = if_else(html,"html","latex")
  )
if (!html) {
  tab <- gsub("\\\\centering","\\\\vspace*{0.8cm}\\\\centering\\\\small",tab)
  tab <- gsub("\\[t\\]","\\[!ht\\]",tab)
}
tab

## ----recode-----------------------------------------------------------------------------------------------
tourism <- tsibble::tourism %>%
  mutate(State = recode(State,
    `New South Wales` = "NSW",
    `Northern Territory` = "NT",
    `Queensland` = "QLD",
    `South Australia` = "SA",
    `Tasmania` = "TAS",
    `Victoria` = "VIC",
    `Western Australia` = "WA"
  ))

## ----nested, echo=TRUE------------------------------------------------------------------------------------
tourism_hts <- tourism %>%
  aggregate_key(State / Region, Trips = sum(Trips))
tourism_hts

## ----tourismStates, fig.width=9, fig.asp=0.7, fig.cap="Domestic overnight trips from 1998 Q1 to 2017 Q4 aggregated by state.", warning=FALSE, message=FALSE, echo=TRUE----
tourism_hts %>%
  filter(is_aggregated(Region)) %>%
  autoplot(Trips) +
  labs(y = "Trips ('000)",
       title = "Australian tourism: national and states") +
  facet_wrap(vars(State), scales = "free_y", ncol = 3) +
  theme(legend.position = "none")

## ----seasonStates, echo=FALSE, fig.cap="Seasonal plots for overnight trips for Queensland and the Northern Territory, and Victoria and Tasmania highlighting the contrast in seasonal patterns between northern and southern states in Australia.", fig.asp=0.5, fig.width=7, out.width="80%", message=FALSE, warning=FALSE----
tourism_hts %>%
  filter(State == "NT" | State == "QLD" |
           State == "TAS" | State == "VIC", is_aggregated(Region)) %>%
  select(-Region) %>%
  mutate(State = factor(State, levels=c("QLD","VIC","NT","TAS"))) %>%
  gg_season(Trips) +
  facet_wrap(vars(State), nrow = 2, scales = "free_y")+
  labs(y = "Trips ('000)")

## ----tourismRegions, echo=FALSE, fig.asp=0.6, fig.cap="Domestic overnight trips from 1998 Q1 to 2017 Q4 for some selected regions.", fig.width=9, message=FALSE, warning=FALSE----
tourism_hts %>%
  filter(vctrs::vec_in(Region, c("North Coast NSW", "Snowy Mountains", "Hunter", "New England North West", "Alice Springs", "Darwin", "Kakadu Arnhem", "MacDonnell", "Brisbane", "Gold Coast", "Northern Outback", "Sunshine Coast", "Tropical North Queensland", "Adelaide Hills", "Murraylands", "Yorke Peninsula", "Kangaroo Island", "Ballarat", "Great Ocean Road", "High Country", "Goulburn", "Australia's Coral Coast", "Australia's Golden Outback", "Australia's North West", "Australia's North West"))) %>%
  autoplot() +
  facet_wrap(State ~ ., scales = "free_y", ncol = 3) +
  labs(y = "Trips ('000)",
       title = "Australian tourism: by regions nested within states") +
  theme(legend.position = "none")

## ----GroupTree, echo=FALSE, fig.cap="Alternative representations of a two level grouped structure.", out.width="60%", fig.show = "hold",fig.height=8,fig.width=14----
par(mfrow = c(1, 2))
g <- igraph::graph_from_literal(Total - -A:B, A - -AX:AY, B - -BX:BY)
layout <- igraph::layout_as_tree(g, root = "Total")
igraph::V(g)$color <- c(
  "Thistle", "GreenYellow", "LightBlue",
  rep("GreenYellow", 2), rep("LightBlue", 2)
)
igraph::V(g)$label.cex <- 3
plot(g, layout = layout, vertex.size = 54)

g2 <- igraph::graph_from_literal(Total - -X:Y, X - -AX:BX, Y - -AY:BY)
layout2 <- igraph::layout_as_tree(g2, root = "Total")
igraph::V(g2)$color <- c(
  "Thistle", "GreenYellow", "LightBlue",
  rep("GreenYellow", 2), rep("LightBlue", 2)
)
igraph::V(g2)$label.cex <- 3
plot(g2, layout = layout2, vertex.size = 54)
par(mfrow = c(1, 1))

## ----prisongts, fig.width=9, fig.asp = .7, echo=FALSE, fig.cap="Total Australian quarterly adult prison population, disaggregated by state, by legal status, and by gender.", warning=FALSE, message=FALSE, fig.pos="b", fig.env="figure*"----
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(Gender, Legal, State, Indigenous),
             index = Quarter) %>%
  relocate(Quarter)

prison_gts <- prison %>%
  aggregate_key(Gender * Legal * State, Count = sum(Count) / 1e3)

p1 <- prison_gts %>%
  filter(
    is_aggregated(Gender),
    is_aggregated(Legal),
    is_aggregated(State)
  ) %>%
  autoplot(Count) +
  labs(y = "Number of prisoners ('000)",
       title = "Prison population: Total")

p2 <- prison_gts %>%
  filter(
    (!is_aggregated(Gender)) +
    (!is_aggregated(Legal)) +
    (!is_aggregated(State)) == 1) %>%
  mutate(
    disaggregator = case_when(
      !is_aggregated(Gender) ~ "Gender",
      !is_aggregated(Legal) ~ "Legal",
      !is_aggregated(State) ~ "State"
    ),
    value = case_when(
      !is_aggregated(Gender) ~ as.character(Gender),
      !is_aggregated(Legal) ~ as.character(Legal),
      !is_aggregated(State) ~ as.character(State)
    ),
    series = paste(disaggregator, value, sep = "/")
  ) %>%
  ggplot(aes(x = Quarter, y = Count, colour = series)) +
  geom_line() +
  labs(y = "Number of prisoners ('000)") +
  facet_wrap(vars(disaggregator), scales = "free_y")

p1 / p2

## ----prison1, fig.width = 9, fig.asp = 0.8, echo=FALSE, fig.cap="Australian adult prison population disaggregated by pairs of attributes.", message=FALSE, warning=FALSE, dependson="prisongts", fig.env="figure*"----
p5 <- prison_gts %>%
  filter(
    !is_aggregated(Gender),
    !is_aggregated(Legal),
    !is_aggregated(State)
  ) %>%
  mutate(Gender = factor(Gender, levels=c("Male","Female"))) %>%
  ggplot(aes(x = Quarter, y = Count, group = Gender, colour = Gender)) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and gender", y = "") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_wrap(~ as.character(State), nrow = 1, scales = "free_y") +
  scale_colour_manual(values=c(Male = "#0072B2", Female="#D55E00")) +
  guides(colour = guide_legend("Gender"))

p6 <- prison_gts %>%
  filter(
    !is_aggregated(Gender),
    !is_aggregated(Legal),
    !is_aggregated(State)
  ) %>%
  mutate(Legal = factor(Legal, levels=c("Sentenced","Remanded"))) %>%
  ggplot(aes(
    x = Quarter, y = Count,
    group = Legal, colour = Legal
  )) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by state and legal status",
       y = "Number of prisoners ('000)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(colour = guide_legend("Legal status")) +
  facet_wrap(~ as.character(State), nrow = 1, scales = "free_y") +
  scale_colour_manual(values=c(Remanded = "#cc79a7", Sentenced="#e69f00"))

p7 <- prison_gts %>%
  filter(
    !is_aggregated(Gender),
    !is_aggregated(Legal),
    !is_aggregated(State)
  ) %>%
  mutate(Gender = factor(Gender, levels=c("Male","Female"))) %>%
  ggplot(aes(
    x = Quarter, y = Count,
    group = Gender, colour=Gender,
  )) +
  stat_summary(fun = sum, geom = "line") +
  labs(title = "Prison population by legal status and gender", y = "") +
  facet_wrap(~ as.character(Legal), nrow = 1, scales = "free_y") +
  scale_colour_manual(values=c(Male = "#0072B2", Female="#D55E00")) +
  guides(colour = "none")

(p5 / p6 / p7)

## ----prisonBTS, fig.width=9, fig.asp = 0.3, echo=FALSE, fig.cap="Bottom-level time series for the Australian adult prison population, grouped by state, legal status and gender.", message=FALSE, warning=FALSE, dependson="prisongts",fig.env="figure*"----
prison_gts %>%
  filter(
    !is_aggregated(State), !is_aggregated(Gender),
    !is_aggregated(Legal)
  ) %>%
  mutate(
    faceter = factor(paste(Legal, Gender, sep = " / "),
      levels = c("Sentenced / Male", "Remanded / Male",
                 "Sentenced / Female", "Remanded / Female")
    )
  ) %>%
  ggplot(aes(x = Quarter, y = Count, colour = faceter)) +
  geom_line() +
  labs(title = "Australian prison population: bottom-level series",
       y = "Number of prisoners ('000)") +
  guides(colour = guide_legend("Legal status & Gender")) +
  facet_wrap(vars(as.character(State)), nrow = 1, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----mixed, echo=TRUE-------------------------------------------------------------------------------------
tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

## ----mixed-purpose, fig.width=10, fig.asp = 0.6, echo=FALSE, fig.cap="Australian domestic overnight trips from 1998 Q1 to 2017 Q4 disaggregated by purpose of travel.", message=FALSE, warning=FALSE, dependson="mixed",fig.env="figure*"----
tourism_full %>%
  filter(is_aggregated(State), is_aggregated(Region), !is_aggregated(Purpose)) %>%
  ggplot(aes(x = Quarter, y = Trips,
             group = as.character(Purpose), colour = as.character(Purpose))
  ) +
  stat_summary(fun = sum, geom = "line") +
  facet_wrap(~ as.character(Purpose), scales = "free_y", nrow = 2) +
  labs(title = "Australian tourism: by purpose of travel",
       y = "Trips ('000)") +
  # theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(colour = guide_legend("Purpose"))

## ----mixed-state-purpose, fig.width=10, fig.asp = 0.6, echo=FALSE, fig.cap="Australian domestic overnight trips over the period 1998 Q1 to 2017 Q4 disaggregated by purpose of travel and by state.", message=FALSE, warning=FALSE, dependson="mixed",fig.env="figure*"----
tourism_full %>%
  filter(!is_aggregated(State), is_aggregated(Region), !is_aggregated(Purpose)) %>%
  ggplot(aes(x = Quarter, y = Trips,
             group = as.character(Purpose), colour = as.character(Purpose))
  ) +
  stat_summary(fun = sum, geom = "line") +
  facet_wrap(~ as.character(State), scales = "free_y", nrow = 2) +
  labs(title = "Australian tourism: by purpose of travel and state",
       y = "Trips ('000)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  guides(colour = guide_legend("Purpose"))

## ----tourism_states, message=FALSE------------------------------------------------------------------------
tourism_states <- tourism %>%
  aggregate_key(State, Trips = sum(Trips))

## ----bu_by_hand, message=FALSE----------------------------------------------------------------------------
fcasts_state <- tourism_states %>%
  filter(!is_aggregated(State)) %>%
  model(ets = ETS(Trips)) %>%
  forecast()

# Sum bottom-level forecasts to get top-level forecasts
fcasts_national <- fcasts_state %>%
  summarise(value = sum(Trips), .mean = mean(value))

## ----bottom_up, message=FALSE-----------------------------------------------------------------------------
tourism_states %>%
  model(ets = ETS(Trips)) %>%
  reconcile(bu = bottom_up(ets)) %>%
  forecast()

## ----tourismfit, echo=TRUE--------------------------------------------------------------------------------
tourism_full <- tourism %>%
  aggregate_key((State/Region) * Purpose, Trips = sum(Trips))

fit <- tourism_full %>%
  filter(year(Quarter) <= 2015) %>%
  model(base = ETS(Trips)) %>%
  reconcile(
    bu = bottom_up(base),
    ols = min_trace(base, method = "ols"),
    mint = min_trace(base, method = "mint_shrink"),
  )

## ----tourismfc, echo=TRUE, message=FALSE, warning=FALSE, dependson="tourismfit"---------------------------
fc <- fit %>% forecast(h = "2 years")

## ----tourism-states, fig.width=10, fig.asp = .55, echo=TRUE, fig.cap="Forecasts of overnight trips for Australia and its states over the test period 2016Q1--2017Q4.", warning=FALSE, message=FALSE, fig.pos="!htb", fig.env="figure*", dependson="tourismfc"----
fc %>%
  filter(is_aggregated(Region), is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(State), scales = "free_y")

## ----tourism-purpose, echo=FALSE, fig.asp=.45, echo=TRUE, fig.cap="Forecasts of overnight trips by purpose of travel over the test period 2016Q1--2017Q4.", fig.env="figure*", fig.pos="!htb", fig.width=10, message=FALSE, warning=FALSE, dependson="tourismfc"----
fc %>%
  filter(is_aggregated(State), !is_aggregated(Purpose)) %>%
  autoplot(
    tourism_full %>% filter(year(Quarter) >= 2011),
    level = NULL
  ) +
  labs(y = "Trips ('000)") +
  facet_wrap(vars(Purpose), scales = "free_y")

## ----tourism-evaluation, echo=FALSE, message=FALSE, warning=FALSE, dependson="tourismfc"------------------
tab <- matrix(NA, ncol = 8, nrow = 6)
rownames(tab) <- c("Total", "Purpose", "State", "Regions", "Bottom", "All series")
colnames(tab) <- c("Base", "Bottom-up", "MinT", "OLS", "Base", "Bottom-up", "MinT", "OLS")

filter_tab <- matrix(NA, ncol = 1, nrow = 6)

filter_tab[1] <- "fc %>% filter(is_aggregated(State),is_aggregated(Region),is_aggregated(Purpose))"
filter_tab[2] <- "fc %>%
filter(is_aggregated(State),is_aggregated(Region),!is_aggregated(Purpose))"
filter_tab[3] <- "fc %>% filter(!is_aggregated(State),is_aggregated(Region),is_aggregated(Purpose))"
filter_tab[4] <- "fc %>%
filter(!is_aggregated(State),!is_aggregated(Region),is_aggregated(Purpose))"
filter_tab[5] <- "fc %>% filter(!is_aggregated(State),!is_aggregated(Region),!is_aggregated(Purpose))"
filter_tab[6] <- "fc"

for (i in 1:6) {
  eval(parse(text = filter_tab[i])) %>%
    accuracy(data = tourism_full, measures = list(rmse = RMSE, mase = MASE)) %>%
    group_by(.model) %>%
    summarise(rmse = mean(rmse), mase = mean(mase)) -> err
  tab[i, ] <- cbind(t(err[, 2]), t(err[, 3]))
}

out <- knitr::kable(tab,
  digits = 2, booktabs = TRUE,
  format = ifelse(html, "html", "latex"),
  caption = "Accuracy of forecasts for Australian overnight trips over the test set 2016Q1--2017Q4.",
  table.envir="table*"
) %>%
  kableExtra::add_header_above(c(" " = 1, "RMSE" = 4, "MASE" = 4))
if (!html) {
  out <- gsub("\\[t\\]", "\\[b\\]", out)
}
out

## ----fcaccuracy2, message=FALSE, dependson="tourismfc"----------------------------------------------------
fc %>%
  filter(is_aggregated(State), is_aggregated(Purpose)) %>%
  accuracy(
    data = tourism_full,
    measures = list(rmse = RMSE, mase = MASE)
  ) %>%
  group_by(.model) %>%
  summarise(rmse = mean(rmse), mase = mean(mase))

## ----prisonfc---------------------------------------------------------------------------------------------
fit <- prison_gts %>%
  filter(year(Quarter) <= 2014) %>%
  model(base = ETS(Count)) %>%
  reconcile(
    bottom_up = bottom_up(base),
    MinT = min_trace(base, method = "mint_shrink")
  )
fc <- fit %>% forecast(h = 8)

## ----prisonforecasts-aggregate, fig.width=8, fig.asp = .5, echo=TRUE, fig.cap="Forecasts for the total Australian quarterly adult prison population for the period 2015Q1--2016Q4.", warning=FALSE, message=FALSE, fig.pos="b", fig.env="figure*", dependson="prisonfc"----
fc %>%
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) %>%
  autoplot(prison_gts, alpha = 0.7, level = 90) +
  labs(y = "Number of prisoners ('000)",
       title = "Australian prison population (total)")

## ----prisonforecasts-State, fig.width=8, fig.asp = 0.45, echo=TRUE, fig.cap="Forecasts for the Australian quarterly adult prison population, disaggregated by state.", warning=FALSE, message=FALSE, fig.pos="!htb", fig.env="figure*"----
fc %>%
  filter(
    .model %in% c("base", "MinT"),
    !is_aggregated(State), is_aggregated(Legal),
    is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by state)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(State), scales = "free_y", ncol = 4) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----prisonforecasts-LegalGender, fig.width=9, fig.asp = 0.35, echo=FALSE, fig.cap="Forecasts for the Australian quarterly adult prison population, disaggregated by legal status and by gender.", warning=FALSE, message=FALSE, fig.pos="!htb", fig.env="figure*"----
p1 <- fc %>%
  filter(
    .model %in% c("MinT", "base"), !is_aggregated(Legal),
    is_aggregated(State), is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by legal status)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(Legal), scales = "free_y", nrow = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p2 <- fc %>%
  filter(
    .model %in% c("MinT", "base"),
    is_aggregated(State), is_aggregated(Legal),
    !is_aggregated(Gender)
  ) %>%
  autoplot(
    prison_gts %>% filter(year(Quarter) >= 2010),
    alpha = 0.7, level = 90
  ) +
  labs(title = "Prison population (by gender)",
       y = "Number of prisoners ('000)") +
  facet_wrap(vars(Gender), scales = "free_y", nrow = 1) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

(p1 | p2) + plot_layout(guides = "collect")

## ----prisonforecasts-bottom, fig.width=8, fig.asp = 0.8, echo=FALSE, fig.cap="Forecasts for bottom-level series the Australian quarterly adult prison population, disaggregated by state, by legal status and by gender.", warning=FALSE, message=FALSE, fig.pos="!htb", fig.env="figure*"----
fc %>%
  mutate(Sex_Legal = paste(Gender, "+", Legal)) %>%
  as_fable(index = Quarter, key = c(Sex_Legal, State, .model)) %>%
  filter(
    .model %in% c("base", "MinT"),
    vctrs::vec_in(State, c("NSW", "QLD", "VIC", "WA")), !is_aggregated(Gender),
    !is_aggregated(Legal)
  ) %>%
  autoplot(
    prison_gts %>%
      filter(year(Quarter) >= 2010) %>%
      mutate(Sex_Legal = paste(Gender, "+", Legal)) %>%
      as_tsibble(index = Quarter, key = c(Sex_Legal, State)),
    alpha = 0.7, level = 90
  ) +
  labs(y = "Number of prisoners ('000)") +
  facet_wrap(Sex_Legal ~ State, scales = "free_y") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

## ----prisonaccuracy, dependson="prisonfc", message=FALSE--------------------------------------------------
fc %>%
  filter(is_aggregated(State), is_aggregated(Gender),
         is_aggregated(Legal)) %>%
  accuracy(data = prison_gts,
           measures = list(mase = MASE,
                           ss = skill_score(CRPS)
                           )
           ) %>%
  group_by(.model) %>%
  summarise(mase = mean(mase), sspc = mean(ss) * 100)

## ----tab-crime-evaluation,echo=FALSE, message=FALSE-------------------------------------------------------
tab <- matrix(NA, ncol = 6, nrow = 6)
rownames(tab) <- c("Total", "State", "Legal status", "Gender", "Bottom", "All series")
colnames(tab) <- c("Base", "Bottom-up", "MinT", "Base", "Bottom-up", "MinT")

filter_tab <- matrix(NA, ncol = 1, nrow = 6)

filter_tab[1] <- "fc %>% filter(is_aggregated(State),is_aggregated(Gender),is_aggregated(Legal))"
filter_tab[2] <- "fc %>% filter(!is_aggregated(State),is_aggregated(Gender),is_aggregated(Legal))"
filter_tab[3] <- "fc %>% filter(is_aggregated(State),is_aggregated(Gender),!is_aggregated(Legal))"
filter_tab[4] <- "fc %>% filter(is_aggregated(State),!is_aggregated(Gender),is_aggregated(Legal))"
filter_tab[5] <- "fc %>% filter(!is_aggregated(State),!is_aggregated(Gender),!is_aggregated(Legal))"
filter_tab[6] <- "fc "

i <- 1
for (i in 1:6) {
  eval(parse(text = filter_tab[i])) %>%
    accuracy(
      data = prison_gts,
      measures = list(mase = MASE, ss = skill_score(CRPS))
    ) %>%
    group_by(.model) %>%
    summarise(mase = mean(mase), sspc = mean(ss) * 100) -> err
  tab[i, ] <- cbind(t(err[, 2]), t(err[, 3]))
}

out <- knitr::kable(tab,
  digits = 2, booktabs = TRUE,
  format = ifelse(html, "html", "latex"),
  caption = "Accuracy of Australian prison population forecasts for different groups of series."
) %>%
  kableExtra::add_header_above(c(" " = 1, "MASE" = 3, "Skill Score (CRPS)" = 3))
if (!html) {
  out <- gsub("\\[t\\]", "\\[b\\]", out)
}
out

