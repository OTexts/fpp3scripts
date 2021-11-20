source("before-each-chapter.R")

## ----7-oil, fig.cap="Exports of goods and services from Algeria from 1960 to 2017.", echo=TRUE------------
algeria_economy <- global_economy %>%
  filter(Country == "Algeria")
algeria_economy %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Exports: Algeria")

## ----alpha, echo=FALSE------------------------------------------------------------------------------------
tab <- as.data.frame(matrix(NA, nrow = 6, ncol = 4))
rownames(tab) <- c("$y_{T}$", paste("$y_{T-", 1:5, "}$", sep = ""))
alpha <- c(0.2, 0.4, 0.6, 0.8)
colnames(tab) <- paste("$\\alpha=", alpha, "$", sep = "")
for (i in 1:6) {
  tab[i, ] <- alpha * (1 - alpha)^(i - 1)
}
knitr::kable(tab, digits = 4, booktabs = TRUE, escape = FALSE)

## ----sesfit, echo=TRUE------------------------------------------------------------------------------------
# Estimate parameters
fit <- algeria_economy %>%
  model(ETS(Exports ~ error("A") + trend("N") + season("N")))
fc <- fit %>%
  forecast(h = 5)

## ----sesparam, echo=FALSE, dependson='sesfit'-------------------------------------------------------------
alpha <- tidy(fit)$estimate[1]
l0 <- tidy(fit)$estimate[2]

## ----export-ses, echo=FALSE, dependson='sesfit'-----------------------------------------------------------
format_num <- function(x, decplaces = 2) {
  fmt <- paste0("%.", decplaces, "f")
  ifelse(is.na(x), "", sprintf(fmt, x))
}

# Data set for table
fc_tbl <- fc %>%
  as_tibble() %>%
  transmute(Year, .fitted = .mean) %>%
  mutate(Time = as.character(row_number()), .fitted = format_num(.fitted)) %>%
  bind_rows(tibble(Time = "$h$", .fitted = "$\\hat{y}_{T+h\\vert T}$"), .)

options(knitr.kable.NA = "")
as_tsibble(components(fit)) %>%
  left_join(augment(fit), by = c("Country", ".model", "Exports", "Year")) %>%
  mutate(
    Time = as.character(row_number() - 1),
    .fitted = format_num(.fitted),
    Exports = format_num(Exports),
    level = format_num(level)
  ) %>%
  select(Year, Time, Exports, level, .fitted) %>%
  as_tibble() -> tmp

tab <- tmp %>%
  slice(1:9) %>%
  bind_rows(summarise_at(tmp, vars(-Year), function(...) if (html) "&#8942;" else "\\vdots")) %>%
  bind_rows(slice(tmp, (n() - 3):n())) %>%
  bind_rows(fc_tbl) %>%
  bind_rows(tibble(Time = "$t$", Exports = "$y_t$", level = "$\\ell_t$", .fitted = "$\\hat{y}_{t\\vert t-1}$"), .) %>%
  transmute(Year, Time, Observation = Exports, Level = level, Forecast = .fitted) %>%
  knitr::kable(caption = "Forecasting goods and services exports from Algeria using simple exponential smoothing.", booktabs = TRUE, escape = FALSE)

if (html) {
  tab
} else {
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.3cm}", tab)
}

## ----ses, fig.cap="Simple exponential smoothing applied to exports from Algeria (1960--2017). The orange curve shows the one-step-ahead fitted values.", echo=TRUE, dependson='sesfit'----
fc %>%
  autoplot(algeria_economy) +
  geom_line(aes(y = .fitted), col="#D55E00",
            data = augment(fit)) +
  labs(y="% of GDP", title="Exports: Algeria") +
  guides(colour = "none")

## ----auspop, echo=TRUE, fig.cap="Australia's population, 1960-2017."--------------------------------------
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Pop = Population / 1e6)
autoplot(aus_economy, Pop) +
  labs(y = "Millions", title = "Australian population")

## ----popholt0, echo=TRUE----------------------------------------------------------------------------------
fit <- aus_economy %>%
  model(
    AAN = ETS(Pop ~ error("A") + trend("A") + season("N"))
  )
fc <- fit %>% forecast(h = 10)

## ----popcoeff, echo=FALSE---------------------------------------------------------------------------------
est <- tidy(fit)
alpha <- est %>% filter(term=="alpha") %>% pull(estimate)
beta <- est %>% filter(term=="beta") %>% pull(estimate)
betastar <- beta/alpha

## ----popholt, echo=FALSE----------------------------------------------------------------------------------
# Data set for table
fc_tbl <- fc %>%
  as_tibble() %>%
  transmute(Year, .fitted = .mean) %>%
  mutate(Time = as.character(row_number()), .fitted = format_num(.fitted)) %>%
  bind_rows(tibble(Time = "$h$", .fitted = "$\\hat{y}_{T+h\\mid T}$"), .)

options(knitr.kable.NA = "")
tmp <- as_tsibble(components(fit)) %>%
  left_join(augment(fit), by = c("Country", ".model", "Pop", "Year")) %>%
  mutate(
    Time = as.character(row_number() - 1),
    .fitted = format_num(.fitted),
    Pop = format_num(Pop),
    level = format_num(level),
    slope = format_num(slope)
  ) %>%
  select(Year, Time, Pop, level, slope, .fitted) %>%
  as_tibble() %>%
  bind_rows(tibble(Time = "$t$", Pop = "$y_t$", level = "$\\ell_t$", .fitted = "$\\hat{y}_{t+1\\mid t}$"), .)

tmp <- tmp %>%
  slice(1:9) %>%
  bind_rows(summarise_at(tmp, vars(-Year), function(...) if (html) "&#8942;" else "\\vdots")) %>%
  bind_rows(slice(tmp, (n() - 3):n())) %>%
  bind_rows(fc_tbl) %>%
  transmute(Year, Time, Observation = Pop, Level = level, Slope = slope, Forecast = .fitted)

caption <- "Forecasting Australian annual population using Holt's linear trend method."

if (html) {
  knitr::kable(tmp, booktabs = TRUE, escape = FALSE, caption = caption)
} else {
  knitr::kable(tmp, booktabs = TRUE, escape = FALSE, caption = caption) %>%
    kableExtra::kable_styling(font_size = 9)
}

## ----checkholt, echo=FALSE--------------------------------------------------------------------------------
if (sum(abs(tidy(fit)$estimate[1:2] - c(0.9999, 0.3266))) > 1e-4) {
  stop("Parameter error")
}

## ----dampedtrend, fig.cap="Forecasting annual Australian population (millions) over 2018-2032. For the damped trend method, $\\phi=0.90$.", echo=TRUE, fig.asp=0.55----
aus_economy %>%
  model(
    `Holt's method` = ETS(Pop ~ error("A") +
                       trend("A") + season("N")),
    `Damped Holt's method` = ETS(Pop ~ error("A") +
                       trend("Ad", phi = 0.9) + season("N"))
  ) %>%
  forecast(h = 15) %>%
  autoplot(aus_economy, level = NULL) +
  labs(title = "Australian population",
       y = "Millions") +
  guides(colour = guide_legend(title = "Forecast"))

## ----www-usage, fig.cap="Users connected to the internet through a server", fig.asp=0.5, echo=TRUE--------
www_usage <- as_tsibble(WWWusage)
www_usage %>% autoplot(value) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

## ----expsmoothcv, echo=TRUE, warning=FALSE----------------------------------------------------------------
www_usage %>%
  stretch_tsibble(.init = 10) %>%
  model(
    SES = ETS(value ~ error("A") + trend("N") + season("N")),
    Holt = ETS(value ~ error("A") + trend("A") + season("N")),
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  ) %>%
  forecast(h = 1) %>%
  accuracy(www_usage)

## ----holtmodel, echo=TRUE---------------------------------------------------------------------------------
fit <- www_usage %>%
  model(
    Damped = ETS(value ~ error("A") + trend("Ad") +
                   season("N"))
  )
# Estimated parameters:
tidy(fit)

## ----fig-7-comp, fig.cap="Forecasting internet usage: comparing forecasting performance of non-seasonal methods.", echo=TRUE, dependson='holtmodel', fig.asp=0.5----
fit %>%
  forecast(h = 10) %>%
  autoplot(www_usage) +
  labs(x="Minute", y="Number of users",
       title = "Internet usage per minute")

## ----wwwcoef, echo=FALSE----------------------------------------------------------------------------------
phi <- fit %>% tidy() %>% filter(term=="phi") %>% pull(estimate) %>% format_num(3)

## ----7-HW, fig.cap="Forecasting domestic overnight trips in Australia using the Holt-Winters method with both additive and multiplicative seasonality.", echo=TRUE----
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays %>%
  model(
    additive = ETS(Trips ~ error("A") + trend("A") +
                                                season("A")),
    multiplicative = ETS(Trips ~ error("M") + trend("A") +
                                                season("M"))
  )
fc <- fit %>% forecast(h = "3 years")
fc %>%
  autoplot(aus_holidays, level = NULL) +
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)") +
  guides(colour = guide_legend(title = "Forecast"))

## ----tab75, echo=FALSE, dependson="7-HW"------------------------------------------------------------------
fit_add <- fit %>% select(additive)

# Data set for table
fc_tbl <- fc %>%
  filter(.model == "additive") %>%
  as_tibble() %>%
  transmute(Quarter = as.character(Quarter), .fitted = .mean) %>%
  mutate(Time = as.character(row_number()), .fitted = format_num(.fitted, 1)) %>%
  bind_rows(tibble(Time = "$h$", .fitted = "$\\hat{y}_{T+h\\vert T}$"), .)

options(knitr.kable.NA = "")
cmp_tbl <- as_tsibble(components(fit_add)) %>%
  left_join(augment(fit_add), by = c(".model", "Trips", "Quarter")) %>%
  as_tibble() %>%
  mutate(
    Quarter = as.character(Quarter),
    Time = as.character(row_number() - 1),
    .fitted = format_num(.fitted, 1),
    Trips = format_num(Trips, 1),
    level = format_num(level, 1),
    slope = format_num(slope, 1),
    season = format_num(season, 1)
  ) %>%
  select(Quarter, Time, Trips, level, slope, season, .fitted) %>%
  bind_rows(tibble(
    Quarter = "",
    Time = "$t$",
    Trips = "$y_t$",
    level = "$\\ell_t$",
    slope = "$b_t$",
    season = "$s_t$",
    .fitted = "$\\hat{y}_{t+1\\vert t}$"
  ), .)

ctab <- cmp_tbl %>%
  slice(1:9) %>%
  bind_rows(summarise_at(cmp_tbl, vars(-Quarter), function(...) if (html) "&#8942;" else "\\vdots")) %>%
  bind_rows(slice(cmp_tbl, (n() - 3):n())) %>%
  bind_rows(fc_tbl) %>%
  transmute(Quarter, Time, Observation = Trips, Level = level, Slope = slope, Season = season, Forecast = .fitted)

fit_add_coef <- tidy(fit_add)
alpha <- fit_add_coef %>% filter(term=="alpha") %>% pull(estimate)
beta <- fit_add_coef %>% filter(term=="beta") %>% pull(estimate)
betastar <- format_num(beta / alpha, 4)
alpha <- format_num(alpha, 4)
gamma <- fit_add_coef %>% filter(term=="gamma") %>% pull(estimate) %>% format_num(4)
rmse <- accuracy(fit_add) %>% pull(RMSE) %>% format_num(4)

caption <- paste0("Applying Holt-Winters' method with additive seasonality for forecasting domestic tourism in Australia. Notice that the additive seasonal component sums to approximately zero. The smoothing parameters and initial estimates for the components are $\\alpha = ", alpha, "$, $\\beta^* = ", betastar, "$, $\\gamma = ", gamma, "$ and RMSE $=", rmse,"$.")
if (html) {
  knitr::kable(ctab, digits = 2, escape = FALSE, caption = caption, align = "r")
} else {
  knitr::kable(ctab,
    digits = 2, escape = FALSE,
    booktabs = TRUE, caption = caption, align = "r"
  ) %>%
    kableExtra::kable_styling(font_size = 9)
}

## ----tab76, echo=FALSE, dependson="tab75"-----------------------------------------------------------------
fit_mult <- fit %>% select(multiplicative)

# Data set for table
fc_tbl <- fc %>%
  filter(.model == "multiplicative") %>%
  as_tibble() %>%
  transmute(Quarter = as.character(Quarter), .fitted = .mean) %>%
  mutate(Time = as.character(row_number()), .fitted = format_num(.fitted, 1)) %>%
  bind_rows(tibble(Time = "$h$", .fitted = "$\\hat{y}_{T+h\\vert T}$"), .)

options(knitr.kable.NA = "")
cmp_tbl <- as_tsibble(components(fit_mult)) %>%
  left_join(augment(fit_mult), by = c(".model", "Trips", "Quarter")) %>%
  as_tibble() %>%
  mutate(
    Quarter = as.character(Quarter),
    Time = as.character(row_number() - 1),
    .fitted = format_num(.fitted, 1),
    Trips = format_num(Trips, 1),
    level = format_num(level, 1),
    slope = format_num(slope, 1),
    season = format_num(season, 1)
  ) %>%
  select(Quarter, Time, Trips, level, slope, season, .fitted) %>%
  bind_rows(tibble(
    Quarter = "",
    Time = "$t$",
    Trips = "$y_t$",
    level = "$\\ell_t$",
    slope = "$b_t$",
    season = "$s_t$",
    .fitted = "$\\hat{y}_{t+1\\vert t}$"
  ), .)

ctab <- cmp_tbl %>%
  slice(1:9) %>%
  bind_rows(summarise_at(cmp_tbl, vars(-Quarter), function(...) if (html) "&#8942;" else "\\vdots")) %>%
  bind_rows(slice(cmp_tbl, (n() - 3):n())) %>%
  bind_rows(fc_tbl) %>%
  transmute(Quarter, Time, Observation = Trips, Level = level, Slope = slope, Season = season, Forecast = .fitted)

fit_mult_coef <- tidy(fit_mult)
alpha <- fit_mult_coef %>% filter(term=="alpha") %>% pull(estimate)
beta <- fit_mult_coef %>% filter(term=="beta") %>% pull(estimate)
betastar <- format_num(beta / alpha, 4)
alpha <- format_num(alpha, 4)
gamma <- fit_mult_coef %>% filter(term=="gamma") %>% pull(estimate) %>% format_num(4)
rmse <- accuracy(fit_mult) %>% pull(RMSE) %>% format_num(4)

caption <- paste0("Applying Holt-Winters' method with multiplicative seasonality for forecasting domestic tourism in Australia. Notice that the multiplicative seasonal component sums to approximately $m=4$. The smoothing parameters and initial estimates for the components are $\\alpha = ", alpha, "$, $\\beta^* = ", betastar, "$, $\\gamma = ", gamma, "$ and RMSE $=", rmse,"$.")
# Output
if (html) {
  knitr::kable(ctab, digits = 2, escape = FALSE, caption = caption, align = "r")
} else {
  knitr::kable(ctab,
    digits = 2, escape = FALSE,
    booktabs = TRUE, caption = caption, align = "r"
  ) %>%
    kableExtra::kable_styling(font_size = 9)
}

## ----fig-7-LevelTrendSeas, fig.cap="Estimated components for the Holt-Winters method with additive and multiplicative seasonal components.", echo=FALSE, dependson="7-HW", warning = FALSE, fig.width=8, fig.height=8, fig.asp=0.9, fig.env = 'figure*'----
p1 <- fit %>% select(additive) %>% components() %>% autoplot() + labs(subtitle = "Additive seasonality")
p2 <- fit %>% select(multiplicative) %>% components() %>% autoplot() + labs(subtitle = "Multiplicative seasonality")
p1 | p2

## ----hyndsight,  echo=TRUE, fig.cap="Forecasts of daily pedestrian traffic at the Southern Cross railway station, Melbourne.", fig.asp=0.45----
sth_cross_ped <- pedestrian %>%
  filter(Date >= "2016-07-01",
         Sensor == "Southern Cross Station") %>%
  index_by(Date) %>%
  summarise(Count = sum(Count)/1000)
sth_cross_ped %>%
  filter(Date <= "2016-07-31") %>%
  model(
    hw = ETS(Count ~ error("M") + trend("Ad") + season("M"))
  ) %>%
  forecast(h = "2 weeks") %>%
  autoplot(sth_cross_ped %>% filter(Date <= "2016-08-14")) +
  labs(title = "Daily traffic: Southern Cross",
       y="Pedestrians ('000)")

## ----taxonomy, echo=FALSE---------------------------------------------------------------------------------
tab <- data.frame(
  `X` = c("", "N (None)", "A (Additive)", "A$_d$ (Additive damped)"),
  N = c("(None)", "(N,N)", "(A,N)", "(A$_d$,N)"),
  A = c("(Additive)", "(N,A)", "(A,A)", "(A$_d$,A)"),
  M = c("(Multiplicative)", "(N,M)", "(A,M)", "(A$_d$,M)")
)
colnames(tab) <- c("", "N", "A", "M")
caption <- "A two-way classification of exponential smoothing methods."
if (html) {
  tab %>%
    knitr::kable(format = "html", escape = FALSE, caption = caption) %>%
    kableExtra::row_spec(0, background = "#e6e6e6") %>%
    kableExtra::row_spec(1, bold = TRUE, background = "#e6e6e6") %>%
    kableExtra::row_spec(2, background = "#f6f6f6") %>%
    kableExtra::row_spec(3, background = "#f6f6f6") %>%
    kableExtra::row_spec(4, background = "#f6f6f6") %>%
    kableExtra::column_spec(1, bold = TRUE, background = "#e6e6e6") %>%
    kableExtra::add_header_above(
      c("Trend Component" = 1, "Seasonal Component" = 3),
      align = "l"
    )
} else {
  colnames(tab) <- c("", "(None)", "(Additive)", "(Multiplicative)")
  tab <- tab[-1, ]
  tab <- knitr::kable(tab,
    booktabs = TRUE, format = "latex",
    row.names = FALSE, escape = FALSE, caption = caption
  ) %>%
    kableExtra::add_header_above(
      c(" " = 1, "N" = 1, "A" = 1, "M" = 1),
      align = "l", line = FALSE
    ) %>%
    kableExtra::add_header_above(
      c("Trend Component" = 1, "Seasonal Component" = 3),
      align = "l", bold = TRUE, line = FALSE
    ) %>%
    kableExtra::kable_styling(latex_options = "hold_position")
  tab <- gsub(
    "\\\\midrule",
    "\\\\cmidrule(l{2pt}r{2pt}){1-1} \\\\cmidrule(l{2pt}r{2pt}){2-4}",
    tab
  )
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.3cm}", tab)
}

## ----shorthand, echo=FALSE--------------------------------------------------------------------------------
tab <- tibble::tibble(
  `Short hand` = c(
    "(N,N)", "(A,N)", "(A$_d$,N)", "(A,A)", "(A,M)", "(A$_d$,M)"
  ),
  "Method" = c(
    "Simple exponential smoothing",
    "Holt's linear method",
    "Additive damped trend method",
    "Additive Holt-Winters' method",
    "Multiplicative Holt-Winters' method",
    "Holt-Winters' damped method"
  )
)
knitr::kable(tab, booktabs = TRUE, longtable = FALSE, escape = FALSE)

## ----austouristsets, echo=TRUE----------------------------------------------------------------------------
aus_holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  summarise(Trips = sum(Trips)/1e3)
fit <- aus_holidays %>%
  model(ETS(Trips))
report(fit)

## ----checkmna, include=FALSE------------------------------------------------------------------------------
stopifnot(as.character(fit[[1]]) == "<ETS(M,N,A)>")
alpha <- tidy(fit) %>% filter(term=="alpha") %>% pull(estimate) %>% format_num(4)
gamma <- tidy(fit) %>% filter(term=="gamma") %>% pull(estimate) %>% format_num(4)

## ----MNAstates, fig.cap="Graphical representation of the estimated states over time.", echo=TRUE, dependson="austouristets", warning=FALSE----
components(fit) %>%
  autoplot() +
  labs(title = "ETS(M,N,A) components")

## ----MNAresiduals, echo=FALSE, fig.cap="Residuals and one-step forecast errors from the ETS(M,N,A) model.", dependson="austouristets"----
augment(fit) %>%
  as_tibble() %>%
  select(-Trips, -.fitted) %>%
  pivot_longer(c(.resid,.innov), values_to = "Residual") %>%
  mutate(name = recode(name, .resid = "Regular residuals", .innov = "Innovation residuals")) %>%
  ggplot(aes(x = Quarter, y = Residual)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y=NULL)

## ----MNAforecasts, fig.cap="Forecasting Australian domestic overnight trips using an ETS(M,N,A) model.", echo=TRUE, dependson="austouristets"----
fit %>%
  forecast(h = 8) %>%
  autoplot(aus_holidays)+
  labs(title="Australian domestic tourism",
       y="Overnight trips (millions)")

## ----pitable, echo=FALSE, escape=FALSE--------------------------------------------------------------------
tab <- rbind(
  c("(A,N,N)", "$\\sigma_h^2 = \\sigma^2\\big[1 + \\alpha^2(h-1)\\big]$"),
  c("(A,A,N)", "$\\sigma_h^2 = \\sigma^2\\Big[1 + (h-1)\\big\\{\\alpha^2 + \\alpha\\beta h + \\frac16\\beta^2h(2h-1)\\big\\}\\Big]$"),
  c("(A,A$_d$,N)", "$\\sigma_h^2 = \\sigma^2\\biggl[1 + \\alpha^2(h-1) + \\frac{\\beta\\phi h}{(1-\\phi)^2} \\left\\{2\\alpha(1-\\phi) +\\beta\\phi\\right\\}$"),
  c("", "$\\hspace*{1cm}\\mbox{} - \\frac{\\beta\\phi(1-\\phi^h)}{(1-\\phi)^2(1-\\phi^2)} \\left\\{ 2\\alpha(1-\\phi^2)+ \\beta\\phi(1+2\\phi-\\phi^h)\\right\\}\\biggr]$"),
  c("(A,N,A)", "$\\sigma_h^2 = \\sigma^2\\Big[1 + \\alpha^2(h-1) + \\gamma k(2\\alpha+\\gamma)\\Big]$"),
  c("(A,A,A)", "$\\sigma_h^2 = \\sigma^2\\Big[1 + (h-1)\\big\\{\\alpha^2 + \\alpha\\beta h + \\frac16\\beta^2h(2h-1)\\big\\}$"),
  c("", "$\\hspace*{1cm}\\mbox{} + \\gamma k \\big\\{2\\alpha+ \\gamma + \\beta m (k+1)\\big\\} \\Big]$"),
  c("(A,A$_d$,A)", "$\\sigma_h^2 = \\sigma^2\\biggl[1 + \\alpha^2(h-1) + \\gamma k(2\\alpha+\\gamma)$"),
  c("", "$\\hspace*{1cm}\\mbox{} +\\frac{\\beta\\phi h}{(1-\\phi)^2} \\left\\{2\\alpha(1-\\phi)  + \\beta\\phi \\right\\}$"),
  c("", "$\\hspace*{1cm}\\mbox{} - \\frac{\\beta\\phi(1-\\phi^h)}{(1-\\phi)^2(1-\\phi^2)} \\left\\{ 2\\alpha(1-\\phi^2)+ \\beta\\phi(1+2\\phi-\\phi^h)\\right\\}$"),
  c("", "$\\hspace*{1cm}\\mbox{} + \\frac{2\\beta\\gamma\\phi}{(1-\\phi)(1-\\phi^m)}\\left\\{k(1-\\phi^m) - \\phi^m(1-\\phi^{mk})\\right\\}\\biggr]$")
)
colnames(tab) <- c("Model", "Forecast variance: $\\sigma_h^2$")
caption <- "Forecast variance expressions for each additive state space model,  where $\\sigma^2$ is the residual variance, $m$ is the seasonal period, and $k$ is the integer part of $(h-1) /m$ (i.e., the number of complete years in the forecast period prior to time $T+h$)."
if (html) {
  tab <- gsub("\\$\\\\hspace\\*\\{1cm\\}", paste0(c(rep("&nbsp;", 20), "$"), collapse = ""), tab)
  tab <- knitr::kable(tab, format = "html", escape = FALSE, caption = caption) %>%
    kableExtra::row_spec(c(1, 3, 4, 6, 7), background = "#f7f7f7") %>%
    kableExtra::row_spec(c(2, 5, 8:11), background = "#ececec")
  tab <- gsub("<tr>", "<tr style='border-top-width: 0px !important;'>", tab)
  tab
} else {
  tab[2, 1] <- paste0("\\midrule", tab[2, 1])
  tab[3, 1] <- paste0("\\midrule", tab[3, 1])
  tab[5, 1] <- paste0("\\midrule", tab[5, 1])
  tab[6, 1] <- paste0("\\midrule", tab[6, 1])
  tab[8, 1] <- paste0("\\midrule", tab[8, 1])
  tab <- knitr::kable(tab,
    format = "latex", booktabs = TRUE, escape = FALSE,
    caption = caption)
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.3cm}", tab)
}

