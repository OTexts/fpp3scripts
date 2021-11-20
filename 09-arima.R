source("before-each-chapter.R")

## ----stationary, fig.cap="Which of these series are stationary? (a) Google closing stock price in 2015; (b) Daily change in the Google stock price in 2015; (c) Annual number of strikes in the US; (d) Monthly sales of new one-family houses sold in the US; (e) Annual price of a dozen eggs in the US (constant dollars); (f) Monthly total of pigs slaughtered in Victoria, Australia; (g) Annual total of Canadian Lynx furs traded by the Hudson Bay Company; (h) Quarterly Australian beer production; (i) Monthly Australian gas production.", echo=FALSE, fig.width=10,fig.asp=0.75, fig.env="figure*", fig.pos="t", warning = FALSE, message=FALSE----
p1 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  autoplot(Close) +
  labs(subtitle = "(a) Google closing price", x = "Day", y = " $US")

p2 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  autoplot(difference(Close)) +
  labs(subtitle = "(b) Change in google price", x = "Day", y = "$US")

p3 <- as_tsibble(fma::strikes) %>%
  autoplot(value) +
  labs(subtitle = "(c) Strikes: US",
       y = "Number of strikes",
       x = "Year")

p4 <- as_tsibble(fma::hsales) %>%
  autoplot(value) +
  labs(subtitle = "(d) House sales: US",
       y = "Number of houses",
       x = "Month")

p5 <- as_tsibble(fma::eggs) %>%
  autoplot(value) +
  labs(subtitle = "(e) Egg prices: US",
       y = "$US (contant prices)",
       x = "Year")

p6 <- aus_livestock %>%
  filter(State == "Victoria", Animal == "Pigs") %>%
  autoplot(Count) +
  labs(subtitle = "(f) Pigs slaughtered: Victoria, Australia",
       y = "Number of pigs",
       x="Month")

p7 <- pelt %>%
  autoplot(Lynx) +
  labs(subtitle = "(g) Lynx trapped: Canada",
       y = "Number of lynx",
       x = "Year")

p8 <- aus_production %>%
  filter(year(Quarter) %in% 1991:1995) %>%
  autoplot(Beer) +
  labs(subtitle = "(h) Beer production: Australia",
       y = "Megalitres",
       x = "Quarter")

p9 <- aus_production %>%
  autoplot(Gas) +
  labs(subtitle = "(i) Gas production: Australia",
       y = "Petajoules",
       x = "Quarter")

(p1 | p2 | p3) / (p4 | p5 | p6) / (p7 | p8 | p9)

## ----acfstationary, echo=FALSE, fig.asp=0.35, fig.cap="The ACF of the Google closing stock price in 2015 (left) and of the daily changes in Google closing stock price in 2015 (right).", warning=FALSE----
google_2015 <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) == 2015) %>%
  mutate(trading_day = row_number()) %>%
  update_tsibble(index = trading_day, regular = TRUE)
p1 <- google_2015 %>% ACF(Close) %>% autoplot() + labs(subtitle = "Google closing stock price")
p2 <- google_2015 %>% ACF(difference(Close)) %>% autoplot() + labs(subtitle = "Changes in Google closing stock price")
p1 | p2

## ----googlb, echo=TRUE------------------------------------------------------------------------------------
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10)

## ----googlb2, echo=FALSE----------------------------------------------------------------------------------
pv <- google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, ljung_box, lag = 10) %>%
  pull(lb_pvalue)

## ----a10diff, fig.cap="Logs and seasonal differences of the A10 (antidiabetic) sales data. The logarithms stabilise the variance, while the seasonal differences remove the seasonality and trend.", fig.asp=0.95, echo = FALSE----
PBS %>%
  filter(ATC2 == "A10") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12)
  ) %>%
  pivot_longer(-Month, names_to="Type", values_to="Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales"))
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Antidiabetic drug sales", y = NULL)

## ----h02diff, fig.asp=1.2, fig.cap="(ref:h02diff)"--------------------------------------------------------
PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6) %>%
  transmute(
    `Sales ($million)` = Cost,
    `Log sales` = log(Cost),
    `Annual change in log sales` = difference(log(Cost), 12),
    `Doubly differenced log sales` =
                     difference(difference(log(Cost), 12), 1)
  ) %>%
  pivot_longer(-Month, names_to="Type", values_to="Sales") %>%
  mutate(
    Type = factor(Type, levels = c(
      "Sales ($million)",
      "Log sales",
      "Annual change in log sales",
      "Doubly differenced log sales"))
  ) %>%
  ggplot(aes(x = Month, y = Sales)) +
  geom_line() +
  facet_grid(vars(Type), scales = "free_y") +
  labs(title = "Corticosteroid drug sales", y = NULL)

## ----googkpss---------------------------------------------------------------------------------------------
google_2015 %>%
  features(Close, unitroot_kpss)

## ----googkpss_store, include=FALSE------------------------------------------------------------------------
goog_kpss <- google_2015 %>%
  features(Close, unitroot_kpss)

## ----googkpss2, dependson="googkpss"----------------------------------------------------------------------
google_2015 %>%
  mutate(diff_close = difference(Close)) %>%
  features(diff_close, unitroot_kpss)

## ----googndiffs-------------------------------------------------------------------------------------------
google_2015 %>%
  features(Close, unitroot_ndiffs)

## ----ausretaildiff----------------------------------------------------------------------------------------
aus_total_retail <- aus_retail %>%
  summarise(Turnover = sum(Turnover))
aus_total_retail %>%
  mutate(log_turnover = log(Turnover)) %>%
  features(log_turnover, unitroot_nsdiffs)

aus_total_retail %>%
  mutate(log_turnover = difference(log(Turnover), 12)) %>%
  features(log_turnover, unitroot_ndiffs)

## ----arp, fig.cap="Two examples of data from autoregressive models with different parameters. Left: AR(1) with $y_t = 18 -0.8y_{t-1} + \\varepsilon_t$. Right: AR(2) with $y_t = 8 + 1.3y_{t-1}-0.7y_{t-2}+\\varepsilon_t$. In both cases, $\\varepsilon_t$ is normally distributed white noise with mean zero and variance one.", echo=FALSE, fig.asp=0.45----
set.seed(1)
p1 <- autoplot(as_tsibble(10 + arima.sim(list(ar = -0.8), n = 100)),value) +
  labs(y="", x="Time", title="AR(1)")
p2 <- autoplot(as_tsibble(20 + arima.sim(list(ar = c(1.3, -0.7)), n = 100)),value) +
  labs(y="", x="Time", title="AR(2)")
p1 | p2

## ----maq, fig.cap="Two examples of data from moving average models with different parameters. Left: MA(1) with $y_t = 20 + \\varepsilon_t + 0.8\\varepsilon_{t-1}$. Right: MA(2) with $y_t = \\varepsilon_t- \\varepsilon_{t-1}+0.8\\varepsilon_{t-2}$. In both cases, $\\varepsilon_t$ is normally distributed white noise with mean zero and variance one.", echo=FALSE, fig.asp=0.45,fig.pos="t"----
set.seed(2)
p1 <- autoplot(as_tsibble(20 + arima.sim(list(ma = 0.8), n = 100)),value) +
    labs(y="", x="Time", title="MA(1)")
p2 <- autoplot(as_tsibble(arima.sim(list(ma = c(-1, +0.8)), n = 100)),value) +
  labs(y="", x="Time", title="MA(2)")
p1 | p2

## ----pdqtable, echo=FALSE, warning=FALSE------------------------------------------------------------------
cbind(
  paste0("$",c("p","d","q"),"=$"),
  c(
    "order of the autoregressive part;",
    "degree of first differencing involved;",
     "order of the moving average part.")
  ) %>%
  knitr::kable(escape=FALSE, booktabs=TRUE, longtable=FALSE, align="rl")

## ----arimaspecialcases, echo=FALSE, warning=FALSE---------------------------------------------------------
df <- rbind(
  c("White noise",           "ARIMA(0,0,0) with no constant"),
  c("Random walk",           "ARIMA(0,1,0) with no constant"),
  c("Random walk with drift","ARIMA(0,1,0) with a constant"),
  c("Autoregression",        "ARIMA($p$,0,0)"),
  c("Moving average",        "ARIMA(0,0,$q$)"))
out <- knitr::kable(df, escape=FALSE, booktabs=TRUE, caption="Special cases of ARIMA models.")
if(!html) {
  out <- kableExtra::kable_styling(out, latex_options="hold_position")
  out <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", out)
}
out

## ----egyptexports, fig.cap="Annual Egyptian exports as a percentage of GDP since 1960.", fig.asp=0.5------
global_economy %>%
  filter(Code == "EGY") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Egyptian exports")

## ----egyptexportsauto-------------------------------------------------------------------------------------
fit <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports))
report(fit)

## ----egyptexportsmodel, include=FALSE, warning=FALSE, dependson="egyptexportsauto"------------------------
stopifnot(identical(
  unlist(fit[1,2][[1]][[1]]$fit$spec),
  c(p=2L, d=0L, q=1L, P=0, D=0, Q=0, constant=TRUE, period.year=1)
))
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)

## ----egyptexportsf, fig.cap="Forecasts of Egyptian exports.", fig.asp=0.5, dependson="egyptexportsauto"----
fit %>% forecast(h=10) %>%
  autoplot(global_economy) +
  labs(y = "% of GDP", title = "Egyptian exports")

## ----egyptacf, fig.cap="ACF of Egyptian exports.", fig.asp=0.3--------------------------------------------
global_economy %>%
  filter(Code == "EGY") %>%
  ACF(Exports) %>%
  autoplot()

## ----egyptpacf, fig.cap="PACF of Egyptian exports.", fig.asp=0.3------------------------------------------
global_economy %>%
  filter(Code == "EGY") %>%
  PACF(Exports) %>%
  autoplot()

## ----egyptar----------------------------------------------------------------------------------------------
fit2 <- global_economy %>%
  filter(Code == "EGY") %>%
  model(ARIMA(Exports ~ pdq(4,0,0)))
report(fit2)

## ----ARMAgridsearch, echo=FALSE, fig.cap = "An illustrative example of the Hyndman-Khandakar stepwise search process", message=FALSE, warning=FALSE, fig.asp=1, out.width="60%", fig.width=4, fig.height=4, fig.pos="!h"----
start <- tribble(
  ~p, ~q,
  0, 0,
  1, 0,
  0, 1,
  2, 2
)
selected <- tribble(
  ~p, ~q,
  2, 2,
  3, 3,
  4, 2
)
griddf <- expand.grid(p = 0:6, q = 0:6) %>%
  as_tibble() %>%
  left_join(start %>% mutate(start = TRUE)) %>%
  left_join(selected %>% mutate(chosen = TRUE)) %>%
  replace_na(list(start = FALSE, chosen = FALSE)) %>%
  mutate(
    step = case_when(
      start ~ 1,
      (p - selected$p[1])^2 + (q - selected$q[1])^2 <= 2 ~ 2,
      (p - selected$p[2])^2 + (q - selected$q[2])^2 <= 2 ~ 3,
      (p - selected$p[3])^2 + (q - selected$q[3])^2 <= 2 ~ 4,
      TRUE ~ NA_real_
    )
  ) %>%
  left_join(selected %>%
              mutate(step = row_number() + 1) %>%
              rename(fromp = p, fromq = q)
  ) %>%
  mutate(step = as.character(step))
griddf %>%
  ggplot(aes(x = q, y = p)) +
  geom_point(aes(alpha = 0.2), colour = "gray", size = 5, shape = 19) +
  geom_segment(aes(x = fromq, y = fromp, xend = q, yend = p, col=step),
               arrow = arrow(length = unit(0.15, "inches"), type='open'),
               size = 1, lineend = "butt") +
  geom_point(aes(col = step), size = 5, shape = 19) +
  geom_point(data = filter(griddf, chosen), size = 12, shape = 21, stroke = 1.4) +
  scale_y_reverse(breaks = 0:6) +
  scale_x_continuous(position = "top", breaks = 0:6) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 10),
    axis.title.x = element_text(size = 12, hjust = 0),
    axis.title.y = element_text(
      size = 12, hjust = 1,
      angle = 0, margin = margin(t = 0, r = 10, b = 0, l = 0)
    )
  ) +
  scale_colour_manual(
    breaks = paste(1:4),
    values = c("#D55E00", "#0072B2","#009E73", "#CC79A7")
  ) +
  guides(alpha = "none")

## ----caf, fig.cap="Exports of the Central African Republic as a percentage of GDP.", fig.asp=0.55---------
global_economy %>%
  filter(Code == "CAF") %>%
  autoplot(Exports) +
  labs(title="Central African Republic exports",
       y="% of GDP")

## ----caf2, fig.cap="Time plot and ACF and PACF plots for the differenced Central African Republic Exports.", warning=FALSE----
global_economy %>%
  filter(Code == "CAF") %>%
  gg_tsdisplay(difference(Exports), plot_type='partial')

## ----caf_fit----------------------------------------------------------------------------------------------
caf_fit <- global_economy %>%
  filter(Code == "CAF") %>%
  model(arima210 = ARIMA(Exports ~ pdq(2,1,0)),
        arima013 = ARIMA(Exports ~ pdq(0,1,3)),
        stepwise = ARIMA(Exports),
        search = ARIMA(Exports, stepwise=FALSE))

caf_fit %>% pivot_longer(!Country, names_to = "Model name",
                         values_to = "Orders")
glance(caf_fit) %>% arrange(AICc) %>% select(.model:BIC)

## ----cafhidden, dependson='caf_fit', include=FALSE--------------------------------------------------------
best <- format((caf_fit %>%  pull(search))[[1]])
stepwise <- format((caf_fit %>% pull(stepwise))[[1]])
stopifnot(best == "ARIMA(3,1,0)")

## ----cafres, dependson='caf_fit', fig.cap="Residual plots for the ARIMA(3,1,0) model."--------------------
caf_fit %>%
  select(search) %>%
  gg_tsresiduals()

## ----caf_lb, dependson='caf_fit'--------------------------------------------------------------------------
augment(caf_fit) %>%
  filter(.model=='search') %>%
  features(.innov, ljung_box, lag = 10, dof = 3)

## ----caffc, fig.cap="Forecasts for the Central African Republic Exports.", fig.asp=0.65, dependson="caf_fit"----
caf_fit %>%
  forecast(h=5) %>%
  filter(.model=='search') %>%
  autoplot(global_economy)

## ----armaroots, fig.cap="Inverse characteristic roots for the ARIMA(3,1,0) model fitted to the Central African Republic Exports.", fig.width=3.8, fig.asp=1, out.width="65%"----
gg_arma(caf_fit %>% select(Country, search))

## ----usemployment1, fig.cap="Monthly US leisure and hospitality employment, 2000-2019.", fig.asp=0.45-----
leisure <- us_employment %>%
  filter(Title == "Leisure and Hospitality",
         year(Month) > 2000) %>%
  mutate(Employed = Employed/1000) %>%
  select(Month, Employed)
autoplot(leisure, Employed) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

## ----usemployment2, fig.cap="Seasonally differenced Monthly US leisure and hospitality employment.", fig.asp=0.55, warning=FALSE----
leisure %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type='partial', lag=36) +
  labs(title="Seasonally differenced", y="")

## ----usemployment3, fig.cap="Double differenced Monthly US leisure and hospitality employment.", fig.asp=0.55, warning=FALSE----
leisure %>%
  gg_tsdisplay(difference(Employed, 12) %>% difference(),
               plot_type='partial', lag=36) +
  labs(title = "Double differenced", y="")

## ----usemployment4----------------------------------------------------------------------------------------
fit <- leisure %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0,1,2) + PDQ(0,1,1)),
    arima210011 = ARIMA(Employed ~ pdq(2,1,0) + PDQ(0,1,1)),
    auto = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )
fit %>% pivot_longer(everything(), names_to = "Model name",
                     values_to = "Orders")
glance(fit) %>% arrange(AICc) %>% select(.model:BIC)

## ----conshidden, include=FALSE, dependson="usemployment4"-------------------------------------------------
leisure_best <- format((fit %>% pull(auto))[[1]]) %>%
  stringr::str_replace("\\[12\\]","$_{12}$")

## ----usemployment5, fig.cap="(ref:usemployment5)", fig.asp=0.6, dependson="usemployment4"-----------------
fit %>% select(auto) %>% gg_tsresiduals(lag=36)

## ----usemployment6, dependson="usemployment4"-------------------------------------------------------------
augment(fit) %>%
  filter(.model == "auto") %>%
  features(.innov, ljung_box, lag=24, dof=4)

## ----usemployment7, fig.cap="(ref:usemployment7)", fig.asp=0.5, dependson="usemployment4"-----------------
forecast(fit, h=36) %>%
  filter(.model=='auto') %>%
  autoplot(leisure) +
  labs(title = "US employment: leisure and hospitality",
       y="Number of people (millions)")

## ----h02, fig.cap="Corticosteroid drug sales in Australia (in millions of scripts per month). Logged data shown in bottom panel."----
h02 <- PBS %>%
  filter(ATC2 == "H02") %>%
  summarise(Cost = sum(Cost)/1e6)
h02 %>%
  mutate(log(Cost)) %>%
  pivot_longer(-Month) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(y="", title="Corticosteroid drug scripts (H02)")

## ----h02b, fig.cap="Seasonally differenced corticosteroid drug sales in Australia (in millions of scripts per month).", warning=FALSE----
h02 %>% gg_tsdisplay(difference(log(Cost), 12),
                     plot_type='partial', lag_max = 24)

## ----h02aicc, echo=FALSE----------------------------------------------------------------------------------
models <- list(
  c(3,0,0,2,1,0),
  c(3,0,1,2,1,0),
  c(3,0,2,2,1,0),
  c(3,0,1,1,1,0),
  c(3,0,1,0,1,1),
  c(3,0,1,0,1,2),
  c(3,0,1,1,1,1)
)
library(purrr)
model_defs <- map(models, ~ ARIMA(log(Cost) ~ 0 + pdq(!!.[1], !!.[2], !!.[3]) + PDQ(!!.[4], !!.[5], !!.[6])))
model_defs <- set_names(model_defs, map_chr(models,
  ~ sprintf("ARIMA(%i,%i,%i)(%i,%i,%i)$_{12}$", .[1], .[2], .[3], .[4], .[5], .[6])))

fit <- h02 %>%
  model(!!!model_defs)

tab <- fit %>%
  glance %>%
  arrange(AICc) %>%
  select(.model, AICc) %>%
  knitr::kable(digits=2, row.names=FALSE, align='cc', booktabs=TRUE, escape = FALSE,
               col.names=c("Model","AICc"),
               caption="AICc values for various ARIMA models applied for H02 monthly script sales data.")

if (html) {
  tab
} else {
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.0cm}", tab)
}

## ----checkclaimh02, echo=FALSE, warning=FALSE-------------------------------------------------------------
if(glance(fit)$.model[which.min(glance(fit)$AICc)] != "ARIMA(3,0,1)(0,1,2)$_{12}$")
  stop("Not best model")

## ----h02res, fig.cap="Innovation residuals from the ARIMA(3,0,1)(0,1,2)$_{12}$ model applied to the H02 monthly script sales data."----
fit <- h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2)))
fit %>% gg_tsresiduals(lag_max=36)

## ----h02resb----------------------------------------------------------------------------------------------
augment(fit) %>%
  features(.innov, ljung_box, lag = 36, dof = 6)

## ----autoh02, echo=FALSE, warning=FALSE-------------------------------------------------------------------
manual <- gsub("[<>]","",format(fit[[1]])[[1]])
manual <- gsub("[","$_{",manual, fixed=TRUE)
manual <- gsub("]","}$",manual, fixed=TRUE)
autoh2 <- gsub("[<>]","",format(model(h02, ARIMA(log(Cost)))[[1]])[[1]])
autoh2 <- gsub("[","$_{",autoh2, fixed=TRUE)
autoh2 <- gsub("]","}$",autoh2, fixed=TRUE)
besth2 <- gsub("[<>]","",format(model(h02, ARIMA(log(Cost), stepwise=FALSE, approximation=FALSE))[[1]])[[1]])
besth2 <- gsub("[","$_{",besth2, fixed=TRUE)
besth2 <- gsub("]","}$",besth2, fixed=TRUE)

## ----h02search, echo=FALSE, warning=FALSE-----------------------------------------------------------------
models <- list(
  c(3,0,0,2,1,0),
  c(3,0,1,2,1,0),
  c(3,0,2,2,1,0),
  c(3,0,1,1,1,0),
  c(3,0,1,0,1,1),
  c(3,0,1,0,1,2),
  c(3,0,1,0,1,3),
  c(3,0,1,1,1,1),
  c(3,0,3,0,1,1),
  c(3,0,2,0,1,1),
  #c(2,1,0,0,1,0),
  c(2,1,0,0,1,1),
  c(2,1,0,1,1,0),
  c(2,1,1,0,1,1),
  c(2,1,2,0,1,1),
  c(2,1,3,0,1,1),
  c(2,1,4,0,1,1))

model_defs <- map(models, ~ ARIMA(log(Cost) ~ 0 + pdq(!!.[1], !!.[2], !!.[3]) + PDQ(!!.[4], !!.[5], !!.[6])))
model_defs <- set_names(model_defs, map_chr(models,
  ~ sprintf("ARIMA(%i,%i,%i)(%i,%i,%i)$_{12}$", .[1], .[2], .[3], .[4], .[5], .[6])))

# WARNING generated by following code indicating convergence issue.
fit <- h02 %>%
  filter_index(~ "2006 Jun") %>%
  model(!!!model_defs)

rmse_table <- fit %>%
  forecast(h = "2 years") %>%
  accuracy(h02 %>% filter_index("2006 Jul" ~ .)) %>%
  arrange(RMSE) %>%
  select(.model, RMSE)
# Check manual model is still second
stopifnot(rmse_table$.model[2] == manual)

tab <- rmse_table %>%
  knitr::kable(escape=FALSE,
               digits=4, row.names=FALSE, align='cc', booktabs=TRUE,
               caption="RMSE values for various ARIMA models applied for H02 monthly script sales data over test set July 2006 -- June 2008 .")

if (html) {
  tab
} else {
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}\\\\small", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.3cm}", tab)
}

## ----h02f, fig.cap="Forecasts from the ARIMA(3,0,1)(0,1,2)$_{12}$ model applied to the H02 monthly script sales data.", fig.asp=0.5----
h02 %>%
  model(ARIMA(log(Cost) ~ 0 + pdq(3,0,1) + PDQ(0,1,2))) %>%
  forecast() %>%
  autoplot(h02) +
  labs(y=" $AU (millions)",
       title="Corticosteroid drug scripts (H02) sales")

## ----venn, fig.cap="The ETS and ARIMA model classes overlap with the additive ETS models having equivalent ARIMA forms.", echo=FALSE, message=FALSE, warning=FALSE, out.width="90%"----
library(latex2exp)
cols = c(ets = "#D55E00", arima = "#0072b2")
tibble(
    x = c(-0.866, 0.866),
    y = c(-0.5, -0.5),
    labels = c("ets", "arima"),
  ) %>%
  ggplot(aes(colour = labels, fill=labels)) +
  ggforce::geom_circle(aes(x0 = x, y0 = y, r = 1.5), alpha = 0.3, size = 1) +
  scale_colour_manual(values=cols) + scale_fill_manual(values=cols) +
  coord_fixed() + guides(fill = "none") +
  geom_text(aes(label = "ETS models", x = -1.5, y = 1.15), col = cols["ets"], fontface = "bold", size=6) +
  geom_text(aes(label = "Combination\n of components", x = -1.5, y = 0.3), col = cols["ets"], size=4) +
  geom_text(aes(label = "9 non-additive\n ETS models", x = -1.5, y = -0.6), col = cols["ets"], size=4) +
  geom_text(aes(label = "All ETS models\n with M components", x = -.95, y = -1.6), col = cols["ets"], size=4) +
  geom_text(aes(label = "ARIMA models", x = 1.5, y = 1.15), col = cols["arima"], fontface = "bold", size=6) +
  geom_text(aes(label = "Modelling\n autocorrelations", x = 1.5, y = 0.3), col = cols["arima"], size=4) +
  annotate("text", label = TeX("Potentially $\\infty$ models"), x = 1.5, y = -0.6, col = cols["arima"], size=4) +
  geom_text(aes(label = "All stationary models\n Many large models", x = 1.01, y = -1.6), col = cols["arima"], size=4) +
  geom_text(aes(label = "6 additive\n ETS models", x = 0, y = -0.6), col = "#6b6859", size=4) +
  guides(colour = "none", fill = "none") +  theme_void()

## ----etsarima, echo=FALSE---------------------------------------------------------------------------------
tab <- data.frame(
  etsmodel = c(
    "ETS(A,N,N)",
    "ETS(A,A,N)","",
    "ETS(A,A$_d$,N)","","",
    "ETS(A,N,A)",
    "ETS(A,A,A)",
    "ETS(A,A$_d$,A)"),
  arimamodel = c(
    "ARIMA(0,1,1)",
    "ARIMA(0,2,2)","",
    "ARIMA(1,1,2)","","",
    "ARIMA(0,1,$m$)(0,1,0)$_m$",
    "ARIMA(0,1,$m+1$)(0,1,0)$_m$",
    "ARIMA(1,0,$m+1$)(0,1,0)$_m$"),
  parameters = c(
    "$\\theta_1=\\alpha-1$",
    "$\\theta_1=\\alpha+\\beta-2$",
    "$\\theta_2=1-\\alpha$",
    "$\\phi_1=\\phi$",
    "$\\theta_1=\\alpha+\\phi\\beta-1-\\phi$",
    "$\\theta_2=(1-\\alpha)\\phi$",
    "","",""))
if(html) {
  out <- knitr::kable(tab, format='html', col.names=c("ETS model","ARIMA model", "Parameters"), booktabs=TRUE, escape=FALSE, caption="Equivalence relationships between ETS and ARIMA models.") %>%
    kableExtra::row_spec(c(1,4:6,8), background="#f6f6f6") %>%
    kableExtra::row_spec(c(2:3,7,9), background="#e6e6e6")
  out <- gsub("<tr>","<tr style='border-top-width: 0px !important;'>",out)
  out
} else {
  tab <- knitr::kable(tab,
                      format='latex',
                      col.names=c("ETS model","ARIMA model", "Parameters"),
                      booktabs=TRUE,
                      escape=FALSE,
                      caption="Equivalence relationships between ETS and ARIMA models.")
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.5cm}", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\vspace*{0.3cm}", tab)
}

## ----tscvpop, echo=TRUE, warning=FALSE--------------------------------------------------------------------
aus_economy <- global_economy %>%
  filter(Code == "AUS") %>%
  mutate(Population = Population/1e6)

aus_economy %>%
  slice(-n()) %>%
  stretch_tsibble(.init = 10) %>%
  model(
    ETS(Population),
    ARIMA(Population)
  ) %>%
  forecast(h = 1) %>%
  accuracy(aus_economy) %>%
  select(.model, RMSE:MAPE)

## ----popetsplot, echo=TRUE, fig.cap="Forecasts from an ETS model fitted to the Australian population.", fig.asp=0.55----
aus_economy %>%
  model(ETS(Population)) %>%
  forecast(h = "5 years") %>%
  autoplot(aus_economy %>% filter(Year >= 2000)) +
  labs(title = "Australian population",
       y = "People (millions)")

## ----qcement1, echo=TRUE----------------------------------------------------------------------------------
cement <- aus_production %>%
  select(Cement) %>%
  filter_index("1988 Q1" ~ .)
train <- cement %>% filter_index(. ~ "2007 Q4")

## ----qcement2, echo=TRUE, fig.cap="Residual diagnostic plots for the ARIMA model fitted to the quarterly cement production training data.", dependson="qcement1"----
fit_arima <- train %>% model(ARIMA(Cement))
report(fit_arima)
fit_arima %>% gg_tsresiduals(lag_max = 16)

## ----qcement2b, echo=TRUE, dependson="qcement1"-----------------------------------------------------------
augment(fit_arima) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

## ----qcement3, echo=TRUE, fig.cap="Residual diagnostic plots for the ETS model fitted to the quarterly cement production training data.", dependson="qcement1"----
fit_ets <- train %>% model(ETS(Cement))
report(fit_ets)
fit_ets %>%
  gg_tsresiduals(lag_max = 16)

## ----qcement3b, echo=TRUE, dependson="qcement1"-----------------------------------------------------------
augment(fit_ets) %>%
  features(.innov, ljung_box, lag = 16, dof = 6)

## ----qcement4, echo=TRUE, dependson=c("qcement2","qcement3")----------------------------------------------
# Generate forecasts and compare accuracy over the test set
bind_rows(
    fit_arima %>% accuracy(),
    fit_ets %>% accuracy(),
    fit_arima %>% forecast(h = 10) %>% accuracy(cement),
    fit_ets %>% forecast(h = 10) %>% accuracy(cement)
  ) %>%
  select(-ME, -MPE, -ACF1)

## ----qcement5, echo=TRUE, fig.cap="Forecasts from an ARIMA model fitted to all of the available quarterly cement production data since 1988.", dependson="qcement1", fig.asp=0.45----
cement %>%
  model(ARIMA(Cement)) %>%
  forecast(h="3 years") %>%
  autoplot(cement) +
  labs(title = "Cement production in Australia",
       y = "Tonnes ('000)")

## ----wnacfplus, fig.asp=0.3, echo=FALSE, fig.cap="Left: ACF for a white noise series of 36 numbers. Middle: ACF for a white noise series of 360 numbers. Right: ACF for a white noise series of 1,000 numbers.", fig.env="figure*"----
set.seed(1)
x1 <- tsibble(idx = seq_len(36), y = rnorm(36), index = idx)
x2 <- tsibble(idx = seq_len(360), y = rnorm(360), index = idx)
x3 <- tsibble(idx = seq_len(1000), y = rnorm(1000), index = idx)
p1 <- x1 %>% ACF(y, lag_max = 20) %>% autoplot() + ylim(c(-1, 1)) + labs(x="lag")
p2 <- x2 %>% ACF(y, lag_max = 20) %>% autoplot() + ylim(c(-1, 1)) + labs(x="lag")
p3 <- x3 %>% ACF(y, lag_max = 20) %>% autoplot() + ylim(c(-1, 1)) + labs(x="lag")
p1 | p2 | p3

## ----ex9.6------------------------------------------------------------------------------------------------
y <- numeric(100)
e <- rnorm(100)
for(i in 2:100)
  y[i] <- 0.6*y[i-1] + e[i]
sim <- tsibble(idx = seq_len(100), y = y, index = idx)

## ----hares, echo=FALSE, warning=FALSE, message=FALSE------------------------------------------------------
pelt_table <- pelt %>%
  tail(5) %>%
  select(Year, Hare)
tab <- as.data.frame(matrix(c(NA,pelt_table$Hare), nrow=1))
colnames(tab) <- c("Year",pelt_table$Year)
tab[1,1] <- "Number of hare pelts"
tab %>%
  knitr::kable(booktabs=TRUE)

fit <- pelt %>% model(ARIMA(Hare ~ pdq(4,0,0)))
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)
constant <- coef['constant']
phi1 <- coef['ar1']
phi2 <- coef['ar2']
phi3 <- coef['ar3']
phi4 <- coef['ar4']

## ----swisspop, echo=FALSE, warning=FALSE, message=FALSE---------------------------------------------------
swiss_pop <- global_economy %>%
  filter(Country=="Switzerland") %>%
  tail(5) %>%
  select(Year, Population) %>%
  mutate(Population = Population/1e6)
tab <- as.data.frame(matrix(c(NA,swiss_pop$Population), nrow=1))
colnames(tab) <- c("Year",swiss_pop$Year)
tab[1,1] <- "Population (millions)"
tab %>%
  knitr::kable(digits=2, booktabs=TRUE)
fit <- global_economy %>%
  filter(Country=="Switzerland") %>%
  model(ARIMA(Population/1e6 ~ 1 + pdq(3,1,0)))
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)
phi1 <- coef['ar1']
phi2 <- coef['ar2']
phi3 <- coef['ar3']
intercept <- coef['constant']

