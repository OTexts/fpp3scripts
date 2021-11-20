source("before-each-chapter.R")

## ----workflow, echo = FALSE, fig.height=2.5, fig.asp=0.4--------------------------------------------------
line_curve <- function(x, y, xend, yend, ...) {
  geom_curve(
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(type = "closed", length = unit(0.03, "npc")),
    ...
  )
}

ggplot() +
  geom_text(
    aes(x = x, y = y, label = label),
    data = tribble(
      ~x, ~y, ~label,
      1, 0, "Tidy",
      7/3, 0, "Visualise",
      3, 0.5, "Specify",
      11/3, 0, "Estimate",
      3, -0.5, "Evaluate",
      5, 0, "Forecast"
    ),
    size = 5
  ) +
  geom_segment(
    aes(x = x, y = y, xend = xend, yend = yend),
    data = tribble(
      ~x, ~y, ~xend, ~yend,
      1.3, 0, 1.9, 0,
      4.1, 0, 4.6, 0
    ),
    arrow = arrow(type = "closed", length = unit(0.03, "npc"))
  ) +
  line_curve(7/3, 0.1, 8/3, 0.5, angle = 250, curvature = -0.3) +
  line_curve(10/3, 0.5, 11/3, 0.1, angle = 250, curvature = -0.3) +
  line_curve(8/3, -0.5, 7/3, -0.1, angle = 250, curvature = -0.3) +
  line_curve(11/3, -0.1, 10/3, -0.5, angle = 250, curvature = -0.3) +
  theme_void() +
  xlim(0.8, 5.2) +
  ylim(-0.6, 0.6)

## ----gdppc------------------------------------------------------------------------------------------------
gdppc <- global_economy %>%
  mutate(GDP_per_capita = GDP / Population)

## ----swedengdp, fig.cap="GDP per capita data for Sweden from 1960 to 2017.", dependson='gdppc'------------
gdppc %>%
  filter(Country == "Sweden") %>%
  autoplot(GDP_per_capita) +
  labs(y = "$US", title = "GDP per capita for Sweden")

## ----gdp_models, warning=FALSE, message=FALSE, dependson='gdppc'------------------------------------------
fit <- gdppc %>%
  model(trend_model = TSLM(GDP_per_capita ~ trend()))

## ----gdp_models2, dependson='gdp_models'------------------------------------------------------------------
fit

## ----gdp_forecasts, dependson='gdp_models', warning=FALSE, message=FALSE----------------------------------
fit %>% forecast(h = "3 years")

## ----gdpforecastplot, fig.asp=0.55, dependson='gdp_models', warning=FALSE, message=FALSE, fig.cap="Forecasts of GDP per capita for Sweden using a simple trend model."----
fit %>%
  forecast(h = "3 years") %>%
  filter(Country == "Sweden") %>%
  autoplot(gdppc) +
  labs(y = "$US", title = "GDP per capita for Sweden")

## ----bricks-----------------------------------------------------------------------------------------------
bricks <- aus_production %>%
  filter_index("1970 Q1" ~ "2004 Q4") %>%
  select(Bricks)

## ----mean-method-explained, fig.asp=0.55, echo=FALSE, message=FALSE, warning=FALSE, fig.cap="Mean (or average) forecasts applied to clay brick production in Australia."----
bricks <- bricks %>%
  mutate(average = mean(Bricks))

fc <- as_tibble(bricks) %>%
  filter(row_number() == n()) %>%
  mutate(Quarter = list(as_date(Quarter) + months(c(0, 12*5)))) %>%
  unnest(Quarter)

bricks %>%
  ggplot(aes(x = Quarter, y = Bricks)) +
  geom_line() +
  geom_line(aes(y = average), colour = "blue", linetype = "dashed") +
  geom_line(aes(y = average), data = fc, colour = "blue") +
  labs(title = "Clay brick production in Australia")

## ----naive-method-explained, fig.asp=0.55, echo = FALSE, warning = FALSE, fig.cap="Naïve forecasts applied to clay brick production in Australia."----
bricks %>%
  model(NAIVE(Bricks)) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks, level = NULL) +
  geom_point(aes(y = Bricks), data = slice(bricks, n()), colour = "blue") +
  labs(title = "Clay brick production in Australia")

## ----snaive-method-explained, fig.asp=0.55, echo = FALSE, warning = FALSE, fig.cap="Seasonal naïve forecasts applied to clay brick production in Australia."----
bricks %>%
  model(SNAIVE(Bricks ~ lag("year"))) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks, level = NULL) +
  geom_point(aes(y = Bricks), data = slice(bricks, (n() - 3):n()), colour = "blue") +
  labs(title = "Clay brick production in Australia")

## ----drift-method-explained, fig.asp=0.55, echo = FALSE, warning = FALSE, fig.cap="Drift forecasts applied to clay brick production in Australia."----
bricks %>%
  model(RW(Bricks ~ drift())) %>%
  forecast(h = "5 years") %>%
  autoplot(bricks, level = NULL) +
  geom_line(aes(y = Bricks),
    data = slice(bricks, range(cumsum(!is.na(Bricks)))),
    linetype = "dashed", colour = "blue"
  ) +
  labs(title = "Clay brick production in Australia")

## ----beerf, fig.cap="Forecasts of Australian quarterly beer production.", warning=FALSE, message=FALSE, fig.asp=0.5----
# Set training data from 1992 to 2006
train <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")
# Fit the models
beer_fit <- train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer)
  )
# Generate forecasts for 14 quarters
beer_fc <- beer_fit %>% forecast(h = 14)
# Plot forecasts against actual values
beer_fc %>%
  autoplot(train, level = NULL) +
  autolayer(
    filter_index(aus_production, "2007 Q1" ~ .),
    colour = "black"
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

## ----google2015, fig.cap="Forecasts based on Google's daily closing stock price in 2015.", message=FALSE, warning=FALSE, fig.asp=0.5----
# Re-index based on trading days
google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)
# Filter the year of interest
google_2015 <- google_stock %>% filter(year(Date) == 2015)
# Fit the models
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = NAIVE(Close ~ drift())
  )
# Produce forecasts for the trading days in January 2016
google_jan_2016 <- google_stock %>%
  filter(yearmonth(Date) == yearmonth("2016 Jan"))
google_fc <- google_fit %>%
  forecast(new_data = google_jan_2016)
# Plot the forecasts
google_fc %>%
  autoplot(google_2015, level = NULL) +
  autolayer(google_jan_2016, Close, colour = "black") +
  labs(y = "$US",
       title = "Google daily closing stock prices",
       subtitle = "(Jan 2015 - Jan 2016)") +
  guides(colour = guide_legend(title = "Forecast"))

## ----augment, dependson='beerf'---------------------------------------------------------------------------
augment(beer_fit)

## ----GSPautoplot, fig.cap="Daily Google stock prices in 2015.", dependson='google_2015'-------------------
autoplot(google_2015, Close) +
  labs(y = "$US",
       title = "Google daily closing stock prices in 2015")

## ----GSPresid, fig.cap="Residuals from forecasting the Google stock price using the naïve method.", warning=FALSE, fig.asp=0.4, dependson='google_2015'----
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
autoplot(aug, .innov) +
  labs(y = "$US",
       title = "Residuals from the naïve method")

## ----GSPhist, fig.cap="Histogram of the residuals from the naïve method applied to the Google stock price. The right tail seems a little too long for a normal distribution.", warning=FALSE, message=FALSE, dependson="GSPresid", fig.asp=0.5----
aug %>%
  ggplot(aes(x = .innov)) +
  geom_histogram() +
  labs(title = "Histogram of residuals")

## ----GSPacf, fig.cap="ACF of the residuals from the naïve method applied to the Google stock price. The lack of correlation suggesting the forecasts are good.", fig.asp=0.3, dependson="GSPresid"----
aug %>%
  ACF(.innov) %>%
  autoplot() +
  labs(title = "Residuals from the naïve method")

## ----tsresiduals, warning=FALSE, dependson='google_2015', fig.cap="Residual diagnostic graphs for the naïve method applied to the Google stock price."----
google_2015 %>%
  model(NAIVE(Close)) %>%
  gg_tsresiduals()

## ----Boxtest, dependson="GSPresid"------------------------------------------------------------------------
aug %>% features(.innov, box_pierce, lag = 10, dof = 0)

aug %>% features(.innov, ljung_box, lag = 10, dof = 0)

## ----goog_drift, warning=FALSE, dependson='google_2015'---------------------------------------------------
fit <- google_2015 %>% model(RW(Close ~ drift()))
tidy(fit)

## ----tsresid_drift, warning=FALSE, dependson='googl_drift'------------------------------------------------
augment(fit) %>% features(.innov, ljung_box, lag=10, dof=1)

## ----pcmultipliers, echo=FALSE----------------------------------------------------------------------------
tab <- tibble(Percentage = c(seq(50, 95, by = 5), 96:99)) %>%
  mutate(Multiplier = qnorm(0.5 + Percentage / 200)) %>%
  knitr::kable(
    digits = 2,
    booktabs = TRUE,
    caption = "Multipliers to be used for prediction intervals."
  )
if(!html) {
  tab <- gsub("\\\\centering","\\\\vspace*{-0.4cm}\\\\centering",tab)
  tab <- gsub("\\\\end\\{tabular\\}","\\\\end\\{tabular\\}\\\\vspace*{0.3cm}",tab)
}
tab

## ----GSPpi, echo=FALSE, dependson="google2015"------------------------------------------------------------
aug <- google_2015 %>%
  model(NAIVE(Close)) %>%
  augment()
googsd <- sqrt(mean(aug$.resid^2, na.rm = TRUE))
googf <- round(last(google_2015$Close), 2)
mult <- -qnorm(.5 - c(80, 95) / 200)
upper <- c(googf) + mult * googsd
lower <- c(googf) - mult * googsd

## ----sigmatable, echo=FALSE-------------------------------------------------------------------------------
tab <- rbind(
  c("Mean", "$\\hat\\sigma_h = \\hat\\sigma\\sqrt{1 + 1/T}$"),
  c("Naïve", "$\\hat\\sigma_h = \\hat\\sigma\\sqrt{h}$"),
  c("Seasonal naïve", "$\\hat\\sigma_h = \\hat\\sigma\\sqrt{k+1}$"),
  c("Drift", "$\\hat\\sigma_h = \\hat\\sigma\\sqrt{h(1+h/T)}$")
)
colnames(tab) <- c("Benchmark method", "$h$-step forecast standard deviation")
caption <- "Multi-step forecast standard deviation for the four benchmark methods, where $\\sigma$ is the residual standard deviation, $m$ is the seasonal period, and $k$ is the integer part of $(h-1) /m$ (i.e., the number of complete years in the forecast period prior to time $T+h$)."
tab <- knitr::kable(tab, format=if_else(html, 'html', 'latex'), booktabs = TRUE, escape = FALSE, caption = caption)
if(!html) {
  tab <- gsub("\\\\centering","\\\\vspace*{-0.4cm}\\\\centering",tab)
  tab <- gsub("\\\\end\\{tabular\\}","\\\\end\\{tabular\\}\\\\vspace*{0.3cm}",tab)
}
tab

## ----googforecasts, dependson="GSPpi"---------------------------------------------------------------------
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  hilo()

## ----googforecasts2, echo=TRUE, dependson="GSPpi", fig.asp=0.55, fig.cap="(ref:googforecasts2)"-----------
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10) %>%
  autoplot(google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

## ----generate, dependson="google2015"---------------------------------------------------------------------
fit <- google_2015 %>%
  model(NAIVE(Close))
sim <- fit %>% generate(h = 30, times = 5, bootstrap = TRUE)
sim

## ----showsim, fig.cap="Five simulated future sample paths of the Google closing stock price based on a naïve method with bootstrapped residuals.", dependson="generate"----
google_2015 %>%
  ggplot(aes(x = day)) +
  geom_line(aes(y = Close)) +
  geom_line(aes(y = .sim, colour = as.factor(.rep)),
    data = sim) +
  labs(title="Google daily closing stock price", y="$US" ) +
  guides(colour = "none")

## ----fcbootstrap, dependson="generate"--------------------------------------------------------------------
fc <- fit %>% forecast(h = 30, bootstrap = TRUE)
fc

## ----fcbootstrapplot, fig.cap="Forecasts of the Google closing stock price based on a naïve method with bootstrapped residuals.", dependson="fcbootstrap"----
autoplot(fc, google_2015) +
  labs(title="Google daily closing stock price", y="$US" )

## ----googforecastsboot, echo=TRUE, dependson="google2015"-------------------------------------------------
google_2015 %>%
  model(NAIVE(Close)) %>%
  forecast(h = 10, bootstrap = TRUE, times = 1000) %>%
  hilo()

## ----biasadjust, message=FALSE, warning=FALSE, echo=TRUE, fig.cap="Forecasts of egg prices using the drift method applied to the logged data. The bias-adjusted mean forecasts are shown with a solid line, while the median forecasts are dashed."----
prices %>%
  filter(!is.na(eggs)) %>%
  model(RW(log(eggs) ~ drift())) %>%
  forecast(h = 50) %>%
  autoplot(prices %>% filter(!is.na(eggs)),
    level = 80, point_forecast = lst(mean, median)
  ) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

## ----print-media4, fig.cap="Naïve forecasts of the seasonally adjusted data obtained from an STL decomposition of the total US retail employment.", echo=TRUE----
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade")
dcmp <- us_retail_employment %>%
  model(STL(Employed ~ trend(window = 7), robust = TRUE)) %>%
  components() %>%
  select(-.model)
dcmp %>%
  model(NAIVE(season_adjust)) %>%
  forecast() %>%
  autoplot(dcmp) +
  labs(y = "Number of people",
       title = "US retail employment")

## ----print-media5, fig.cap="Forecasts of the total US retail employment data based on a naïve forecast of the seasonally adjusted data and a seasonal naïve forecast of the seasonal component, after an STL decomposition of the data.", echo=TRUE----
fit_dcmp <- us_retail_employment %>%
  model(stlf = decomposition_model(
    STL(Employed ~ trend(window = 7), robust = TRUE),
    NAIVE(season_adjust)
  ))
fit_dcmp %>%
  forecast() %>%
  autoplot(us_retail_employment)+
  labs(y = "Number of people",
       title = "US retail employment")

## ----print-media5-resids, fig.cap="Checking the residuals.", echo=TRUE, warning=FALSE, dependson="print-media5"----
fit_dcmp %>% gg_tsresiduals()

## ----traintest, fig.asp=0.1, echo=FALSE-------------------------------------------------------------------
train <- 1:18
test <- 19:24
par(mar = c(0, 0, 0, 0))
plot(0, 0, xlim = c(0, 26), ylim = c(0, 2), xaxt = "n", yaxt = "n", bty = "n", xlab = "", ylab = "", type = "n")
arrows(0, 0.5, 25, 0.5, 0.05)
points(train, train * 0 + 0.5, pch = 19, col = "#0072B2")
points(test, test * 0 + 0.5, pch = 19, col = "#D55E00")
text(26, 0.5, "time")
text(10, 1, "Training data", col = "#0072B2")
text(21, 1, "Test data", col = "#D55E00")

## ----beeraccuracy, fig.cap="Forecasts of Australian quarterly beer production using data up to the end of 2007.", message=FALSE,warning=FALSE, fig.asp=0.5----
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
beer_train <- recent_production %>%
  filter(year(Quarter) <= 2007)

beer_fit <- beer_train %>%
  model(
    Mean = MEAN(Beer),
    `Naïve` = NAIVE(Beer),
    `Seasonal naïve` = SNAIVE(Beer),
    Drift = RW(Beer ~ drift())
  )

beer_fc <- beer_fit %>%
  forecast(h = 10)

beer_fc %>%
  autoplot(
    aus_production %>% filter(year(Quarter) >= 1992),
    level = NULL
  ) +
  labs(
    y = "Megalitres",
    title = "Forecasts for quarterly beer production"
  ) +
  guides(colour = guide_legend(title = "Forecast"))

## ----beeraccuracytable, echo=FALSE, dependson="beeraccuracy"----------------------------------------------
accuracy(beer_fc, recent_production) %>%
  mutate(Method = paste(.model, "method")) %>%
  select(Method, RMSE, MAE, MAPE, MASE) %>%
  knitr::kable(digits = 2, booktabs = TRUE)

## ----GSPfc0, fig.cap="Forecasts of the Google stock price for Jan 2016.", warning=FALSE, fig.asp=0.55, dependson="google2015"----
google_fit <- google_2015 %>%
  model(
    Mean = MEAN(Close),
    `Naïve` = NAIVE(Close),
    Drift = RW(Close ~ drift())
  )

google_fc <- google_fit %>%
  forecast(google_jan_2016)

## ----GSPfc, fig.cap="Forecasts of the Google stock price for Jan 2016.", warning=FALSE, fig.asp=0.55, dependson="GSPfc0"----
google_fc %>%
  autoplot(bind_rows(google_2015, google_jan_2016),
    level = NULL) +
  labs(y = "$US",
       title = "Google closing stock prices from Jan 2015") +
  guides(colour = guide_legend(title = "Forecast"))

## ----GSPaccuracytable, echo=FALSE, dependson="GSPfc", warning=FALSE---------------------------------------
accuracy(google_fc, google_stock) %>%
  mutate(Method = paste(.model, "method")) %>%
  select(Method, RMSE, MAE, MAPE, MASE) %>%
  knitr::kable(digits = 2, booktabs = TRUE)

## ----googlepi, fig.cap="(ref:googlepi)", warning=FALSE, fig.asp=0.55, dependson='GSPfc'-------------------
google_fc %>%
  filter(.model == "Naïve") %>%
  autoplot(bind_rows(google_2015, google_jan_2016), level=80)+
  labs(y = "$US",
       title = "Google closing stock prices")

## ----qp, dependson='GSPfc', echo=FALSE--------------------------------------------------------------------
lo80 <- google_fc %>%
  select(Date, .model, Close) %>%
  hilo(Close, level = 80) %>%
  unpack_hilo(`80%`) %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  pull(`80%_lower`)
actual <- google_stock %>%
  filter(Date == ymd("2016-01-04")) %>%
  pull(Close)
pi80 <- google_fc %>%
  select(Date, .model, Close) %>%
  hilo(Close, level = 80) %>%
  unpack_hilo(`80%`) %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  select(`80%_lower`, `80%_upper`) %>%
  rename(lo = `80%_lower`, hi = `80%_upper`)

## ----googlepcscore, dependson='GSPfc'---------------------------------------------------------------------
google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock, list(qs=quantile_score), probs=0.10)

## ----googlewinklerscore, dependson='GSPfc'----------------------------------------------------------------
google_fc %>%
  filter(.model == "Naïve", Date == "2016-01-04") %>%
  accuracy(google_stock,
    list(winkler = winkler_score), level = 80)

## ----crps, dependson='GSPfc'------------------------------------------------------------------------------
google_fc %>%
  accuracy(google_stock, list(crps = CRPS))

## ----skillscore, dependson='GSPfc'------------------------------------------------------------------------
google_fc %>%
  accuracy(google_stock, list(skill = skill_score(CRPS)))

## ----cairo-dep, include=FALSE-----------------------------------------------------------------------------
# Declare Cairo suggest as renv dependency
library(Cairo)

## ----cv1, echo=FALSE, fig.asp=0.47, dev=if_else(html,'CairoSVG','pdf')------------------------------------
tscv_plot <- function(.init, .step, h = 1) {
  expand.grid(
    time = seq(26),
    .id = seq(trunc(20 / .step))
  ) %>%
    group_by(.id) %>%
    mutate(
      observation = case_when(
        time <= ((.id - 1) * .step + .init) ~ "train",
        time %in% c((.id - 1) * .step + .init + h) ~ "test",
        TRUE ~ "unused"
      )
    ) %>%
    ungroup() %>%
    filter(.id <= 26 - .init) %>%
    ggplot(aes(x = time, y = .id)) +
    geom_segment(
      aes(x = 0, xend = 27, y = .id, yend = .id),
      arrow = arrow(length = unit(0.015, "npc")),
      col = "black", size = .25
    ) +
    geom_point(aes(col = observation), size = 2) +
    scale_y_reverse() +
    scale_colour_manual(values = c(train = "#0072B2", test = "#D55E00", unused = "gray")) +
    #theme_void() +
    #geom_label(aes(x = 28.5, y = 1, label = "time")) +
    guides(colour = "none") +
    labs(x="time", y="") +
    theme_void() +
    theme(axis.title = element_text())
}
tscv_plot(.init = 6, .step = 1, h = 1)

## ----cv4, echo=FALSE, fig.asp=0.47, dependson='cv1', dev=if_else(html,'CairoSVG','pdf')-------------------
tscv_plot(.init = 6, .step = 1, h = 4)

## ----googtscv, dependson="google2015", warning=FALSE------------------------------------------------------
# Time series cross-validation accuracy
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)
google_2015_tr

## ----googtscv2,  results='hide', warning=FALSE, dependson="googtscv"--------------------------------------
# TSCV accuracy
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)
# Training set accuracy
google_2015 %>%
  model(RW(Close ~ drift())) %>%
  accuracy()

## ----googtscveval, warning=FALSE, echo = FALSE, dependson='googtscv'--------------------------------------
google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015) %>%
  mutate(.type = "Cross-validation") %>%
  bind_rows(google_2015 %>% model(RW(Close ~ drift())) %>% accuracy()) %>%
  rename(`Evaluation method` = .type) %>%
  select(`Evaluation method`, RMSE, MAE, MAPE, MASE) %>%
  knitr::kable(digits = 2, booktabs = TRUE)

## ----CV-accuracy-plot, echo=TRUE, warning=FALSE, fig.cap="RMSE as a function of forecast horizon for the drift method applied to Google closing stock prices."----
google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 8) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup()
fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE)) +
  geom_point()

