source("before-each-chapter.R")

## ----usconsump, fig.cap="Percentage changes in quarterly personal consumption expenditure and personal disposable income for the USA, 1970 Q1 to 2019 Q2.", fig.asp=0.55----
us_change %>%
  pivot_longer(c(Consumption, Income),
               names_to = "var", values_to = "value") %>%
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income",
       y = "Quarterly % change")

## ----usconsump2, fig.cap="Residuals ($e_t$) obtained from a regression of change in consumption expenditure on change in disposable income, assuming an ARIMA(1,0,2) error model."----
fit <- us_change %>%
  model(ARIMA(Consumption ~ Income))
report(fit)

## ----usconsumpparam, echo=FALSE, dependson="usconsump2"---------------------------------------------------
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)
phi1 <- coef["ar1"]
theta1 <- coef["ma1"]
theta2 <- coef["ma2"]
intercept <- coef["intercept"]
slope <- coef["Income"]
sigma2 <- glance(fit)$sigma2

## ----usconsumpres, fig.cap="Regression residuals ($\\eta_t$) and ARIMA residuals ($\\varepsilon_t$) from the fitted model.", fig.asp=0.55, dependson="usconsump2"----
bind_rows(
    `Regression residuals` =
        as_tibble(residuals(fit, type = "regression")),
    `ARIMA residuals` =
        as_tibble(residuals(fit, type = "innovation")),
    .id = "type"
  ) %>%
  mutate(
    type = factor(type, levels=c(
      "Regression residuals", "ARIMA residuals"))
  ) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

## ----digits, echo=FALSE-----------------------------------------------------------------------------------
options(digits = 5)

## ----usconsumpres2, fig.cap="The innovation residuals (i.e., the estimated ARIMA errors) are not significantly different from white noise.", dependson=c('usconsump2','digits'), class.output='r', dependson="usconsump2"----
fit %>% gg_tsresiduals()

## ----usconsumpres3, dependson="usconsump2"----------------------------------------------------------------
augment(fit) %>%
  features(.innov, ljung_box, dof = 5, lag = 8)

## ----usconsump3, fig.cap="Forecasts obtained from regressing the percentage change in consumption expenditure on the percentage change in disposable income, with an ARIMA(1,0,2) error model.", fig.asp=0.5, dependson='usconsump2'----
us_change_future <- new_data(us_change, 8) %>%
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) %>%
  autoplot(us_change) +
  labs(y = "Percentage change")

## ----elecscatter, echo=TRUE, fig.cap="Daily electricity demand versus maximum daily temperature for the state of Victoria in Australia for 2014.", fig.asp=0.75----
vic_elec_daily <- vic_elec %>%
  filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) %>%
  mutate(Day_Type = case_when(
    Holiday ~ "Holiday",
    wday(Date) %in% 2:6 ~ "Weekday",
    TRUE ~ "Weekend"
  ))

vic_elec_daily %>%
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)",
       x = "Maximum daily temperature")

## ----electime, fig.cap="Daily electricity demand and maximum daily temperature for the state of Victoria in Australia for 2014."----
vic_elec_daily %>%
  pivot_longer(c(Demand, Temperature)) %>%
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") + ylab("")

## ----elecdailyfit, fig.cap="Residuals diagnostics for a dynamic regression model for daily electricity demand with workday and quadratic temperature effects."----
fit <- vic_elec_daily %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (Day_Type == "Weekday")))
fit %>% gg_tsresiduals()
augment(fit) %>%
  features(.innov, ljung_box, dof = 9, lag = 14)

## ----checkelecfit, include=FALSE, dependson="elecdailyfit"------------------------------------------------
ncoef <- fit %>% tidy() %>% NROW()
if(ncoef != 9L)
  stop("dof incorrect")

## ----elecdailyfc, fig.cap="Forecasts from the dynamic regression model for daily electricity demand. All future temperatures have been set to 26 degrees, and the working day dummy variable has been set to known future values.", dependson='elecdailyfit'----
vic_elec_future <- new_data(vic_elec_daily, 14) %>%
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) %>%
  autoplot(vic_elec_daily) +
  labs(title="Daily electricity demand: Victoria",
       y="GW")

## ----austa, fig.cap="Total annual passengers (in millions) for Australian air carriers, 1970--2016.", fig.asp=0.5----
aus_airpassengers %>%
  autoplot(Passengers) +
  labs(y = "Passengers (millions)",
       title = "Total annual air passengers")

## ----deterministictrend-----------------------------------------------------------------------------------
fit_deterministic <- aus_airpassengers %>%
  model(deterministic = ARIMA(Passengers ~ 1 + trend() +
                                pdq(d = 0)))
report(fit_deterministic)

## ----austaparams, echo=FALSE, dependson='deterministictrend'----------------------------------------------
coef <- rlang::set_names(tidy(fit_deterministic)$estimate, tidy(fit_deterministic)$term)
phi1 <- coef["ar1"]
intercept <- coef["intercept"]
slope <- coef["trend()"]
sigma2 <- glance(fit_deterministic)$sigma2

## ----stochastictrend--------------------------------------------------------------------------------------
fit_stochastic <- aus_airpassengers %>%
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))
report(fit_stochastic)

## ----austaparams2, echo=FALSE, dependson='stochastictrend'------------------------------------------------
coef <- rlang::set_names(tidy(fit_stochastic)$estimate, tidy(fit_stochastic)$term)
drift <- coef["constant"]
sigma2 <- glance(fit_stochastic)$sigma2

## ----austaf, fig.cap="Forecasts of annual passengers for Australian air carriers using a deterministic trend model (orange) and a stochastic trend model (blue).", message=FALSE,dependson=c("deterministictrend","stochastictrend")----
aus_airpassengers %>%
  autoplot(Passengers) +
  autolayer(fit_stochastic %>% forecast(h = 20),
    colour = "#0072B2", level = 95) +
  autolayer(fit_deterministic %>% forecast(h = 20),
    colour = "#D55E00", alpha = 0.65, level = 95) +
  labs(y = "Air passengers (millions)",
       title = "Forecasts from trend models")

## ----eatout, fig.width=10, fig.asp=0.8,fig.cap="Using Fourier terms and ARIMA errors for forecasting monthly expenditure on eating out in Australia.", fig.env="figure*"----
aus_cafe <- aus_retail %>%
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) %>%
  summarise(Turnover = sum(Turnover))

fit <- model(aus_cafe,
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K=1) + PDQ(0,0,0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K=2) + PDQ(0,0,0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K=3) + PDQ(0,0,0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K=4) + PDQ(0,0,0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K=5) + PDQ(0,0,0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K=6) + PDQ(0,0,0))
)

fit %>%
  forecast(h = "2 years") %>%
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(x = yearmonth("2007 Jan"), y = 4250,
        label = paste0("AICc = ", format(AICc))),
    data = glance(fit)
  ) +
  labs(title= "Total monthly eating-out expenditure",
       y="$ billions")

## ----tvadvert, fig.cap="Numbers of insurance quotations provided per month and the expenditure on advertising per month."----
insurance %>%
  pivot_longer(Quotes:TVadverts) %>%
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

## ----tvadvert2, dependson='tdadvert'----------------------------------------------------------------------
fit <- insurance %>%
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) %>%
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(Quotes ~ pdq(d = 0) +
                 TVadverts + lag(TVadverts)),
    lag2 = ARIMA(Quotes ~ pdq(d = 0) +
                 TVadverts + lag(TVadverts) +
                 lag(TVadverts, 2)),
    lag3 = ARIMA(Quotes ~ pdq(d = 0) +
                 TVadverts + lag(TVadverts) +
                 lag(TVadverts, 2) + lag(TVadverts, 3))
  )

## ----tvadvertaicc, dependson="tvadvert2"------------------------------------------------------------------
glance(fit)

## ----tvfitcheck, echo = FALSE, dependson='tdadvert2'------------------------------------------------------
# Check AICc order
if (sort.int(glance(fit)$AICc, index.return = TRUE)$ix[1] != 2) {
  stop("TV model not correct")
}

## ----tvadvert3, dependson="tvadvert"----------------------------------------------------------------------
fit_best <- insurance %>%
  model(ARIMA(Quotes ~ pdq(d = 0) +
              TVadverts + lag(TVadverts)))
report(fit_best)

## ----tvadvertparam, echo=FALSE, dependson='tdadvert3'-----------------------------------------------------
# Check model
tidy_fit <- tidy(fit_best)
if (!identical(tidy_fit$term[1:3], c("ar1", "ma1", "ma2")) | NROW(tidy_fit) != 6L) {
  stop("Not an ARMA(1,2) model")
}
# Store coefficients
coef <- rlang::set_names(tidy_fit$estimate, tidy_fit$term)
phi1 <- coef["ar1"]
ma1 <- coef["ma1"]
ma2 <- coef["ma2"]
intercept <- coef["intercept"]
gamma0 <- coef["TVadverts"]
gamma1 <- coef["lag(TVadverts)"]

## ----tvadvertf8, fig.cap="Forecasts of monthly insurance quotes, assuming that the future advertising expenditure is 8 units in each future month.", dependson='tvadvert3', fig.asp=0.55----
insurance_future <- new_data(insurance, 20) %>%
  mutate(TVadverts = 8)
fit_best %>%
  forecast(insurance_future) %>%
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )

