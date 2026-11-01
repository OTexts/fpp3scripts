## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----fig-usconsump----
us_change |>
  pivot_longer(c(Consumption, Income), names_to = "var", values_to = "value") |>
  ggplot(aes(x = Quarter, y = value)) +
  geom_line() +
  facet_grid(vars(var), scales = "free_y") +
  labs(title = "US consumption and personal income", y = "Quarterly % change")

## ----fig-usconsump2----
fit <- us_change |>
  model(ARIMA(Consumption ~ Income))
report(fit)

## ----usconsumpparam----
coef <- rlang::set_names(tidy(fit)$estimate, tidy(fit)$term)
phi1 <- coef["ar1"]
theta1 <- coef["ma1"]
theta2 <- coef["ma2"]
intercept <- coef["intercept"]
slope <- coef["Income"]
sigma2 <- glance(fit)$sigma2

## ----fig-usconsumpres----
bind_rows(
  `Regression residuals` = as_tibble(residuals(fit, type = "regression")),
  `ARIMA residuals` = as_tibble(residuals(fit, type = "innovation")),
  .id = "type"
) |>
  mutate(
    type = factor(
      type,
      levels = c(
        "Regression residuals",
        "ARIMA residuals"
      )
    )
  ) |>
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type))

## ----digits----
options(digits = 5)

## ----fig-usconsumpres2----
fit |> gg_tsresiduals()

## ----usconsumpres3----
augment(fit) |>
  features(.innov, ljung_box, dof = 3, lag = 8)

## ----fig-usconsump3----
us_change_future <- new_data(us_change, 8) |>
  mutate(Income = mean(us_change$Income))
forecast(fit, new_data = us_change_future) |>
  autoplot(us_change) +
  labs(y = "Percentage change")

## ----fig-elecscatter----
vic_elec_daily <- vic_elec |>
  filter(year(Time) == 2014) |>
  index_by(Date = date(Time)) |>
  summarise(
    Demand = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday = any(Holiday)
  ) |>
  mutate(
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )

vic_elec_daily |>
  ggplot(aes(x = Temperature, y = Demand, colour = Day_Type)) +
  geom_point() +
  labs(y = "Electricity demand (GW)", x = "Maximum daily temperature")

## ----fig-electime----
vic_elec_daily |>
  pivot_longer(c(Demand, Temperature)) |>
  ggplot(aes(x = Date, y = value)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  ylab("")

## ----fig-elecdailyfit----
fit <- vic_elec_daily |>
  model(ARIMA(
    Demand ~ Temperature + I(Temperature^2) + (Day_Type == "Weekday")
  ))
fit |> gg_tsresiduals()

## ----elecmodel----
elecmodel <- format(fit[[1]]) |>
  stringr::str_remove("<LM w/ ") |>
  stringr::str_remove(" errors>")
narma <- tidy(fit) |> NROW() - 3
stopifnot(narma == 6L)

## ----elecfit----
augment(fit) |>
  features(.innov, ljung_box, dof = 6, lag = 14)

## ----fig-elecdailyfc----
vic_elec_future <- new_data(vic_elec_daily, 14) |>
  mutate(
    Temperature = 26,
    Holiday = c(TRUE, rep(FALSE, 13)),
    Day_Type = case_when(
      Holiday ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE ~ "Weekend"
    )
  )
forecast(fit, vic_elec_future) |>
  autoplot(vic_elec_daily) +
  labs(title = "Daily electricity demand: Victoria", y = "GW")

## ----fig-austa----
aus_airpassengers |>
  autoplot(Passengers) +
  labs(y = "Passengers (millions)", title = "Total annual air passengers")

## ----deterministictrend----
fit_deterministic <- aus_airpassengers |>
  model(deterministic = ARIMA(Passengers ~ 1 + trend() + pdq(d = 0)))
report(fit_deterministic)

## ----austaparams----
coef <- rlang::set_names(
  tidy(fit_deterministic)$estimate,
  tidy(fit_deterministic)$term
)
phi1 <- coef["ar1"]
intercept <- coef["intercept"]
slope <- coef["trend()"]
sigma2 <- glance(fit_deterministic)$sigma2

## ----stochastictrend----
fit_stochastic <- aus_airpassengers |>
  model(stochastic = ARIMA(Passengers ~ pdq(d = 1)))
report(fit_stochastic)

## ----austaparams2----
coef <- rlang::set_names(
  tidy(fit_stochastic)$estimate,
  tidy(fit_stochastic)$term
)
drift <- coef["constant"]
sigma2 <- glance(fit_stochastic)$sigma2

## ----fig-austaf----
aus_airpassengers |>
  autoplot(Passengers) +
  autolayer(
    fit_stochastic |> forecast(h = 20),
    colour = "#0072B2",
    level = 95
  ) +
  autolayer(
    fit_deterministic |> forecast(h = 20),
    colour = "#D55E00",
    alpha = 0.65,
    level = 95
  ) +
  labs(y = "Air passengers (millions)", title = "Forecasts from trend models")

## ----fig-eatout----
aus_cafe <- aus_retail |>
  filter(
    Industry == "Cafes, restaurants and takeaway food services",
    year(Month) %in% 2004:2018
  ) |>
  summarise(Turnover = sum(Turnover))

fit <- model(
  aus_cafe,
  `K = 1` = ARIMA(log(Turnover) ~ fourier(K = 1) + PDQ(0, 0, 0)),
  `K = 2` = ARIMA(log(Turnover) ~ fourier(K = 2) + PDQ(0, 0, 0)),
  `K = 3` = ARIMA(log(Turnover) ~ fourier(K = 3) + PDQ(0, 0, 0)),
  `K = 4` = ARIMA(log(Turnover) ~ fourier(K = 4) + PDQ(0, 0, 0)),
  `K = 5` = ARIMA(log(Turnover) ~ fourier(K = 5) + PDQ(0, 0, 0)),
  `K = 6` = ARIMA(log(Turnover) ~ fourier(K = 6) + PDQ(0, 0, 0))
)

fit |>
  forecast(h = "2 years") |>
  autoplot(aus_cafe, level = 95) +
  facet_wrap(vars(.model), ncol = 2) +
  guides(colour = "none", fill = "none", level = "none") +
  geom_label(
    aes(
      x = yearmonth("2007 Jan"),
      y = 4250,
      label = paste0("AICc = ", format(AICc))
    ),
    data = glance(fit)
  ) +
  labs(title = "Total monthly eating-out expenditure", y = "$ billions")

## ----fig-tvadvert----
insurance |>
  pivot_longer(Quotes:TVadverts) |>
  ggplot(aes(x = Month, y = value)) +
  geom_line() +
  facet_grid(vars(name), scales = "free_y") +
  labs(y = "", title = "Insurance advertising and quotations")

## ----tvadvert2----
fit <- insurance |>
  # Restrict data so models use same fitting period
  mutate(Quotes = c(NA, NA, NA, Quotes[4:40])) |>
  # Estimate models
  model(
    lag0 = ARIMA(Quotes ~ pdq(d = 0) + TVadverts),
    lag1 = ARIMA(
      Quotes ~ pdq(d = 0) +
        TVadverts +
        lag(TVadverts)
    ),
    lag2 = ARIMA(
      Quotes ~ pdq(d = 0) +
        TVadverts +
        lag(TVadverts) +
        lag(TVadverts, 2)
    ),
    lag3 = ARIMA(
      Quotes ~ pdq(d = 0) +
        TVadverts +
        lag(TVadverts) +
        lag(TVadverts, 2) +
        lag(TVadverts, 3)
    )
  )

## ----tvadvertaicc----
glance(fit)

## ----tvfitcheck----
# Check AICc order
if (sort.int(glance(fit)$AICc, index.return = TRUE)$ix[1] != 2) {
  stop("TV model not correct")
}

## ----tvadvert3----
fit_best <- insurance |>
  model(ARIMA(
    Quotes ~ pdq(d = 0) +
      TVadverts +
      lag(TVadverts)
  ))
report(fit_best)

## ----tvadvertparam----
# Check model
tidy_fit <- tidy(fit_best)
if (
  !identical(tidy_fit$term[1:3], c("ar1", "ma1", "ma2")) | NROW(tidy_fit) != 6L
) {
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

## ----fig-tvadvertf8----
insurance_future <- new_data(insurance, 20) |>
  mutate(TVadverts = 8)
fit_best |>
  forecast(insurance_future) |>
  autoplot(insurance) +
  labs(
    y = "Quotes",
    title = "Forecast quotes with future advertising set to 8"
  )
