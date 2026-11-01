## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----fig-calls----
p1 <- bank_calls |>
  fill_gaps() |>
  autoplot(Calls) +
  labs(y = "Calls", title = "Five-minute call volume to bank")
p2 <- bank_calls |>
  fill_gaps() |>
  filter(as_date(DateTime) <= "2003-03-28") |>
  autoplot(Calls) +
  labs(y = "Calls", title = "Five-minute call volume over 4 weeks")
p1 / p2

## ----callsmstl0----
calls <- bank_calls |>
  mutate(t = row_number()) |>
  update_tsibble(index = t, regular = TRUE)

## ----fig-callsmstl----
calls |>
  model(
    STL(
      sqrt(Calls) ~ season(period = 169) +
        season(period = 5 * 169),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot() +
  labs(x = "Observation")

## ----fig-callsmstlf----
# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(
    sqrt(Calls) ~ season(period = 169) +
      season(period = 5 * 169),
    robust = TRUE
  ),
  ETS(season_adjust ~ season("N"))
)
fc <- calls |>
  model(my_dcmp_spec) |>
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
  new_data(n = 7 * 24 * 60 / 5) |>
  mutate(time = format(DateTime, format = "%H:%M:%S")) |>
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) |>
  mutate(t = row_number() + max(calls$t)) |>
  left_join(fc, by = "t") |>
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times |>
  fill_gaps() |>
  autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
  labs(y = "Calls", title = "Five-minute call volume to bank")

## ----callsharmonics0----
fit <- calls |>
  model(
    dhr = ARIMA(
      sqrt(Calls) ~ PDQ(0, 0, 0) +
        pdq(d = 0) +
        fourier(period = 169, K = 10) +
        fourier(period = 5 * 169, K = 5)
    )
  )

fc <- fit |> forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls |>
  new_data(n = 7 * 24 * 60 / 5) |>
  mutate(time = format(DateTime, format = "%H:%M:%S")) |>
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) |>
  mutate(t = row_number() + max(calls$t)) |>
  left_join(fc, by = "t") |>
  as_fable(response = "Calls", distribution = Calls)

## ----fig-callsharmonics----
# Plot results with last 3 weeks of data
fc_with_times |>
  fill_gaps() |>
  autoplot(bank_calls |> tail(14 * 169) |> fill_gaps()) +
  labs(y = "Calls", title = "Five-minute call volume to bank")

## ----callsorder----
ncoef <- fit |>
  broom::tidy() |>
  NROW()
if (ncoef == 0L) {
  stop(paste("Model error", broom::tidy(fit)))
}
arma <- fit |>
  tidy() |>
  filter(stringr::str_detect(term, "[mar][mar][0-9]")) |>
  NROW()
nf169 <- fit |>
  tidy() |>
  filter(stringr::str_detect(term, "fourier\\(period = 169")) |>
  NROW()
nf845 <- fit |>
  tidy() |>
  filter(stringr::str_detect(term, "fourier\\(period = 5")) |>
  NROW()
if (ncoef != (arma + nf169 + nf845 + 1L)) {
  stop(paste(
    "Coefficients don't add up",
    ncoef,
    arma,
    nf169,
    nf845
  ))
}

## ----fig-elecdemand----
vic_elec |>
  pivot_longer(Demand:Temperature, names_to = "Series") |>
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "")

## ----fig-elecdemand2----
elec <- vic_elec |>
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )
elec |>
  ggplot(aes(x = Temperature, y = Demand, col = Working_Day)) +
  geom_point(alpha = 0.6) +
  labs(x = "Temperature (degrees Celsius)", y = "Demand (MWh)")

## ----elecdemand3----
fit <- elec |>
  model(
    ARIMA(
      Demand ~ PDQ(0, 0, 0) +
        pdq(d = 0) +
        Temperature +
        Cooling +
        Working_Day +
        fourier(period = "day", K = 10) +
        fourier(period = "week", K = 5) +
        fourier(period = "year", K = 3)
    )
  )

## ----fig-elecdemand4----
elec_newdata <- new_data(elec, 2 * 48) |>
  mutate(
    Temperature = tail(elec$Temperature, 2 * 48),
    Date = lubridate::as_date(Time),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2015-01-01") &
      !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )
fc <- fit |>
  forecast(new_data = elec_newdata)

fc |>
  autoplot(elec |> tail(10 * 48)) +
  labs(
    title = "Half hourly electricity demand: Victoria",
    y = "Demand (MWh)",
    x = "Time [30m]"
  )

## ----fig-elecdemand5----
fit |> gg_tsresiduals()

## ----propheteg----
library(fable.prophet)
cement <- aus_production |>
  filter(year(Quarter) >= 1988)
train <- cement |>
  filter(year(Quarter) <= 2007)
fit <- train |>
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(
      Cement ~ season(period = 4, order = 2, type = "multiplicative")
    )
  )

## ----fig-prophetegfc----
fc <- fit |> forecast(h = "2 years 6 months")
fc |> autoplot(cement)

## ----prophetegaccuracy----
fc |> accuracy(cement)

## ----fig-prophetelec----
fit <- elec |>
  model(
    prophet(
      Demand ~ Temperature +
        Cooling +
        Working_Day +
        season(period = "day", order = 10) +
        season(period = "week", order = 5) +
        season(period = "year", order = 3)
    )
  )
fit |>
  components() |>
  autoplot() +
  theme(panel.spacing = unit(1, "lines"))

## ----fig-prophetelecres----
fit |> gg_tsresiduals()

## ----prophetfc0----
fc <- fit |>
  forecast(new_data = elec_newdata)

## ----fig-prophetfc----
fc |>
  autoplot(elec |> tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")

## ----varselect----
fit <- us_change |>
  model(
    aicc = VAR(vars(Consumption, Income)),
    bic = VAR(vars(Consumption, Income), ic = "bic")
  )
fit

## ----varglance----
glance(fit)

## ----fig-varplots----
fit |>
  augment() |>
  ACF(.innov) |>
  autoplot()

## ----fig-VAR5----
fit |>
  select(aicc) |>
  forecast() |>
  autoplot(us_change |> filter(year(Quarter) > 2010))

## ----sunspot1----
fit <- sunspot.year |>
  as_tsibble() |>
  model(NNETAR(sqrt(value)))

## ----sunspotname----
sunspot_model <- fit[[1]] |>
  as.character() |>
  stringr::str_remove_all("[<>]*")
sunspot_p <- sunspot_model |>
  stringr::str_extract("[0-9]*,") |>
  stringr::str_remove(",") |>
  as.numeric()
sunspot_k <- sunspot_model |>
  stringr::str_extract(",[0-9]*") |>
  stringr::str_remove(",") |>
  as.numeric()
stopifnot(sunspot_p == 9L & sunspot_k == 5L)

## ----fig-sunspotnnetar----
sunspots <- sunspot.year |> as_tsibble()
fit <- sunspots |>
  model(NNETAR(sqrt(value)))
fit |>
  forecast(h = 30) |>
  autoplot(sunspots) +
  labs(x = "Year", y = "Counts", title = "Yearly sunspots")

## ----fig-nnetarsim----
fit |>
  generate(times = 9, h = 30) |>
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")

## ----fig-cementstl----
cement <- aus_production |>
  filter(year(Quarter) >= 1988) |>
  select(Quarter, Cement)
cement_stl <- cement |>
  model(stl = STL(Cement))
cement_stl |>
  components() |>
  autoplot()

## ----fig-cementbootstrapped----
cement_stl |>
  generate(new_data = cement, times = 10, bootstrap_block_size = 8) |>
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: Bootstrapped series", y = "Tonnes ('000)")

## ----cementsim----
sim <- cement_stl |>
  generate(new_data = cement, times = 100, bootstrap_block_size = 8) |>
  select(-.model, -Cement)

## ----fig-cementnboot----
ets_forecasts <- sim |>
  model(ets = ETS(.sim)) |>
  forecast(h = 12)
ets_forecasts |>
  update_tsibble(key = .rep) |>
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: bootstrapped forecasts", y = "Tonnes ('000)")

## ----fig-baggedf----
bagged <- ets_forecasts |>
  summarise(bagged_mean = mean(.mean))
cement |>
  model(ets = ETS(Cement)) |>
  forecast(h = 12) |>
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "Cement production in Australia", y = "Tonnes ('000)")
