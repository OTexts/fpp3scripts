source("before-each-chapter.R")

## ----calls, echo=FALSE, fig.cap="Five-minute call volume handled on weekdays between 7:00am and 9:05pm in a large North American commercial bank. Top panel: data from 3 March -- 24 October 2003. Bottom panel: first four weeks of data.", fig.asp=0.7, fig.pos="htb", warning=FALSE, message=FALSE----
p1 <- bank_calls %>%
  fill_gaps() %>%
  autoplot(Calls) +
  labs(y = "Calls",
       title = "Five-minute call volume to bank")
p2 <- bank_calls %>%
  fill_gaps() %>%
  filter(as_date(DateTime) <= "2003-03-28") %>%
  autoplot(Calls) +
  labs(y = "Calls",
       title = "Five-minute call volume over 4 weeks")
p1 / p2

## ----callsmstl0-------------------------------------------------------------------------------------------
calls <- bank_calls %>%
  mutate(t = row_number()) %>%
  update_tsibble(index = t, regular = TRUE)

## ----callsmstl, fig.cap="Multiple STL for the call volume data.", fig.asp=.95, fig.height=6---------------
calls %>%
  model(
    STL(sqrt(Calls) ~ season(period = 169) +
                      season(period = 5*169),
        robust = TRUE)
  ) %>%
  components() %>%
  autoplot() + labs(x = "Observation")

## ----callsmstlf, fig.cap="Multiple STL for the call volume data.", fig.asp=0.55---------------------------
# Forecasts from STL+ETS decomposition
my_dcmp_spec <- decomposition_model(
  STL(sqrt(Calls) ~ season(period = 169) +
                    season(period = 5*169),
      robust = TRUE),
  ETS(season_adjust ~ season("N"))
)
fc <- calls %>%
  model(my_dcmp_spec) %>%
  forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(y = "Calls",
       title = "Five-minute call volume to bank")

## ----callsharmonics0, echo=TRUE, dependson='callsmstl',warning=FALSE--------------------------------------
fit <- calls %>%
  model(
    dhr = ARIMA(sqrt(Calls) ~ PDQ(0, 0, 0) + pdq(d = 0) +
                  fourier(period = 169, K = 10) +
                  fourier(period = 5*169, K = 5)))

fc <- fit %>% forecast(h = 5 * 169)

# Add correct time stamps to fable
fc_with_times <- bank_calls %>%
  new_data(n = 7 * 24 * 60 / 5) %>%
  mutate(time = format(DateTime, format = "%H:%M:%S")) %>%
  filter(
    time %in% format(bank_calls$DateTime, format = "%H:%M:%S"),
    wday(DateTime, week_start = 1) <= 5
  ) %>%
  mutate(t = row_number() + max(calls$t)) %>%
  left_join(fc, by = "t") %>%
  as_fable(response = "Calls", distribution = Calls)

## ----callsharmonics, echo=TRUE, fig.cap="Forecasts from a dynamic harmonic regression applied to the call volume data.",fig.asp=0.55,dependson='callsharmonics0',warning=FALSE----
# Plot results with last 3 weeks of data
fc_with_times %>%
  fill_gaps() %>%
  autoplot(bank_calls %>% tail(14 * 169) %>% fill_gaps()) +
  labs(y = "Calls",
       title = "Five-minute call volume to bank")

## ----callsorder, echo=FALSE, dependson='callsharmonics'---------------------------------------------------
ncoef <- fit %>%
  broom::tidy() %>%
  NROW()
if (ncoef == 0L) stop(paste("Model error", broom::tidy(fit)))
arma <- fit %>%
  tidy() %>%
  filter(stringr::str_detect(term, "[mar][mar][0-9]")) %>%
  NROW()
nf169 <- fit %>%
  tidy() %>%
  filter(stringr::str_detect(term, "fourier\\(period = 169")) %>%
  NROW()
nf845 <- fit %>%
  tidy() %>%
  filter(stringr::str_detect(term, "fourier\\(period = 5")) %>%
  NROW()
if (ncoef != (arma + nf169 + nf845 + 1L)) {
  stop(paste(
    "Coefficients don't add up",
    ncoef, arma, nf169, nf845
  ))
}

## ----elecdemand, echo=TRUE, fig.cap="Half-hourly electricity demand and corresponding temperatures in 2012--2014, Victoria, Australia."----
vic_elec %>%
  pivot_longer(Demand:Temperature, names_to = "Series") %>%
  ggplot(aes(x = Time, y = value)) +
  geom_line() +
  facet_grid(rows = vars(Series), scales = "free_y") +
  labs(y = "")

## ----elecdemand2, echo=TRUE, fig.cap="Half-hourly electricity demand for Victoria, plotted against temperatures for the same times in Melbourne, the largest city in Victoria."----
elec <- vic_elec %>%
  mutate(
    DOW = wday(Date, label = TRUE),
    Working_Day = !Holiday & !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )
elec %>%
  ggplot(aes(x=Temperature, y=Demand, col=Working_Day)) +
  geom_point(alpha = 0.6) +
  labs(x="Temperature (degrees Celsius)", y="Demand (MWh)")

## ----elecdemand3, echo=TRUE, dependson='elecdemand2', warning=FALSE---------------------------------------
fit <- elec %>%
  model(
    ARIMA(Demand ~ PDQ(0, 0, 0) + pdq(d = 0) +
          Temperature + Cooling + Working_Day +
          fourier(period = "day", K = 10) +
          fourier(period = "week", K = 5) +
          fourier(period = "year", K = 3))
  )

## ----elecdemand4, echo=TRUE, fig.cap="Forecasts from a dynamic harmonic regression model applied to half-hourly electricity demand data.", dependson='elecdemand3', fig.asp=0.55----
elec_newdata <- new_data(elec, 2*48) %>%
  mutate(
    Temperature = tail(elec$Temperature, 2 * 48),
    Date = lubridate::as_date(Time),
    DOW = wday(Date, label = TRUE),
    Working_Day = (Date != "2015-01-01") &
                   !(DOW %in% c("Sat", "Sun")),
    Cooling = pmax(Temperature, 18)
  )
fc <- fit %>%
  forecast(new_data = elec_newdata)

fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(title="Half hourly electricity demand: Victoria",
       y = "Demand (MWh)", x = "Time [30m]")

## ----elecdemand5, echo=TRUE, fig.cap="Residual diagnostics for the dynamic harmonic regression model.", dependson='elecdemand4'----
fit %>% gg_tsresiduals()

## ----propheteg, message=FALSE-----------------------------------------------------------------------------
library(fable.prophet)
cement <- aus_production %>%
  filter(year(Quarter) >= 1988)
train <- cement %>%
  filter(year(Quarter) <= 2007)
fit <- train %>%
  model(
    arima = ARIMA(Cement),
    ets = ETS(Cement),
    prophet = prophet(Cement ~ season(period = 4, order = 2,
                                    type = "multiplicative"))
  )

## ----prophetegfc, fig.cap="Prophet compared to ETS and ARIMA on the Cement production data, with a 10-quarter test set."----
fc <- fit %>% forecast(h = "2 years 6 months")
fc %>% autoplot(cement)

## ----prophetegaccuracy------------------------------------------------------------------------------------
fc %>% accuracy(cement)

## ----prophetelec, fig.cap="Components of a Prophet model fitted to the Victorian electricity demand data.", fig.height=6, fig.asp=0.9----
fit <- elec %>%
  model(
    prophet(Demand ~ Temperature + Cooling + Working_Day +
            season(period = "day", order = 10) +
            season(period = "week", order = 5) +
            season(period = "year", order = 3))
  )
fit %>%
  components() %>%
  autoplot()

## ----prophetelecres, fig.asp=0.6, fig.cap="Residuals from the Prophet model for Victorian electricity demand.", dependson="prophetelec"----
fit %>% gg_tsresiduals()

## ----prophetfc0, dependson="prophetelec"------------------------------------------------------------------
fc <- fit %>%
  forecast(new_data = elec_newdata)

## ----prophetfc, fig.cap="Two day forecasts from the Prophet model for Victorian electricity demand.", dependson="prophetfc0"----
fc %>%
  autoplot(elec %>% tail(10 * 48)) +
  labs(x = "Date", y = "Demand (MWh)")

## ----varselect--------------------------------------------------------------------------------------------
fit <- us_change %>%
  model(
    aicc = VAR(vars(Consumption, Income)),
    bic = VAR(vars(Consumption, Income), ic = "bic")
  )
fit

## ----varglance, dependson='varselect'---------------------------------------------------------------------
glance(fit)

## ----varplots, fig.height=6, fig.asp=0.7, fig.cap="ACF of the residuals from the two VAR models. A VAR(5) model is selected by the AICc, while a VAR(1) model is selected using the BIC.", dependson='varselect',out.width="100%"----
fit %>%
  augment() %>%
  ACF(.innov) %>%
  autoplot()

## ----VAR5, fig.cap="Forecasts for US consumption and income generated from a VAR(5) model.", fig.asp=0.4, dependson='varselect'----
fit %>%
  select(aicc) %>%
  forecast() %>%
  autoplot(us_change %>% filter(year(Quarter) > 2010))

## ----sunspot1, echo=FALSE, message=FALSE------------------------------------------------------------------
fit <- sunspot.year %>%
  as_tsibble() %>%
  model(NNETAR(sqrt(value)))

## ----sunspotname, echo=FALSE------------------------------------------------------------------------------
sunspot_model <- fit[[1]] %>%
  as.character() %>%
  stringr::str_remove_all("[<>]*")
sunspot_p <- sunspot_model %>%
  stringr::str_extract("[0-9]*,") %>%
  stringr::str_remove(",") %>%
  as.numeric()
sunspot_k <- sunspot_model %>%
  stringr::str_extract(",[0-9]*") %>%
  stringr::str_remove(",") %>%
  as.numeric()
stopifnot(sunspot_p == 9L & sunspot_k == 5L)

## ----sunspotnnetar, fig.cap="Forecasts from a neural network with ten lagged inputs and one hidden layer containing six neurons."----
sunspots <- sunspot.year %>% as_tsibble()
sunspots %>%
  model(NNETAR(sqrt(value))) %>%
  forecast(h = 30) %>%
  autoplot(sunspots) +
  labs(x = "Year", y = "Counts",
       title = "Yearly sunspots")

## ----nnetarsim, message=FALSE, fig.cap="Future sample paths for the  annual sunspot data."----------------
fit %>%
  generate(times = 9, h = 30) %>%
  autoplot(.sim) +
  autolayer(sunspots, value) +
  theme(legend.position = "none")

## ----cementstl, message=FALSE, warning=FALSE, fig.cap="STL decomposition of quarterly Australian cement production.", fig.asp=0.9----
cement <- aus_production %>%
  filter(year(Quarter) >= 1988) %>%
  select(Quarter, Cement)
cement_stl <- cement %>%
  model(stl = STL(Cement))
cement_stl %>%
  components() %>%
  autoplot()

## ----cementbootstrapped, fig.cap="Ten bootstrapped versions of quarterly Australian cement production (coloured), along with the original data (black).",dependson="cementstl"----
cement_stl %>%
  generate(new_data = cement, times = 10,
           bootstrap_block_size = 8) %>%
  autoplot(.sim) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: Bootstrapped series",
       y="Tonnes ('000)")

## ----cementsim, dependson="cementstl"---------------------------------------------------------------------
sim <- cement_stl %>%
  generate(new_data = cement, times = 100,
           bootstrap_block_size = 8) %>%
  select(-.model, -Cement)

## ----cementnboot, dependson="cementsim", fig.cap="Forecasts of 100 bootstrapped series obtained using ETS models.", message=FALSE, warning=FALSE, fig.asp=0.5----
ets_forecasts <- sim %>%
  model(ets = ETS(.sim)) %>%
  forecast(h = 12)
ets_forecasts %>%
  update_tsibble(key = .rep) %>%
  autoplot(.mean) +
  autolayer(cement, Cement) +
  guides(colour = "none") +
  labs(title = "Cement production: bootstrapped forecasts",
       y="Tonnes ('000)")

## ----baggedf, dependson="cementnboot", fig.cap="Comparing bagged ETS forecasts (the average of 100 bootstrapped forecasts in orange) and ETS applied directly to the data (in blue)."----
bagged <- ets_forecasts %>%
  summarise(bagged_mean = mean(.mean))
cement %>%
  model(ets = ETS(Cement)) %>%
  forecast(h = 12) %>%
  autoplot(cement) +
  autolayer(bagged, bagged_mean, col = "#D55E00") +
  labs(title = "Cement production in Australia",
       y="Tonnes ('000)")

