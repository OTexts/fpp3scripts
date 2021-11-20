source("before-each-chapter.R")

## ----gasstl, fig.cap="Forecasts for weekly US gasoline production using an STL decomposition with an ETS model for the seasonally adjusted data.", fig.asp=0.58----
my_dcmp_spec <- decomposition_model(
  STL(Barrels),
  ETS(season_adjust ~ season("N"))
)
us_gasoline %>%
  model(stl_ets = my_dcmp_spec) %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")

## ----gasweekly_choosemodel, message=FALSE, include=FALSE--------------------------------------------------
library(purrr)
model_defs <- map(
  as.list(seq(25)),
  ~ ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = !!.[1]))
)
model_defs <- set_names(model_defs, sprintf("ARIMA + fourier(%i)", seq(25)))
gas_aicc <- numeric(length(model_defs))
for (k in seq_along(model_defs)) {
  fit <- us_gasoline %>%
    model(model_defs[[k]]) %>%
    glance()
  if (!is.null(fit$AICc)) {
    gas_aicc[k] <- fit$AICc
  }
}
bestK <- which.min(gas_aicc)
if (bestK != 6L) {
  stop("Gas DHR model changed")
}

## ----gasweekly, message=FALSE-----------------------------------------------------------------------------
gas_dhr <- us_gasoline %>%
  model(dhr = ARIMA(Barrels ~ PDQ(0, 0, 0) + fourier(K = 6)))

## ----gasarima, message=FALSE, warning=FALSE, include=FALSE, dependson="gasweekly"-------------------------
arimaorder <- as.numeric(gas_dhr$dhr[[1]]$fit$spec[1, 1:3])
modelname <- paste0("ARIMA(", arimaorder[1], ",", arimaorder[2], ",", arimaorder[3], ")")
if (!identical(arimaorder, c(0, 1, 1))) {
  stop("Gas DHR ARIMA error changed")
}

## ----gasforecast, fig.cap="Forecasts for weekly US gasoline production using a dynamic harmonic regression model.", fig.asp=0.58, dependson="gasweekly"----
gas_dhr %>%
  forecast(h = "2 years") %>%
  autoplot(us_gasoline) +
  labs(y = "Millions of barrels per day",
       title = "Weekly US gasoline production")

## ----j06, fig.cap="Numbers of scripts sold for Immune sera and immunoglobulins on the Australian Pharmaceutical Benefits Scheme.", fig.height=3, fig.asp=0.5----
j06 <- PBS %>%
  filter(ATC2 == "J06") %>%
  summarise(Scripts = sum(Scripts))

j06 %>% autoplot(Scripts) +
  labs(y="Number of scripts",
       title = "Sales for immune sera and immunoglobulins")

## ----j06table, echo=FALSE---------------------------------------------------------------------------------
firstten <- j06 %>%
  mutate(
    nonzero = Scripts > 0,
    cumdemand = cumsum(nonzero)
  ) %>%
  filter(cumdemand <= 10) %>%
  select(Month, Scripts)
tab <- firstten %>%
  knitr::kable(
    booktabs = TRUE,
    caption = "The first 10 non-zero demand values."
  )
if (!html) {
  tab <- gsub("\\\\centering","\\\\vspace*{-0.2cm}\\\\centering\\\\small",tab)
  tab <- gsub("\\[t\\]","\\[!ht\\]",tab)
}
tab

## ----j06table2, echo=FALSE, warning=FALSE-----------------------------------------------------------------
options(knitr.kable.NA = "")
q <- firstten$Scripts[firstten$Scripts > 0]
a <- c(NA, diff(which(firstten$Scripts > 0)))
out <- rbind(i = seq(10), q, a)
rownames(out) <- c("$i$", "$q_i$", "$a_i$")
out <- out %>%
  knitr::kable(
    booktabs = TRUE,
    escape = FALSE,
    caption = "The first 10 non-zero demand values shown as  demand and inter-arrival series."
  )
if (!html) {
  out <- gsub("\\\\centering","\\\\vspace*{-0.cm}\\\\centering\\\\small",out)
  out <- gsub("\\[t\\]","\\[!ht\\]",out)
  out <- gsub("\\\\end\\{tabular\\}","\\\\end{tabular}\\\\vspace*{0.2cm}",out)
}
out
lastq <- tail(j06$Scripts[j06$Scripts > 0], 1)
if (tail(j06$Scripts, 1) > 0) {
  lasta <- tail(diff(which(j06$Scripts > 0)), 1)
} else {
  lasta <- NROW(j06) - max(which(j06$Scripts > 0))
}

## ----crostonfit, include=FALSE----------------------------------------------------------------------------
fit <- j06 %>%
  model(CROSTON(Scripts))

## ----crostoncheck, echo=FALSE-----------------------------------------------------------------------------
q0 <- fit[[1]][[1]]$fit$par$estimate[1]
alphaq <- fit[[1]][[1]]$fit$par$estimate[2]
a0 <- fit[[1]][[1]]$fit$par$estimate[3]
alphaa <- fit[[1]][[1]]$fit$par$estimate[4]
q <- j06$Scripts[j06$Scripts > 0]
a <- c(NA, diff(which(j06$Scripts > 0)))
qhat <- alphaq * sum(rev((1 - alphaq)^(seq_along(q) - 1)) * q)
ahat <- alphaa * sum(rev((1 - alphaa)^(seq_along(a) - 1)) * a, na.rm = TRUE)

## ----crostonfc--------------------------------------------------------------------------------------------
j06 %>%
  model(CROSTON(Scripts)) %>%
  forecast(h = 6)

## ----positiveeggs, fig.cap="Forecasts for the price of a dozen eggs, constrained to be positive using a Box-Cox transformation."----
egg_prices <- prices %>% filter(!is.na(eggs))
egg_prices %>%
  model(ETS(log(eggs) ~ trend("A"))) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

## ----constrained, fig.cap="Forecasts for the price of a dozen eggs, constrained to be lie between 50 and 400 cents US."----
scaled_logit <- function(x, lower = 0, upper = 1) {
  log((x - lower) / (upper - x))
}
inv_scaled_logit <- function(x, lower = 0, upper = 1) {
  (upper - lower) * exp(x) / (1 + exp(x)) + lower
}
my_scaled_logit <- new_transformation(
                    scaled_logit, inv_scaled_logit)
egg_prices %>%
  model(
    ETS(my_scaled_logit(eggs, lower = 50, upper = 400)
          ~ trend("A"))
  ) %>%
  forecast(h = 50) %>%
  autoplot(egg_prices) +
  labs(title = "Annual egg prices",
       y = "$US (in cents adjusted for inflation) ")

## ----auscafe, message=FALSE, warning=FALSE----------------------------------------------------------------
auscafe <- aus_retail %>%
  filter(stringr::str_detect(Industry, "Takeaway")) %>%
  summarise(Turnover = sum(Turnover))
train <- auscafe %>%
  filter(year(Month) <= 2013)
STLF <- decomposition_model(
  STL(log(Turnover) ~ season(window = Inf)),
  ETS(season_adjust ~ season("N"))
)
cafe_models <- train %>%
  model(
    ets = ETS(Turnover),
    stlf = STLF,
    arima = ARIMA(log(Turnover))
  ) %>%
  mutate(combination = (ets + stlf + arima) / 3)
cafe_fc <- cafe_models %>%
  forecast(h = "5 years")

## ----combineplot, dependson="auscafe", fig.cap="Point forecasts from various methods applied to Australian monthly expenditure on eating out."----
cafe_fc %>%
  autoplot(auscafe %>% filter(year(Month) > 2008),
           level = NULL) +
  labs(y = "$ billion",
       title = "Australian monthly expenditure on eating out")

## ----combineaccuracy, dependson="auscafe"-----------------------------------------------------------------
cafe_fc %>%
  accuracy(auscafe) %>%
  arrange(RMSE)

## ----cafe_fc_dist-----------------------------------------------------------------------------------------
cafe_fc %>% filter(Month == min(Month))

## ----cafe_fc_gen, warning=FALSE, message=FALSE------------------------------------------------------------
cafe_futures <- cafe_models %>%
  # Generate 1000 future sample paths
  generate(h = "5 years", times = 1000) %>%
  # Compute forecast distributions from future sample paths
  as_tibble() %>%
  group_by(Month, .model) %>%
  summarise(
    dist = distributional::dist_sample(list(.sim))
  ) %>%
  ungroup() %>%
  # Create fable object
  as_fable(index = Month, key = .model,
           distribution = dist, response = "Turnover")

## ----cafe_fc_gen_futures, warning=FALSE, message=FALSE, dependson="cafe_fc_gen"---------------------------
# Forecast distributions for h=1
cafe_futures %>% filter(Month == min(Month))

## ----auscafecombPI, fig.cap="Prediction intervals for the combination forecast of Australian monthly expenditure on eating out.", fig.asp=0.5----
cafe_futures %>%
  filter(.model == "combination") %>%
  autoplot(auscafe %>% filter(year(Month) > 2008)) +
  labs(y = "$ billion",
       title = "Australian monthly expenditure on eating out")

## ----auscafe_winkler--------------------------------------------------------------------------------------
cafe_futures %>%
  accuracy(auscafe, measures = interval_accuracy_measures,
    level = 95) %>%
  arrange(winkler)

## ----aggregates, message=FALSE, dependson="auscafe"-------------------------------------------------------
fit <- auscafe %>%
  # Fit a model to the data
  model(ETS(Turnover))
futures <- fit %>%
  # Simulate 10000 future sample paths, each of length 12
  generate(times = 10000, h = 12) %>%
  # Sum the results for each sample path
  as_tibble() %>%
  group_by(.rep) %>%
  summarise(.sim = sum(.sim)) %>%
  # Store as a distribution
  summarise(total = distributional::dist_sample(list(.sim)))

## ----aggregates2, dependson="aggregates"------------------------------------------------------------------
futures %>%
  mutate(
    mean = mean(total),
    pi80 = hilo(total, 80),
    pi95 = hilo(total, 95)
  )

## ----aggregates3, dependson="aggregates"------------------------------------------------------------------
forecast(fit, h = 12) %>%
  as_tibble() %>%
  summarise(total = sum(.mean))

## ----backcasting, fig.cap="Backcasts for Australian monthly expenditure on cafés, restaurants and takeaway food services using an ETS model.", dependson='auscafe'----
backcasts <- auscafe %>%
  mutate(reverse_time = rev(row_number())) %>%
  update_tsibble(index = reverse_time) %>%
  model(ets = ETS(Turnover ~ season(period = 12))) %>%
  forecast(h = 15) %>%
  mutate(Month = auscafe$Month[1] - (1:15)) %>%
  as_fable(index = Month, response = "Turnover",
    distribution = "Turnover")
backcasts %>%
  autoplot(auscafe %>% filter(year(Month) < 1990)) +
  labs(title = "Backcasts of Australian food expenditure",
       y = "$ (billions)")

## ----shortseries, message=FALSE---------------------------------------------------------------------------
m3totsibble <- function(z) {
  bind_rows(
    as_tsibble(z$x) %>% mutate(Type = "Training"),
    as_tsibble(z$xx) %>% mutate(Type = "Test")
  ) %>%
    mutate(
      st = z$st,
      type = z$type,
      period = z$period,
      description = z$description,
      sn = z$sn,
    ) %>%
    as_tibble()
}
short <- Mcomp::M3 %>%
  subset("yearly") %>%
  purrr::map_dfr(m3totsibble) %>%
  group_by(sn) %>%
  mutate(n = max(row_number())) %>%
  filter(n <= 20) %>%
  ungroup() %>%
  as_tsibble(index = index, key = c(sn, period, st))

## ----shortfit, dependson="shortseries"--------------------------------------------------------------------
short_fit <- short %>%
  model(arima = ARIMA(value))

## ----shortfit_results, dependson="shortfit", include=FALSE------------------------------------------------
nptable <- tidy(short_fit) %>%
  group_by(sn) %>%
  summarise(n = n()) %>%
  right_join(short_fit) %>%
  replace_na(list(n = 0)) %>%
  group_by(n) %>%
  summarise(count = n())

## ----isms, warning=FALSE, fig.cap="Forecasts from an ARIMA model fitted to the Australian monthly expenditure on cafés, restaurants and takeaway food services.", dependson='auscafe', fig.asp=0.58----
training <- auscafe %>% filter(year(Month) <= 2013)
test <- auscafe %>% filter(year(Month) > 2013)
cafe_fit <- training %>%
  model(ARIMA(log(Turnover)))
cafe_fit %>%
  forecast(h = 60) %>%
  autoplot(auscafe) +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")

## ----isms2, dependson="isms", warning=FALSE, fig.cap="Twelve-step fitted values from an ARIMA model fitted to the Australian café training data.", fig.asp=0.58----
fits12 <- fitted(cafe_fit, h = 12)
training %>%
  autoplot(Turnover) +
  autolayer(fits12, .fitted, col = "#D55E00") +
  labs(title = "Australian food expenditure",
       y = "$ (billions)")

## ----oosos2, dependson='isms'-----------------------------------------------------------------------------
cafe_fit %>%
  refit(test) %>%
  accuracy()

## ----ahoutlier, fig.cap="Number of overnight trips to the Adelaide Hills region of South Australia.", fig.asp=0.4----
tourism %>%
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) %>%
  autoplot(Trips) +
  labs(title = "Quarterly overnight trips to Adelaide Hills",
       y = "Number of trips")

## ----stlahdecomp, fig.cap="STL decomposition of visitors to the Adelaide Hills region of South Australia, with no seasonal component.", fig.asp=0.7----
ah_decomp <- tourism %>%
  filter(
    Region == "Adelaide Hills", Purpose == "Visiting"
  ) %>%
  # Fit a non-seasonal STL decomposition
  model(
    stl = STL(Trips ~ season(period = 1), robust = TRUE)
  ) %>%
  components()
ah_decomp %>% autoplot()

## ----stl_outliers, dependson="stlahdecomp"----------------------------------------------------------------
outliers <- ah_decomp %>%
  filter(
    remainder < quantile(remainder, 0.25) - 3*IQR(remainder) |
    remainder > quantile(remainder, 0.75) + 3*IQR(remainder)
  )
outliers

## ----ah_miss, message=FALSE, dependson="stl_outliers"-----------------------------------------------------
ah_miss <- tourism %>%
  filter(
    Region == "Adelaide Hills",
    Purpose == "Visiting"
  ) %>%
  # Remove outlying observations
  anti_join(outliers) %>%
  # Replace with missing values
  fill_gaps()
ah_fill <- ah_miss %>%
  # Fit ARIMA model to the data containing missing values
  model(ARIMA(Trips)) %>%
  # Estimate Trips for all periods
  interpolate(ah_miss)
ah_fill %>%
  # Only show outlying periods
  right_join(outliers %>% select(-Trips))

## ----replacment, include=FALSE, dependson="ah_miss"-------------------------------------------------------
outlier <- outliers$Trips
replacement <- ah_miss %>%
  model(ARIMA(Trips)) %>%
  interpolate(ah_miss) %>%
  right_join(outliers %>% select(-Trips)) %>%
  pull(Trips)

## ----replacement-plot, fig.cap="Number of overnight trips to the Adelaide Hills region of South Australia with the 2002Q4 outlier being replaced using an ARIMA model for interpolation.", dependson="ah_miss"----
ah_fill %>%
  autoplot(Trips) +
  autolayer(ah_fill %>% filter_index("2002 Q3"~"2003 Q1"),
    Trips, colour="#D55E00") +
  labs(title = "Quarterly overnight trips to Adelaide Hills",
       y = "Number of trips")

