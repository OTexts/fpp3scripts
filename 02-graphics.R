## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----tstable----
x <- c(123, 39, 78, 52, 110)
yr <- 2015:2019
knitr::kable(tibble(Year = yr, Observation = x), booktabs = TRUE)

## ----first-tsibble----
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

## ----tstablemonth----
z <- tibble(
  Month = paste(2019, month.abb[1:5]),
  Observation = c(50, 23, 34, 30, 25)
)
# knitr::kable(z, booktabs=TRUE)

## ----tstablemonth2----
z

## ----month-tsibble----
z |>
  mutate(Month = yearmonth(Month)) |>
  as_tsibble(index = Month)

## ----tstable2----
tab <- tribble(
  ~`Frequency` , ~Function                      ,
  "Annual"     , "`start:end`"                  ,
  "Quarterly"  , "`yearquarter()`"              ,
  "Monthly"    , "`yearmonth()`"                ,
  "Weekly"     , "`yearweek()`"                 ,
  "Daily"      , "`as_date()`, `ymd()`"         ,
  "Sub-daily"  , "`as_datetime()`, `ymd_hms()`"
)
if (!html) {
  tab <- tab |>
    mutate(
      Function = stringr::str_replace(Function, "`", "\\\\texttt{"),
      Function = stringr::str_replace(Function, "`, `", "}, \\\\texttt{"),
      Function = stringr::str_replace(Function, "`", "}"),
      Function = stringr::str_replace_all(Function, "_", "\\\\_")
    )
}
tab |> knitr::kable(booktabs = TRUE, escape = html)

## ----tstablekey----
olympic_running

## ----distinctfn----
olympic_running |> distinct(Sex)

## ----pbs1----
PBS

## ----pbs2----
PBS |>
  filter(ATC2 == "A10")

## ----pbs3----
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost)

## ----pbs4----
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost))

## ----pbs5----
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6)

## ----a10----
PBS |>
  filter(ATC2 == "A10") |>
  select(Month, Concession, Type, Cost) |>
  summarise(TotalC = sum(Cost)) |>
  mutate(Cost = TotalC / 1e6) -> a10

## ----prison----
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison |>
  head(10) |>
  knitr::kable(booktabs = TRUE)

## ----loadprison_html----
# prison <- readr::read_csv(
#   "https://OTexts.com/fpp3/extrafiles/prison_population.csv"
# )

## ----loadprison_latex----
# prison <- readr::read_csv(
#   "https://OTexts.com/fpp3/extrafiles/
#                                       prison_population.csv"
# )

## ----prison2----
prison <- prison |>
  mutate(Quarter = yearquarter(Date)) |>
  select(-Date) |>
  as_tsibble(key = c(State, Gender, Legal, Indigenous), index = Quarter)

prison

## ----freqtable----
intervals <- list(
  Quarters = tsibble::new_interval(quarter = 1),
  Months = tsibble::new_interval(month = 1),
  Weeks = tsibble::new_interval(week = 1),
  Days = tsibble::new_interval(day = 1),
  Hours = tsibble::new_interval(hour = 1),
  Minutes = tsibble::new_interval(minute = 1),
  Seconds = tsibble::new_interval(second = 1)
)

tab <- intervals |>
  purrr::map(common_periods) |>
  purrr::map(as.list) |>
  purrr::map_dfr(as_tibble, .id = "Data") |>
  rename_with(stringr::str_to_sentence) |>
  select(Data, Minute, Hour, Day, Week, Year) |>
  mutate_all(format, scientific = FALSE, nsmall = 2) |>
  mutate_all(~ gsub(".00", "", ., fixed = TRUE)) |>
  mutate_all(~ gsub("   NA", "", ., fixed = TRUE))
knitr::kable(tab, booktabs = TRUE, align = c("l", rep("r", ncol(tab) - 1)))

## ----fig-ansett----
melsyd_economy <- ansett |>
  filter(Airports == "MEL-SYD", Class == "Economy") |>
  mutate(Passengers = Passengers / 1000)
autoplot(melsyd_economy, Passengers) +
  labs(
    title = "Ansett airlines economy class",
    subtitle = "Melbourne-Sydney",
    y = "Passengers ('000)"
  )

## ----fig-a10plot----
autoplot(a10, Cost) +
  labs(y = "$ (millions)", title = "Australian antidiabetic drug sales")

## ----fig-fourexamples----
smallfonts <- theme(
  text = element_text(size = 9),
  axis.text = element_text(size = 8)
)
p1 <- fma::hsales |>
  as_tsibble() |>
  autoplot(value) +
  smallfonts +
  labs(y = "Houses (millions)", title = "Sales of new one-family houses, USA")
p2 <- fma::ustreas |>
  as_tsibble() |>
  autoplot(value) +
  smallfonts +
  labs(x = "Day", y = "Number", title = "US treasury bill contracts")
p3 <- aus_production |>
  autoplot(Electricity) +
  smallfonts +
  labs(
    y = "kWh (billion) ",
    title = "Australian quarterly electricity production"
  )
p4 <- gafa_stock |>
  filter(Symbol == "GOOG") |>
  autoplot(difference(Close)) +
  smallfonts +
  labs(y = "$US", title = "Daily changes in Google closing stock price")

(p1 | p2) / (p3 | p4)

## ----fig-seasonplot1code----
# a10 |>
#   gg_season(Cost, labels = "both") +
#   labs(y = "$ (millions)", title = "Seasonal plot: Antidiabetic drug sales")

## ----fig-seasonplot1----
a10 |>
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)", title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1973-12-04")))

## ----fig-multipleseasonplots1----
vic_elec |>
  gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

## ----fig-multipleseasonplots2----
vic_elec |>
  gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

## ----fig-multipleseasonplots3----
vic_elec |>
  gg_season(Demand, period = "year") +
  labs(y = "MWh", title = "Electricity demand: Victoria")

## ----fig-subseriesplot----
a10 |>
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

## ----holidays----
holidays <- tourism |>
  filter(Purpose == "Holiday") |>
  group_by(State) |>
  summarise(Trips = sum(Trips))

## ----holidaysprint----
holidays

## ----fig-holidays-plot----
autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)", title = "Australian domestic holidays")

## ----fig-holidaysseason----
gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)", title = "Australian domestic holidays") +
  theme(panel.spacing.y = unit(1, "lines"))

## ----fig-holidayssubseries----
holidays |>
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)", title = "Australian domestic holidays") +
  theme(panel.spacing.y = unit(1, "lines"))

## ----fig-edemand----
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Demand) +
  labs(y = "GW", title = "Half-hourly electricity demand: Victoria")

## ----fig-victemp----
vic_elec |>
  filter(year(Time) == 2014) |>
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

## ----fig-edemand2----
vic_elec |>
  filter(year(Time) == 2014) |>
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(
    title = "Electricity demand versus Temperature",
    x = "Temperature (degrees Celsius)",
    y = "Electricity demand (GW)"
  )

## ----fig-corr----
corplot <- function(rho) {
  library(mvtnorm)
  x <- rmvnorm(100, sigma = matrix(c(1, rho, rho, 1), 2, 2))
  ggplot(as.data.frame(x), aes(x = V1, y = V2)) +
    geom_point() +
    labs(
      x = "",
      y = "",
      title = paste("Correlation =", sprintf("%.2f", rho))
    ) +
    xlim(-3.5, 3.5) +
    ylim(-3.5, 3.5)
}
set.seed(12345)
p1 <- corplot(-0.99)
p2 <- corplot(-0.75)
p3 <- corplot(-0.5)
p4 <- corplot(-0.25)
p5 <- corplot(0.99)
p6 <- corplot(0.75)
p7 <- corplot(0.5)
p8 <- corplot(0.25)

(p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)

## ----eleccorrelation----
x <- vic_elec |> filter(year(Time) == 2014)
eleccor <- cor(x$Temperature, x$Demand)

## ----fig-anscombe----
p1 <- ggplot(anscombe, aes(x = x1, y = y1)) +
  geom_point() +
  labs(x = "x", y = "y")
p2 <- ggplot(anscombe, aes(x = x2, y = y2)) +
  geom_point() +
  labs(x = "x", y = "y")
p3 <- ggplot(anscombe, aes(x = x3, y = y3)) +
  geom_point() +
  labs(x = "x", y = "y")
p4 <- ggplot(anscombe, aes(x = x4, y = y4)) +
  geom_point() +
  labs(x = "x", y = "y")
(p1 | p2) / (p3 | p4)

## ----fig-vntimeplots----
visitors <- tourism |>
  group_by(State) |>
  summarise(Trips = sum(Trips))
visitors |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism", y = "Overnight trips ('000)")

## ----fig-ScatterMatrixch2----
visitors |>
  pivot_wider(values_from = Trips, names_from = State) |>
  GGally::ggpairs(columns = 2:9) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.spacing = unit(1, "lines")
  )

## ----fig-beerlagplot----
recent_production <- aus_production |>
  filter(year(Quarter) >= 2000)
recent_production |>
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

## ----beeracfraw----
recent_production |> ACF(Beer, lag_max = 9)

## ----fig-beeracf----
recent_production |>
  ACF(Beer) |>
  autoplot() +
  labs(title = "Australian beer production")

## ----fig-acfa10----
a10 |>
  ACF(Cost, lag_max = 48) |>
  autoplot() +
  labs(title = "Australian antidiabetic drug sales")

## ----fig-wnoise----
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y |> autoplot(wn) + labs(title = "White noise", y = "")

## ----fig-wnoiseacf----
y |>
  ACF(wn) |>
  autoplot() +
  labs(title = "White noise")

## ----acfguess----
cowtemp <- as_tsibble(fma::cowtemp + 800)
USAccDeaths <- as_tsibble(USAccDeaths)
AirPassengers <- as_tsibble(AirPassengers)
mink <- as_tsibble(fma::mink)
tp1 <- autoplot(cowtemp, value) +
  labs(x = "", y = "chirps per 5 minutes", title = "1. Daily temperature of cow")
tp2 <- autoplot(USAccDeaths, value) +
  labs(x = "", y = "thousands", title = "2. Monthly accidental deaths")
tp3 <- autoplot(AirPassengers, value) +
  labs(x = "", y = "thousands", title = "3. Monthly air passengers")
tp4 <- autoplot(mink, value) +
  labs(x = "", y = "thousands", title = "4. Annual mink trappings")
acfb <- ACF(cowtemp, value) |>
  autoplot() +
  labs(x = "", title = "B") +
  ylim(-0.45, 1)
acfa <- ACF(USAccDeaths, value) |>
  autoplot() +
  labs(x = "", title = "A") +
  ylim(-0.45, 1)
acfd <- ACF(AirPassengers, value) |>
  autoplot() +
  labs(x = "", title = "D") +
  ylim(-0.45, 1)
acfc <- ACF(mink, value) |>
  autoplot() +
  labs(x = "", title = "C") +
  ylim(-0.45, 1)
(tp1 / acfa) | (tp2 / acfb) | (tp3 / acfc) | (tp4 / acfd)
