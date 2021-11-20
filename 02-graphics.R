source("before-each-chapter.R")

## ----tstable, echo=FALSE----------------------------------------------------------------------------------
x <- c(123, 39, 78, 52, 110)
yr <- 2015:2019
knitr::kable(tibble(Year = yr, Observation = x), booktabs = TRUE)

## ----first-tsibble----------------------------------------------------------------------------------------
y <- tsibble(
  Year = 2015:2019,
  Observation = c(123, 39, 78, 52, 110),
  index = Year
)

## ----tstablemonth, echo=FALSE-----------------------------------------------------------------------------
z <- tibble(Month = paste(2019, month.abb[1:5]), Observation = c(50, 23, 34, 30, 25))
# knitr::kable(z, booktabs=TRUE)

## ----tstablemonth2----------------------------------------------------------------------------------------
z

## ----month-tsibble----------------------------------------------------------------------------------------
z %>%
  mutate(Month = yearmonth(Month)) %>%
  as_tsibble(index = Month)

## ----tstable2, echo=FALSE, results=ifelse(html, 'markup', 'asis')-----------------------------------------
tab <- tribble(
    ~`Frequency`, ~Function,
    "Annual", "`start:end`",
    "Quarterly", "`yearquarter()`",
    "Monthly", "`yearmonth()`",
    "Weekly", "`yearweek()`",
    "Daily", "`as_date()`, `ymd()`",
    "Sub-daily", "`as_datetime()`, `ymd_hms()`"
  )
if(!html) {
  tab <- tab %>%
    mutate(
      Function = stringr::str_replace(Function, "`","\\\\texttt{"),
      Function = stringr::str_replace(Function, "`, `","}, \\\\texttt{"),
      Function = stringr::str_replace(Function, "`","}"),
      Function = stringr::str_replace_all(Function,"_","\\\\_")
    )
}
tab %>% knitr::kable(booktabs = TRUE, escape=html)

## ----tstablekey-------------------------------------------------------------------------------------------
olympic_running

## ----distinctfn-------------------------------------------------------------------------------------------
olympic_running %>% distinct(Sex)

## ----pbs1-------------------------------------------------------------------------------------------------
PBS

## ----pbs2-------------------------------------------------------------------------------------------------
PBS %>%
  filter(ATC2 == "A10")

## ----pbs3-------------------------------------------------------------------------------------------------
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost)

## ----pbs4-------------------------------------------------------------------------------------------------
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost))

## ----pbs5-------------------------------------------------------------------------------------------------
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC/1e6)

## ----a10--------------------------------------------------------------------------------------------------
PBS %>%
  filter(ATC2 == "A10") %>%
  select(Month, Concession, Type, Cost) %>%
  summarise(TotalC = sum(Cost)) %>%
  mutate(Cost = TotalC / 1e6) -> a10

## ----prison, echo=FALSE, warning=FALSE, message=FALSE, eval=TRUE------------------------------------------
prison <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv")
prison %>%
  head(10) %>%
  knitr::kable(booktabs = TRUE)

## ----prison2, dependson='prison'--------------------------------------------------------------------------
prison <- prison %>%
  mutate(Quarter = yearquarter(Date)) %>%
  select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

prison

## ----freqtable, echo=FALSE, message=FALSE-----------------------------------------------------------------
intervals <- list(
  Quarters = tsibble::new_interval(quarter = 1),
  Months = tsibble::new_interval(month = 1),
  Weeks = tsibble::new_interval(week = 1),
  Days = tsibble::new_interval(day = 1),
  Hours = tsibble::new_interval(hour = 1),
  Minutes = tsibble::new_interval(minute = 1),
  Seconds = tsibble::new_interval(second = 1)
)

intervals %>%
  purrr::map(common_periods) %>%
  purrr::map(as.list) %>%
  purrr::map_dfr(as_tibble, .id = "Data") %>%
  purrr::set_names(., stringr::str_to_sentence(colnames(.))) %>%
  select(Data, Minute, Hour, Day, Week, Year) %>%
  mutate_all(format, scientific = FALSE, nsmall = 2) %>%
  mutate_all(~ gsub(".00", "", ., fixed = TRUE)) %>%
  mutate_all(~ gsub("   NA", "", ., fixed = TRUE)) %>%
  knitr::kable(booktabs = TRUE)

## ----ansett, fig.cap="Weekly economy passenger load on Ansett Airlines."----------------------------------
melsyd_economy <- ansett %>%
  filter(Airports == "MEL-SYD", Class == "Economy") %>%
  mutate(Passengers = Passengers/1000)
autoplot(melsyd_economy, Passengers) +
  labs(title = "Ansett airlines economy class",
       subtitle = "Melbourne-Sydney",
       y = "Passengers ('000)")

## ----a10plot, fig.cap="Monthly sales of antidiabetic drugs in Australia.", dependson='a10'----------------
autoplot(a10, Cost) +
  labs(y = "$ (millions)",
       title = "Australian antidiabetic drug sales")

## ----fourexamples, echo=FALSE, fig.cap="Four examples of time series showing different patterns.", fig.env="figure*", warning = FALSE, message=FALSE----
smallfonts <- theme(
  text = element_text(size = 9),
  axis.text = element_text(size = 8)
)
p1 <- fma::hsales %>%
  as_tsibble() %>%
  autoplot(value) + smallfonts +
  labs(y = "Houses (millions)", title = "Sales of new one-family houses, USA")
p2 <- fma::ustreas %>%
  as_tsibble() %>%
  autoplot(value) + smallfonts +
  labs(x = "Day", y = "Number", title = "US treasury bill contracts")
p3 <- aus_production %>%
  autoplot(Electricity) + smallfonts +
  labs(y = "kWh (billion) ", title = "Australian quarterly electricity production")
p4 <- gafa_stock %>%
  filter(Symbol == "GOOG") %>%
  autoplot(difference(Close)) + smallfonts +
  labs(y = "$US", title = "Daily changes in Google closing stock price")

(p1 | p2) / (p3 | p4)

## ----seasonplot1, fig.cap="Seasonal plot of monthly antidiabetic drug sales in Australia.", dependson='a10', warning=FALSE, echo=FALSE----
a10 %>%
  gg_season(Cost, labels = "both") +
  labs(y = "$ (millions)",
       title = "Seasonal plot: Antidiabetic drug sales") +
  expand_limits(x = ymd(c("1972-12-28", "1973-12-04")))

## ----multipleseasonplots1, warning=FALSE, fig.cap="Seasonal plot showing daily seasonal patterns for Victorian electricity demand.", fig.asp=0.6----
vic_elec %>% gg_season(Demand, period = "day") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

## ----multipleseasonplots2, warning=FALSE, fig.cap="Seasonal plot showing weekly seasonal patterns for Victorian electricity demand.", fig.asp=0.6----
vic_elec %>% gg_season(Demand, period = "week") +
  theme(legend.position = "none") +
  labs(y="MWh", title="Electricity demand: Victoria")

## ----multipleseasonplots3, warning=FALSE, fig.cap="Seasonal plot showing yearly seasonal patterns for Victorian electricity demand.", fig.asp=0.6----
vic_elec %>% gg_season(Demand, period = "year") +
  labs(y="MWh", title="Electricity demand: Victoria")

## ----subseriesplot, fig.cap="Seasonal subseries plot of monthly antidiabetic drug sales in Australia.", dependson='a10', fig.height=3, fig.width=8, fig.asp=0.375, warning=FALSE----
a10 %>%
  gg_subseries(Cost) +
  labs(
    y = "$ (millions)",
    title = "Australian antidiabetic drug sales"
  )

## ----holidays---------------------------------------------------------------------------------------------
holidays <- tourism %>%
  filter(Purpose == "Holiday") %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))

## ----holidaysprint----------------------------------------------------------------------------------------
holidays

## ----holidays-plot, echo=TRUE, dependson="holidays", fig.height=3.9, fig.asp=0.5, fig.cap="Time plots of Australian domestic holidays by state."----
autoplot(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

## ----holidaysseason, fig.height=9, fig.asp=1.3, fig.cap="Season plots of Australian domestic holidays by state.", warning=FALSE----
gg_season(holidays, Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

## ----holidayssubseries, fig.height=10, fig.width=8, fig.asp=1.3, fig.cap="Subseries plots of Australian domestic holidays by state.", warning=FALSE----
holidays %>%
  gg_subseries(Trips) +
  labs(y = "Overnight trips ('000)",
       title = "Australian domestic holidays")

## ----edemand, fig.cap="Half hourly electricity demand in Victoria, Australia, for 2014.", fig.height=2.5, fig.asp=0.45----
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Demand) +
  labs(y = "GW",
       title = "Half-hourly electricity demand: Victoria")

## ----victemp, fig.cap="Half hourly temperature in Melbourne, Australia, for 2014.", fig.height=2.5, fig.asp=0.45----
vic_elec %>%
  filter(year(Time) == 2014) %>%
  autoplot(Temperature) +
  labs(
    y = "Degrees Celsius",
    title = "Half-hourly temperatures: Melbourne, Australia"
  )

## ----edemand2, fig.cap="Half-hourly electricity demand plotted against temperature for 2014 in Victoria, Australia."----
vic_elec %>%
  filter(year(Time) == 2014) %>%
  ggplot(aes(x = Temperature, y = Demand)) +
  geom_point() +
  labs(x = "Temperature (degrees Celsius)",
       y = "Electricity demand (GW)")

## ----corr, fig.cap="Examples of data sets with different levels of correlation.", echo=FALSE, warning=FALSE, message=FALSE, fig.width=10, fig.height=4.5, fig.asp=0.55----
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

## ----eleccorrelation, include=FALSE-----------------------------------------------------------------------
x <- vic_elec %>% filter(year(Time) == 2014)
eleccor <- cor(x$Temperature, x$Demand)

## ----anscombe, fig.cap="(ref:anscombe)", echo=FALSE, fig.asp=1, out.width="55%", fig.height=4,fig.width=4----
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

## ----vntimeplots, fig.cap="Quarterly visitor nights for the states and territories of Australia.", fig.asp=1.3, out.width="100%"----
visitors <- tourism %>%
  group_by(State) %>%
  summarise(Trips = sum(Trips))
visitors %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State), scales = "free_y") +
  labs(title = "Australian domestic tourism",
       y= "Overnight trips ('000)")

## ----ScatterMatrixch2, fig.cap="A scatterplot matrix of the quarterly visitor nights in the states and territories of Australia.", fig.asp=1, fig.height=10, fig.width=10, out.width="100%", message=FALSE, fig.env="figure*"----
visitors %>%
  pivot_wider(values_from=Trips, names_from=State) %>%
  GGally::ggpairs(columns = 2:9)

## ----beerlagplot, fig.cap="Lagged scatterplots for quarterly beer production.", fig.asp=1-----------------
recent_production <- aus_production %>%
  filter(year(Quarter) >= 2000)
recent_production %>%
  gg_lag(Beer, geom = "point") +
  labs(x = "lag(Beer, k)")

## ----beeracfraw, dependson='beerlagplot'------------------------------------------------------------------
recent_production %>% ACF(Beer, lag_max = 9)

## ----beeracf, fig.cap="Autocorrelation function of quarterly beer production.", fig.asp=0.3, dependson="beerlagplot"----
recent_production %>%
  ACF(Beer) %>%
  autoplot() + labs(title="Australian beer production")

## ----acfa10, echo=TRUE, fig.cap="ACF of monthly Australian antidiabetic drug sales.", fig.asp=0.3, dependson="aelec"----
a10 %>%
  ACF(Cost, lag_max = 48) %>%
  autoplot() +
  labs(title="Australian antidiabetic drug sales")

## ----wnoise, fig.cap="A white noise time series.", fig.asp=0.5--------------------------------------------
set.seed(30)
y <- tsibble(sample = 1:50, wn = rnorm(50), index = sample)
y %>% autoplot(wn) + labs(title = "White noise", y = "")

## ----wnoiseacf, fig.cap="Autocorrelation function for the white noise series.", fig.asp=0.3, dependson="wnoise"----
y %>%
  ACF(wn) %>%
  autoplot() + labs(title = "White noise")

## ----acfguess, fig.asp=0.45, fig.width=10, echo=FALSE, message=FALSE, warning=FALSE, out.width="135%"-----
cowtemp <- as_tsibble(fma::cowtemp)
USAccDeaths <- as_tsibble(USAccDeaths)
AirPassengers <- as_tsibble(AirPassengers)
mink <- as_tsibble(fma::mink)
tp1 <- autoplot(cowtemp, value) +
  labs(x = "", y = "chirps per minute", title = "1. Daily temperature of cow")
tp2 <- autoplot(USAccDeaths, value) +
  labs(x = "", y = "thousands", title = "2. Monthly accidental deaths")
tp3 <- autoplot(AirPassengers, value) +
  labs(x = "", y = "thousands", title = "3. Monthly air passengers")
tp4 <- autoplot(mink, value) +
  labs(x = "", y = "thousands", title = "4. Annual mink trappings")
acfb <- ACF(cowtemp, value) %>%
  autoplot() +
  labs(x = "", title = "B") +
  ylim(-0.45, 1)
acfa <- ACF(USAccDeaths, value) %>%
  autoplot() +
  labs(x = "", title = "A") +
  ylim(-0.45, 1)
acfd <- ACF(AirPassengers, value) %>%
  autoplot() +
  labs(x = "", title = "D") +
  ylim(-0.45, 1)
acfc <- ACF(mink, value) %>%
  autoplot() +
  labs(x = "", title = "C") +
  ylim(-0.45, 1)
(tp1 / acfa) | (tp2 / acfb) | (tp3 / acfc) | (tp4 / acfd)

