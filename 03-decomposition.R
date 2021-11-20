source("before-each-chapter.R")

## ----gdp-per-capita, fig.cap = "Australian GDP per-capita.", fig.asp=0.45---------------------------------
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(GDP/Population) +
  labs(title= "GDP per capita", y = "$US")

## ----printretail0, message=FALSE, warning=FALSE-----------------------------------------------------------
print_retail <- aus_retail %>%
  filter(Industry == "Newspaper and book retailing") %>%
  group_by(Industry) %>%
  index_by(Year = year(Month)) %>%
  summarise(Turnover = sum(Turnover))
aus_economy <- global_economy %>%
  filter(Code == "AUS")

## ----printretail, message=FALSE, warning=FALSE, fig.cap='Turnover for the Australian print media industry in Australian dollars. The "Adjusted" turnover has been adjusted for inflation using the CPI.'----
print_retail %>%
  left_join(aus_economy, by = "Year") %>%
  mutate(Adjusted_turnover = Turnover / CPI * 100) %>%
  pivot_longer(c(Turnover, Adjusted_turnover),
               values_to = "Turnover") %>%
  mutate(name = factor(name,
         levels=c("Turnover","Adjusted_turnover"))) %>%
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry",
       y = "$AU")

## ----BoxCoxlambda, echo=TRUE, fig.asp=0.5, fig.cap="Transformed Australian quarterly gas production with the $\\lambda$ parameter chosen using the Guerrero method."----
lambda <- aus_production %>%
  features(Gas, features = guerrero) %>%
  pull(lambda_guerrero)
aus_production %>%
  autoplot(box_cox(Gas, lambda)) +
  labs(y = "",
       title = latex2exp::TeX(paste0(
         "Transformed gas production with $\\lambda$ = ",
         round(lambda,2))))

## ----usretailemployment, fig.cap="Total number of persons employed in US retail.", fig.asp=0.5------------
us_retail_employment <- us_employment %>%
  filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## ----empl-stl1, echo=TRUE, dependson='us_retail_employment'-----------------------------------------------
dcmp <- us_retail_employment %>%
  model(stl = STL(Employed))
components(dcmp)

## ----empltrend, fig.cap="Total number of persons employed in US retail: the trend-cycle component (orange) and the raw data (grey).", fig.asp=0.5----
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour="gray") +
  geom_line(aes(y=trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

## ----emplstl, fig.cap="The total number of persons employed in US retail (top) and its three additive components.", fig.asp=0.9----
components(dcmp) %>% autoplot()

## ----empl-retail-sa, fig.cap="Seasonally adjusted retail employment data (blue) and the original data (grey).", fig.asp=0.5----
components(dcmp) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## ----aus-exports, fig.cap="Australian exports of goods and services: 1960--2017.", echo=TRUE, fig.asp=0.5----
global_economy %>%
  filter(Country == "Australia") %>%
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

## ----aus-exports-tbl, echo=FALSE--------------------------------------------------------------------------
options(knitr.kable.NA = "")

aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  select(Exports) %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean, .before = 2L, .after = 2L, .complete = TRUE)
  ) %>%
  as_tibble() %>%
  select(Year, Exports, `5-MA`)
out <- bind_rows(
  head(aus_exports, 8),
  tail(aus_exports, 8)
) %>%
  mutate(
    Year = as.integer(Year),
    Exports = format(Exports, digits = 4) %>% stringr::str_replace("   NA", ""),
    `5-MA` = format(`5-MA`, digits = 4) %>% stringr::str_replace("   NA", "")
  )
out <- rbind(
  out[1:8, ],
  rep("...", 3),
  out[9:16, ]
) %>% knitr::kable(
  booktabs = TRUE, format.args = list(digits = 6, trim = FALSE),
  caption = "Annual Australian exports of goods and services: 1960--2017."
)
if (!html) {
  out <- kableExtra::kable_styling(out, latex_options = "hold_position")
}
out

## ----aus-exports-code-------------------------------------------------------------------------------------
aus_exports <- global_economy %>%
  filter(Country == "Australia") %>%
  mutate(
    `5-MA` = slider::slide_dbl(Exports, mean,
                .before = 2, .after = 2, .complete = TRUE)
  )

## ----aus-exports-plot, fig.cap="Australian exports (black) along with the 5-MA estimate of the trend-cycle (orange).", echo=TRUE, warning=FALSE,message=FALSE, fig.asp=0.5----
aus_exports %>%
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP",
       title = "Total Australian exports") +
  guides(colour = guide_legend(title = "series"))

## ----aus-exports-compare, fig.cap="Different moving averages applied to the Australian exports data.", echo=FALSE, warning=FALSE,message=FALSE----
global_economy %>%
  filter(Country == "Australia") %>%
  transmute(
    Exports = Exports,
    `3-MA` = slider::slide_dbl(Exports, mean, .before = 1, .after = 1, .complete = TRUE),
    `5-MA` = slider::slide_dbl(Exports, mean, .before = 2, .after = 2, .complete = TRUE),
    `7-MA` = slider::slide_dbl(Exports, mean, .before = 3, .after = 3, .complete = TRUE),
    `9-MA` = slider::slide_dbl(Exports, mean, .before = 4, .after = 4, .complete = TRUE),
  ) %>%
  pivot_longer(-c(Year, Exports)) %>%
  ggplot(aes(x = Year, y = Exports)) +
  geom_line() +
  geom_line(aes(y = value), colour = "#D55E00") +
  facet_wrap(name ~ .) +
  labs(y="% of GDP",
       title = "Total Australian exports")

## ----beerma, echo=TRUE------------------------------------------------------------------------------------
beer <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)
beer_ma <- beer %>%
  mutate(
    `4-MA` = slider::slide_dbl(Beer, mean,
                .before = 1, .after = 2, .complete = TRUE),
    `2x4-MA` = slider::slide_dbl(`4-MA`, mean,
                .before = 1, .after = 0, .complete = TRUE)
  )

## ----matable, results='asis', echo=FALSE------------------------------------------------------------------
format_num <- function(x) ifelse(is.na(x), "", format(x, nsmall = 2))
options(knitr.kable.NA = "")
out <- bind_rows(
  head(beer_ma, 6),
  tail(beer_ma, 6)
) %>%
  mutate_if(is.numeric, format_num) %>%
  as_tibble() %>%
  mutate(Quarter = as.character(Quarter))
out <- rbind(
  out[1:6, ],
  rep("...", 4),
  out[7:12, ]
) %>% knitr::kable(
  booktabs = TRUE,
  caption = "A moving average of order 4 applied to the quarterly beer data, followed by a moving average of order 2."
)
if (!html) {
  out <- kableExtra::kable_styling(out, latex_options = "hold_position")
}
out

## ----empl-MA, fig.cap="A 2x12-MA applied to the US retail employment series.", echo=TRUE, warning=FALSE, dependson='us_retail_employment', fig.asp=0.5----
us_retail_employment_ma <- us_retail_employment %>%
  mutate(
    `12-MA` = slider::slide_dbl(Employed, mean,
                .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                .before = 1, .after = 0, .complete = TRUE)
  )
us_retail_employment_ma %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail")

## ----classical-empl, warning=FALSE, echo=FALSE, fig.asp=0.9, fig.cap="A classical additive decomposition of US retail employment.", dependson='us_retail_employment'----
us_retail_employment %>%
  model(classical_decomposition(Employed, type = "additive")) %>%
  components() %>%
  autoplot() +
  labs(title = "Classical additive decomposition of total US retail employment")

## ----x11, echo=TRUE, warning=FALSE, fig.asp=0.85, fig.cap="A multiplicative decomposition of US retail employment using X-11.", dependson='us_retail_employment'----
x11_dcmp <- us_retail_employment %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()
autoplot(x11_dcmp) +
  labs(title =
    "Decomposition of total US retail employment using X-11.")

## ----x11-seasadj, echo=TRUE, dependson='x11',warning=FALSE, fig.asp=0.45, fig.cap="US retail employment: the original data (grey), the trend-cycle component (orange) and the seasonally adjusted data (barely visible in blue)."----
x11_dcmp %>%
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust,
                colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)",
       title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

## ----print-media3, dependson='x11',fig.cap="Seasonal sub-series plot of the seasonal component from the X-11 method applied to total US retail employment.", echo=TRUE, fig.asp=0.5----
x11_dcmp %>%
  gg_subseries(seasonal)

## ----seasonal-dep, include=FALSE--------------------------------------------------------------------------
# Declare seasonal suggest as renv dependency
library(seasonal)

## ----seats, eval=TRUE, echo=TRUE, warning=FALSE, fig.asp=0.9, fig.cap="A  decomposition of US retail employment obtained using SEATS.", dependson='us_retail_employment'----
seats_dcmp <- us_retail_employment %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()
autoplot(seats_dcmp) +
  labs(title =
    "Decomposition of total US retail employment using SEATS")

## ----empl-stl2, fig.cap="Total US retail employment (top) and its three additive components obtained from a robust STL decomposition with flexible trend-cycle and fixed seasonality.",fig.asp=0.85, echo=TRUE----
us_retail_employment %>%
  model(
    STL(Employed ~ trend(window = 7) +
                   season(window = "periodic"),
    robust = TRUE)) %>%
  components() %>%
  autoplot()

## ----labour, echo=FALSE, fig.cap="Decomposition of the number of persons in the civilian labour force in Australia each month from February 1978 to August 1995.", fig.asp=0.9, message=FALSE----
dcmp <- as_tsibble(fma::labour) %>%
  model(stl = STL(value ~ season(window = 11), robust = TRUE))
components(dcmp) %>% autoplot()

## ----labour2, echo=FALSE, fig.cap="Seasonal component from the decomposition shown in the previous figure.", dependson="labour"----
components(dcmp) %>%
  gg_subseries(season_year)

