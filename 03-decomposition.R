## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----fig-gdp-per-capita----
global_economy |>
  filter(Country == "Australia") |>
  autoplot(GDP / Population) +
  labs(title = "GDP per capita", y = "$US")

## ----printretail0----
print_retail <- aus_retail |>
  filter(Industry == "Newspaper and book retailing") |>
  group_by(Industry) |>
  index_by(Year = year(Month)) |>
  summarise(Turnover = sum(Turnover))
aus_economy <- global_economy |>
  filter(Code == "AUS")

## ----fig-printretail----
print_retail |>
  left_join(aus_economy, by = "Year") |>
  mutate(Adjusted_turnover = Turnover / CPI * 100) |>
  pivot_longer(c(Turnover, Adjusted_turnover), values_to = "Turnover") |>
  mutate(name = factor(name, levels = c("Turnover", "Adjusted_turnover"))) |>
  ggplot(aes(x = Year, y = Turnover)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  labs(title = "Turnover: Australian print media industry", y = "$AU")

## ----fig-BoxCoxlambda----
lambda <- aus_production |>
  features(Gas, features = guerrero) |>
  pull(lambda_guerrero)
aus_production |>
  autoplot(box_cox(Gas, lambda)) +
  labs(
    y = "",
    title = TeX(paste0(
      "Transformed gas production with $\\lambda$ = ",
      round(lambda, 2)
    ))
  )

## ----fig-usretailemployment----
us_retail_employment <- us_employment |>
  filter(year(Month) >= 1990, Title == "Retail Trade") |>
  select(-Series_ID)
autoplot(us_retail_employment, Employed) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

## ----empl-stl1----
dcmp <- us_retail_employment |>
  model(stl = STL(Employed))
components(dcmp)

## ----fig-empltrend----
components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = trend), colour = "#D55E00") +
  labs(
    y = "Persons (thousands)",
    title = "Total employment in US retail"
  )

## ----fig-emplstl----
components(dcmp) |> autoplot()

## ----fig-empl-retail-sa----
components(dcmp) |>
  as_tsibble() |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = "#0072B2") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

## ----fig-aus-exports----
global_economy |>
  filter(Country == "Australia") |>
  autoplot(Exports) +
  labs(y = "% of GDP", title = "Total Australian exports")

## ----tbl-aus-exports-tbl----
options(knitr.kable.NA = "")

aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  select(Exports) |>
  mutate(
    `5-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 2L,
      .after = 2L,
      .complete = TRUE
    )
  ) |>
  as_tibble() |>
  select(Year, Exports, `5-MA`)
out <- bind_rows(
  head(aus_exports, 8),
  tail(aus_exports, 8)
) |>
  mutate(
    Year = as.integer(Year),
    Exports = format(Exports, digits = 4) |> stringr::str_replace("   NA", ""),
    `5-MA` = format(`5-MA`, digits = 4) |> stringr::str_replace("   NA", "")
  )
out <- rbind(
  out[1:8, ],
  rep("...", 3),
  out[9:16, ]
) |>
  knitr::kable(
    booktabs = TRUE,
    format.args = list(digits = 6, trim = FALSE),
    caption = "Annual Australian exports of goods and services: 1960--2017."
  )
if (!html) {
  out <- kableExtra::kable_styling(out, latex_options = "hold_position")
}
out

## ----aus-exports-code----
aus_exports <- global_economy |>
  filter(Country == "Australia") |>
  mutate(
    `5-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 2,
      .after = 2,
      .complete = TRUE
    )
  )

## ----fig-aus-exports-plot----
aus_exports |>
  autoplot(Exports) +
  geom_line(aes(y = `5-MA`), colour = "#D55E00") +
  labs(y = "% of GDP", title = "Total Australian exports")

## ----fig-aus-exports-compare----
global_economy |>
  filter(Country == "Australia") |>
  transmute(
    Exports = Exports,
    `3-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 1,
      .after = 1,
      .complete = TRUE
    ),
    `5-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 2,
      .after = 2,
      .complete = TRUE
    ),
    `7-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 3,
      .after = 3,
      .complete = TRUE
    ),
    `9-MA` = slider::slide_dbl(
      Exports,
      mean,
      .before = 4,
      .after = 4,
      .complete = TRUE
    ),
  ) |>
  pivot_longer(-c(Year, Exports)) |>
  ggplot(aes(x = Year, y = Exports)) +
  geom_line() +
  geom_line(aes(y = value), colour = "#D55E00") +
  facet_wrap(name ~ .) +
  labs(y = "% of GDP", title = "Total Australian exports")

## ----beerma----
beer <- aus_production |>
  filter(year(Quarter) >= 1992) |>
  select(Quarter, Beer)
beer_ma <- beer |>
  mutate(
    `4-MA` = slider::slide_dbl(
      Beer,
      mean,
      .before = 1,
      .after = 2,
      .complete = TRUE
    ),
    `2x4-MA` = slider::slide_dbl(
      `4-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )

## ----tbl-matable----
format_num <- function(x) ifelse(is.na(x), "", format(x, nsmall = 2))
options(knitr.kable.NA = "")
out <- bind_rows(
  head(beer_ma, 6),
  tail(beer_ma, 6)
) |>
  mutate_if(is.numeric, format_num) |>
  as_tibble() |>
  mutate(Quarter = as.character(Quarter))
out <- rbind(
  out[1:6, ],
  rep("...", 4),
  out[7:12, ]
) |>
  knitr::kable(
    booktabs = TRUE,
    caption = "A moving average of order 4 applied to the quarterly beer data, followed by a moving average of order 2."
  )
if (!html) {
  out <- kableExtra::kable_styling(out, latex_options = "hold_position")
}
out

## ----fig-empl-MA----
us_retail_employment_ma <- us_retail_employment |>
  mutate(
    `12-MA` = slider::slide_dbl(
      Employed,
      mean,
      .before = 5,
      .after = 6,
      .complete = TRUE
    ),
    `2x12-MA` = slider::slide_dbl(
      `12-MA`,
      mean,
      .before = 1,
      .after = 0,
      .complete = TRUE
    )
  )
us_retail_employment_ma |>
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = "#D55E00") +
  labs(y = "Persons (thousands)", title = "Total employment in US retail")

## ----fig-classical-empl----
us_retail_employment |>
  model(classical_decomposition(Employed, type = "additive")) |>
  components() |>
  autoplot() +
  labs(title = "Classical additive decomposition of total US retail employment")

## ----fig-x11----
x11_dcmp <- us_retail_employment |>
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) |>
  components()
autoplot(x11_dcmp) +
  labs(title = "Decomposition of total US retail employment using X-11.")

## ----fig-x11-seasadj----
x11_dcmp |>
  ggplot(aes(x = Month)) +
  geom_line(aes(y = Employed, colour = "Data")) +
  geom_line(aes(y = season_adjust, colour = "Seasonally Adjusted")) +
  geom_line(aes(y = trend, colour = "Trend")) +
  labs(y = "Persons (thousands)", title = "Total employment in US retail") +
  scale_colour_manual(
    values = c("gray", "#0072B2", "#D55E00"),
    breaks = c("Data", "Seasonally Adjusted", "Trend")
  )

## ----fig-print-media3----
x11_dcmp |>
  gg_subseries(seasonal) +
  scale_x_yearmonth(
    breaks = yearmonth(c("2000 Jan", "2010 Jan")),
    date_labels = "%Y"
  )

## ----fig-seats----
seats_dcmp <- us_retail_employment |>
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) |>
  components()
autoplot(seats_dcmp) +
  labs(title = "Decomposition of total US retail employment using SEATS")

## ----fig-empl-stl2----
us_retail_employment |>
  model(
    STL(
      Employed ~ trend(window = 7) +
        season(window = "periodic"),
      robust = TRUE
    )
  ) |>
  components() |>
  autoplot()

## ----fig-labour----
dcmp <- as_tsibble(fma::labour) |>
  model(stl = STL(value ~ season(window = 11), robust = TRUE))
components(dcmp) |> autoplot()

## ----fig-labour2----
components(dcmp) |>
  gg_subseries(season_year) +
  scale_x_yearmonth(
    breaks = yearmonth(c("1980 Jan", "1990 Jan")),
    date_labels = "%Y"
  )
