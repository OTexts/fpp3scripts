source("before-each-chapter.R")

## ----SLRpop1, fig.cap="An example of data from a simple linear regression model.", echo=FALSE, warning=FALSE, message=FALSE, fig.pos="t"----
set.seed(2)
x <- runif(50, 0, 4)
df <- data.frame(
  x = x,
  y = 3 + 10 * x + rnorm(50, 0, 10)
)
ggplot(df, aes(x, y)) +
  geom_point() +
  geom_abline(
    slope = 10, intercept = 3,
    col = "#D55E00", size = 0.3
  ) +
  geom_label(
    x = .3, y = 40, parse = TRUE, col = "#D55E00",
    label = " beta[0] + beta[1] * x"
  ) +
  geom_segment(
    x = .3, y = 36, xend = 0, yend = 4,
    arrow = arrow(length = unit(0.02, "npc")),
    size = 0.2, col = "#D55E00"
  ) +
  geom_label(
    x = 1.5, y = 55, parse = TRUE, col = "#0072B2",
    label = "y[t] == beta[0] + beta[1] * x[t] + epsilon[t]"
  ) +
  geom_segment(
    x = 1.5, y = 52, xend = df$x[19] - 0.03, yend = df$y[19] + 1.5,
    arrow = arrow(length = unit(0.02, "npc")),
    size = 0.2, col = "#0072B2"
  ) +
  geom_segment(
    x = df$x[19], y = df$y[19],
    xend = df$x[19], yend = 3 + 10 * df$x[19],
    col = "#009E73", size = 0.2,
    arrow = arrow(length = unit(0.02, "npc"), ends = "both")
  ) +
  geom_label(
    x = df$x[19] - 0.07,
    y = (df$y[19] + 3 + 10 * df$x[19]) / 2,
    col = "#009E73", label = "epsilon[t]",
    parse = TRUE
  )

## ----ConsInc, echo=TRUE, fig.cap="Percentage changes in personal consumption expenditure and personal income for the US.", fig.asp=0.45----
us_change %>%
  pivot_longer(c(Consumption, Income), names_to="Series") %>%
  autoplot(value) +
  labs(y = "% change")

## ----fitcons, include=FALSE-------------------------------------------------------------------------------
fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))

## ----ConsInc2, echo=TRUE, fig.cap="Scatterplot of quarterly changes in consumption expenditure  versus quarterly changes in personal income and the fitted regression line.", message=FALSE----
us_change %>%
  ggplot(aes(x = Income, y = Consumption)) +
  labs(y = "Consumption (quarterly % change)",
       x = "Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

## ----tslmcons, echo=TRUE----------------------------------------------------------------------------------
us_change %>%
  model(TSLM(Consumption ~ Income)) %>%
  report()

## ----MultiPredictors, echo=TRUE, fig.cap="Quarterly percentage changes in industrial production and personal savings and quarterly changes in the unemployment rate for the US over the period 1970Q1-2019Q2."----
us_change %>%
  select(-Consumption, -Income) %>%
  pivot_longer(-Quarter) %>%
  ggplot(aes(Quarter, value, color = name)) +
  geom_line() +
  facet_grid(name ~ ., scales = "free_y") +
  guides(colour = "none") +
  labs(y="% change")

## ----ScatterMatrix, message=FALSE, fig.cap="A scatterplot matrix of US consumption expenditure and the four predictors.", fig.asp=1, message=FALSE, fig.env="figure*"----
us_change %>%
  GGally::ggpairs(columns = 2:6)

## ----usestim, echo=TRUE, fig.cap="Multiple regression output from least squares estimation."--------------
fit_consMR <- us_change %>%
  model(tslm = TSLM(Consumption ~ Income + Production +
                                    Unemployment + Savings))
report(fit_consMR)

## ----usfitted1, echo=TRUE, fig.cap="Time plot of actual US consumption expenditure and predicted US consumption expenditure.", dependson="usestim"----
augment(fit_consMR) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Consumption, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  labs(y = NULL,
    title = "Percent change in US consumption expenditure"
  ) +
  scale_colour_manual(values=c(Data="black",Fitted="#D55E00")) +
  guides(colour = guide_legend(title = NULL))

## ----usfitted2, echo=TRUE, fig.cap="Actual US consumption expenditure plotted against predicted US consumption expenditure.", message=FALSE, warning=FALSE, dependson="usestim"----
augment(fit_consMR) %>%
  ggplot(aes(x = Consumption, y = .fitted)) +
  geom_point() +
  labs(
    y = "Fitted (predicted values)",
    x = "Data (actual values)",
    title = "Percent change in US consumption expenditure"
  ) +
  geom_abline(intercept = 0, slope = 1)

## ----corfitconsMR, echo=FALSE, dependson="usestim"--------------------------------------------------------
r <- with(augment(fit_consMR), cor(.fitted, Consumption))

## ----uschangeresidcheck, fig.cap="Analysing the residuals from a regression model for US quarterly consumption.", message=FALSE, warning=FALSE, dependson="usestim", fig.asp=0.6----
fit_consMR %>% gg_tsresiduals()

## ----uschangeresidcheck2, message=FALSE, warning=FALSE, class.output='r', dependson='usestim'-------------
augment(fit_consMR) %>%
  features(.innov, ljung_box, lag = 10, dof = 5)

## ----resids, echo=TRUE, fig.cap="Scatterplots of residuals versus each predictor.", dependson='usestim'----
us_change %>%
  left_join(residuals(fit_consMR), by = "Quarter") %>%
  pivot_longer(Income:Unemployment,
               names_to = "regressor", values_to = "x") %>%
  ggplot(aes(x = x, y = .resid)) +
  geom_point() +
  facet_wrap(. ~ regressor, scales = "free_x") +
  labs(y = "Residuals", x = "")

## ----resids2, echo=TRUE, fig.cap="Scatterplots of residuals versus fitted values.", fig.asp=0.55, dependson='usestim'----
augment(fit_consMR) %>%
  ggplot(aes(x = .fitted, y = .resid)) +
  geom_point() + labs(x = "Fitted", y = "Residuals")

## ----outlier, fig.cap="The effect of outliers and influential observations on regression", fig.asp=0.45, echo=FALSE, message=FALSE,warning=FALSE----
fit1 <- lm(Consumption ~ Income, data = us_change)

us_change_outliers <- us_change
us_change_outliers$Consumption[1] <- -4
us_change_outliers$Income[1] <- 1
fit2 <- lm(Consumption ~ Income, data = us_change_outliers)
p1 <- ggplot(us_change_outliers, aes(x = Income, y = Consumption)) +
  labs(y = "% change in consumption", x = "% change in income") +
  geom_point() +
  geom_abline(intercept = fit1$coefficients[1], slope = fit1$coefficients[2]) +
  geom_abline(intercept = fit2$coefficients[1], slope = fit2$coefficients[2], colour = "#D55E00") +
  geom_point(x = 1, y = -4, shape = 1, size = 7, col = "#0072B2")

us_change_outliers$Income[1] <- 6
fit2 <- lm(Consumption ~ Income, data = us_change_outliers)
p2 <- ggplot(us_change_outliers, aes(x = Income, y = Consumption)) +
  labs(y = "% change in consumption", x = "% change in income") +
  geom_point() +
  geom_abline(intercept = fit1$coefficients[1], slope = fit1$coefficients[2]) +
  geom_abline(intercept = fit2$coefficients[1], slope = fit2$coefficients[2], colour = "#D55E00") +
  geom_point(x = 6, y = -4, shape = 1, size = 7, col = "#0072B2")

p1 | p2

## ----spurious, echo=FALSE, fig.asp=0.5, warning=FALSE, fig.cap="Trending time series data can appear to be related, as shown in this example where air passengers in Australia are regressed against rice production in Guinea.", message=FALSE----
p1 <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  autoplot(Passengers) +
  labs(title = "Air transport: Australia",
       y="Passengers (millions)")
p2 <- guinea_rice %>%
  autoplot(Production) +
  labs(title = "Rice production: Guinea",
       y = "Metric tons (millions)")
p3 <- aus_airpassengers %>%
  left_join(guinea_rice, by = "Year") %>%
  ggplot(aes(x = Production, y = Passengers)) +
  geom_point() +
  labs(y = "Air passengers in Australia (millions)",
       x = "Rice production in Guinea (million tons)")

(p1 / p2) | p3

## ----tslmspurious, echo=TRUE, dependson="spurious"--------------------------------------------------------
fit <- aus_airpassengers %>%
  filter(Year <= 2011) %>%
  left_join(guinea_rice, by = "Year") %>%
  model(TSLM(Passengers ~ Production))
report(fit)

## ----tslmspurious2, fig.cap="Residuals from a spurious regression.", dependson='tslmspurious', class.output='r', dependson='tslmspurious'----
fit %>% gg_tsresiduals()

## ----dowdummy, echo=FALSE---------------------------------------------------------------------------------
df <- matrix("0", nrow = 13, ncol = 6)
df[1:6, ] <- paste(diag(6))
df[8:12, ] <- paste(diag(6)[1:5, ])
rownames(df) <- rep(c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 2)[1:13]
colnames(df) <- paste("$d_{", 1:6, ",t}$", sep = "")

dfrows <- 9
df <- df[seq(dfrows), ]
if (html) {
  rownames(df)[dfrows] <- df[dfrows, ] <- "&#8942;"
  knitr::kable(df)
} else {
  rownames(df)[dfrows] <- df[dfrows, ] <- "$\\vdots$"
  tab <- knitr::kable(df, booktabs = TRUE, format = "latex", escape = FALSE)
  tab <- gsub("\\\\begin", "\\\\begingroup\\\\fontsize{10}{12}\\\\selectfont\\\\begin", tab)
  gsub("\\\\end\\{tabular\\}", "\\\\end{tabular}\\\\endgroup", tab)
}

## ----beeragain, echo=TRUE, fig.cap="Australian quarterly beer production.", fig.asp=0.5-------------------
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
recent_production %>%
  autoplot(Beer) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production")

## ----fig.beerfit, echo=TRUE, dependson="beeragain"--------------------------------------------------------
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
report(fit_beer)

## ----beerlm2, echo=TRUE, fig.cap="Time plot of beer production and predicted beer production.", dependson="fig.beerfit"----
augment(fit_beer) %>%
  ggplot(aes(x = Quarter)) +
  geom_line(aes(y = Beer, colour = "Data")) +
  geom_line(aes(y = .fitted, colour = "Fitted")) +
  scale_colour_manual(
    values = c(Data = "black", Fitted = "#D55E00")
  ) +
  labs(y = "Megalitres",
       title = "Australian quarterly beer production") +
  guides(colour = guide_legend(title = "Series"))

## ----beerlm3, echo=TRUE, fig.cap="Actual beer production plotted against predicted beer production.", dependson="fig.beerfit"----
augment(fit_beer) %>%
  ggplot(aes(x = Beer, y = .fitted,
             colour = factor(quarter(Quarter)))) +
  geom_point() +
  labs(y = "Fitted", x = "Actual values",
       title = "Australian quarterly beer production") +
  geom_abline(intercept = 0, slope = 1) +
  guides(colour = guide_legend(title = "Quarter"))

## ----fourierbeer, echo=TRUE, dependson="beeragain"--------------------------------------------------------
fourier_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + fourier(K = 2)))
report(fourier_beer)

## ----CVfitconsMR, echo=TRUE, dependson='usestim'----------------------------------------------------------
glance(fit_consMR) %>%
  select(adj_r_squared, CV, AIC, AICc, BIC)

## ----tblusMR, echo=FALSE, message = FALSE-----------------------------------------------------------------
library(rlang)
library(purrr)

opts <- expand.grid(Income = 0:1, Production = 0:1, Savings = 0:1, Unemployment = 0:1) %>%
  mutate(
    formulae =
      pmap(
        list(Income = Income, Production = Production, Savings = Savings, Unemployment = Unemployment),
        function(...) {
          spec <- list(...)
          new_formula(
            sym("Consumption"),
            reduce(syms(names(spec)[spec == 1]), call2, .fn = "+", .init = 1)
          )
        }
      ),
    models = set_names(map(formulae, TSLM), map_chr(formulae, deparse))
  )

tab <- us_change %>%
  model(
    !!!opts$models
  ) %>%
  glance() %>%
  bind_cols(opts) %>%
  transmute(Income, Production, Savings, Unemployment, AdjR2 = adj_r_squared, CV, AIC, AICc, BIC) %>%
  arrange(AICc) %>%
  mutate(across(where(is.integer), function(x){if_else(x==1, ifelse(html,"⬤","$\\bullet$"), "")})) %>%
  knitr::kable(
    format = if_else(html, "html", "latex"),
    digits = 3, align = c(rep("c", 4)), booktabs = TRUE, escape=FALSE,
    caption = "All 16 possible models for forecasting US consumption with 4 predictors."
  )
if (html) {
  tab
} else {
  tab <- kableExtra::kable_styling(tab, latex_options = c("scale_down", "hold_position"))
  tab <- gsub("\\\\centering", "\\\\centering\\\\vspace*{-0.3cm}", tab)
  gsub("\\\\end\\{tabular\\}\\}", "\\\\end{tabular}}\\\\vspace{0.3cm}", tab)
}

## ----beerlm1, echo=TRUE, fig.cap="(ref:figcapbeerlm1)"----------------------------------------------------
recent_production <- aus_production %>%
  filter(year(Quarter) >= 1992)
fit_beer <- recent_production %>%
  model(TSLM(Beer ~ trend() + season()))
fc_beer <- forecast(fit_beer)
fc_beer %>%
  autoplot(recent_production) +
  labs(
    title = "Forecasts of beer production using regression",
    y = "megalitres"
  )

## ----ConsInc4a, echo=TRUE---------------------------------------------------------------------------------
fit_consBest <- us_change %>%
  model(
    lm = TSLM(Consumption ~ Income + Savings + Unemployment)
  )
future_scenarios <- scenarios(
  Increase = new_data(us_change, 4) %>%
    mutate(Income=1, Savings=0.5, Unemployment=0),
  Decrease = new_data(us_change, 4) %>%
    mutate(Income=-1, Savings=-0.5, Unemployment=0),
  names_to = "Scenario")

fc <- forecast(fit_consBest, new_data = future_scenarios)

## ----ConsInc4, echo=TRUE, fig.cap="Forecasting percentage changes in personal consumption expenditure for the US under scenario based forecasting."----
us_change %>%
  autoplot(Consumption) +
  autolayer(fc) +
  labs(title = "US consumption", y = "% change")

## ----fitconsupdown, include=FALSE-------------------------------------------------------------------------
fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))
fcast_ave <- forecast(fit_cons, new_data(us_change, 4) %>% mutate(Income = mean(us_change$Income)))
fcast_up <- forecast(fit_cons, new_data(us_change, 4) %>% mutate(Income = 5))

## ----savePI, echo=FALSE-----------------------------------------------------------------------------------
PI80 <- as.data.frame(hilo(fcast_ave$Consumption[1], 80))[1, ][[1]]
PI95 <- as.data.frame(hilo(fcast_ave$Consumption[1], 95))[1, ][[1]]

## ----conSimplePI, fig.cap="(ref:figcapconSimplePI)", echo=TRUE--------------------------------------------
fit_cons <- us_change %>%
  model(TSLM(Consumption ~ Income))
new_cons <- scenarios(
  "Average increase" = new_data(us_change, 4) %>%
    mutate(Income = mean(us_change$Income)),
  "Extreme increase" = new_data(us_change, 4) %>%
    mutate(Income = 12),
  names_to = "Scenario"
)
fcast <- forecast(fit_cons, new_cons)

us_change %>%
  autoplot(Consumption) +
  autolayer(fcast) +
  labs(title = "US consumption", y = "% change")

## ----boston, echo=TRUE------------------------------------------------------------------------------------
boston_men <- boston_marathon %>%
  filter(Year >= 1924) %>%
  filter(Event == "Men's open division") %>%
  mutate(Minutes = as.numeric(Time)/60)

## ----marathonLinear, echo=FALSE, fig.cap="Fitting a linear trend to the Boston marathon winning times is inadequate", message=FALSE, warning=FALSE, dependson='boston'----
fit_lin <- boston_men %>%
  model(TSLM(Minutes ~ trend()))
p1 <- boston_men %>%
  ggplot(aes(x = Year, y = Minutes)) +
  geom_line() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Boston marathon winning times",
       y = "Minutes")
p2 <- augment(fit_lin) %>%
  autoplot(.resid) +
  labs(title = "Residuals from a linear trend",
       y="Minutes")
p1 / p2

## ----marathonNonlinear, echo=TRUE, message=TRUE, warning=FALSE, fig.cap="Projecting forecasts from linear, exponential and piecewise linear trends for the Boston marathon winning times.", dependson='boston'----
fit_trends <- boston_men %>%
  model(
    linear = TSLM(Minutes ~ trend()),
    exponential = TSLM(log(Minutes) ~ trend()),
    piecewise = TSLM(Minutes ~ trend(knots = c(1950, 1980)))
  )
fc_trends <- fit_trends %>% forecast(h = 10)

boston_men %>%
  autoplot(Minutes) +
  geom_line(data = fitted(fit_trends),
            aes(y = .fitted, colour = .model)) +
  autolayer(fc_trends, alpha = 0.5, level = 95) +
  labs(y = "Minutes",
       title = "Boston marathon winning times")

## ----ex7.1------------------------------------------------------------------------------------------------
jan14_vic_elec <- vic_elec %>%
  filter(yearmonth(Time) == yearmonth("2014 Jan")) %>%
  index_by(Date = as_date(Time)) %>%
  summarise(
    Demand = sum(Demand),
    Temperature = max(Temperature)
  )

## ----ex7.1c, results = "hide", fig.show = "hide", message=FALSE, warning=FALSE, dependson='ex7.1'---------
jan14_vic_elec %>%
  model(TSLM(Demand ~ Temperature)) %>%
  forecast(
    new_data(jan14_vic_elec, 1) %>%
      mutate(Temperature = 15)
  ) %>%
  autoplot(jan14_vic_elec)

