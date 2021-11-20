source("before-each-chapter.R")

## ----feature_mean2----------------------------------------------------------------------------------------
tourism %>%
  features(Trips, list(mean = mean)) %>%
  arrange(mean)

## ----feature_fivenum--------------------------------------------------------------------------------------
tourism %>% features(Trips, quantile)

## ----feature_acf------------------------------------------------------------------------------------------
tourism %>% features(Trips, feat_acf)

## ----stl-features, echo = TRUE----------------------------------------------------------------------------
tourism %>%
  features(Trips, feat_stl)

## ----featuresplot, fig.height=4.6, fig.cap="Seasonal strength vs trend strength for all tourism series.", fig.env="figure*"----
tourism %>%
  features(Trips, feat_stl) %>%
  ggplot(aes(x = trend_strength, y = seasonal_strength_year,
             col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

## ----extreme, fig.height=3, fig.asp=0.45, fig.cap="The most seasonal series in the Australian tourism data."----
tourism %>%
  features(Trips, feat_stl) %>%
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

## ----all_features, warning=FALSE--------------------------------------------------------------------------
tourism_features <- tourism %>%
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

## ----seasonalfeatures, fig.cap="Pairwise plots of all the seasonal features for the Australian tourism data", message=FALSE, fig.width=12, fig.height=12, fig.asp=1, fig.env = 'figure*'----
library(glue)
tourism_features %>%
  select_at(vars(contains("season"), Purpose)) %>%
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4*(seasonal_peak_year==0),
    seasonal_trough_year = seasonal_trough_year +
      4*(seasonal_trough_year==0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) %>%
  GGally::ggpairs(mapping = aes(colour = Purpose))

## ----pca, fig.cap="(ref:pca)", out.width="70%", fig.width=4, fig.height=4, fig.asp=1----------------------
library(broom)
pcs <- tourism_features %>%
  select(-State, -Region, -Purpose) %>%
  prcomp(scale = TRUE) %>%
  augment(tourism_features)
pcs %>%
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

## ----pcaoutliers, dependson='pca', fig.asp=1, fig.cap="Four anomalous time series from the Australian tourism data."----
outliers <- pcs %>%
  filter(.fittedPC1 > 10) %>%
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers
outliers %>%
  left_join(tourism, by = c("State", "Region", "Purpose")) %>%
  mutate(
    Series = glue("{State}", "{Region}", "{Purpose}",
                  .sep = "\n\n")
  ) %>%
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")

