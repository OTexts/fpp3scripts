## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----feature_mean2----
tourism |>
  features(Trips, list(mean = mean)) |>
  arrange(mean)

## ----feature_fivenum----
tourism |> features(Trips, quantile)

## ----feature_acf----
tourism |> features(Trips, feat_acf)

## ----stl-features----
tourism |>
  features(Trips, feat_stl)

## ----fig-featuresplot----
tourism |>
  features(Trips, feat_stl) |>
  ggplot(aes(x = trend_strength, y = seasonal_strength_year, col = Purpose)) +
  geom_point() +
  facet_wrap(vars(State))

## ----fig-extreme----
tourism |>
  features(Trips, feat_stl) |>
  filter(
    seasonal_strength_year == max(seasonal_strength_year)
  ) |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(vars(State, Region, Purpose))

## ----all_features----
tourism_features <- tourism |>
  features(Trips, feature_set(pkgs = "feasts"))
tourism_features

## ----fig-seasonalfeatures----
library(glue)
tourism_features |>
  select_at(vars(contains("season"), Purpose)) |>
  mutate(
    seasonal_peak_year = seasonal_peak_year +
      4 * (seasonal_peak_year == 0),
    seasonal_trough_year = seasonal_trough_year +
      4 * (seasonal_trough_year == 0),
    seasonal_peak_year = glue("Q{seasonal_peak_year}"),
    seasonal_trough_year = glue("Q{seasonal_trough_year}"),
  ) |>
  GGally::ggpairs(
    mapping = aes(colour = Purpose),
    lower = list(combo = function(data, mapping, ...) {
      GGally::ggally_facethist(data, mapping, ...) +
        scale_y_continuous(breaks = c(0, 10))
    })
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

## ----fig-pca----
library(broom)
pcs <- tourism_features |>
  select(-State, -Region, -Purpose) |>
  prcomp(scale = TRUE) |>
  augment(tourism_features)
pcs |>
  ggplot(aes(x = .fittedPC1, y = .fittedPC2, col = Purpose)) +
  geom_point() +
  theme(aspect.ratio = 1)

## ----fig-pcaoutliers----
outliers <- pcs |>
  filter(.fittedPC1 > 10) |>
  select(Region, State, Purpose, .fittedPC1, .fittedPC2)
outliers
outliers |>
  left_join(tourism, by = c("State", "Region", "Purpose"), multiple = "all") |>
  mutate(Series = glue("{State}", "{Region}", "{Purpose}", .sep = "\n\n")) |>
  ggplot(aes(x = Quarter, y = Trips)) +
  geom_line() +
  facet_grid(Series ~ ., scales = "free") +
  labs(title = "Outlying time series in PC space")
