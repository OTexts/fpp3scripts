## ---------------------------------------------------------------
source("before-each-chapter.R")

## ----fig-beer----
ausbeer2 <- aus_production |>
  filter(year(Quarter) >= 2000)
ausbeer2 |>
  model(ETS(Beer)) |>
  forecast() |>
  autoplot(ausbeer2) +
  labs(y = "Megalitres", title = "Australian beer production")

## ----fig-austa1----
austa <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/austa.csv") |>
  as_tsibble(index = Year)
sim <- austa |>
  model(ETS()) |>
  generate(h = 10, times = 10) |>
  mutate(
    replicate = factor(.rep, levels = 1:10, labels = paste("Future", 1:10))
  )
p1 <- ggplot(austa, aes(x = Year)) +
  geom_line(aes(y = Visitors, colour = "Data")) +
  geom_line(aes(y = .sim, colour = replicate), data = sim) +
  labs(
    y = "Visitors (millions)",
    title = "Total international arrivals to Australia"
  ) +
  scale_colour_manual(
    values = c("#000000", rainbow(10)),
    breaks = c("Data", paste("Future", 1:10)),
    name = " "
  )
p2 <- austa |>
  model(ETS = ETS(Visitors)) |>
  forecast(h = "10 years") |>
  autoplot(austa) +
  labs(
    y = "Visitors (millions)",
    title = "Forecasts of total international arrivals to Australia"
  )
aligned_plots <- patchwork::align_plots(p1, p2)
aligned_plots[[1]]

## ----fig-austa2----
aligned_plots[[2]]
