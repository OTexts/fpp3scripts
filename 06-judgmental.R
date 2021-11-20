source("before-each-chapter.R")

## ----tfc, fig.cap="(ref:tfc)", message=FALSE, warning=FALSE, echo=FALSE, fig.asp=0.5----------------------
tfcvn <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/dat_3_TFC.csv") %>%
  as_tsibble(index = Year) %>%
  pivot_longer(c(TFC,Regression,ETS), names_to="Method", values_to="Forecast")
tfcvn %>%
  ggplot(aes(x = Year, y = Data/1000)) +
  geom_line() +
  geom_line(aes(y = Forecast/1000, colour = Method)) +
  labs(title= "Australian domestic tourism: Total ",
         y = "Visitor nights (millions)") +
  scale_colour_manual(
    name = "",
    values = c("#E69F00", "#0072B2", "#CC79A7"),
    breaks = c("TFC", "Regression", "ETS")
  )

