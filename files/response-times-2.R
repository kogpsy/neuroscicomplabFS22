
## ------------------------------------------------
library(tidyverse)
library(rtdists)
library(viridis)

data(speed_acc)

speed_acc <- speed_acc |>
  as_tibble()


df_speed_acc <- speed_acc |>
   # zwischen 180 ms and 3000 ms
  filter(rt > 0.18, rt < 3) |>
   # zu Character konvertieren (damit filter funktioniert)
  mutate(across(c(stim_cat, response), as.character)) |>
  # Korrekte Antworten
  filter(response != 'error', stim_cat == response) |>
  # wieder zu Factor konvertieren
  mutate(across(c(stim_cat, response), as_factor))


## ------------------------------------------------
df_speed_acc


## ------------------------------------------------
data_plot <- df_speed_acc |>
  filter(id %in% c(1, 8, 11, 15))

data_plot |>
  ggplot(aes(x = rt)) +
    geom_histogram(aes(fill = condition), alpha = 0.5, bins = 60) +
    facet_wrap(~id) +
    coord_cartesian(xlim=c(0, 1.6)) +
    scale_fill_viridis(discrete = TRUE, option = "E")


## ------------------------------------------------
out <- rogme::hsf(df_speed_acc, rt ~ condition + id)


## ------------------------------------------------
rogme::plot_hsf(out)


## ------------------------------------------------
out_pb <- rogme::hsf_pb(df_speed_acc, rt ~ condition + id)


## ------------------------------------------------
rogme::plot_hsf_pb(out_pb, interv = "ci")


## ------------------------------------------------
by_subject <- df_speed_acc |>
  group_by(id, condition) |>
  summarise(mean = median(rt))

agg <- Rmisc::summarySEwithin(by_subject,
                       measurevar = "mean",
                       withinvars = "condition",
                       idvar = "id",
                       na.rm = FALSE,
                       conf.interval = .95)


## ------------------------------------------------
agg |>
  ggplot(aes(condition, mean, fill = condition)) +
  geom_col(alpha = 0.8) +
  geom_line(aes(group = 1), linetype = 3) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.1, size=1, color="black") +
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  theme(legend.position = "none")

