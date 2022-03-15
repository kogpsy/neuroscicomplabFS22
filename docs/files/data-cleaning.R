## ----setup, include=FALSE-------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- xaringanExtra-clipboard, echo=FALSE, include=FALSE------
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "<i class=\"fa fa-clone fa-2x\" style=\"color: #301e64\"></i>",
    success_text = "<i class=\"fa fa-check fa-2x\" style=\"color: #90BE6D\"></i>",
    error_text = "<i class=\"fa fa-times fa-2x\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
)


## ----load-packages, include=TRUE, warning=FALSE---------------
library(tidyverse)
library(viridis)

theme_set(theme_grey(base_size = 14) +
            theme(panel.grid = element_blank()))


## ----echo=FALSE-----------------------------------------------
tibble(norm = rnorm(1e4, 1, 0.4), 
       lognorm = rlnorm(1e4, 0, 1)) |> 
  pivot_longer(everything(), names_to = "Distribution", values_to = "value") |> 
  ggplot(aes(value, fill = Distribution)) +
  geom_histogram(alpha = 0.4, position = "identity", color = "black", binwidth = 0.1) +
  # geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = 3) +
  scale_fill_viridis(discrete = TRUE, option = "E") +
  coord_cartesian(xlim = c(-2, 8))


## -------------------------------------------------------------
URL <- "https://raw.githubusercontent.com/kogpsy/neuroscicomplab/main/data/mental-chronometry.csv"

mentalchronometry <- read_csv(URL) |> 
  mutate(across(c(subj, block, stimulus, handedness, gender), ~as_factor(.)))


## -------------------------------------------------------------
glimpse(mentalchronometry)


## -------------------------------------------------------------
mentalchronometry


## ----message=FALSE, warning=FALSE, layout="l-body-outset", fig.width=9, fig.height=4.5----
set.seed(98)
subjects <- sample(levels(mentalchronometry$subj), 6)
df <- mentalchronometry |>
  filter(subj %in% subjects)

df |> 
  ggplot(aes(RT, fill = block)) +
  geom_histogram(alpha = 0.8, position = "identity", color = "black") +
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  facet_grid(block ~ subj, scales = "free_x") +
  theme(legend.position = "none")


## ----message=FALSE, warning=FALSE, layout="l-body-outset", fig.width=9, fig.height=4.5----
df |> 
  filter(subj %in% subjects) |> 
  ggplot(aes(y = RT, x = block, fill = block)) +
  geom_violin(alpha = 0.6) +
  geom_jitter(width = 0.1) +
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  facet_wrap(~ subj, scales = "free_x") +
  theme(legend.position = "none")


## -------------------------------------------------------------
# summary stats (means) for participants
sum_stats_participants <- mentalchronometry |> 
  group_by(subj, block) |> 
  dplyr::summarise(
    mean_P = mean(RT))

# summary stats (means and SDs) for conditions
sum_stats_conditions <- mentalchronometry |> 
  group_by(block) |> 
  dplyr::summarise(
    mean_C = mean(RT),
    sd_C = sd(RT))
  
sum_stats_participants <- 
  full_join(
    sum_stats_participants,
    sum_stats_conditions,
    by = "block") |> 
  mutate(
    outlier_P = abs(mean_P - mean_C) > 2 * sd_C)

# show outlier participants
sum_stats_participants |> 
  filter(outlier_P == 1) |> 
  show()


## -------------------------------------------------------------
mentalchronometry |> 
  semi_join(sum_stats_participants |> filter(outlier_P == 1), 
    by = c("subj")) |> 
  ggplot(aes(x = trial_number, y = RT)) +
  geom_point() +
  facet_wrap(~block)


## -------------------------------------------------------------
excluded <- sum_stats_participants |> 
  filter(outlier_P == 1)

excluded


## -------------------------------------------------------------
mentalchronometry_cleaned <- mentalchronometry |> 
  filter(!(subj %in% excluded$subj)) |> 
  mutate(subj = fct_drop(subj))


## -------------------------------------------------------------
# mark individual trials as outliers
mentalchronometry_cleaned <- mentalchronometry_cleaned |> 
  full_join(
    sum_stats_conditions,
    by = "block") |> 
  mutate(
    trial_type = case_when(
      abs(RT - mean_C) > 2 * sd_C ~ "zu weit vom Mittelwert",
      RT < 100 ~ "< 100ms",
      TRUE ~ "OK") |> 
      factor(levels = c("OK", "< 100ms", "zu weit vom Mittelwert")),
    trial = row_number())


## ----layout="l-body-outset", fig.width=9, fig.height=4.5------
# visualize outlier trials

mentalchronometry_cleaned |> 
  ggplot(aes(x = trial, y = RT, color = trial_type, shape = trial_type)) +
  geom_point(alpha = 0.6) + 
  geom_point(data = filter(mentalchronometry_cleaned, trial_type != "OK"), 
             alpha = 0.9) + 
  facet_grid(~block) +
  scale_color_manual(values = c("gray70", "red", "steelblue"))


## -------------------------------------------------------------
mentalchronometry_cleaned |> 
  filter(trial_type != "OK")


## -------------------------------------------------------------
mentalchronometry_cleaned <- mentalchronometry_cleaned |> 
  filter(trial_type == "OK")


## -------------------------------------------------------------
mentalchronometry_cleaned |> 
  ggplot(aes(x = RT, color = block, fill = block)) +
  geom_density(alpha = 0.3) +
  scale_fill_viridis(discrete=TRUE, option="cividis") +
  scale_color_viridis(discrete=TRUE, option="cividis")

