



## ----load-packages, include=TRUE, warning=FALSE--------
library(tidyverse)
library(viridis)

theme_set(theme_grey(base_size = 14) +
              theme(panel.grid = element_blank()))


## ----echo=FALSE----------------------------------------
tibble(norm = rnorm(1e4, 1, 0.4),
       lognorm = rlnorm(1e4, 0, 1)) |>
    pivot_longer(everything(), names_to = "Distribution", values_to = "value") |>
    ggplot(aes(value, fill = Distribution)) +
    geom_histogram(alpha = 0.4, position = "identity", color = "black", binwidth = 0.1) +
    # geom_density(alpha = 0.6) +
    geom_vline(xintercept = 0, linetype = 3) +
    scale_fill_viridis(discrete = TRUE, option = "E") +
    coord_cartesian(xlim = c(-2, 8))


## ------------------------------------------------------
library(tidyverse)


d <- read_csv("data/session-6.csv")

d <- d |>
    mutate(across(c(ID, condition, direction, choice), ~as_factor(.))) |>
    drop_na()


## ------------------------------------------------------
glimpse(d)


## ------------------------------------------------------


## ----message=FALSE, warning=FALSE, layout="l-body-outset", fig.width=9, fig.height=4.5----
set.seed(98)
subjects <- sample(levels(d$ID), 6)
df <- d |>
    filter(ID %in% subjects)

df |>
    ggplot(aes(rt, fill = condition)) +
    geom_histogram(alpha = 0.8, position = "identity", color = "black") +
    scale_fill_viridis(discrete=TRUE, option="cividis") +
    facet_grid(condition ~ ID, scales = "free_x") +
    theme(legend.position = "none")


## ------------------------------------------------------
# summary stats (means) for participants
sum_stats_participants <- d |>
    group_by(ID, condition) |>
    dplyr::summarise(
        mean_P = mean(rt))

# summary stats (means and SDs) for conditions
sum_stats_conditions <- d |>
    group_by(condition) |>
    dplyr::summarise(
        mean_C = mean(rt),
        sd_C = sd(rt))

sum_stats_participants <-
    full_join(
        sum_stats_participants,
        sum_stats_conditions,
        by = "condition") |>
    mutate(
        outlier_P = abs(mean_P - mean_C) > 2 * sd_C)

# show outlier participants
sum_stats_participants |>
    filter(outlier_P == 1) |>
    show()


## ------------------------------------------------------
d |>
    semi_join(sum_stats_participants |> filter(outlier_P == 1),
              by = c("ID")) |>
    ggplot(aes(x = trial, y = rt)) +
    geom_point() +
    facet_wrap(~condition)


## ------------------------------------------------------
excluded <- sum_stats_participants |>
    filter(outlier_P == 1)

excluded


## ------------------------------------------------------
d_cleaned <- d |>
    filter(!(ID %in% excluded$ID)) |>
    mutate(ID = fct_drop(ID))


## ------------------------------------------------------
# mark individual trials as outliers
d_cleaned <- d_cleaned |>
    full_join(
        sum_stats_conditions,
        by = "condition") |>
    mutate(
        trial_type = case_when(
            (rt - mean_C) > 4 * sd_C ~ "zu langsam",
            rt < 0.1 ~ "zu schnell",
            TRUE ~ "OK") |>
            factor(levels = c("OK", "zu schnell", "zu langsam")),
        trial = row_number())


## ----layout="l-body-outset", fig.width=9, fig.height=4.5----
# visualize outlier trials

d_cleaned |>
    ggplot(aes(x = trial, y = rt, color = trial_type, shape = trial_type)) +
    geom_point(alpha = 0.6) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray70", "red", "steelblue"))


## ------------------------------------------------------
test <- d_cleaned |>
    filter(trial_type != "OK")


## ------------------------------------------------------
d_cleaned <- d_cleaned |>
    filter(trial_type == "OK")


## ------------------------------------------------------
d_cleaned |>
    ggplot(aes(x = rt, color = condition, fill = condition)) +
    geom_density(alpha = 0.3) +
    scale_fill_viridis(discrete=TRUE, option="cividis") +
    scale_color_viridis(discrete=TRUE, option="cividis")

d_cleaned |> write_csv(file = "data/session-6.csv")
