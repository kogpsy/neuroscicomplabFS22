library(tidyverse)

d <- read_csv("data/data-exercise-03.csv")

glimpse(d)



d <- d |>
    mutate(subject = as_factor(subject))

subjects <- sample(levels(d$subject), 20)

d <- d |>
    filter(subject %in% subjects,
           rt < 100) |>
    mutate(subject = fct_drop(subject))

d <- d |>
    group_by(subject, condition) |>
    mutate(trial_num = row_number())



d |>
    ggplot(aes(x = trial_num, y = rt)) +
    geom_point(alpha = 0.6) +
    # geom_point(data = filter(d_cleaned, trial_type != "OK"),
    #            alpha = 0.9) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray70", "red", "steelblue"))

d <- d |>
    mutate(rt = if_else(rt > 10, rt/8, rt))


d |>
    ggplot(aes(x = trial_num, y = rt)) +
    geom_point(alpha = 0.6) +
    # geom_point(data = filter(d_cleaned, trial_type != "OK"),
    #            alpha = 0.9) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray70", "red", "steelblue"))

d |> write_csv("data/data-exercise-03.csv")


## clean subjects ----
sum_stats_participants <- d |>
    group_by(subject, condition) |>
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
    mutate(outlier_P = abs(mean_P - mean_C) > 2 * sd_C)

# show outlier participants
sum_stats_participants |>
    filter(outlier_P == 1) |>
    show()

d |>
    # semi_join(sum_stats_participants |> filter(outlier_P == 1),
    #           by = c("ID")) |>
    ggplot(aes(x = trial_num, y = rt)) +
    geom_point() +
    facet_wrap(~condition)

excluded <- sum_stats_participants |>
    filter(outlier_P == 1)

excluded


## ------------------------------------------------------
d_cleaned <- d |>
    filter(!(subject %in% excluded$subject)) |>
    mutate(subject = fct_drop(subject))


## clean trials ----
d_cleaned <- d_cleaned |>
    full_join(
        sum_stats_conditions,
        by = "condition") |>
    mutate(
        trial_type = case_when(
            (rt - mean_C) > 4 * sd_C ~ "too slow",
            rt < 0.100 ~ "too fast",
            TRUE ~ "OK") |>
            factor(levels = c("OK", "too fast", "too slow")))

# visualize outlier trials

d_cleaned |>
    ggplot(aes(x = trial_num, y = rt, color = trial_type, shape = trial_type)) +
    geom_point(alpha = 0.6) +
    # geom_point(data = filter(d_cleaned, trial_type != "OK"),
    #            alpha = 0.9) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray70", "red", "steelblue"))


## ------------------------------------------------------
d_cleaned |>
    filter(trial_type != "OK")


## ------------------------------------------------------
d_cleaned <- d_cleaned |>
    filter(trial_type == "OK") |>
    select(subject, trial_num, condition, signal_present, correct, rt)

d_cleaned |>
    ggplot(aes(x = trial_num, y = rt)) +
    geom_point(alpha = 0.6) +
    facet_grid(~condition) +
    scale_color_manual(values = c("gray70", "red", "steelblue"))

d_cleaned |> write_csv("data/data-exercise-03-cleaned.csv")


d_cleaned |>
    group_by(subject, condition) |>
    summarise(accuracy = mean(correct))

d_cleaned |>
    Rmisc::summarySEwithin(measurevar = "correct",
                       withinvars = "condition",
                       idvar = "subject",
                       na.rm = FALSE,
                       conf.interval = 0.95)

d_cleaned |>
    Rmisc::summarySEwithin(measurevar = "rt",
                           withinvars = "condition",
                           idvar = "subject",
                           na.rm = FALSE,
                           conf.interval = 0.95)



