
## ---------------------------------------------------------------
library(tidyverse)
data <- read_csv("data_clean/rdkdata.csv")


## ---------------------------------------------------------------
data <- data |>
    mutate_if(is.character, as.factor)


## ---------------------------------------------------------------
glimpse(data)


## ---------------------------------------------------------------
data


## ---------------------------------------------------------------
data |>
  group_by(ID, condition)


## ---------------------------------------------------------------
accuracy <- data |>
    group_by(ID, condition) |>
    summarise(N = n(),
              ncorrect = sum(correct),
              accuracy = mean(correct))


## ---------------------------------------------------------------
accuracy


## ----fig.height=12, fig.width=15--------------------------------
accuracy |>
  ggplot(aes(x = condition, y = accuracy, fill = condition)) +
  geom_col() +
  geom_line(aes(group = ID), size = 2) +
  geom_point(size = 8) +
  scale_fill_manual(
    values = c(invalid = "#9E0142",
    neutral = "#C4C4B7",
    valid = "#2EC762")
  ) +
  labs(
    x = "Cue",
    y = "Proportion correct",
    title = "Accuracy per person/condition"
  ) +
  theme_linedraw(base_size = 28) +
  facet_wrap(~ID)


## ---------------------------------------------------------------
library(tidyverse)

dfw <- tribble(
 ~subject, ~pretest, ~posttest,
       1,   59.4,     64.5,
       2,   46.4,     52.4,
       3,   46.0,     49.7,
       4,   49.0,     48.7,
       5,   32.5,     37.4,
       6,   45.2,     49.5,
       7,   60.3,     59.9,
       8,   54.3,     54.1,
       9,   45.4,     49.6,
      10,   38.9,     48.5) |>
    mutate(subject = as.factor(subject))


## ---------------------------------------------------------------
dfl <- dfw |>
    pivot_longer(contains("test"),
                 names_to = "condition",
                 values_to = "value") |>
    mutate(condition = as_factor(condition))


## ---------------------------------------------------------------
dflsum <- dfl |>
    Rmisc::summarySEwithin(measurevar = "value",
                               withinvars = "condition",
                               idvar = "subject",
                               na.rm = FALSE,
                               conf.interval = 0.95)


## ---------------------------------------------------------------
dflsum |>
    ggplot(aes(x = condition, y = value, group = 1)) +
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = value-ci, ymax = value+ci)) +
    geom_point(shape = 21, size = 3, fill = "white") +
    ylim(40,60)


## ---------------------------------------------------------------
# Use a consistent y range
ymax <- max(dfl$value)
ymin <- min(dfl$value)


## ---------------------------------------------------------------
# Plot the individuals
dfl |>
    ggplot(aes(x=condition, y=value, colour=subject, group=subject)) +
    geom_line() + geom_point(shape=21, fill="white") +
    ylim(ymin,ymax)


## ---------------------------------------------------------------
dfNorm_long <- Rmisc::normDataWithin(data=dfl, idvar="subject", measurevar="value")
?Rmisc::normDataWithin

dfNorm_long |>
    ggplot(aes(x=condition, y=valueNormed, colour=subject, group=subject)) +
    geom_line() + geom_point(shape=21, fill="white") +
    ylim(ymin,ymax)


## ---------------------------------------------------------------
# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dflsum_between <- Rmisc::summarySE(data = dfl,
                                   measurevar = "value",
                                   groupvars = "condition",
                                   na.rm = FALSE,
                                   conf.interval = .95)
dflsum_between


## ---------------------------------------------------------------
# Show the between-S CI's in red, and the within-S CI's in black
dflsum_between |>
    ggplot(aes(x=condition, y=value, group=1)) +
    geom_line() +
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), colour="red") +
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), data=dflsum) +
    geom_point(shape=21, size=3, fill="white") +
    ylim(ymin,ymax)


## ---------------------------------------------------------------
accuracy |>
  ggplot(aes(x = condition, y = accuracy, colour = ID, group = ID)) +
    geom_line() +
  geom_point(shape=21, fill="white")



## ---------------------------------------------------------------
se <- function(x) sd(x)/sqrt(length(x))


## ---------------------------------------------------------------
datasum <- data |>
   group_by(condition) |>
   summarise(N = n(),
             ccuracy = mean(correct),
             sd = sd(correct),
             se = se(correct))
datasum


## ---------------------------------------------------------------
datasum_2 <- data |>
    Rmisc::summarySE(measurevar = "correct",
                              groupvars = "condition",
                               na.rm = FALSE,
                               conf.interval = 0.95)
datasum_2


## ---------------------------------------------------------------
datasum_3 <- data |>
    Rmisc::summarySEwithin(measurevar = "correct",
                               withinvars = "condition",
                               idvar = "ID",
                               na.rm = FALSE,
                               conf.interval = 0.95)
datasum_3


## ---------------------------------------------------------------
p_accuracy <- datasum_3 |>
    ggplot(aes(x = condition, y = correct, group = 1)) +
    geom_line() +
    geom_errorbar(width = .1, aes(ymin = correct-se, ymax = correct+se), colour="red") +
    geom_point(shape=21, size=3, fill="white")
p_accuracy


## ---------------------------------------------------------------
funs <- list(mean = mean, median = median, sd = sd)

by_subj <- data %>%
  drop_na(rt) |>
  group_by(ID, condition) %>%
  dplyr::summarise(across(rt, funs, .names = "{.fn}"))


## ---------------------------------------------------------------
by_subj


## ---------------------------------------------------------------
by_subj <- data |>
  drop_na(rt) |>
  group_by(ID, condition) |>
  dplyr::summarise(mean = mean(rt),
                   median = median(rt),
                   sd = sd(rt))


## ----fig.height=12, fig.width=15--------------------------------
by_subj |>
  ggplot(aes(x = condition, y = mean, fill = condition)) +
  geom_col() +
  geom_line(aes(group = ID), size = 2) +
  geom_point(size = 8) +
  scale_fill_manual(
    values = c(invalid = "#9E0142",
    neutral = "#C4C4B7",
    valid = "#2EC762")
  ) +
  labs(
    x = "Cue",
    y = "Response time") +
  theme_linedraw(base_size = 28) +
  facet_wrap(~ID)


## ----eval=TRUE--------------------------------------------------
se <- function(x, ...) sd(x, ...)/sqrt(length(x))

by_subj <- data %>%
  group_by(ID, condition) %>%
  summarise(mean = mean(rt, na.rm = TRUE),
            median = median(rt, na.rm = TRUE),
            sd = sd(rt, na.rm = TRUE),
            se = se(rt, na.rm = TRUE))


## ----eval=TRUE--------------------------------------------------
by_subj |>
  ggplot(aes(condition, mean)) +
  geom_line(aes(group = 1), linetype = 3) +
  geom_errorbar(aes(ymin = mean-se, ymax = mean+se),
                width = 0.2, size=1, color="blue") +
  geom_point(size = 2) +
  facet_wrap(~ID, scales = "free_y")


## ---------------------------------------------------------------
rtsum <- data |>
  drop_na(rt) |>
    Rmisc::summarySEwithin(measurevar = "rt",
                               withinvars = "condition",
                               idvar = "ID",
                               na.rm = FALSE,
                               conf.interval = 0.95)
rtsum


## ---------------------------------------------------------------
p_rt <- rtsum |>
    ggplot(aes(x = condition, y = rt, group = 1)) +
    geom_line() +
    geom_errorbar(width = .1, aes(ymin = rt-se, ymax = rt+se), colour="red") +
    geom_point(shape=21, size=3, fill="white")


## ---------------------------------------------------------------
p_rt


## ---------------------------------------------------------------
library(patchwork)


## ---------------------------------------------------------------
p_accuracy / p_rt

