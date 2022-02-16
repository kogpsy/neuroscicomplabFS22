library(rtdists)
library(tidyverse)
library(faux)

# set.seed(4324)
#
# controls <- rdiffusion(n = 30, v = 1, a = 2, t0 = 0.5) %>%
#     mutate(group = "control")
#
# adhd <- rdiffusion(n = 21, v = 0.8, a = 2, t0 = 0.7) %>%
#     mutate(group = "adhd")
#
# d <- bind_rows(controls, adhd)
#
#
# d <- d %>%
#     mutate(response = recode(response,
#                              lower = "error",
#                              upper = "correct"))
#



## Aufgabe 1 ----

set.seed(535)
schizophrenia <- tibble(rt = 550 + rnorm(n = 17, 900, 300),
               group = "schizophrenia")

control <- tibble(rt = 500 + rnorm(n = 24, 800, 200),
                  group = "control")

d <- bind_rows(schizophrenia, control)


d %>% ggplot(aes(rt)) +
    geom_histogram() +
    facet_wrap(~group)

d %>% group_by(group) %>%
    summarise(mean = mean(rt),
              sd = sd(rt))

t.test(rt ~ group,
       data = d,
       alternative = "greater")

d %>% write_csv(file = "data/schizophrenia-wm.csv")



## Aufgabe 2 ----

library(tidyverse)
library(rtdists)


sub_n  <- 34 # number of subjects in this simulation
sub_sd <- 0.3 # in (ms) SD for the subjects' random intercept
age <- sample(61:72, size = sub_n, replace = T)


sub <- tibble(
    subject = 1:sub_n,
    sub_intercept  = rnorm(sub_n, 0, sub_sd), # random intercept
    age = age
)


sub

trial_n  <- 5 # number of stimuli
trial_sd <- 0.1 # SD for the random intercept


trial <- tibble(
    trial = 1:trial_n,
    trial_intercept = rnorm(trial_n, 0, trial_sd)
)


trials <- crossing(
    subject = sub$subject,
    trial = trial$trial,
    stimulation = c("TPJ", "control"),
    block = 1:5) %>%
    left_join(sub, by = "subject") %>%
    left_join(trial, by = "trial")




grand_intercept <- 0.0 # overall mean DV
stim_version_eff <- 0.3 # mean difference between versions: incongruent - congruent
error_sd         <- 1.0  # residual (error) SD
age_effect <- rnorm(1, mean = -0.02, sd = 0.1)




d <- trials %>%
    mutate(
        # effect-code subject condition and stimulus version
        # sub_cond.e = recode(sub_cond, "hard" = -0.5, "easy" = +0.5),
        stim_version_e = recode(stimulation,
                                "TPJ" = 0.5,
                                "control" = -0.5),
        err = rnorm(nrow(.), 0, error_sd),
        driftrate = grand_intercept + sub_intercept + trial_intercept + err + # intercepts and error
            (stim_version_e * stim_version_eff) + (age * age_effect),
        dv = rtdists::rdiffusion(nrow(.), a = 2, v = driftrate, t0 = 0.9)
    )


d <- d %>%
    mutate(rt = pull(dv, rt), response = pull(dv, response)) %>%
    select(-dv)


# d %>%
#   ggplot(aes(rt, fill = response)) +
#   geom_histogram() +
#   facet_grid(sub_id ~ stim_version)



theme_set(theme_grey(base_size = 14) +
              theme(panel.grid = element_blank()))

# d %>%
#     ggplot(aes(x = rt, y = ..density.., color = sub_rec)) +
#     geom_density() +
#     facet_grid(stim_version ~ sub_id) +
#     scale_color_viridis_c(end = 0.8)

d %>%
    mutate(block = as_factor(block)) %>%
    filter(subject < 5) %>%
    ggplot(aes(x = response, y = rt, fill = age)) +
    geom_violin(alpha = 0.4) +
    geom_boxplot(alpha = 0.6) +
    geom_point() +
    facet_wrap(~ subject) +
    scale_fill_viridis_c(end = 0.8)



d <- d %>%
    select(subject, stimulation, block,, trial, age, rt, response) %>%
    arrange(subject, stimulation, block) %>%
    mutate(response = fct_recode(response,
                                 correct = "upper", error = "lower"),
           correct = if_else(response == "correct", 1, 0))


summ <- d %>%
    group_by(subject, stimulation) %>%
    summarise(acc = mean(correct))



d1 <- d %>%
    group_by(subject, stimulation, block, age, response) %>%
    summarise(rt = mean(rt)) %>%
    ungroup() %>%
    filter(response == "correct")

d2 <- d %>%
    group_by(subject, stimulation, block, age) %>%
    summarise(correct = mean(correct))

d3 <- d2 %>% left_join(d1) %>%
    select(-response)



d3 %>% write_csv(file = "data/tdcs-tpj.csv")


## Aufgabe 3


library(tidyverse)
library(readxl)

if (!file.exists("data.zip")) {
    download.file("https://ndownloader.figshare.com/files/1878093", "data.zip")
}
unzip("data.zip")
files <- list.files(
    "Behavioural_data/",
    pattern = "sub[0-9]+.xlsx", full.names = T
)
dat <- map(
    files,
    ~ read_xlsx(.x, range = "A4:G100", col_types = rep("text", 7))
) %>%
    bind_rows(.id = "id")

dat <- dat %>%
    # filter(angle %in% c("0", "50")) %>%
    transmute(
        id = factor(id),
        angle = factor(angle),
        rt = as.numeric(Time),
        accuracy = as.numeric(`correct/incorrect`)
    )


dat %>% write_csv("data/mental-rotation.csv")
