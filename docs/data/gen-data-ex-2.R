library(rtdists)
library(tidyverse)

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

set.seed(4324)
adhd <- tibble(rt = 0.55 + rnorm(n = 21, 0.8, 0.5),
               group = "adhd")

control <- tibble(rt = 0.5 + rnorm(n = 30, 0.7, 0.2),
                  group = "control")

d <- bind_rows(adhd, control)


d %>% ggplot(aes(rt)) +
    geom_histogram() +
    facet_wrap(~group)

d %>% group_by(group) %>%
    summarise(mean = mean(rt),
              sd = sd(rt))

t.test(rt ~ group,
       data = d,
       alternative = "greater")

d %>% write_csv(file = "data/adhd-nback.csv")



## Aufgabe 2 ----

sub_n  <- 20 # number of subjects in this simulation
sub_sd <- 100 # SD for the subjects' random intercept

sub <- tibble(
    sub_id = 1:sub_n,
    sub_i  = rnorm(sub_n, 0, sub_sd), # random intercept
    # sub_cond = rep(c("easy","hard"), each = sub_n/2) # between-subjects factor
)

ntrials  <- 60 # number of stimuli in this simulation
# trial_sd <- 50 # SD for the stimuli's random intercept

# stim <- tibble(
#     trial_id = 1:stim_n,
#     stim_i = rnorm(stim_n, 0, stim_sd) # random intercept
# )

trials <- crossing(
    sub_id = sub$sub_id,
    trial = 1:ntrials,
    load = c("lo", "hi")) %>%
    left_join(sub, by = "sub_id")

# trials %>% write_csv("data/cognitive-load-nback.csv")


grand_i          <- 800 # overall mean DV
sub_cond_eff     <- 50  # mean difference between conditions: hard - easy
stim_version_eff <- 50  # mean difference between versions: incongruent - congruent
cond_version_ixn <-  0  # interaction between version and condition
error_sd         <- 25  # residual (error) SD


# conditions <- c("lo" = -0.5, "hard" = +0.5)
conditions   <- c("lo" = -0.5, "hi" = +0.5)

dat <- trials %>%
    mutate(
        # effect-code subject condition and stimulus version
        sub_cond.e = recode(load, !!!conditions),
        # stim_version.e = recode(stim_version, !!!versions),
        # calculate error term (normally distributed residual with SD set above)
        err = rnorm(nrow(.), 0, error_sd),
        # calculate DV from intercepts, effects, and error
        rt = round(grand_i + sub_i + err +
            (sub_cond.e * sub_cond_eff) +
            # (stim_version.e * stim_version_eff) +
            (sub_cond.e * cond_version_ixn), 0) # in this example, this is always 0 and could be omitted
    )

dat %>%
    select(id = sub_id, trial, load, rt) %>%
    arrange(id, load, trial) %>%
    write_csv("data/cognitive-load-nback.csv")

