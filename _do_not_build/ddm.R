
library(tidyverse)
library(rtdists)
library(brms)

v <-  0.5  # average driftrate
a <- 1.0   # average boundary separation
ndt <-  0.3    # average ndt
bias <- 0.5

ntrials <- 1e3

d_up <- tibble(rdiffusion(ntrials, a = a, v = v, z = 0.8, t0 = ndt),
                          cue = "valid")
d_lo <- tibble(rdiffusion(ntrials, a = a, v = v, z = 0.2, t0 = ndt),
                          cue = "invalid")

d <- rbind(d_up, d_lo)
d |>
    ggplot(aes(rt, fill = response)) +
    geom_density() +
    geom_rug() +
    scale_color_brewer(type = "qual") + scale_fill_brewer(type = "qual") +
    facet_grid(response ~ cue)






formula <- bf(rt | dec(response) ~ 0 + cue,
              bias ~ 0 + cue)

get_prior(formula, family = wiener(), data = d)

priors <- prior(normal(0, 1), class = b) +
    prior(normal(0, 1), class = b, dpar = bias)

fit1 <- brm(formula,
            prior = priors,
            family = wiener(link = "identity", link_bias = "logit"),
            data = d)
fit1

fit2 <- brm(bf(rt | dec(response) ~ 1,
               bias ~ 0 + cue),
            prior = prior(normal(0, 1), class = b, dpar = bias),
            family = wiener(link = "identity", link_bias = "logit"),
            data = d)
fit2







d |>
    group_by(bias) |>
    summarise(accuracy = mean(response == "upper"))


d_acc <- tibble(rdiffusion(ntrials, a = 2, v = v, z = bias, t0 = ndt,
                          sz = 0.05, st0 = 0.01, sv = 0.1), condition = "accuracy")
d_speed <- tibble(rdiffusion(ntrials, a = 0.8, v = v, z = bias, t0 = ndt,
                          sz = 0.05, st0 = 0.01, sv = 0.1), condition = "speed")

d <- rbind(d_acc, d_speed)

d |>
    ggplot(aes(rt, fill = response)) +
    geom_histogram() +
    scale_fill_viridis_d(option = "B", direction = -1,
                         begin = 1/3, end = 3/3) +
    facet_wrap(~ condition)


d_wv <- tibble(rdiffusion(n = ntrials, a = 2, t0 = 0.3, z = 0.5, v = 0.5))

d_wv |>
    ggplot(aes(rt, fill = response, color = response)) +
    geom_density(alpha = 0.6) +
    geom_rug() +
    scale_color_brewer(type = "qual") + scale_fill_brewer(type = "qual")








sigma_b <-  0.8  # std dev in slopes -> set to 0.5
rho <- -.7   # correlation between intercepts and slopes -> set to -0.7

# the next three lines of code simply combine the terms, above
mu     <- c(a, b)
cov_ab <- sigma_a * sigma_b * rho
sigma  <- matrix(c(sigma_a^2, cov_ab,
                   cov_ab, sigma_b^2), ncol = 2)

# how many subjects?
n_subjects <- 3

varying_effects <-
    MASS::mvrnorm(n_subjects, mu, sigma) |>
    as_tibble(.name_repair = "unique") |>
    set_names("a_j", "b_j")



n_trials <- 10
sigma    <-  0.5  # std dev within subjects

# set.seed(13)  # used to replicate example

d_linpred <-
    varying_effects |>
    mutate(subject  = 1:n_subjects) |>
    expand(nesting(subject, a_j, b_j), post = c(0, 1)) |>
    mutate(mu = a_j + b_j * post,
           sigma = sigma) |>
    mutate(treatment = ifelse(post == 0, "pre", "post"),
           treatment = factor(treatment, levels = c("pre", "post")))



d <- d_linpred |>
    slice(rep(1:n(), each = n_trials)) |>
    mutate(response = rnorm(n = n(), mean = mu, sd = sigma))

