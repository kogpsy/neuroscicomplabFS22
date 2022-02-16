set.seed(747)

ntrials <- 100

df_shifted_lognormals <- tibble(condition1 = brms::rshifted_lnorm(ntrials,
                                                                  meanlog = 0.5,
                                                                  sdlog = 0.8,
                                                                  shift = 0.2),
                                condition2 = brms::rshifted_lnorm(ntrials,
                                                                  meanlog = 0.8,
                                                                  sdlog = 0.8,
                                                                  shift = 0.2)) %>%
    pivot_longer


df_shifted_lognormals
