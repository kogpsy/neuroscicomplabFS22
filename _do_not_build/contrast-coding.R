library(tidyverse)
library(brms)


df_contrasts1 <- read_csv("https://raw.githubusercontent.com/kogpsy/neuroscicomplab/main/data/df_contrasts1.csv")


df_contrasts1 <- df_contrasts1 %>%
    mutate(F = as_factor(F))

mm1 <- model.matrix(~ F, data = df_contrasts1)
df_contrasts1$treat <- mm1[, 2]

m1 <- brm(DV ~ F,
          prior = prior(normal(0, 1), class = b),
          data = df_contrasts1)


df_contrasts1 <- within(df_contrasts1, {
    contrasts(F) <- contr.sum(n = 2)
})

mm2 <- model.matrix(~ F, data = df_contrasts1)

df_contrasts1$sum <- mm2[, 2]

m2 <- brm(DV ~ F,
          prior = prior(normal(0, 1), class = b),
          data = df_contrasts1)


df_contrasts1 <- within(df_contrasts1, {
    contrasts(F) <- c(-1/2, 1/2)
})

mm3 <- model.matrix(~ F, data = df_contrasts1)

df_contrasts1$sumhalf <- mm3[, 2]

m3 <- brm(DV ~ F,
          prior = prior(normal(0, 1), class = b),
          data = df_contrasts1)

