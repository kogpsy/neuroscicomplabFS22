library(tidyverse)

d <- read_csv("data/session-6.csv")

d <- d |>
    select(ID, condition, cue, direction, choice) |>
    mutate(across(where(is.character), ~as_factor(.)),
           cue = fct_relevel(cue, "left", "none", "right")) |>
    drop_na()


sdt <- d |>
    mutate(type = case_when(
        direction == "right" & choice == "right" ~ "Hit",
        direction == "right" & choice == "left" ~ "Miss",
        direction == "left" & choice == "left" ~ "CR",
        direction == "left" & choice == "right" ~ "FA"))

sdt_summary <- sdt |>
    group_by(ID, cue) |>
    count(type) |>
    pivot_wider(names_from = type, values_from = n)

replace_NA <- function(x) {
    x = ifelse(is.na(x), 0, x)
    x
}

correct_zero_one <- function(x) {
    if (identical(x, 0)) {
        x = x + 0.001
    } else if (identical(x, 1)) {
        x = x - 0.001
    }
    x
}

sdt_summary <- sdt_summary |>
    mutate(across(c(Hit, Miss, FA, CR), replace_NA))

sdt_summary <- sdt_summary |>
    mutate(hit_rate = Hit/(Hit + Miss),
           fa_rate = FA/(FA + CR))


sdt_summary <- sdt_summary |>
    mutate(across(c(hit_rate, fa_rate), correct_zero_one))


sdt_summary <- sdt_summary |>
    mutate(zhr = qnorm(hit_rate),
           zfa = qnorm(fa_rate))


sdt_summary <- sdt_summary |>
    mutate(dprime = zhr - zfa,
           k = -zfa,
           c = -0.5 * (zhr + zfa)) |>
    mutate(across(c(dprime, k, c), round, 2))

sdt_final <- sdt_summary |>
    select(ID, cue, dprime, k, c)


dprimes <- sdt_final |>
    select(ID, cue, dprime) |>
    Rmisc::summarySEwithin(measurevar = "dprime",
                           withinvars = "cue",
                           idvar = "ID",
                           na.rm = FALSE,
                           conf.interval = 0.95)
ks <- sdt_final |>
    select(ID, cue, k) |>
    Rmisc::summarySEwithin(measurevar = "k",
                           withinvars = "cue",
                           idvar = "ID",
                           na.rm = FALSE,
                           conf.interval = 0.95)
cs <- sdt_final |>
    select(ID, cue, c) |>
    Rmisc::summarySEwithin(measurevar = "c",
                           withinvars = "cue",
                           idvar = "ID",
                           na.rm = FALSE,
                           conf.interval = 0.95)

p1 <- dprimes |>
    ggplot(aes(x = cue, y = dprime, group = 1)) +
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = dprime - ci,
                                   ymax = dprime + ci)) +
    geom_point(shape = 21, size = 3, fill = "white")


p2 <- ks |>
    ggplot(aes(x = cue, y = k, group = 1)) +
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = k - ci,
                                   ymax = k + ci)) +
    geom_point(shape = 21, size = 3, fill = "white")

p3 <- cs |>
    ggplot(aes(x = cue, y = c, group = 1)) +
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = c - ci,
                                   ymax = c + ci)) +
    geom_point(shape = 21, size = 3, fill = "white")


library(patchwork)
p1 / (p2 + p3)



## single subject ----

SU6460 <- d |>
    filter(ID %in% "SU6460")

SU6460_sdt <- sdt_final |>
    filter(ID %in% "SU6460")

SU6460_sdt |>
    ggplot(aes(x = cue, y = c, group = 1)) +
    geom_line() +
    geom_point(shape = 21, size = 3, fill = "white")

SU6460_sdt

levels(SU6460$choice)

SU6460_glm_left <- glm(choice ~ direction,
                      family = binomial(link = "probit"),
                      data = SU6460 |> filter(cue == "left"))

SU6460_glm_right <- glm(choice ~ direction,
                       family = binomial(link = "probit"),
                       data = SU6460 |> filter(cue == "right"))

summary(SU6460_glm_left)
summary(SU6460_glm_right)

SU6460 <- SU6460 |>
    mutate(dir = if_else(direction == "left", -1/2, 1/2))

SU6460_glm_left <- glm(choice ~ dir,
                       family = binomial(link = "probit"),
                       data = SU6460 |> filter(cue == "left"))

SU6460_glm_right <- glm(choice ~ dir,
                        family = binomial(link = "probit"),
                        data = SU6460 |> filter(cue == "right"))
