

library(tidyverse)
d <- read_csv("data/session-6.csv")


d <- d |>
    select(ID, condition, cue, direction, choice) |>
    mutate(across(where(is.character), ~as_factor(.)),
           cue = fct_relevel(cue, "left", "none", "right")) |>
    drop_na()


sdt <- d |>
    mutate(type = case_when(
        direction == "___" & choice == "___" ~ "___"),
        ___,
        ___,
        ___)





sdt


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
    mutate(hit_rate = ___,
           fa_rate = ___)





sdt_summary <- sdt_summary |>
    mutate(across(c(hit_rate, fa_rate), correct_zero_one))


sdt_summary <- sdt_summary |>
    mutate(zhr = ___,
           zfa = ___)




sdt_summary <- sdt_summary |>
    mutate(dprime = ___,
           k = ___,
           c = ___) |>
    mutate(across(c(dprime, k, c), round, 2))



sdt_final <- sdt_summary |>
    select(ID, cue, dprime, k, c)




## GLM ---- 
SU6460 <- d |>
    filter(ID %in% "SU6460")

SU6460_sdt <- sdt_final |>
    filter(ID %in% "SU6460")



SU6460_sdt


SU6460_sdt |>
    ggplot(aes(x = cue, y = dprime, group = 1)) +
    geom_line() +
    geom_point(shape = 21, size = 3, fill = "white")


SU6460_sdt |>
    ggplot(aes(x = cue, y = c, group = 1)) +
    geom_line() +
    geom_point(shape = 21, size = 3, fill = "white")


levels(SU6460$choice)


SU6460_glm_k_left <- glm(choice ~ direction,
                      family = binomial(link = "probit"),
                      data = SU6460 |> filter(cue == "left"))

summary(SU6460_glm_k_left)


SU6460_glm_k_right <- glm(choice ~ direction,
                       family = binomial(link = "probit"),
                       data = SU6460 |> filter(cue == "right"))

summary(SU6460_glm_k_right)


SU6460 <- SU6460 |>
    mutate(dir = if_else(direction == "left", -1/2, 1/2))


SU6460_glm_c_left <- glm(choice ~ dir,
                       family = binomial(link = "probit"),
                       data = SU6460 |> filter(cue == "left"))
summary(SU6460_glm_c_left)


SU6460_glm_c_right <- glm(choice ~ dir,
                        family = binomial(link = "probit"),
                        data = SU6460 |> filter(cue == "right"))

summary(SU6460_glm_c_right)
