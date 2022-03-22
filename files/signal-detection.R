

library(tidyverse)
library(viridis)
xlim <- c(-4.5, 4.5)
alpha <- c(0.6, 0.2)

dprime <- 1
criterion <- -0.2
colors <- viridis(n = 4, 
                  begin = 0, 
                  end = 0.98, 
                  direction = -1)

p1 <- tibble(x = seq(xlim[1], xlim[2], by = 0.1)) |> 
    ggplot(aes(x)) +
    stat_function(fun = dnorm, colour = colors[1], 
                  args = list(mean = -dprime/2, sd = 1),
                  size = 1.5) +
    stat_function(fun = dnorm, colour = colors[4], 
                  args = list(mean = dprime/2, sd = 1),
                  size = 1.5) +
    geom_vline(xintercept = c(-dprime/2, dprime/2), size = 1, linetype = "dotted", 
               alpha =  0.4) +
    scale_y_continuous(breaks = NULL) +
    scale_x_continuous(labels = NULL) +
    labs(x = "Familarity", y = "") +
    annotate("text", 
           x = 0.1, 
           y = dnorm(dprime/2, mean = dprime/2, sd = 1) + 0.03,
           label = "d'", 
           size = 8) +
    annotate("segment", x = -dprime/2, 
                 xend = dprime/2, 
                 y = dnorm(dprime/2, mean = dprime/2, sd = 1) + 0.01, 
                 yend = dnorm(dprime/2, mean = dprime/2, sd = 1) + 0.01,
           size = 1) +
    annotate("text", 
           x = -1.5, 
           y = dnorm(dprime/2, mean = dprime/2, sd = 1) + 0.03,
           label = "new", 
           size = 6, 
           color = "grey60") +
      annotate("text", 
           x = 1.5, 
           y = dnorm(dprime/2, mean = dprime/2, sd = 1) + 0.03,
           label = "old", 
           size = 6, 
           color = "grey60") +
  theme_linedraw()
p1


## ----echo=FALSE------------------------------------------
p2 <- p1 + 
  geom_vline(xintercept = 0, size = 1, 
               alpha = 0.4,
             linetype = "dashed") +
  geom_area(stat = "function", fun = dnorm, 
              args = list(mean = -dprime/2, sd = 1),
              fill = colors[1], alpha = 0.6,
              xlim = c(criterion, xlim[2])) +

  geom_area(stat = "function", fun = dnorm, 
              args = list(mean = dprime/2, sd = 1),
              fill = colors[4], alpha = alpha[2],
              xlim = c(criterion, xlim[2])) +
  geom_vline(xintercept = criterion, size = 1, 
               linetype = 1) +
    annotate("text", 
           x = -0.05,
           y = -0.02,
           label = "c", 
           size = 8)
p2


## ----echo=FALSE------------------------------------------
tibble(x = seq(0, 1, by = 0.01)) |> 
  ggplot(aes(x)) +
  stat_function(fun = qnorm, colour = "steelblue3", 
                  args = list(mean = 0, sd = 1),
                  size = 1.5) +
  labs(x = "Probability", y = "Score") +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(-3.5, 3.5), breaks = -3:3) + 
  ggtitle("Probit function / quantile function / inverse cdf / qnorm()") +
  theme_linedraw()


## ----echo=FALSE------------------------------------------
xlim <- c(-4.5, 4.5)
alpha <- c(0.6, 0.2)

dprime <- 1
criterion <- -0.2
colors <- viridis(n = 4, 
                  begin = 0, 
                  end = 0.98, 
                  direction = -1)

tibble(x = seq(xlim[1], xlim[2], by = 0.1)) |> 
    ggplot(aes(x)) +
    stat_function(fun = dnorm, colour = colors[1], 
                  args = list(mean = -dprime/2, sd = 1),
                  size = 1.5) +
    geom_area(stat = "function", fun = dnorm, 
              args = list(mean = -dprime/2, sd = 1),
              fill = colors[1], alpha = 0.6,
              xlim = c(criterion, xlim[2])) +
    geom_vline(xintercept = criterion, size = 1, 
               linetype = 1) +
  ggtitle("False Alarms") +
  theme_linedraw()


## ----echo=FALSE------------------------------------------
tibble(x = seq(xlim[1], xlim[2], by = 0.1)) |> 
    ggplot(aes(x)) +
    stat_function(fun = dnorm, colour = colors[4], 
                  args = list(mean = dprime/2, sd = 1),
                  size = 1.5) +
    geom_area(stat = "function", fun = dnorm, 
              args = list(mean = dprime/2, sd = 1),
              fill = colors[4], alpha = 0.6,
              xlim = c(criterion, xlim[2])) +
    geom_vline(xintercept = criterion, size = 1, 
               linetype = 1) +
  ggtitle("Hits") +
  theme_linedraw()


## ----echo=FALSE------------------------------------------
p2


## --------------------------------------------------------
sim_sdt <- function(dp = 1, c = 0, N = 100) {
  nS <- nN <- N

  pFA <- 1 - pnorm(c + dp/2)
  pH <- 1 - pnorm(c - dp/2)
  
  FA <- rbinom(1, nN, pFA)
  Hit <- rbinom(1, nS, pH)
  
  CR <- nN-FA
  Miss <- nS-Hit

  tibble(Hit, Miss, FA, CR)
}


## --------------------------------------------------------
set.seed(89)
ideal_observer <- sim_sdt(d = 1, c = 0)
ideal_observer


## --------------------------------------------------------
ideal_observer |> 
  summarise(accuracy = (Hit + CR)/(Hit + CR + Miss + FA))


## --------------------------------------------------------
set.seed(89)
yes_observer <- sim_sdt(d = 1, c = -1)
yes_observer


## --------------------------------------------------------
yes_observer |> 
  summarise(accuracy = (Hit + CR)/(Hit + CR + Miss + FA))


## --------------------------------------------------------
yes_observer |> 
    pivot_longer(everything(), names_to = "type")

yes_observer <- yes_observer |>
    mutate(hit_rate = Hit/(Hit + Miss),
           fa_rate = FA/(FA + CR))

yes_observer <- yes_observer |>
    mutate(zhr = qnorm(hit_rate),
           zfa = qnorm(fa_rate))

yes_observer <- yes_observer |>
    mutate(dprime = zhr - zfa,
           k = - zfa,
           c = -0.5 * (zhr + zfa)) |>
    mutate(across(c(dprime, c), round, 2))


## --------------------------------------------------------
yes_observer 


## --------------------------------------------------------
yes_observer |> pull(c, dprime)


## --------------------------------------------------------
# You might first need to install the `remotes` package
# install.packages("remotes")
# install sdtalt
# remotes::install_github("cran/sdtalt")

library(sdtalt)
library(tidyverse)

data(confcontr)

confcontr <- as_tibble(confcontr) |> 
  mutate(subno = as_factor(subno),
         item = isold - 0.5)


## --------------------------------------------------------
confcontr


## --------------------------------------------------------
sdt <- confcontr |> 
  mutate(type = case_when(
        isold==1 & sayold==1 ~ "Hit",
        isold==1 & sayold==0 ~ "Miss",
        isold==0 & sayold==0 ~ "CR",
        isold==0 & sayold==1 ~ "FA"))


## --------------------------------------------------------
sdt_summary <- sdt |>
    group_by(subno) |>
    count(type) |> 
  pivot_wider(names_from = type, values_from = n) 


## --------------------------------------------------------
correct_zero_count <- function(x) {
    x = ifelse(is.na(x), 0, x)
    x = x + 0.001
}

sdt_summary <- sdt_summary |>
    mutate(hit_rate = Hit/(Hit + Miss),
           fa_rate = FA/(FA + CR),
           zhr = qnorm(hit_rate),
           zfa = qnorm(fa_rate),
           dprime = zhr - zfa,
           c = -0.5 * (zhr + zfa)) |>
    mutate(across(c(dprime, c), round, 2))


## --------------------------------------------------------
sdt_summary |> 
  filter(subno == 53) |> 
  select(subno, hit_rate, fa_rate, zhr, zfa, dprime, c)


## --------------------------------------------------------
subno53 <- confcontr |> 
  filter(subno == 53)

fit_glm_53 <- glm(sayold ~ isold, 
                  family = binomial(link = "probit"),
                  data = subno53)
summary(fit_glm_53)

