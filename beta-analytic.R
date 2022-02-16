library(tidyverse)
library(scales)
x <- seq(0, 1, by = .001)
a1 <- 3; a2 <- 6; b1 <- 10; b2 <- 4
Prior <- dbeta(x, a1, a2)
Likelihood <- dbeta(x, b1, b2)
Posterior <- dbeta(x, a1+b1, a2+b2)
tibble(x, Prior, Likelihood, Posterior) %>% 
  pivot_longer(-x) %>% 
  mutate(name = factor(name, levels = c("Prior", "Likelihood", "Posterior"), labels = c(~italic(p)(theta), ~italic(p)(Y~"|"~theta), ~italic(p)(theta~"|"~Y)))) %>% 
  ggplot(aes(x, value, col = name, fill = name)) +
  scale_colour_viridis_d("", aesthetics = c("color", "fill"), direction = -1) +
  scale_y_continuous(expand = expansion(c(0, .1))) +
  scale_x_continuous("Parameter value", breaks = pretty_breaks()) +
  geom_ribbon(aes(ymin = 0, ymax = value), size = 0, alpha = .8) +
  facet_wrap(
    "name", strip.position = "left", labeller = label_parsed
    ) +
  theme(
    legend.position = "none",
    aspect.ratio = 1,
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank()
    )