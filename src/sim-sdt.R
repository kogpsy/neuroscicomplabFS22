N <- 150
pH <- 0.9
pFA <- 0.1
H <- rbinom(1, N, pH)

M <- N - H
FA <- rbinom(1, N, pFA)
CR <- N - FA

Responses <- as.table(matrix(c(H, M, FA, CR), nc = 2,
                                     dimnames = list(Response = c("Yes", "No"),
                                                     Signal = c("Present", "Absent"))))




dp <- 1
crit <- 0.1
nS <- nN <- 1000


pFA <- 1 - pnorm(crit)
pH <- 1-pnorm(crit-dp)
FA <- rbinom(1,nN, pFA)
H <- rbinom(1, nS, pH)
CR <- nN-FA
M <- nS-H



sim_sdt <- function(dp = 1, c = 0, N = 1000) {

    nS <- nN <- N
    # pFA <- 1 - pnorm(crit)
    pFA <- 1 - pnorm(-0.5*dp - c)
    # pH <- 1 - pnorm(crit - dp)
    pH <- 1 - pnorm(0.5*dp - c)
    FA <- rbinom(1,nN, pFA)
    H <- rbinom(1, nS, pH)
    CR <- nN-FA
    M <- nS-H
res <- matrix(c(H, M, FA, CR),
              nc = 2,
              dimnames = list(c("Yes", "No"),
                              c("Signal", "No Signal")))
as.table(res)
}

sim_sdt()


## -----
library(sdtalt)
data("confcontr")

sdt <- confcontr %>%
    mutate(
        type = "hit",
        type = ifelse(isold == 1 & sayold == 0, "miss", type),
        type = ifelse(isold == 0 & sayold == 0, "cr", type), # Correct rejection
        type = ifelse(isold == 0 & sayold == 1, "fa", type) # False alarm
    )

sdt <- sdt %>%
    group_by(subno, type) %>%
    summarise(count = n()) %>%
    spread(type, count) # Format data to one row per person

sdt <- sdt %>%
    mutate(
        zhr = qnorm(hit / (hit + miss)),
        zfa = qnorm(fa / (fa + cr)),
        dprime = zhr - zfa,
        crit = -zfa
    )


# ```{r sdtplot-1, echo = F, fig.cap = "The equal variance Gaussian signal detection model for the first participant in the data, based on manual calculation of the parameter's point estimates. The two distributions are the noise distribution (dashed) and the signal distribution (solid); the dotted vertical line represents the response criterion. d' is the distance between the peaks of the two distributions."}
# Basic plot
p1 <- ggplot(data.frame(x = seq(-5, 6, by = .01)), aes(x = x)) +
    coord_cartesian(ylim = c(0, 0.5), xlim = c(-3.5, 4), expand = 0) +
    labs(x = "Signal strength", y = "Probability")
# Add noise & signal distributions, and criterion
p1 + stat_function(
    fun = dnorm, geom = "line", lty = 2,
    args = list(mean = 0, sd = 1)
) +
    stat_function(
        fun = dnorm, geom = "line",
        args = list(mean = sdt$dprime[1], sd = 1)
    ) +
    geom_vline(xintercept = .5, lty = 3)
# ```
