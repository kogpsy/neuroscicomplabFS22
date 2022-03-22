
# Simulate a signal detection experiment ----

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




## write a function to simulate experiments ----
dp <- 1
crit <- 0.1
nS <- nN <- 1000


pFA <- 1 - pnorm(crit)
pH <- 1-pnorm(crit-dp)
FA <- rbinom(1, nN, pFA)
H <- rbinom(1, nS, pH)
CR <- nN-FA
M <- nS-H


## function ----

sim_sdt_k <- function(dp = 1, k = dp/2, N = 100) {

    nS <- nN <- N
    pFA <- 1 - pnorm(k)
    pH <- 1 - pnorm(k - dp)
    FA <- rbinom(1, nN, pFA)
    H <- rbinom(1, nS, pH)
    CR <- nN-FA
    M <- nS-H
    res <- matrix(c(H, M, FA, CR),
                  nc = 2,
                  dimnames = list(c("Yes", "No"),
                                  c("Signal", "No Signal")))
    as.table(res)
}

set.seed(21)
sim_sdt_k(dp = 1, k = 0.5)


sim_sdt_c <- function(dp = 1, c = 0, N = 100) {

    # c = k - dp/2
    # k = c + dp/2

    nS <- nN <- N

    pFA <- 1 - pnorm(c + dp/2)
    pH <- 1 - pnorm(c - dp/2)
    FA <- rbinom(1, nN, pFA)
    H <- rbinom(1, nS, pH)
    CR <- nN-FA
    M <- nS-H
    res <- matrix(c(H, M, FA, CR),
                  nc = 2,
                  dimnames = list(c("Yes", "No"),
                                  c("Signal", "No Signal")))
    as.table(res)
}

## call function with default arguments ----
set.seed(21)
sim_sdt_c()




## return a tibble ----
sim_sdt <- function(dp = 1, c = 0, N = 100) {

    # c = k - dp/2
    # k = c + dp/2

    nS <- nN <- N

    pFA <- 1 - pnorm(c + dp/2)
    pH <- 1 - pnorm(c - dp/2)
    FA <- rbinom(1, nN, pFA)
    Hit <- rbinom(1, nS, pH)
    CR <- nN-FA
    Miss <- nS-Hit

    tibble(Hit, Miss, FA, CR)
}

d <- sim_sdt()
d |>
    pivot_longer(everything(), names_to = "type")

d <- d |>
    mutate(hit_rate = Hit/(Hit + Miss),
           fa_rate = FA/(FA + CR))

d <- d |>
    mutate(zhr = qnorm(hit_rate),
           zfa = qnorm(fa_rate))

d <- d |>
    mutate(dprime = zhr - zfa,
           k = - zfa,
           c = -0.5 * (zhr + zfa)) |>
    mutate(across(c(dprime, c), round, 2))

