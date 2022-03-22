
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

## call function with default arguments ----
sim_sdt()

