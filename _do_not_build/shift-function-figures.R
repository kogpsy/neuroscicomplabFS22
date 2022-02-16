

# packages:
# library(rogme)
library(ggplot2)
library(patchwork)
## ============================
source("_do_not_build/shift_function/Rallfun-v30.R")
source("_do_not_build/shift_function/wilcox_modified.R")
source("_do_not_build/shift_function/rgar_visualisation.R")
source("_do_not_build/shift_function/rgar_utils.R")


# --------------------------------
# example 1: difference in spread
# --------------------------------
set.seed(21)
g1 <- rnorm(1000) + 6
g2 <- rnorm(1000)*1.5 + 6

ks(g1,g2) # Kolmogorov-Smirnov test
# ks(cont,expt,w=T) # uses a weighted version more sensitive to differences occuring in the tails
t.test(g1,g2) # regular Welsh t-test


# make data frame
df <- mkdf2(g1,g2)

# kernel density estimate + rug plot + superimposed deciles
kde <- plot.kde_rug_dec2(df)

# compute shift function
out <- shifthd( g1, g2, nboot=200)

# plot shift function
sf <- plot.sf(data=out) # function from rgar_visualisation.txt

kde / sf
ggsave(filename = "images/shiftfun-1.png")


## example 2 ----

set.seed(21)
g1 <- rnorm(1000) + 6
g2 <- rnorm(1000) + 6.5

ks(g1,g2) # Kolmogorov-Smirnov test
t.test(g1,g2) # regular Welsh t-test

# make data frame
df <- mkdf2(g1,g2)
# kernel density estimate + rug plot + superimposed deciles
kde <- plot.kde_rug_dec2(df)
# compute shift function
out <- shifthd( g1, g2, nboot=200)
# plot shift function
sf <- plot.sf(data=out) # function from rgar_visualisation.txt
# combine KDE + SF

kde / sf
ggsave(filename = "images/shiftfun-2.png")


## example 3 ----
set.seed(21)
g1 <- rnorm(1000)
g1[g1>0] <- g1[g1>0]*2
g2 <- rnorm(1000)

ks(g1,g2) # Kolmogorov-Smirnov test
t.test(g1,g2) # regular Welsh t-test

# make data frame
df <- mkdf2(g1,g2)
# kernel density estimate + rug plot + superimposed deciles
kde <- plot.kde_rug_dec2(df)
# compute shift function
out <- shifthd( g1, g2, nboot=200)
# plot shift function
sf <- plot.sf(data=out) # function from rgar_visualisation.txt

# combine KDE + SF

kde / sf
ggsave(filename = "images/shiftfun-3.png")






# -----------------------------------
## example 8: test-retest ----
# -----------------------------------
# ses1 contains ERP onsets from 120 participants
# we only use participants tested twice
x <- read.table("data/onset.txt")
twoses <- scan("data/onset_twoses.txt")
ses1 <- x[twoses==1,3]
# ses2 contains onsets from 74 participants who also provided ses1 onsets
x <- read.table("onset2.txt")
ses2 <- x[,3]

# make data frame
df <- mkdf2(ses1,ses2)
# kernel density estimate + rug plot + superimposed deciles
kde <- plot.kde_rug_dec2(df) + scale_x_continuous(breaks = seq(50,200,25))
# compute shift function
set.seed(4)
out <- shiftdhd( ses1, ses2, nboot=200)
# plot shift function
sf <- plot.sf(data=out) +
    scale_x_continuous(breaks = seq(70,120,10))
# combine KDE + SF
plot_grid(kde, sf, labels=c("A", "B"), ncol = 1, nrow = 2, rel_heights = c(1.5, 1),label_size = 18,hjust = -1,scale=.95)

kde / sf

# save figure
ggsave(filename='shift_function_ex8_onsets.jpeg') #path=pathname

# with KDE of pairwise differences ----------------------------------
diff <- ses1-ses2 # pairwise differences
df1 <- mkdf1(diff) # make data frame
kde1 <- plot.kde_rug_dec1(df1) + xlab("Paired differences") +
    scale_x_continuous(breaks = seq(-50,60,10))

# combine KDE + KDE(difference) + SF
plot_grid(kde, kde1, sf, labels=c("A", "B", "C"), ncol = 1, nrow = 3, rel_heights = c(1.5, 1, 1),label_size = 18,hjust = -1,scale=.95)
# save figure
ggsave(filename='shift_function_ex9_onsets_diff.jpeg') #path=pathname
