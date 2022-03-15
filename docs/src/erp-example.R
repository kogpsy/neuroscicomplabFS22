# https://www.mattcraddock.com/blog/2016/11/28/erp-visualization-within-subject-confidence-intervals/


library(ggplot2)
library(reshape2)
library(Rmisc)
library(dplyr)
library(purrr)
library(magrittr)
#
# levCatGA <- read.csv("https://raw.githubusercontent.com/craddm/ExploringERPs/master/levCatObjNon.csv",
#                      header = FALSE)
# names(levCatGA) <- c("Object", "Non-Object", "Time", "Subject")
# levCatGA <- levCatGA[(levCatGA$Time >= -100) & (levCatGA$Time <= 400),]
# levCatGA$Subject <- as.factor(levCatGA$Subject)
# levCatGA <- reshape2::melt(levCatGA, id.vars = c("Subject", "Time"))
# names(levCatGA) <- c("Subject", "Time", "condition", "amplitude")

# levCatGA |> write_csv(file = "data/erp-example.csv")
levCatGA <- read_csv("data/erp-example.csv")

theme_set(theme_classic())

levCat.plot <- ggplot(levCatGA, aes(Time, amplitude))+
    scale_color_brewer(palette = "Set1")


runningT <- levCatGA %>%
    split(.$Time) %>%
    map(~t.test(amplitude~condition, paired = TRUE, data = .))

runningSE <- levCatGA %>%
    split(.$Time) %>%
    map(~ Rmisc::summarySEwithin(data = .,
                                 measurevar = "amplitude",
                                 withinvars = "condition",
                                 idvar = "Subject"))


pvals <- data.frame(
    Time = unique(levCatGA$Time),
    p.value = map_dbl(runningT,"p.value")
)

pvals$crit <- 0+(pvals$p.value <= .05)
pvals$crit[pvals$crit == 0] <- NA

levCat.plot+
    stat_summary(fun.data = mean_cl_boot, geom = "ribbon",
                 aes(fill = condition, colour = condition),
                 linetype = "dashed",alpha = 0.3)+
    guides(fill = "none")+
    stat_summary(fun.y = mean,geom = "line", size = 1, aes(colour = condition))+
    labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")), colour = "")+
    geom_line(data = pvals, aes(x = Time, y = crit-3), na.rm = TRUE, size = 2)+
    geom_vline(xintercept = 0, linetype = "dashed")+
    geom_hline(yintercept = 0, linetype = "dashed")



WSCI <- map_df(runningSE, extract) %>%
    mutate(
        Time = rep(unique(levCatGA$Time), each = 2)
        #Note, you'll have to change 2 to match the number of conditions
    )

levCat.plot+
    geom_ribbon(data = WSCI, aes(ymin = amplitude-ci, ymax = amplitude+ci,
                                 fill = condition, colour = condition),
                linetype="dashed", alpha = 0.3)+
    guides(fill = "none")+
    stat_summary(fun.y = mean, geom = "line", size = 1, aes(colour = condition))+
    labs(x = "Time (ms)", y = expression(paste("Amplitude (",mu,"V)")), colour = "")+
    geom_line(data = pvals, aes(x = Time, y = crit-3),na.rm = TRUE,size = 2)+
    geom_vline(xintercept = 0, linetype = "dashed" )+
    geom_hline(yintercept = 0, linetype = "dashed")
