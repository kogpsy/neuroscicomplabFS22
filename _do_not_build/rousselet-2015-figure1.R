# R code to reproduce Figure 1 of the European Journal of Neuroscience editorial
# A few simple steps to improve the description of group results in neuroscience
# by Guillaume A. Rousselet, John J. Foxe and J. Paul Bolam

# Copyright (C) 2016 Guillaume Rousselet - University of Glasgow

# R version 3.3.0 (2016-05-03)
# required packages:
# ggplot2 version 2.1.0
# cowplot 0.6.2

library(ggplot2)
library(cowplot)

# set working directory to the "figure1" folder
source("_do_not_build/rgar_stats.R") # file also available here: https://github.com/GRousselet/rstats

# load data
data <- read.csv("data/rousselet-2015-data.csv",header=TRUE)
if (dim(data)[2] == 5){data <- data[,2:5]}
# make participant column a factor, other repeated-measure ANOVA won't work
data$participant <- factor(data$participant)

# suppress the useless significance stars
options(show.signif.stars=FALSE)

# ----------------------------------------------------
# Group statistics -----------------------------------
aov_gp_cond <- aov(value ~ group*condition + Error(participant/condition), data=data)
summary(aov_gp_cond)

# group 1 t-test
y <- data[data$group=="group1" & data$condition=="condition1" ,"value"]
x <- data[data$group=="group1" & data$condition=="condition2" ,"value"]
t.test(x,y,paired=T)

# group 2 t-test
y <- data[data$group=="group2" & data$condition=="condition1" ,"value"]
x <- data[data$group=="group2" & data$condition=="condition2" ,"value"]
t.test(x,y,paired=T)

# ----------------------------------------------------
# bar graph ------------------------------------------
datas <- summarySE(data, measurevar="value", groupvars=c("group","condition"))
bargraph <- ggplot(datas, aes(x=group, y=value, fill=condition)) +
  theme_bw() +
  geom_bar(position=position_dodge(), stat="identity", colour="black") +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  scale_fill_manual(values = c("grey60", "grey90")) +
theme(axis.text.x = element_text(colour="grey20",size=16),
      axis.text.y = element_text(colour="grey20",size=16),
      axis.title.x = element_blank(),
      axis.title.y = element_text(colour="grey20",size=16),
      legend.title = element_blank(),
      legend.text = element_text(colour="grey20",size=16),
      legend.key = element_rect(colour = "black",size=.1),
      legend.position = c(.17, .92),
      # plot.margin = unit(c(150,100,5.5,5.5), "pt"),
      plot.margin = unit(c(5.5,5.5,5.5,5.5), "pt"),
      plot.title = element_text(colour="grey20",size=20)) +
      labs(title="Mean +/- SEM") +
      scale_y_continuous(limits=c(0, 15),breaks=seq(0,15,5)) +
  ylab("Scores (a.u.)")
bargraph

# ----------------------------------------------------
# 1D scatterplots = stripcharts of differences -------
value <- data$value[data$condition=="condition2"]-data$value[data$condition=="condition1"]
participant <- data$participant[data$condition=="condition2"]
group <- data$group[data$condition=="condition2"]
diff <- data.frame(participant,group,value)
# ggplot(diff, aes(x=group, y=value)) +
#   geom_point(shape = 21, colour = 'red', fill = "white", size = 3, stroke = 1)
set.seed(8)
diffstrip <- ggplot(diff, aes(x=group, y=value, fill=group, colour=group, shape=group)) +
  geom_abline(intercept = 0, slope = 0) +
  geom_jitter(position=position_jitter(0.3), size=4, stroke=1) +
  theme_bw() +
  scale_shape_manual(values=c(22,21)) +
  scale_fill_manual(values = c("grey70", "grey95")) +
  scale_colour_manual(values = c("grey5","grey5")) +
  theme(legend.position="none",
        axis.text.x = element_text(colour="grey20",size=16),
        axis.text.y = element_text(colour="grey20",size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour="grey20",size=16),
        plot.title = element_text(colour="grey20",size=20),
        plot.margin = unit(c(5.5,5.5,5.5,5.5), "pt")) +
        # plot.margin = unit(c(150,200,5.5,5.5), "pt")
  labs(title="Differences: condition2 - condition1") +
  scale_y_continuous(limits=c(-2.5, 3.5),breaks=seq(-2,3,1)) +
  ylab("Score differences (a.u.)")
diffstrip
# margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")
# theme_grey()

# ----------------------------------------------------
# stripchart + linked observations -----------------
p <- ggplot(data, aes(x=condition, y=value, fill=condition, group=participant,shape=group)) +
  theme_bw() +
  geom_line(size=1, alpha=1) +
  geom_point(colour = "black", size = 4, stroke = 1) +
  scale_shape_manual(values=c(22,21)) +
  scale_fill_manual(values = c("grey60", "grey90")) +
  theme(axis.text.x = element_text(colour="grey20",size=14),
        axis.text.y = element_text(colour="grey20",size=16),
        axis.title.x = element_blank(),
        axis.title.y = element_text(colour="grey20",size=16),
        plot.title = element_text(colour="grey20",size=20),
        legend.position="none",
        # plot.margin = unit(c(150,100,5.5,5.5), "pt")
        plot.margin = unit(c(5.5,5.5,5.5,5.5), "pt")) +
        labs(title="Paired observations") +
  scale_y_continuous(limits=c(0, 20),breaks=seq(0,20,5)) +
  ylab("Scores (a.u.)")
# p
linkedstrip <- p + facet_grid(. ~ group) +
  theme(strip.text.x = element_text(size = 20, colour = "white"),
        strip.background = element_rect(colour="darkgrey", fill="darkgrey"))
linkedstrip

# ----------------------------------------------------
# scatterplot of paired observations -----------------
condition1 <- data$value[data$condition=="condition1"]
condition2 <- data$value[data$condition=="condition2"]
participant <- data$participant[data$condition=="condition2"]
group <- data$group[data$condition=="condition2"]
rdata <- data.frame(participant,group,condition1,condition2)
scatterdiff <- ggplot(rdata, aes(x=condition1,y=condition2,group=group,fill=group,colour=group,shape=group)) +
  geom_abline(intercept = 0) +
  geom_point(size=4,stroke=1) +
  theme_bw() +
  scale_shape_manual(values=c(22,21)) +
  scale_fill_manual(values = c("grey70", "grey95")) +
  scale_colour_manual(values = c("grey5","grey5")) +
  theme(axis.text.x = element_text(colour="grey20",size=16),
        axis.text.y = element_text(colour="grey20",size=16),
        axis.title.x = element_text(colour="grey20",size=18),
        axis.title.y = element_text(colour="grey20",size=18),
        legend.title = element_blank(),
        legend.position = c(.15, .92),
        # plot.margin = unit(c(150,100,5.5,5.5), "pt"),
        plot.margin = unit(c(5.5,5.5,5.5,5.5), "pt"),
          legend.text = element_text(colour="grey20",size=16),
        plot.title = element_text(colour="grey20",size=20)) +
  labs(title="Paired observations") +
  scale_x_continuous(limits=c(6, 16),breaks=seq(6,16,2)) +
  scale_y_continuous(limits=c(6, 19),breaks=seq(6,19,2))
scatterdiff

# ----------------------------------------------------
# Make final figure combining 4 subplots --------------

set.seed(8)
plot_grid(bargraph, diffstrip, linkedstrip, scatterdiff, labels=c("A", "B", "C", "D"),
          ncol = 2, nrow = 2, rel_heights = c(1, 1),label_size = 18,hjust = -1,scale=.95)

ggsave(filename='images/rousselet2016.tiff',width=11,height=11)
# ggplot2::ggsave()
