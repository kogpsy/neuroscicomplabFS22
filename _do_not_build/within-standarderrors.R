dfw <- read.table(header=TRUE, text='
     subject pretest posttest
           1    59.4     64.5
           2    46.4     52.4
           3    46.0     49.7
           4    49.0     48.7
           5    32.5     37.4
           6    45.2     49.5
           7    60.3     59.9
           8    54.3     54.1
           9    45.4     49.6
          10    38.9     48.5
     ')
# Treat subject ID as a factor
dfw$subject <- factor(dfw$subject)

dfw <- dfw %>%
    pivot_longer(ends_with("test"),
                 names_to = "condition",
                 values_to = "value") %>%
    mutate(condition = as_factor(condition))


dfwc <- Rmisc::summarySEwithin(dfw,
                               measurevar="value",
                               withinvars="condition",
                               idvar="subject",
                               na.rm=FALSE,
                               conf.interval=.95)


dfwc %>%
    ggplot(aes(x=condition, y=value,group=1))+
    geom_line()+
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci))+
    geom_point(shape=21, size=3, fill="white")+
    ylim(40,60)


## calculation ----

# Use a consistent y range
ymax <- max(dfw$value)
ymin <- min(dfw$value)
# Plot the individuals
ggplot(dfw, aes(x=condition, y=value, colour=subject,group=subject))+
    geom_line()+ geom_point(shape=21, fill="white")+
    ylim(ymin,ymax)
# Create the normed version of the data
dfwNorm.long<- Rmisc::normDataWithin(data=dfw, idvar="subject", measurevar="value")
# Plot the normed individuals
ggplot(dfwNorm.long, aes(x=condition, y=valueNormed, colour=subject,group=subject))+
    geom_line()+ geom_point(shape=21, fill="white")+
    ylim(ymin,ymax)



# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dfwc_between <- Rmisc::summarySE(data=dfw, measurevar="value", groupvars="condition", na.rm=FALSE, conf.interval=.95)
dfwc_between
#>   condition  N value       sd       se       ci
#> 1   pretest 10 47.74 8.598992 2.719240 6.151348
#> 2  posttest 10 51.43 7.253972 2.293907 5.189179
# Show the between-S CI's in red, and the within-S CI's in black
ggplot(dfwc_between, aes(x=condition, y=value,group=1))+
    geom_line()+
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), colour="red")+
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), data=dfwc)+
    geom_point(shape=21, size=3, fill="white")+
    ylim(ymin,ymax)
