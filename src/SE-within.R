library(tidyverse)

dfw <- tribble(
 ~subject, ~pretest, ~posttest,
       1,   59.4,     64.5,
       2,   46.4,     52.4,
       3,   46.0,     49.7,
       4,   49.0,     48.7,
       5,   32.5,     37.4,
       6,   45.2,     49.5,
       7,   60.3,     59.9,
       8,   54.3,     54.1,
       9,   45.4,     49.6,
      10,   38.9,     48.5) |>
    mutate(subject = as.factor(subject))


dfl <- dfw |>
    pivot_longer(contains("test"),
                 names_to = "condition",
                 values_to = "value") |>
    mutate(condition = as_factor(condition))

dflsum <- dfl |>
    Rmisc::summarySEwithin(measurevar = "value",
                               withinvars = "condition",
                               idvar = "subject",
                               na.rm = FALSE,
                               conf.interval = 0.95)

dflsum |>
    ggplot(aes(x = condition, y = value, group = 1)) +
    geom_line() +
    geom_errorbar(width = 0.1, aes(ymin = value-ci, ymax = value+ci)) +
    geom_point(shape = 21, size = 3, fill = "white") +
    ylim(40,60)


# Use a consistent y range
ymax <- max(dfl$value)
ymin <- min(dfl$value)

# Plot the individuals
dfl |>
    ggplot(aes(x=condition, y=value, colour=subject, group=subject)) +
    geom_line() + geom_point(shape=21, fill="white") +
    ylim(ymin,ymax)

dfNorm_long <- Rmisc::normDataWithin(data=dfl, idvar="subject", measurevar="value")
?Rmisc::normDataWithin

dfNorm_long |>
    ggplot(aes(x=condition, y=valueNormed, colour=subject, group=subject)) +
    geom_line() + geom_point(shape=21, fill="white") +
    ylim(ymin,ymax)



# Instead of summarySEwithin, use summarySE, which treats condition as though it were a between-subjects variable
dflsum_between <- Rmisc::summarySE(data=dfl, measurevar="value", groupvars="condition", na.rm=FALSE, conf.interval=.95)
dflsum_between
#>   condition  N value       sd       se       ci
#> 1   pretest 10 47.74 8.598992 2.719240 6.151348
#> 2  posttest 10 51.43 7.253972 2.293907 5.189179

# Show the between-S CI's in red, and the within-S CI's in black
dflsum_between |>
    ggplot(aes(x=condition, y=value, group=1)) +
    geom_line() +
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), colour="red") +
    geom_errorbar(width=.1, aes(ymin=value-ci, ymax=value+ci), data=dflsum) +
    geom_point(shape=21, size=3, fill="white") +
    ylim(ymin,ymax)
