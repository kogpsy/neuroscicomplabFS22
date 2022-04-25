

library(tidyverse)








## load single CSV file ----
testdata <- read_csv("testdata/ZZ_rdk-discrimination_2022_Mar_07_1403.csv")



testdata

View(testdata)


filter()


## get rid of practice block ----
testdata <- testdata |>
    filter(!is.na(main_blocks_loop.thisN))



select()


testdata <- testdata |>
    select(-contains("practice_block_loop"))

testdata


## get rid of variables ----
testdata <- testdata |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))














## rename variables ----
testdata <- testdata |>
    select(trial= main_blocks_loop.thisN,
           ID = Pseudonym,
           cue,
           direction,
           response = dots_keyboard_response.keys,
           rt = dots_keyboard_response.rt)











mutate()



## recode response variable ----
testdata <- testdata |>
    mutate(choice = if_else(response == "j",
                            "right", "left"),
           response = if_else(choice == "right",
                              1, 0))

testdata

## create cue condition variable ----
testdata <- testdata |>
    mutate(condition = case_when(cue == "none" ~ "neutral",
                                 cue == direction ~ "valid",
                                 cue != direction ~ "invalid"))


testdata


## create correct variable ----
testdata <- testdata |>
    # mutate(correct = choice == direction)
    mutate(correct = if_else(choice == direction, 1, 0))





glimpse(testdata)

testdata |>
    mutate(ID = as.factor(ID))

## convert grouping variables to factor ----
testdata <- testdata |>
    mutate_if(is.character, as.factor)










testaccuracy <- testdata |>
    group_by(condition) |>
    summarise(N = n(),
              ncorrect = sum(correct),
              accuracy = ncorrect/N,
              accuracy2 = mean(correct))

testaccuracy



## load all CSV files ----

## create a function that works on one file --
import_fun <- function(filename) {
    read_csv(filename) |>
        mutate(filename = basename(filename)) |>
        filter(!is.na(main_blocks_loop.thisN)) |>
        select(-contains("practice_block_loop"))
}








## list files ----
datadir <- "data"

datadir |>
    list.files(pattern = "csv")
datadir |>
    list.files(pattern = "csv",
               full.names = TRUE)

list_of_files <- datadir |>
    list.files(pattern = "csv",
               recursive = TRUE,
               full.names = TRUE)



list_of_files


## apply function to each element of list_of_files ----
data <- list_of_files |>
    map_dfr(~import_fun(.))



# list_of_files |>
#     map(~import_fun(.))
#


# f <- function(x) rnorm(1, x, n = 10)
#
# sigmas <- c(1, 2, 4)
# sigmas |> map(f)


## select, rename, create variables ----
data <- data |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))


data <- data |>
    select(trial = main_blocks_loop.thisN,
           ID = Pseudonym,
           cue,
           direction,
           response = dots_keyboard_response.keys,
           rt = dots_keyboard_response.rt)

data <- data |>
    mutate(choice = if_else(response == "j", "right", "left"),
           response = if_else(choice == "right", 1, 0))



## cue condition ----

data <- data |>
    mutate(condition = case_when(cue == "none" ~ "neutral",
                                 cue == direction ~ "valid",
                                 cue != direction ~ "invalid"))

## create correct variable ----
data <- data |>
    mutate(correct = choice == direction)


data <- data |>
    mutate(correct2 = if_else(choice == direction, 1, 0))

data <- data |>
    mutate(correct3 = as.numeric(correct))

data <- data |>
    mutate(correct = as.numeric(choice == direction))


## convert grouping variables to factor ----
data <- data |>
    mutate_if(is.character, as.factor)



## accuracy ----
accuracy <- data |>
    group_by(ID, condition) |>
    summarise(accuracy = mean(correct))



## visualize ----
install.packages("esquisse")
library(esquisse)
esquisser(accuracy)
