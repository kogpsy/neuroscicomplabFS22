library(tidyverse)

## single file ----
testdata <- read_csv("data/ZZ_rdk-discrimination_2022_Mar_07_1403.csv") |>
    filter(!is.na(main_blocks_loop.thisN)) |>
    select(-contains("practice_block_loop"))



## get rid of variables
data <- testdata |>
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

data <- data |>
    mutate_if(is.character, as.factor)



## all files ----
import_fun <- function(filename) {
    read_csv(filename) |>
        mutate(filename = basename(filename)) |>
        filter(!is.na(main_blocks_loop.thisN)) |>
        select(-contains("practice_block_loop"))
}


datadir <- "data"
all_data <- datadir |>
    list.files(pattern = "csv", recursive = TRUE, full.names = TRUE) |>
    map_dfr(~import_fun(.))


all_data <- all_data |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))

all_data <- all_data |>
    select(trial = main_blocks_loop.thisN,
           ID = Pseudonym,
           cue,
           direction,
           response = dots_keyboard_response.keys,
           rt = dots_keyboard_response.rt)

all_data <- all_data |>
    mutate(choice = if_else(response == "j", "right", "left"),
           response = if_else(choice == "right", 1, 0))



## condition ----

all_data <- all_data |>
    mutate(condition = case_when(cue == "none" ~ "neutral",
                                 cue == direction ~ "valid",
                                 cue != direction ~ "invalid"))

all_data <- all_data |>
    mutate(correct = choice == direction)

all_data <- all_data |>
    mutate(correct2 = if_else(choice == direction, 1, 0))

all_data <- all_data |>
    mutate(correct3 = as.numeric(correct))

all_data <- all_data |>
    mutate_if(is.character, as.factor)

all_data <- all_data |>
    mutate(correct = as.numeric(choice == direction))


all_data <- all_data |>
    mutate_if(is.character, as.factor)


accuracy <- all_data |>
    group_by(ID, condition) |>
    summarise(accuracy = mean(correct))


library(esquisse)
esquisser(accuracy)

## other ----

all_files <- "data" |>
    list.files(pattern = "csv", recursive = TRUE, full.names = TRUE) |>
    set_names() |>
    imap_dfr(~ bind_cols(read_csv(.x), filepath = .y))



## ------
tbl <- "data" |>
    list.files(pattern = "csv", recursive = TRUE, full.names = TRUE) |>
    map_df(~read_csv(.))

read_plus <- function(flnm) {
    read_csv(flnm) %>%
        mutate(filename = flnm)
}
tbl_with_sources <- "data" |>
    list.files(pattern = "csv", recursive = TRUE, full.names = TRUE) |>
    map_df(~read_plus(.))

files <- list.files("data", pattern="csv", full.names=TRUE) %>%
    set_names()
merged <- files %>% map_dfr(read_csv, .id="filename")
merged %>% mutate(filename=basename(filename))

## ____


# Load everything into the Global Environment
file_path <- "data/"

csv_file_names <- file_path %>%
    list.files() %>%
    .[str_detect(., ".csv")]

csv_file_names %>%
    purrr::map(function(file_name){ # iterate through each file name
        assign(x = str_remove(file_name, ".csv"), # Remove file extension ".csv"
               value = read_csv(paste0(file_path, file_name)),
               envir = .GlobalEnv)
    })

# Load everything into the Global Environment
csv_file_names %>%
    purrr::map(function(file_name){ # iterate through each file name

        read_csv(paste0(file_path, file_name))

    }) -> df_list_read2 # Assign to a list
