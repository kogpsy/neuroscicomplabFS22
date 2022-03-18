## ----setup, include=FALSE---------------------------------------
knitr::opts_chunk$set(echo = TRUE)


## ---- xaringanExtra-clipboard, echo=FALSE, include=FALSE--------
htmltools::tagList(
  xaringanExtra::use_clipboard(
    button_text = "<i class=\"fa fa-clone fa-2x\" style=\"color: #301e64\"></i>",
    success_text = "<i class=\"fa fa-check fa-2x\" style=\"color: #90BE6D\"></i>",
    error_text = "<i class=\"fa fa-times fa-2x\" style=\"color: #F94144\"></i>"
  ),
  rmarkdown::html_dependency_font_awesome()
)


## ----echo=TRUE, message=FALSE, warning=FALSE--------------------
library(tidyverse)


## ----echo=TRUE--------------------------------------------------
testdata <- read_csv("data/rdktest/ZZ_rdk-discrimination_2022_Mar_07_1403.csv") 


## ---------------------------------------------------------------
glimpse(testdata)


## ---------------------------------------------------------------
library(kableExtra)

testdata |> 
  slice_head(n = 12) |> 
  kbl() |> 
  kable_paper("striped", full_width = FALSE) |> 
  column_spec(2:7, bold = TRUE) |> 
  row_spec(1:6, bold = TRUE, color = "white", background = "#D7261E")


## ---------------------------------------------------------------
testdata |> 
  slice_head(n = 12) |> 
  select(starts_with("main_block")) |> 
  kbl() |> 
  kable_paper("striped", full_width = FALSE) |> 
  row_spec(1:7, bold = TRUE, color = "white", background = "#D7261E")


## ---------------------------------------------------------------
testdata |> 
    filter(!is.na(main_blocks_loop.thisN)) |>
    select(-contains("practice_block_loop"))


## ---------------------------------------------------------------
testdata |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))


## ---------------------------------------------------------------
testdata <- testdata |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))


## ---------------------------------------------------------------
testdata


## ---------------------------------------------------------------
testdata <- testdata |>
    select(trial = main_blocks_loop.thisN,
           ID = Pseudonym,
           cue,
           direction,
           response = dots_keyboard_response.keys,
           rt = dots_keyboard_response.rt)



## ---------------------------------------------------------------
testdata


## ---------------------------------------------------------------
testdata <- testdata |>
    mutate(choice = if_else(response == "j", "right", "left"),
           response = if_else(choice == "right", 1, 0))


## ----eval=FALSE, include=TRUE-----------------------------------
## testdata <- testdata |>
##     mutate(choice = if_else(response == "j", "right", "left"),
##            response = as.numeric(choice == "right"))


## ---------------------------------------------------------------
testdata <- testdata |>
    mutate(condition = case_when(cue == "none" ~ "neutral",
                                 cue == direction ~ "valid",
                                 cue != direction ~ "invalid"))


## ---------------------------------------------------------------
testdata <- testdata |>
    mutate(correct = as.numeric(choice == direction))


## ---------------------------------------------------------------
glimpse(testdata)


## ---------------------------------------------------------------
testdata <- testdata |>
    mutate_if(is.character, as.factor)


## ---------------------------------------------------------------
glimpse(testdata)


## ---------------------------------------------------------------
testaccuracy <- testdata |>
    group_by(condition) |>
    summarise(N = n(),
              ncorrect = sum(correct),
              accuracy = ncorrect/N,
              accuracy2 = mean(correct))

testaccuracy


## ---------------------------------------------------------------
import_function <- function(filename) {
    read_csv(filename) |>
        mutate(filename = basename(filename)) |>
        filter(!is.na(main_blocks_loop.thisN)) |>
        select(-contains("practice_block_loop"))
}


## ---------------------------------------------------------------
datadir <- "data/rdkdata/"
list_of_files <- datadir |>
    list.files(pattern = "csv", recursive = TRUE, full.names = TRUE)


## ---------------------------------------------------------------
list_of_files


## ----message=FALSE, warning=FALSE-------------------------------
data <- list_of_files |> 
    map_dfr(~import_function(.))


## ---------------------------------------------------------------
data <- data |>
    select(-contains("static"),
           -contains("fixation"),
           -contains("image"),
           -contains("instruction"),
           -contains("feedback"))


## ---------------------------------------------------------------
data <- data |>
    select(trial = main_blocks_loop.thisN,
           ID = Pseudonym,
           cue,
           direction,
           response = dots_keyboard_response.keys,
           rt = dots_keyboard_response.rt)



## ---------------------------------------------------------------
data <- data |>
    mutate(choice = if_else(response == "j", "right", "left"),
           response = if_else(choice == "right", 1, 0))



## ---------------------------------------------------------------
data <- data |>
    mutate(correct = as.numeric(choice == direction))


## ---------------------------------------------------------------
glimpse(data)


## ---------------------------------------------------------------
data |> 
  slice_head(n = 20)


## ---------------------------------------------------------------
data <- data |>
    mutate(condition = case_when(cue == "none" ~ "neutral",
                                 cue == direction ~ "valid",
                                 cue != direction ~ "invalid"))


## ---------------------------------------------------------------
data |> write_csv(file = "data/rdkdata.csv")


## ---------------------------------------------------------------
data |> 
  slice_head(n = 20)


## ---------------------------------------------------------------
data <- data |>
    mutate_if(is.character, as.factor)


## ---------------------------------------------------------------
glimpse(data)


## ---------------------------------------------------------------
accuracy <- data |>
    group_by(ID, condition) |>
    summarise(N = n(),
              ncorrect = sum(correct),
              accuracy = mean(correct))



## ---------------------------------------------------------------
accuracy


## ----fig.height=12, fig.width=15--------------------------------
accuracy |> 
  ggplot(aes(x = condition, y = accuracy, fill = condition)) +
  geom_col() +
  scale_fill_manual(
    values = c(invalid = "#9E0142",
    neutral = "#C4C4B7",
    valid = "#2EC762")
  ) +
  labs(
    x = "Cue",
    y = "Proportion correct",
    title = "Accuracy per person/condition"
  ) +
  theme_linedraw(base_size = 28) +
  facet_wrap(~ID)

