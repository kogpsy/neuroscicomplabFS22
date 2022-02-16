
library(tidyverse)
library(readxl)

if (!file.exists("data.zip")) {
    download.file("https://ndownloader.figshare.com/files/1878093", "data.zip")
}
unzip("data.zip")
files <- list.files(
    "Behavioural_data/",
    pattern = "sub[0-9]+.xlsx", full.names = T
)
dat <- map(
    files,
    ~ read_xlsx(.x, range = "A4:G100", col_types = rep("text", 7))
) %>%
    bind_rows(.id = "id")

dat <- dat %>%
    filter(angle %in% c("0", "50")) %>%
    transmute(
        id = factor(id),
        angle = factor(angle),
        rt = as.numeric(Time),
        accuracy = as.numeric(`correct/incorrect`)
    )

