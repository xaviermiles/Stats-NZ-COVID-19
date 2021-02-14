# Exploring data

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)


all_data <- read_csv("covid_19_data_portal.csv")

all_data %<>%
  mutate(class = as.factor(class),
         date_last_updated = as.Date(date_last_updated, format="%d-%m-%y"))


demographics <- all_data %>%
  filter()