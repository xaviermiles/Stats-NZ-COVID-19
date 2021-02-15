# Exploratory data analysis

# > highchart ?

# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)
library(magrittr)
library(ggthemes)


raw_portal_data <- read_csv("covid_19_data_portal.csv")

portal_data <- raw_portal_data %>%
  mutate(class = as.factor(class),
         category = as.factor(category),
         indicator_name = as.factor(indicator_name),
         date_last_updated = as.Date(date_last_updated, format = "%d-%m-%y"))


## Look at jobseeker support by ethnicity --------------------------------------

# helper function for the two plots
get_jobseeker_plot <- function(jobseeker_subset) {
  jobseeker_subset %>%
    ggplot(aes(x = date, y = value, group = Ethnicity)) +
    geom_line(aes(color = Ethnicity)) +
    geom_point(aes(color = Ethnicity)) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_date(date_labels = "%b-%y", breaks = "3 months") +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_line(size = .1, color = "gray"),
          panel.grid.minor.y = element_line(size = .1, color = "gray"),
          axis.ticks = element_blank(),
          panel.background = element_blank(),
          legend.key = element_blank(),
          plot.title = element_text(hjust = 0.5))
}

# Extract jobseeker (work ready) by ethnicity data from overall dataset
jobseeker_data <- portal_data %>%
  filter(indicator_name == "Jobseeker support by ethnicity - work ready") %>%
  select(series_name, parameter, value, date_last_updated) %>%
  rename(Ethnicity = series_name, date = parameter) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Plot jobseeker data broken down by ethnicity
ethnic_order <- c("NZ European", "M\u0101ori", "Pacific peoples",
                  "All other ethnicities")
jobseeker_ethnic <- jobseeker_data %>%
  filter(Ethnicity != "Total - work ready") %>%
  mutate(Ethnicity = factor(Ethnicity, levels = ethnic_order))

jobseeker_ethnic_plot <- get_jobseeker_plot(jobseeker_ethnic) +
  labs(title = "Number on jobseeker support (work ready)", x = "", y = "Number") +
  scale_y_continuous(labels = function(x) {paste(x / 1000, "k", sep = "")}, 
                     limits = c(0, 60000))

jobseeker_ethnic_plot

# Plot jobseeker data broken down by ethnicity, as percentages of total number
# of jobseeker support at each timepoint
jobseeker_total <- jobseeker_data %>%
  filter(Ethnicity == "Total - work ready")

jobseeker_ethnic_perc <- jobseeker_ethnic %>%
  group_by(Ethnicity) %>%
  mutate(value = value / jobseeker_total$value)

jobseeker_ethnic_perc_plot <- get_jobseeker_plot(jobseeker_ethnic_perc) + 
  labs(title = "Percentages on jobseeker support (work ready)", 
       x = "", y = "Percentage") +
  scale_y_continuous(labels = function(x) {paste(100 * x, "%", sep = "")},
                     limits = c(0, 0.5))

jobseeker_ethnic_perc_plot


## Look at confidence measures -------------------------------------------------

