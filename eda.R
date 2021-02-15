# Exploratory data analysis

# > highchart ?

# Set working directory to wherever this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


library(tidyverse)


raw_portal_data <- read_csv("covid_19_data_portal.csv")

portal_data <- raw_portal_data %>%
  mutate(class = as.factor(class),
         category = as.factor(category),
         indicator_name = as.factor(indicator_name),
         date_last_updated = as.Date(date_last_updated, format = "%d-%m-%y"))

# Theme for ggplots
custom_theme <- theme(panel.grid.major.x = element_blank(),
                      panel.grid.minor.x = element_blank(),
                      panel.grid.major.y = element_line(color = "gray"),
                      panel.grid.minor.y = element_line(color = "gray"),
                      axis.ticks = element_blank(),
                      panel.background = element_blank(),
                      legend.key = element_blank(),
                      legend.position = "bottom",
                      plot.title = element_text(hjust = 0.5))

## Economic measures -----------------------------------------------------------

# Economic activity
activity <- portal_data %>%
  filter(sub_series_name %in% c("New Zealand Activity Index (NZAC)",
                                "Retail sales index")) %>%
  select(-c(class, category, series_name, indicator_name)) %>%
  rename(Indicator = sub_series_name, date = parameter) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date >= as.Date("2019-03-01"))

activity_breaks_seq <- seq(round(min(activity$value / 10)) * 10, 
                           round(max(activity$value / 10)) * 10, 
                           by = 20)

activity_plot <- activity %>%
  ggplot(aes(x = date, y = value, group = Indicator)) +
  geom_line(aes(color = Indicator), size = 1) +
  geom_point(aes(color = Indicator)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Economic Activity in New Zealand", x = "", y = "Annual % change") +
  scale_x_date(date_labels = "%b-%y", breaks = "3 months") +
  scale_y_continuous(labels = function(x) {paste(x, "%", sep = "")}, 
                     breaks = activity_breaks_seq) +
  custom_theme

activity_plot
ggsave("plots/activity.jpeg", activity_plot)


# Confidence measures
confidence <- portal_data %>%
  filter(indicator_name == "Economic sentiment" |
           (indicator_name == "Manufacturing index" & 
              series_name == "Performance of manufacturing (overall index)")) %>%
  select(-c(class, category, series_name, sub_series_name)) %>%
  rename(Indicator = indicator_name, date = parameter) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  filter(date >= as.Date("2017-02-28"))

confidence_plot <- confidence %>%
  ggplot(aes(x = date, y = value, group = Indicator)) +
  geom_line(aes(color = Indicator)) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Confidence metrics", x = "", y = "Index") +
  scale_x_date(date_labels = "%b-%y", breaks = "3 months") +
  custom_theme

confidence_plot
ggsave("plots/confidence.jpeg")


## Jobseeker support by ethnicity ----------------------------------------------

# helper function for the two jobseeker plots
get_jobseeker_plot <- function(jobseeker_subset) {
  jobseeker_subset %>%
    ggplot(aes(x = date, y = value, group = Ethnicity)) +
    geom_line(aes(color = Ethnicity), size = 1) +
    geom_point(aes(color = Ethnicity)) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_date(date_labels = "%b-%y", breaks = "3 months") +
    custom_theme
}

# Extract 'jobseeker (work ready) by ethnicity' data from overall dataset
jobseeker_data <- portal_data %>%
  filter(indicator_name == "Jobseeker support by ethnicity - work ready") %>%
  select(series_name, parameter, value, units, date_last_updated) %>%
  rename(Ethnicity = series_name, date = parameter) %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

# Plot per-ethnicity jobseeker data
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
ggsave("plots/jobseeker_ethnic.jpeg")


# Plot jobseeker data, with the per-ethnicity percentages of the total number
# of jobseeker beneficiaries at each timepoint
jobseeker_total <- jobseeker_data %>%
  filter(Ethnicity == "Total - work ready")

jobseeker_ethnic_perc <- jobseeker_ethnic %>%
  group_by(Ethnicity) %>%
  mutate(value = value / jobseeker_total$value)

jobseeker_ethnic_perc_plot <- get_jobseeker_plot(jobseeker_ethnic_perc) + 
  labs(title = "Percentage of total jobseeker population (work ready)", 
       x = "", y = "Percentage") +
  scale_y_continuous(labels = function(x) {paste(100 * x, "%", sep = "")},
                     limits = c(0, 0.5))

jobseeker_ethnic_perc_plot
ggsave("plots/jobseeker_ethnic_perc.jpeg")


# Combine the two jobseeker plots
jobseeker_combined_plot <- ggpubr::ggarrange(jobseeker_ethnic_plot,
                                             jobseeker_ethnic_perc_plot,
                                             common.legend = TRUE,
                                             legend = "bottom",
                                             heights = c(4, 20))
jobseeker_combined_plot
ggsave("plots/jobseeker_combined.jpeg", width = 14, height = 7)


## Social indicators -----------------------------------------------------------

overall_wellbeing <- portal_data %>%
  filter(indicator_name == "Overall wellbeing") %>%
  select(sub_series_name, parameter, value) %>%
  spread(sub_series_name, value) %>%
  rename(date = parameter) %>%
  mutate(date = as.Date(date, format = "%d-%m-%y"))

overall_wellbeing_plot <- overall_wellbeing %>%
  ggplot(aes(x = date, y = `Percentage of respondents`)) +
  geom_line(color = "red", size = 1) +
  geom_point() + 
  geom_ribbon(aes(ymin = `Percentage of respondents_lower`,
                  ymax = `Percentage of respondents_upper`), 
              alpha = 0.3, fill = "grey60") +
  labs(title = "Respondents who said their overall wellbeing at the current alert level is worse than usual", x = "", y = "Percentage") +
  scale_x_date(date_labels = "%d-%b", breaks = "2 weeks") +
  scale_y_continuous(labels = function(x) {paste(x, "%", sep = "")}, 
                     limits = c(0, 30)) +
  custom_theme

overall_wellbeing_plot
ggsave("./plots/overall_wellbeing.jpeg")
