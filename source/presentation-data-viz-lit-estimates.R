library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
library(forecast)
library(pracma)
library(lubridate)
library(performance)

opt <- list()
opt$gt_data_path_hosp_burden_estimates <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_04142024.parquet"
opt$gt_data_path_CDC <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_LIT_CDC_3.9.parquet" 
opt$gt_data_path_Rees <- "data/optimized_totalHosp_daily/Obs_Exp_totalHosp_daily_LIT_Rees_5.0.parquet"


covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_hosp_burden_estimates)
cdc_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_CDC)
rees_covid_totalHosp_data <- arrow::read_parquet(opt$gt_data_path_Rees)

# cleaning data ---------
# Rees, CDC, and Optimize 
rees_covid_totalHosp_data_join <- rees_covid_totalHosp_data %>% 
  rename(rees_total_hosp_estimate = total_hosp_estimate,
         rees_difference = difference) %>% 
  dplyr::select(date, state, rees_total_hosp_estimate, rees_difference)

cdc_covid_totalHosp_data_join <- cdc_covid_totalHosp_data %>% 
  rename(cdc_total_hosp_estimate = total_hosp_estimate,
         cdc_difference = difference) %>% 
  dplyr::select(date, state, cdc_total_hosp_estimate, cdc_difference)

covid_totalHosp_data_lit <- covid_totalHosp_data %>% 
  left_join(rees_covid_totalHosp_data_join, by = c("state", "date")) %>% 
  left_join(cdc_covid_totalHosp_data_join, by = c("state", "date")) %>% 
  select(date, state, total_hosp, total_hosp_estimate, difference, rees_total_hosp_estimate, rees_difference, cdc_total_hosp_estimate, cdc_difference)


##############################################
# Visualizations of hospitalizations
######
#CDC
hosp_viz_2023 <- cdc_covid_totalHosp_data %>%  
  filter(date >= as.Date("2023-01-01") & date <= as.Date("2023-08-31")) %>% 
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1, scales = "free_y") +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  scale_x_date(date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Hospitalization Burden Estimates Time Series",
       subtitle = "2023 CDC Mean LoS Estimate: 3.9") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("presentation/images/hosp_viz_CDC_2023.png", plot = hosp_viz_2023, width = 10, height = 8, dpi = 300)

hosp_viz <- cdc_covid_totalHosp_data %>%  
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1, scales = "free_y")  +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Hospitalization Burden Estimates Time Series",
       subtitle = "All Time CDC Mean LoS Estimate: 3.9") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("presentation/images/hosp_viz_CDC.png", plot = hosp_viz, width = 10, height = 8, dpi = 300)

######
#Rees
hosp_viz_rees <- rees_covid_totalHosp_data %>%  
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1, scales = "free_y")  +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Hospitalization Burden Estimates Time Series",
       subtitle = "All Time Rees et al Mean LoS Estimate: 5.0") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("presentation/images/hosp_viz_Rees.png", plot = hosp_viz_rees, width = 10, height = 8, dpi = 300)

hosp_viz_2020 <- rees_covid_totalHosp_data %>%  
  filter(date <= as.Date("2020-12-31")) %>% 
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1, scales = "free_y") +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  scale_x_date(date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Hospitalization Burden Estimates Time Series",
       subtitle = "2020 Rees et al Mean LoS Estimate: 5.0") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("presentation/images/hosp_viz_Rees_2020.png", plot = hosp_viz_2020, width = 10, height = 8, dpi = 300)

######

hosp_compare <- covid_totalHosp_data_lit %>%  
  filter(state == "MD") %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed"), color = "black", alpha = 0.8) +
  geom_line(aes(y = total_hosp_estimate, color = "Optimization"), linetype = "dashed") +
  geom_line(aes(y = rees_total_hosp_estimate, color = "Systematic Review"), linetype = "dashed") +
  geom_line(aes(y = cdc_total_hosp_estimate, color = "CDC"), linetype = "dashed") +  
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(color = "Difference (Observed - Expected)", 
       title = "Maryland Hospitalization Burden Estimates Time Series",
       subtitle = "Comparison of LOS Sources") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("Observed" = "solid", "Optimization" = "dotted", "Systematic Review" = "dashed", "CDC" = "dashed"), 
                        labels = c("Observed Hospitalizations", "Optimization Estimated Hospitalizations", "Rees Estimated Hospitalizations", "CDC Estimated Hospitalizations")) +
  scale_color_manual(name = "LOS Estimate Source", 
                     values = c("Optimization" = "#33a02c", "Systematic Review" = "#1f78b4", "CDC" =  "#e31a1c"))

ggsave("presentation/images/hosp_viz_comparison.png", plot = hosp_compare, width = 10, height = 8, dpi = 300)


### F test
standard_variance <- var(covid_totalHosp_data_lit$difference)
standard_variance
sysreview_variance <- var(covid_totalHosp_data_lit$rees_difference)
sysreview_variance
cdc_variance <- var(covid_totalHosp_data_lit$cdc_difference)
cdc_variance

var(covid_totalHosp_data_lit$total_hosp, covid_totalHosp_data_lit$total_hosp_estimate)

var(covid_totalHosp_data_lit$total_hosp_estimate, covid_totalHosp_data_lit$total_hosp)

var.test(covid_totalHosp_data_lit$rees_difference, covid_totalHosp_data_lit$difference) 
var.test(covid_totalHosp_data_lit$cdc_difference, covid_totalHosp_data_lit$difference) 

# Paper figures -------------
figure1_LitOptBurden_State <- covid_totalHosp_data_lit %>%  
  mutate(state_names = case_when(
    state == "MD" ~ "Maryland",
    state == "PA" ~ "Pennsylvania",
    state == "NY" ~ "New York",
    state == "NJ" ~ "New Jersey")) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed"), color = "black", alpha = 0.8) +
  geom_line(aes(y = total_hosp_estimate, color = "Optimization"), linetype = "dashed") +
  geom_line(aes(y = rees_total_hosp_estimate, color = "Systematic Review"), linetype = "dashed") +
  geom_line(aes(y = cdc_total_hosp_estimate, color = "CDC"), linetype = "dashed") +  
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  labs(title = "Comparison of Reported and Estimated Inpatient Hospitalizations with Varying Length of Stay Estimates") +
  scale_color_manual(name = "Source of Length of Stay Estimate", 
                     values = c("Optimization" = "#33a02c", "Systematic Review" = "#1f78b4", "CDC" =  "#e31a1c")) +
  scale_linetype_manual(name = "Line Type", 
                        values = c("Observed" = "solid", "Optimization" = "dotted", "Systematic Review" = "dashed", "CDC" = "dashed"), 
                        labels = c("Observed Hospitalizations", "Optimization Estimated Hospitalizations", "Rees Estimated Hospitalizations", "CDC Estimated Hospitalizations")) +
  facet_wrap(~state_names, nrow = 2, scales = "free_y")

ggsave("capstone figures/figure1_hosp_burden.png", plot = figure1_LitOptBurden_State, width = 10, height = 8, dpi = 300)









