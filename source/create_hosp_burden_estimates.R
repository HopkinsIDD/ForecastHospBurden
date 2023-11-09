


# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(tidyverse)
library(readr)
library(lubridate)
library(flepicommon)


# source data functions
source("source/data_setup_source.R")

opt <- list()
opt$gt_data_source <- "hhs_hosp"
opt$delphi_api_key <- "04e7369e1541a"
opt$gt_data_path <- "data/nj_covid_hosp.parquet"

source("source/pull_empirical_data.R")



# LOAD DATA ---------------------------------------------------------------

# only need to run this if want to update data
#nj_data <- arrow::read_parquet(opt$gt_data_path)





# PLOT DATA ---------------------------------------------------------------

nj_data %>%
    ggplot(aes(x = date, y = incidH, color = pathogen)) + 
    geom_line() +
    facet_wrap(~source, ncol = 1)




# BUILD SIMPLE EXAMPLE BURDEN ESTIMATOR -----------------------------------


