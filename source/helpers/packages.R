# Packages used across the pipeline.
#   tidyverse    - dplyr / tidyr / purrr / ggplot2 / stringr / lubridate
#   furrr        - parallel purrr, for per-state fitting
#   arrow        - parquet I/O
#   scoringutils - WIS scoring

library(tidyverse)
library(furrr)
library(arrow)
library(scoringutils)
