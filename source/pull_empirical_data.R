



# SETUP -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(tidycensus)
library(readr)
library(lubridate)
library(flepicommon)


# source data functions
source("source/data_setup_source.R")

# opt <- list()
# opt$gt_data_source <- "hhs_hosp"
# opt$delphi_api_key <- "04e7369e1541a"
# opt$gt_data_path <- "data/nj_covid_hosp.parquet"

# SET DELPHI API KEY ------------------------------------------------------

if (any(grepl("nchs|hhs", opt$gt_data_source))){
    options(covidcast.auth = opt$delphi_api_key)
}



# PULL DATA ---------------------------------------------------------------
opt$start_date <- as_date("2020-01-01")
opt$end_date <- as_date(Sys.Date())
gt_data <- list()


# ~ Pull HHS hospitalization  -------------------

if (any(grepl("hhs", opt$gt_data_source))){

    us_covid_hosp <- get_covidcast_hhs_hosp(geo_level = "state",
                                      limit_date = Sys.Date())
    
    us_flu_hosp <- get_hhs_flu_hosp(opt)

    us_hosp <- us_covid_hosp %>%
        mutate(pathogen = "COVID-19")  %>%
        rename(date = Update) %>%
        select(-cumH) %>%
        bind_rows(
            us_flu_hosp %>% 
                mutate(pathogen = "Influenza")
        )

    us_hosp <- us_hosp %>%
        # dplyr::select(-incidH_all) %>%
        # rename(incidH = incidH_confirmed) %>%
        mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0")) %>%
        filter(date >= as_date(opt$start_date) & date <= as_date(opt$end_date))

    # # Apply variants
    # if (!is.null(config$seeding$variant_filename)){
    #     variant_props_file <- config$seeding$variant_filename
    #     adjust_for_variant <- !is.null(variant_props_file)
    # 
    #     if (adjust_for_variant) {
    #         tryCatch({
    #             us_hosp <- flepicommon::do_variant_adjustment(us_hosp, variant_props_file)
    #         }, error = function(e) {
    #             stop(paste0("Could not use variant file |", variant_props_file,
    #                         "|, with error message", e$message))
    #         })
    #     }
    # }

    # us_hosp <- us_hosp %>% mutate(gt_source = "csse")
    gt_data <- append(gt_data, list(us_hosp))
}


# ~ Pull Data from Covidcast -------------------

if (any(grepl("csse", opt$gt_data_source))){
    gt_source <- "covidcast"
    gt_scale <- "US state"

    csse_target <- unlist(strsplit(opt$gt_data_source, ", "))
    csse_target <- tolower(gsub("csse_", "", csse_target[grepl("csse", csse_target)]))

    csse_data <- flepicommon::get_groundtruth_from_source(source = gt_source, scale = gt_scale,
                                                          incl_unass = TRUE,
                                                          variables = c("incidC", "cumC", "incidD", "cumD"),
                                                          adjust_for_variant = FALSE,
                                                          variant_props_file = NULL)
    csse_data <- csse_data %>%
        mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0")) %>%
        filter(Update >= as_date(opt$start_date) & Update <= as_date(opt$end_date)) %>%
        # mutate(gt_source = "csse") %>%
        filter()
    colnames(csse_data) <- gsub("Deaths", "cumD", colnames(csse_data))
    colnames(csse_data) <- gsub("incidDeath", "incidD", colnames(csse_data))
    colnames(csse_data) <- gsub("Confirmed", "cumC", colnames(csse_data))
    colnames(csse_data) <- gsub("incidI", "incidC", colnames(csse_data))

    if (!any(grepl("case", csse_target))){
        csse_data <- csse_data %>% select(-c(starts_with("incidC"), starts_with("cumC")))
    }
    if (!any(grepl("death", csse_target))){
        csse_data <- csse_data %>% select(-c(starts_with("incidD"), starts_with("cumD")))
    }

    # Apply variants
    if (!is.null(opt$variant_filename)){
        variant_props_file <- opt$variant_filename
        adjust_for_variant <- !is.null(variant_props_file)
        head(read_csv(variant_props_file))

        if (adjust_for_variant) {

            tryCatch({
                csse_data_vars <- flepicommon::do_variant_adjustment(csse_data, variant_props_file)
            }, error = function(e) {
                stop(paste0("Could not use variant file |", variant_props_file,
                            "|, with error message", e$message))
            })
        }
        csse_data <- csse_data_vars
    }
    gt_data <- append(gt_data, list(csse_data))
}





# ~ Combine ---------------------------------------------------------------

us_data <- gt_data %>%
    purrr::reduce(full_join) %>%
    # filter(source != "US", source != "USA") %>%
    mutate(FIPS = stringr::str_pad(FIPS, width=5, side="right", pad="0"))



# ~ Filter ----------------------------------------------------------------

# Filter to dates we care about for speed and space
us_data <- us_data %>%
    filter(date >= lubridate::as_date(opt$start_date) & date <= lubridate::as_date(opt$end_date))

# Filter to states we care about
locs <- "NJ"
us_data <- us_data %>%
    filter(source %in% locs) %>%
    filter(!is.na(source)) 




# ~ Fix non-numeric -------------------------------------------------------------
#  -- leave NAs so its not assuming an NA is a 0 and fitting to it

us_data <- us_data %>%
    # mutate(across(starts_with("incid"), ~ replace_na(.x, 0))) %>%
    mutate(across(starts_with("incid"), ~ as.numeric(.x)))



# Save
arrow::write_parquet(us_data, opt$gt_data_path)



cat(paste0("Ground truth data saved\n",
           "  -- file:      ", opt$gt_data_path,".\n",
           "  -- outcomes:  ", paste(grep("incid", colnames(us_data), value = TRUE), collapse = ", ")))


# END
