
# Build COVID-19 Death data from NCHS



get_covidcast_deaths <- function (scale = "US state",
                                  source = "jhu-csse", # "hhs", "jhu-csse", "nchs-mortality"
                                  #incl_unass = TRUE,
                                  fix_negatives = TRUE, adjust_for_variant = FALSE,
                                  variant_props_file = "data/variant/variant_props_long.csv",
                                  run_parallel = FALSE,
                                  n_cores = 4){
    
    if (scale == "US state") {
        
        signals <- NULL
        data_source <- NULL
        time_type <- NULL
        
        if ("jhu-csse" %in% source){
            signals <- c(signals, "deaths_incidence_num", "deaths_cumulative_num")
            data_source <- c(data_source, "jhu-csse", "jhu-csse")
            time_type <- c(time_type, "day", "day")
        }
        if ("nchs-mortality" %in% source){
            signals <- c(signals, "deaths_covid_incidence_num")
            data_source <- c(data_source, "nchs-mortality")
            time_type <- c(time_type, "week")
        }
        
        rc <- pull_covidcast_deaths(geo_level = "state",
                                    signals = signals,
                                    data_source = data_source,
                                    time_type = time_type,
                                    limit_date = Sys.Date(),
                                    fix_negatives = fix_negatives,
                                    run_parallel = FALSE,
                                    n_cores = 4) #%>%             dplyr::select(Update, FIPS, source, !!variables)
    }
    
    if (adjust_for_variant) {
        tryCatch({
            rc <- do_variant_adjustment(rc, variant_props_file)
        }, error = function(e) {
            stop(paste0("Could not use variant file |", variant_props_file,
                        "|, with error message", e$message))
        })
    }
    return(rc)
}







#' get_covidcast_data
#'
#' @param geo_level
#' @param signals
#' @param limit_date
#' @param fix_negatives
#' @param run_parallel
#' @param n_cores
#'
#' @return
#' @export
#'
#' @examples
pull_covidcast_deaths <- function(
        geo_level = "state",
        data_source = "jhu-csse", # "hhs", "jhu-csse", "nchs-mortality"
        signals = c("deaths_incidence_num", "deaths_cumulative_num", "confirmed_incidence_num", "confirmed_cumulative_num", "confirmed_admissions_covid_1d"),
        time_type = "day",
        limit_date = Sys.Date(),
        fix_negatives = TRUE,
        run_parallel = FALSE,
        n_cores = 4){
    
    # Create dictionary
    # From the GitHub: https://github.com/reichlab/covid19-forecast-hub
    loc_dictionary <- readr::read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
    loc_abbr <- loc_dictionary %>% dplyr::filter(!is.na(abbreviation))
    loc_dictionary <- loc_dictionary %>%
        dplyr::mutate(state_fips = substr(location, 1, 2)) %>%
        dplyr::select(-abbreviation) %>%
        dplyr::full_join(loc_abbr %>% dplyr::select(abbreviation, state_fips=location))
    
    # in folder data-locations
    loc_dictionary_name <- suppressWarnings(
        setNames(c(rep(loc_dictionary$location_name, 2), "US",
                   rep(loc_dictionary$location_name[-1], 2),
                   rep(loc_dictionary$location_name, 2),
                   "New York"),
                 c(loc_dictionary$location,
                   tolower(loc_dictionary$abbreviation), "US",
                   na.omit(as.numeric(loc_dictionary$location)),
                   as.character(na.omit(
                       as.numeric(loc_dictionary$location))),
                   tolower(loc_dictionary$location_name),
                   toupper(loc_dictionary$location_name),
                   "new york state")))
    
    loc_dictionary_abbr <- setNames(loc_dictionary$abbreviation, loc_dictionary$location)
    loc_dictionary_pop <- setNames(loc_dictionary$population, loc_dictionary$location)
    
    # Set up start and end dates of data to pull
    # -- we pull the data in 6-month chunks to speed up and not overwhelm API for county-level
    
    start_dates <- lubridate::as_date("2020-01-01")
    end_dates <- lubridate::as_date(limit_date)
    
    # Call API to generate gold standard data from COVIDCast
    # Set up parallelization to speed up
    if (run_parallel & length(signals)>1){
        doParallel::registerDoParallel(cores=n_cores)
        `%do_fun%` <- foreach::`%dopar%`
    } else {
        `%do_fun%` <- foreach::`%do%`
    }
    
    
    res <- foreach::foreach(x = 1:length(signals),
                            .combine = rbind,
                            .packages = c("covidcast","dplyr","lubridate", "doParallel","foreach","vroom","purrr"),
                            .verbose = TRUE) %do_fun% {
                                
                                df <- covidcast::covidcast_signal(data_source = data_source[x],
                                                                  signal = signals[x],
                                                                  geo_type = geo_level,
                                                                  start_day = lubridate::as_date(start_dates),
                                                                  end_day = lubridate::as_date(end_dates),
                                                                  time_type = time_type[x]) %>%
                                    as_tibble()
                                
                                if (geo_level=="state"){
                                    df <- df %>% mutate(state_abbr = toupper(geo_value)) %>%
                                        dplyr::select(-geo_value) %>%
                                        dplyr::left_join(loc_dictionary %>%
                                                             dplyr::select(state_abbr=abbreviation, geo_value=location) %>%
                                                             dplyr::filter(stringr::str_length(geo_value)==2))
                                }
                                df <- df %>% dplyr::rename(date = time_value)
                                
                                # Get cum hospitalizations
                                if (x == "confirmed_admissions_covid_1d"){
                                    df_cum <- df %>%
                                        dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
                                        dplyr::arrange(state_abbr, geo_value, date) %>%
                                        dplyr::group_by(data_source, signal, geo_value, state_abbr) %>%
                                        dplyr::mutate(value = cumsum(value)) %>%
                                        dplyr::ungroup() %>%
                                        dplyr::mutate(signal = "confirmed_admissions_cum")
                                    df <- rbind(df, df_cum)
                                }
                                
                                df %>% dplyr::select(signal, Update=date, source=state_abbr, FIPS=geo_value, value)
                                
                            }
    
    res <- res %>%
        dplyr::mutate(target = recode(signal,
                                      "deaths_incidence_num"="incidD",
                                      "deaths_cumulative_num"="cumD",
                                      "deaths_covid_incidence_num"="incidD",
                                      "confirmed_incidence_num"="incidC",
                                      "confirmed_cumulative_num"="cumC",
                                      "confirmed_admissions_covid_1d"="incidH",
                                      "confirmed_admissions_cum"="cumH")) %>%
        dplyr::select(-signal) %>%
        
        tidyr::pivot_wider(names_from = target, values_from = value) %>%
        dplyr::mutate(Update=lubridate::as_date(Update),
                      FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""), # clean FIPS if numeric
                      FIPS = paste0(FIPS, "000")) %>%
        dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
        dplyr::distinct()
    
    res <- res %>% tibble::as_tibble()
    
    # Fix incidence counts that go negative and NA values or missing dates
    if (fix_negatives & any(c("incidC", "incidD", "cumD", "cumC") %in% colnames(res))) {
        res <- res %>%
            flepicommon::fix_negative_counts(cum_col_name = "cumC", incid_col_name = "incidI") %>%
            flepicommon::fix_negative_counts(cum_col_name = "cumD", incid_col_name = "incidD")
    }
    
    return(res)
}




get_covidcast_hhs_hosp <- function(
        geo_level = "state",
        limit_date = Sys.Date()) {
    
    loc_dictionary <- readr::read_csv("https://raw.githubusercontent.com/reichlab/covid19-forecast-hub/master/data-locations/locations.csv")
    loc_abbr <- loc_dictionary %>% dplyr::filter(!is.na(abbreviation))
    loc_dictionary <- loc_dictionary %>%
        dplyr::mutate(state_fips = substr(location, 1, 2)) %>%
        dplyr::select(-abbreviation) %>%
        dplyr::full_join(loc_abbr %>%
                             dplyr::select(abbreviation, state_fips = location))
    # loc_dictionary_name <- suppressWarnings(setNames(
    #     c(rep(loc_dictionary$location_name, 2), "US",
    #       rep(loc_dictionary$location_name[-1], 2),
    #       rep(loc_dictionary$location_name, 2), "New York"),
    #     c(loc_dictionary$location, tolower(loc_dictionary$abbreviation), "US",
    #       na.omit(as.numeric(loc_dictionary$location)), as.character(na.omit(as.numeric(loc_dictionary$location))),
    #     tolower(loc_dictionary$location_name), toupper(loc_dictionary$location_name), "new york state")))
    # loc_dictionary_abbr <- setNames(loc_dictionary$abbreviation, loc_dictionary$location)
    
    if (geo_level == "county") {
        years_ <- lubridate::year("2020-01-01"):lubridate::year(limit_date)
        start_dates <- sort(c(lubridate::as_date(paste0(years_, "-01-01")), lubridate::as_date(paste0(years_, "-07-01"))))
        start_dates <- start_dates[start_dates <= limit_date]
        end_dates <- sort(c(lubridate::as_date(paste0(years_, "-06-30")), lubridate::as_date(paste0(years_, "-12-31")), limit_date))
        end_dates <- end_dates[end_dates <= limit_date]
    } else {
        start_dates <- lubridate::as_date("2020-01-01")
        end_dates <- lubridate::as_date(limit_date)
    }
    
    start_dates_ <- start_dates
    start_dates_[1] <- lubridate::as_date("2020-02-01")
    
    df <- covidcast::covidcast_signal(
        data_source = "hhs",
        signal = "confirmed_admissions_covid_1d",
        geo_type = geo_level,
        start_day = lubridate::as_date(start_dates_),
        end_day = lubridate::as_date(end_dates))
    
    df <- df %>% mutate(state_abbr = toupper(geo_value)) %>%
        dplyr::select(-geo_value) %>%
        dplyr::left_join(loc_dictionary %>%
                             dplyr::select(state_abbr = abbreviation, geo_value = location) %>%
                             dplyr::filter(stringr::str_length(geo_value) == 2))
    
    df <- df %>% dplyr::rename(date = time_value)
    df_cum <- df %>% dplyr::mutate(value = tidyr::replace_na(value, 0)) %>%
        dplyr::arrange(state_abbr, geo_value, date) %>%
        dplyr::group_by(data_source, signal, geo_value, state_abbr) %>%
        dplyr::mutate(value = cumsum(value)) %>%
        dplyr::ungroup() %>% dplyr::mutate(signal = "confirmed_admissions_cum")
    df <- rbind(df, df_cum)
    
    df <- df %>% dplyr::select(signal, Update = date, source = state_abbr, FIPS = geo_value, value)
    
    df <- df %>%
        dplyr::mutate(signal = recode(signal,
                                      deaths_incidence_num = "incidD",
                                      deaths_cumulative_num = "cumD",
                                      confirmed_incidence_num = "incidC",
                                      confirmed_cumulative_num = "cumC",
                                      confirmed_admissions_covid_1d = "incidH",
                                      confirmed_admissions_cum = "cumH")) %>%
        tidyr::pivot_wider(names_from = signal, values_from = value) %>%
        dplyr::mutate(Update = lubridate::as_date(Update),
                      FIPS = stringr::str_replace(FIPS, stringr::fixed(".0"), ""),
                      FIPS = paste0(FIPS, "000")) %>%
        dplyr::filter(as.Date(Update) <= as.Date(Sys.time())) %>%
        dplyr::distinct()
    
    validation_date <- Sys.getenv("VALIDATION_DATE")
    if (validation_date != "") {
        print(paste("(DataUtils.R) Limiting CSSE US data to:", validation_date, sep = " "))
        df <- dplyr::filter(df, Update < validation_date)
    }
    df <- df %>% tibble::as_tibble()
    
    return(df)
}




#' Obtain influenza signal at daily or weekly scale
#'
#' @param as_of Date or string in format "YYYY-MM-DD" specifying the date which
#'   the data was available on or before. If `NULL`, the default returns the
#'   most recent available data.
#' @param locations optional list of FIPS or location abbreviations. Defaults to
#'   the US, the 50 states, DC, PR, and VI.
#' @param temporal_resolution "daily" or "weekly"
#' @param source either "covidcast" or "HealthData". HealthData only supports
#' `as_of` being `NULL` or the current date.
#' @param na.rm boolean indicating whether NA values should be dropped when
#'   aggregating state-level values and calculating weekly totals. Defaults to
#'   `FALSE`
#'
#' @return data frame of flu incidence with columns date, location,
#'   location_name, value
load_flu_hosp_data <- function(as_of = NULL,
                               locations = "*",
                               temporal_resolution = "daily",
                               source = "HealthData",
                               na.rm = FALSE) {
  
  # load location data
  location_data <- readr::read_csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv") %>%
    dplyr::mutate(geo_value = tolower(abbreviation)) %>%
    dplyr::select(-c("population", "abbreviation"))
  
  # validate function arguments
  if (!(source %in% c("covidcast", "HealthData"))) {
    stop("`source` must be either covidcast or HealthData")
  } else if (source == "HealthData" && !is.null(as_of)) {
    if (as_of != Sys.Date()) {
      stop("`as_of` must be either `NULL` or the current date if source is HealthData")
    }
  }
  
  valid_locations <- unique(c(
    "*",
    location_data$geo_value,
    tolower(location_data$location)
  ))
  locations <- match.arg(tolower(locations), valid_locations, several.ok = TRUE)
  temporal_resolution <- match.arg(temporal_resolution,
                                   c("daily", "weekly"),
                                   several.ok = FALSE)
  if (!is.logical(na.rm)) {
    stop("`na.rm` must be a logical value")
  }
  
  # get geo_value based on fips if fips are provided
  if (any(grepl("\\d", locations))) {
    locations <-
      location_data$geo_value[location_data$location %in% locations]
  } else {
    locations <- tolower(locations)
  }
  # if US is included, fetch all states
  if ("us" %in% locations) {
    locations_to_fetch <- "*"
  } else {
    locations_to_fetch <- locations
  }
  
  # pull daily state data
  if (source == "covidcast") {
    state_dat <- covidcast::covidcast_signal(
      as_of = as_of,
      geo_values = locations_to_fetch,
      data_source = "hhs",
      signal = "confirmed_admissions_influenza_1d",
      geo_type = "state"
    ) %>%
      dplyr::mutate(
        epiyear = lubridate::epiyear(time_value),
        epiweek = lubridate::epiweek(time_value)
      ) %>%
      dplyr::select(geo_value, epiyear, epiweek, time_value, value) %>%
      dplyr::rename(date = time_value)
  } else {
    # temp <- httr::GET(
    #   "https://healthdata.gov/resource/qqte-vkut.json",
    #   config = httr::config(ssl_verifypeer = FALSE)
    # ) %>%
    #   as.character() %>%
    #   jsonlite::fromJSON() %>%
    #   dplyr::arrange(update_date)
    # csv_path <- tail(temp$archive_link$url, 1)
    temp <- RSocrata::read.socrata(url = "https://healthdata.gov/resource/qqte-vkut.json") %>% 
      dplyr::arrange(update_date)
    
    csv_path <- tail(temp$url, 1)
    data <- readr::read_csv(csv_path)
    state_dat <- data %>%
      dplyr::transmute(
        geo_value = tolower(state),
        date = date - 1,
        epiyear = lubridate::epiyear(date),
        epiweek = lubridate::epiweek(date),
        value = previous_day_admission_influenza_confirmed
      ) %>%
      dplyr::arrange(geo_value, date)
  }
  
  
  # creating US and bind to state-level data if US is specified or locations
  if (locations_to_fetch == "*") {
    us_dat <- state_dat %>%
      dplyr::group_by(epiyear, epiweek, date) %>%
      dplyr::summarize(value = sum(value, na.rm = na.rm), .groups = "drop") %>%
      dplyr::mutate(geo_value = "us") %>%
      dplyr::ungroup() %>%
      dplyr::select(geo_value, epiyear, epiweek, date, value)
    # bind to daily data
    if (locations != "*") {
      dat <- rbind(us_dat, state_dat) %>%
        dplyr::filter(geo_value %in% locations)
    } else {
      dat <- rbind(us_dat, state_dat)
    }
  } else {
    dat <- state_dat
  }
  
  # weekly aggregation
  if (temporal_resolution != "daily") {
    dat <- dat %>%
      dplyr::group_by(epiyear, epiweek, geo_value) %>%
      dplyr::summarize(
        date = max(date),
        num_days = n(),
        value = sum(value, na.rm = na.rm),
        .groups = "drop"
      ) %>%
      dplyr::filter(num_days == 7L) %>%
      dplyr::ungroup() %>%
      dplyr::select(-"num_days")
  }
  
  final_data <- dat %>%
    dplyr::left_join(location_data, by = "geo_value") %>%
    dplyr::select(date, location, location_name, value) %>%
    # drop data for locations retrieved from covidcast,
    # but not included in forecasting exercise -- mainly American Samoa
    dplyr::filter(!is.na(location))
  
  return(final_data)
}
















get_hhs_flu_hosp <- function(opt){
    
    # get locations file
    locs <- read_csv("https://raw.githubusercontent.com/cdcepi/Flusight-forecast-data/master/data-locations/locations.csv")
    
    # Pull daily hospitalizations for model run
    us_data <- load_flu_hosp_data(temporal_resolution = 'daily', na.rm = TRUE)

    # fix string pad issue on left side
    us_data <- us_data %>%
        mutate(location = stringr::str_pad(location, width = 2, side = "left", pad = "0"))
    
    us_data <- us_data %>% 
        filter(location != "US") %>%
        mutate(location = stringr::str_pad(location, width=5, side="right", pad="0")) %>%
        left_join(locs %>% select("USPS" = abbreviation, location) %>% 
                      mutate(location = stringr::str_pad(location, width=5, side="right", pad="0")), 
                  by = c("location")) %>%
        rename(FIPS = location, 
               incidH = value,
               source = USPS) %>%
        select(-location_name)
    
    us_data <- us_data %>%
        filter(date >= lubridate::as_date(opt$start_date) & date <= lubridate::as_date(opt$end_date)) %>%
        filter(!is.na(source))
    
    write_csv(us_data, opt$gt_data_path)
    
    
    # PULL VARIANT DATA -------------------------------------------------------
    
    # variant_props_file <- opt$variant_filename
    # adjust_for_variant <- !is.null(variant_props_file)

    # APPLY VARIANTS ----------------------------------------------------------
    
    # if (adjust_for_variant) {
    #     
    #     us_data <- read_csv(config$inference$gt_data_path)
    #     
    #     tryCatch({
    #         us_data <- flepicommon::do_variant_adjustment(us_data, variant_props_file)
    #         us_data <- us_data %>% 
    #             filter(date >= as_date(config$start_date) & date <= as_date(config$end_date_groundtruth))
    #         write_csv(us_data, config$inference$gt_data_path)
    #     }, error = function(e) {
    #         stop(paste0("Could not use variant file |", variant_props_file, 
    #                     "|, with error message", e$message))
    #     })
    # }
    
    cat(paste0("Ground truth data saved\n", 
               "  -- file:      ", opt$gt_data_path,".\n",
               "  -- outcomes:  ", paste(grep("incid", colnames(us_data), value = TRUE), collapse = ", ")))
    return(us_data)
    
}
















make_daily_data <- function(data = nchs_data,
                            current_timescale = "week"){
    
    if (current_timescale != "week") stop("Only weeks implemented currently")
    
    data <- data %>%
        dplyr::select(-starts_with("cum")) %>%
        mutate(Update = lubridate::floor_date(Update, "weeks")) %>%
        pivot_longer(cols = starts_with("incid"), names_to = "outcome", values_to = "value") %>%
        filter(!is.na(value)) %>%
        group_by(source, FIPS, outcome) %>%
        arrange(Update) %>%
        mutate(value_cum = cumsum(value)) %>%
        ungroup() %>%
        mutate(date_num = as.integer(Update))
    
    data %>%
        group_by(source, FIPS, outcome) %>%
        group_split() %>%
        map_dfr(~get_spline_daily(grp_dat = .)) %>%
        mutate(value = ifelse(value < 0, 0, value)) %>%
        pivot_wider(names_from = outcome, values_from = value) %>%
        dplyr::select(Update, source, FIPS, starts_with("incid"), starts_with("cum"))
}



get_spline_daily <- function(grp_dat) {
    
    smth <- stats::splinefun(x = grp_dat$date_num, y = grp_dat$value_cum, method="monoH.FC")
    preds <- grp_dat %>%
        dplyr::select(source, FIPS, outcome) %>%
        distinct() %>%
        expand_grid(Update = seq.Date(min(grp_dat$Update), (max(grp_dat$Update)+6), by="1 day")) %>%
        mutate(date_num = as.integer(Update))
    preds <- preds %>% mutate(value = smth(x = date_num))
    
    preds <- preds %>%
        mutate(outcome = gsub("incid", "cum", outcome)) %>%
        bind_rows(preds %>%
                      dplyr::arrange(Update, source, FIPS, outcome) %>%
                      mutate(value = diff(c(0, value))))
    return(preds)
}


# CHECK
# sum(data %>% filter(outcome=="incidD") %>% pull(value))
# sum(nchs_data %>% filter(!is.na(incidD)) %>% pull(incidD))


# tmp %>% ggplot(aes(x= Update, y = incidD)) + geom_line()
# # smth <- smooth.spline(x = tmp$date_num, y = tmp$cumD)
# #     plot(smth)
# smth <- stats::splinefun(x = tmp$date_num, y = tmp$cumD, method="monoH.FC")
#
# preds <- tibble(date_num = as.integer(seq.Date(min(tmp$Update), max(tmp$Update), by="1 day")))
# preds <- preds %>% mutate(pred = smth(x = date_num))
#
# # preds <- preds %>% mutate(pred = predict(smth, preds$date_num)$y)
#
#
# tmp %>% ggplot(aes(x = Update)) +
#     geom_line(aes(y=pred)) +
#     geom_point(aes(y=cumD), color = "red")
#
# tmp %>% ggplot(aes(x = Update)) +
#     geom_line(aes(y=incidD)) +
#     geom_point(aes(y=incidDweek), color = "red")
#
# sum(tmp$incidD)
# sum(tmp$incidDweek, na.rm = TRUE)
