library(arrow)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(flextable)
#library(forecast)
#library(pracma)
library(lubridate) # package for working with dates
library(jtools)  # includes summ() functions for better aesthetic summaries from regression models
#library(multcomp) # includes glht command for computing linear combinations (and CIs) from regression
#library(expss) # includes tool for labeling variable names with descriptive text
library(table1) # includes tools for easily making table of summary statistics for specific subgroup comparisons
library(car) # for avPlots() function which constructs added-variables plots for linear models 
#library(forecast)
library(nlme) # use gls functoin
library(lme4)
library(cAIC4)
library(performance)
opt <- list()
opt$gt_data_path_analysis <- "data/analysis_data/hospburden_analysis.parquet"

### Creating datasets 
covid_hosp_analysis <- arrow::read_parquet(opt$gt_data_path_analysis)


covid_hosp_analysis <- covid_hosp_analysis %>%
  mutate(
    year = factor(year),
    academic_year = factor(academic_year),
    season = factor(season),
    dominant_variant = factor(dominant_variant),
    four_week_peak = factor(four_week_peak),
    four_week_valley = factor(four_week_valley),
    peak_period = factor(peak_period),
    middle_period = factor(middle_period),
    trough_period = factor(trough_period),
    dominant_variant = if_else(is.na(dominant_variant), "mixed", dominant_variant),
    state_numeric = as.factor(case_when(
      state == "NJ" ~ 1,
      state == "NY" ~ 2,
      state == "MD" ~ 3,
      state == "PA" ~ 4))
  )
levels(covid_hosp_analysis$dominant_variant)
covid_hosp_analysis$year <- relevel(covid_hosp_analysis$year, ref = "2021")
covid_hosp_analysis$dominant_variant <- relevel(covid_hosp_analysis$dominant_variant, ref = "OMICRON")
covid_hosp_analysis$season <- relevel(covid_hosp_analysis$dominant_variant, ref = "Summer")

df_aas$nas_field <- factor(df_aas$nas_field, ordered = FALSE)
df_aas$nas_field <- relevel(df_aas$nas_field, ref = "phys_math_sci")
# [1] "state"                       
# [2] "date"                        
# [3] "year"                        
# [4] "academic_year"               
# [5] "total_hosp"                  
# [6] "total_hosp_estimate"         
# [7] "difference"                  
# [8] "relative_difference"         
# [9] "capacity_dif_prop"           
# [10] "capacity_prop_per100thousand"
# [11] "season"                      
# [12] "dominant_variant"            
# [13] "four_week_peak"               
# [14] "four_week_valley"             
# [15] "peak_period"                 
# [16] "middle_period"               
# [17] "trough_period" 

##########################################################
# table 1

footnote = "N = number of days"
table1(~year + season + dominant_variant + four_week_peak + four_week_valley  | state, data= covid_hosp_analysis,
       footnote=footnote, 
       )

##########################################################
# Simple Linear Regressions of Categorical Predictors  
# State
    table(covid_hosp_analysis$state) 
    class(covid_hosp_analysis$state)
    slrstate <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(state))
    summ(slrstate,confint=getOption("summ-confint",TRUE), digits=4)
        

  # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by State
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=state,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by State ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days)", x="State") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())

# year
    table(covid_hosp_analysis$year) 
    class(covid_hosp_analysis$year)
    slryear <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(year))
    summ(slryear,confint=getOption("summ-confint",TRUE), digits=4)
    
    
    # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by year
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=year,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by year ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days) (n=5208 days)", x="year") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())
    
# dominant_variant
    table(covid_hosp_analysis$dominant_variant) 
    class(covid_hosp_analysis$dominant_variant)
    slrdominant_variant <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(dominant_variant))
    summ(slrdominant_variant,confint=getOption("summ-confint",TRUE), digits=4)

    
    # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by dominant_variant
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=dominant_variant,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by dominant_variant ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days) ", x="dominant_variant") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())

# season
    table(covid_hosp_analysis$season) 
    class(covid_hosp_analysis$season)
    slrseason <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(season))
    summ(slrseason,confint=getOption("summ-confint",TRUE), digits=4)
    
    
    # Side by side boxplots of Excess Capacity Proportion (per 100,000)  by season
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=season,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by season ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days) (n=5208 days)", x="season") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())
# four_week_peak
    table(covid_hosp_analysis$four_week_peak) 
    class(covid_hosp_analysis$four_week_peak)
    slrfour_week_peak <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(four_week_peak))
    summ(slrfour_week_peak,confint=getOption("summ-confint",TRUE), digits=4)
    
    
    # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by four_week_peak
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=four_week_peak,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by four_week_peak ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days) (n=5208 days)", x="four_week_peak") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())
#     
# # middle_period
#     table(covid_hosp_analysis$middle_period) 
#     class(covid_hosp_analysis$middle_period)
#     slrmiddle_period <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(middle_period))
#     summ(slrmiddle_period,confint=getOption("summ-confint",TRUE), digits=4)
#     
#     
#     # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by middle_period
#     ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=middle_period,)) + 
#       geom_boxplot() +
#       labs(title = "Excess Capacity Proportion (per 100,000)  by middle_period ", 
#            y = "Excess Capacity Proportion (per 100,000)", 
#            caption = "Hosp Burden Data (n=5208 days) (n=5208 days)", x="middle_period") +
#       theme_bw()+
#       theme(panel.grid.major = element_blank(), 
#             panel.grid.minor = element_blank(),
#             panel.background = element_blank(), 
#             plot.title = element_text(hjust = 0.5),
#             plot.caption = element_text(hjust = 0.5),
#             axis.ticks.x = element_blank())
#     
# four_week_valley
    table(covid_hosp_analysis$four_week_valley) 
    class(covid_hosp_analysis$four_week_valley)
    slrfour_week_valley <-lm(data=covid_hosp_analysis,capacity_prop_per100thousand~as.factor(four_week_valley))
    summ(slrfour_week_valley,confint=getOption("summ-confint",TRUE), digits=4)
    
    
    # Side by side boxplots of LDL by Excess Capacity Proportion (per 100,000)  by four_week_valley
    ggplot(covid_hosp_analysis, aes(y= capacity_prop_per100thousand, x=four_week_valley,)) + 
      geom_boxplot() +
      labs(title = "Excess Capacity Proportion (per 100,000)  by four_week_valley ", 
           y = "Excess Capacity Proportion (per 100,000)", 
           caption = "Hosp Burden Data (n=5208 days) (n=5208 days)", x="four_week_valley") +
      theme_bw()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5),
            axis.ticks.x = element_blank())
#########################################################
# unadjusted variables TABLE

tbl_unadjusted_md <- covid_hosp_analysis %>%
      filter(state == "MD") %>% 
      dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
      tbl_uvregression(
        method = glm,
        y = capacity_prop_per100thousand,
        #method.args = list(family = normal),
        exponentiate = FALSE,
        pvalue_fun = ~ style_pvalue(.x, digits = 2),
        hide_n = TRUE
      ) %>%
      add_global_p() # add global p-value
tbl_unadjusted_md

tbl_unadjusted_pa <- covid_hosp_analysis %>%
  filter(state == "PA") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value
tbl_unadjusted_pa

tbl_unadjusted_ny <- covid_hosp_analysis %>%
  filter(state == "NY") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value

tbl_unadjusted_nj <- covid_hosp_analysis %>%
  filter(state == "NJ") %>% 
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value
tbl_unadjusted_nj

tbl_unadjusted_overall <- covid_hosp_analysis %>%
  dplyr::select(year, season, dominant_variant, four_week_peak, four_week_valley, capacity_prop_per100thousand) %>%
  tbl_uvregression(
    method = glm,
    y = capacity_prop_per100thousand,
    #method.args = list(family = normal),
    exponentiate = FALSE,
    pvalue_fun = ~ style_pvalue(.x, digits = 2),
    hide_n = TRUE
  ) %>%
  add_global_p() # add global p-value


merged_tbl <- tbl_merge(
  tbls = list(tbl_unadjusted_md, tbl_unadjusted_nj, tbl_unadjusted_ny, tbl_unadjusted_pa),
  tab_spanner(c("Unadjusted Maryland", "Unadjusted NJ", "Unadjusted NY", "Unadjusted PA"))
  )

merged_tbl <- tbl_merge(
  tbls = list(tbl_unadjusted_overall, tbl_unadjusted_md, tbl_unadjusted_nj, tbl_unadjusted_ny, tbl_unadjusted_pa),
  tab_spanner = c("Overall", "Maryland", "New Jersey", "New York", "Pennsylvania")
)

merged_tbl

#########################################################
# Multivariable Linear Regression 

# maryland 
    covid_analysis_MD <- covid_hosp_analysis %>% 
      filter(state == "MD")
    
    # Multiple linear regression, 
    mlrfull_MD <- lm(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley, 
                     data = covid_analysis_MD)
    summ(mlrfull_MD,confint=getOption("summ-confint",TRUE), digits=4)
    summary(mlrfull_MD)
    
    # Residuals vs fitted values plot
    ggplot(mlrfull_MD, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()
    
    extractAIC(mlrfull_MD)
    
    # ANOVA TESTS
    # Test peaks/troughs
    mlrnopeaks_MD <- lm(capacity_prop_per100thousand ~  year+season+dominant_variant , 
                               data = covid_analysis_MD)
    anova(mlrnopeaks_MD,mlrfull_MD)
    # Test interaction year*season
    mlryrszn_MD <- lm(capacity_prop_per100thousand ~ year*season+dominant_variant+four_week_peak+four_week_valley, 
                        data = covid_analysis_MD)
    anova(mlrfull_MD, mlryrszn_MD)
    # Test interaction peak*season*variant
    mlryrsznvar_MD <- lm(capacity_prop_per100thousand ~ year*season*dominant_variant+four_week_peak+four_week_valley, 
                      data = covid_analysis_MD)
    anova(mlryrszn_MD, mlryrsznvar_MD)
    # Test interaction peak*season*variant + peak*trough
    # could explore peak * season 
    mlryrsznvar_peaktrough_MD <- lm(capacity_prop_per100thousand ~ year*season*dominant_variant + four_week_peak+four_week_valley, 
                         data = covid_analysis_MD)
    anova(mlryrsznvar_MD,mlryrsznvar_peaktrough_MD)
    
# new jersey 
    covid_analysis_NJ <- covid_hosp_analysis %>% 
      filter(state == "NJ")
    
    # Multiple linear regression, 
    mlrfull_NJ <- lm(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley, 
                     data = covid_analysis_NJ)
    summ(mlrfull_NJ,confint=getOption("summ-confint",TRUE), digits=4)
    summary(mlrfull_NJ)
    
    # Residuals vs fitted values plot
    ggplot(mlrfull_NJ, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()

# NEW YORK 
    covid_analysis_NY <- covid_hosp_analysis %>% 
      filter(state == "NY")
    
    # Multiple linear regression, 
    mlrfull_NY <- lm(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley, 
                     data = covid_analysis_NY)
    summ(mlrfull_NY,confint=getOption("summ-confint",TRUE), digits=4)
    summary(mlrfull_NY)
    
    # Residuals vs fitted values plot
    ggplot(mlrfull_NY, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()
  
# Pennsylvania 
    covid_analysis_PA <- covid_hosp_analysis %>% 
      filter(state == "PA")
    
    # Multiple linear regression, 
    mlrfull_PA <- tslm(capacity_prop_per100thousand ~ date+year+season+dominant_variant+four_week_peak+four_week_valley, 
                     data = covid_analysis_PA)
    summ(mlrfull_PA,confint=getOption("summ-confint",TRUE), digits=4)
    summary(mlrfull_PA)
    
    # Residuals vs fitted values plot
    ggplot(mlrfull_PA, aes(x = .fitted, y = .resid)) + geom_point()+geom_smooth()
    
#############################################################
# Multivariable Linear Regression with autocorrelation 
    
    # maryland 
    covid_analysis_MD <- covid_hosp_analysis %>% 
      filter(state == "MD")
    
    # Multiple linear regression, 
    mlrfull_MD <- lm(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley, 
                     data = covid_analysis_MD)
    summ(mlrfull_MD,confint=getOption("summ-confint",TRUE), digits=4)
    summary(mlrfull_MD)
    
    
    gls1 <- gls(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley,
                data = covid_analysis_MD,
                correlation = corAR1(form=~date))  
    round(summary(gls1)$tTable, 3)
    summary(gls1)
    anova(mlrfull_MD, gls1)
    mean(gls1$residuals)
    
    
    glsall <- nlme::gls(capacity_prop_per100thousand ~ year+season+dominant_variant+four_week_peak+four_week_valley + (1|state),
                data = covid_hosp_analysis,
                correlation = corAR1(form=~date|state))  
    round(summary(glsall)$tTable, 3)
    summary(glsall)
    anova(mlrfull_MD, gls1)
    library(lme4)
    
    m0.glm <- glm(capacity_prop_per100thousand ~ 1, family = gaussian, data = covid_hosp_analysis)
    m0.lmer = lmer(capacity_prop_per100thousand ~ 1 + (1|state), REML = T, data = covid_hosp_analysis)

    AIC(logLik(m0.glm))
    AIC(logLik(m0.lmer))

    covid_hosp_analysis$state <- factor(covid_hosp_analysis$state)
    
    # Fit the GLS model with a random intercept for each state
    glsall <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley + (1 | state_numeric),
                        data = covid_hosp_analysis,
                        correlation = corAR1(form = ~ date | state))
    
    # Ensure state_numeric is a factor
    covid_hosp_analysis$state_numeric <- as.factor(covid_hosp_analysis$state_numeric)
    
    # Fit the GLS model
    glsall <- nlme::gls(capacity_prop_per100thousand ~ season + dominant_variant + four_week_peak + four_week_valley + (1 | state_numeric),
                        data = covid_hosp_analysis,
                        correlation = corAR1(form = ~ date | state))
    
    glsall <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley,
                     data = covid_hosp_analysis,
                     weights = varIdent(form = ~ 1 | state))
    glsall_corr <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley,
                        data = covid_hosp_analysis,
                        weights = varIdent(form = ~ 1 | state),
                        correlation = corAR1(form = ~ date | state))
    anova(glsall, glsall_corr)
    
    lmer_all <- lmer(capacity_prop_per100thousand ~ year*season*dominant_variant + four_week_peak + four_week_valley + (1|state),
                     data = covid_hosp_analysis)
    jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)

    summary(lmer_all)   
resid(lmer_all)
plot(resid(lmer_all))

plot(q)


slrgee_age<-gee(data = df, weight ~ age,  id = id, family = gaussian,corstr = "exchangeable")
   summary(slrgee_age)
   

   glsall <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley,
                       data = covid_hosp_analysis,
                       weights = varIdent(form = ~ 1 | state),
                       correlation = corAR1(form = ~ date | state))
   round(summary(glsall)$tTable, 3)
   summary(glsall)

   glsallMD <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley,
                       data = covid_analysis_MD,
                       correlation = corAR1(form = ~ date))
   glsallMD_YS <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant,
                         data = covid_analysis_MD,
                         correlation = corAR1(form = ~ date))
   round(summary(glsallMD)$tTable, 3)
   summary(glsallMD)
   plot(resid(glsallMD))
   anova(glsallMD_YS, glsallMD)
   glsallMD_I <- nlme::gls(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley + year*season,
                         data = covid_analysis_MD,
                         correlation = corAR1(form = ~ date))
   round(summary(glsallMD_I)$tTable, 3)
   summary(glsallMD_I)
   plot(resid(glsallMD_I))
   
   fit1ml <- update(glsallMD, . ~ ., method = "ML")
   fit2ml <- update(glsallMD_YS, . ~ ., method = "ML")
   anova(fit2ml, fit1ml)

   # Plot residuals vs. fitted values
   plot(glsall, which = 1)  # Residuals vs. Fitted
   plot(glsallMD, which = 1)  # Residuals vs. Fitted
   
   # Plot Normal Q-Q plot of residuals
   plot(glsall, which = 2)  # Normal Q-Q
   
   # Plot Scale-Location plot
   plot(glsall, which = 3)  # Scale-Location (Spread-Location)
   
   # Plot residuals vs. leverage
   plot(glsall, which = 5)  # Residuals vs. Leverage

#############################
#take 2
lm2 <- lm(capacity_prop_per100thousand ~ state + year + season + dominant_variant + four_week_peak + four_week_valley,
          data = covid_hosp_analysis)
d <- covid_hosp_analysis
d$pred2 <- lm2$fitted.values     

ggplot(data = d) +
  geom_line(aes(x = date, y = capacity_prop_per100thousand, group = as.factor(state), color = as.factor(year)),
            alpha = 0.3) +
  geom_line(aes(x = date, y = capacity_prop_per100thousand, color = as.factor(year)), size = 1.2) +
  theme_bw()

lm2 <- lm(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley,
          data = covid_analysis_MD)
d <- covid_analysis_MD
d$pred2 <- lm2$fitted.values     

# plot squared residuals vs age for model 2
ggplot(d, aes(x = date, y = (capacity_prop_per100thousand - pred2)^2)) +
  geom_point() +
  geom_smooth() +
  geom_hline(yintercept = 0, color = "red") +
  theme_bw()


###########
# linear mixed effects model 

lmer_base <- lmer(capacity_prop_per100thousand ~ year + season + (1|state),
                 data = covid_hosp_analysis)
jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)

summary(lmer_base)   
plot(resid(lmer_base))

lmer_all <- lmer(capacity_prop_per100thousand ~ year*season+dominant_variant + four_week_peak + four_week_valley + (1|state),
                  data = covid_hosp_analysis)
jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)

summary(lmer_all)   
cAIC(lmer_all)

anova(lmer_base, lmer_all) 

lmer_all_YS <- lmer(capacity_prop_per100thousand ~ year*season+dominant_variant + four_week_peak + four_week_valley + (1|state),
                 data = covid_hosp_analysis)
jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)
summary(lmer_all_YS)   

anova(lmer_all, lmer_all_YS) 

lmer_all_YSV <- lmer(capacity_prop_per100thousand ~ year + season + dominant_variant + year*four_week_peak + year*four_week_valley + season*four_week_peak + season*four_week_valley + dominant_variant*four_week_peak + four_week_valley + (1|state),
                    data = covid_hosp_analysis)
jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)
summary(lmer_all_YSV)   

#lmer_all_YSV <- lmer(capacity_prop_per100thousand ~ year + season + dominant_variant + year*four_week_peak + year*four_week_valley + (1|state),
                    # data = covid_hosp_analysis)
jtools::summ(lmer_all,confint=getOption("summ-confint",TRUE), digits=4)
summary(lmer_all_YSV)   

anova(lmer_all, lmer_all_YSV) 
anova(lmer_all_YS, lmer_all_YSV1) 

plot(resid(lmer_all_YSV))

vif_values <- vif(lmer_all)
residuals <- resid(lmer_all)

# Create a QQ plot of the residuals
qqmath(~ residuals | NULL, data = NULL)
autoplot(lmer_all)

covid_hosp_analysis %>% ggplot(aes(sample = capacity_dif_prop)) +
  geom_qq()+
  stat_qq_line()

# Plot the VIF values
plot(vif_values, main = "VIF Plot", xlab = "Predictor Variables", ylab = "VIF Values")
text(vif_values, labels = names(vif_values), pos = 4)
# year * peak/trough yes 
# season * peak/trough yes 
#year and season --> no


########### 
# Final Final Models

# mean difference, no correlation

slr<-lm(capacity_prop_per100thousand~year + season, data= covid_hosp_analysis)
summ(  slr, digits=4)
summ(  slr,confint=getOption("summ-confint",TRUE),digits=4)
cAIC(slr)
# mean difference, clustered SE estimates, random effects

slrri<-lmer(capacity_prop_per100thousand~year + season + (1|state), data =covid_hosp_analysis)
summ(  slrri,confint=getOption("summ-confint",TRUE), digits=4)

slrri_date <-lmer(capacity_prop_per100thousand~year + date + season + (1|state), data =covid_hosp_analysis)
summ(  slrri_date,confint=getOption("summ-confint",TRUE), digits=4)
cAIC(slrri_date)
## Standard linear regression model with year + season+dominant_variant + four_week_peak + four_week_valley 
lm2 <- lm(capacity_prop_per100thousand ~ year + season+dominant_variant + four_week_peak + four_week_valley, 
          data =covid_hosp_analysis)
summ(lm2)
summ( lm2,confint=getOption("summ-confint",TRUE),digits=4)

## Random intercept  model with year + season+dominant_variant + four_week_peak + four_week_valley 
## Correlated random effects
me2 <- lmer(capacity_prop_per100thousand ~ year + season+dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
summary(me2)
icc(me2)
summ( me2,confint=getOption("summ-confint",TRUE),digits=4)

## Standard linear regression model with age and sex
# create interaction
lm3 <- lm(capacity_prop_per100thousand ~ year*season + dominant_variant*four_week_peak + dominant_variant*four_week_valley,
          data =covid_hosp_analysis)
summ( lm3,confint=getOption("summ-confint",TRUE),digits=4)

## Random intercept  model with age and sex and interaction

me3 <- lmer(capacity_prop_per100thousand ~ year*season + year*dominant_variant + season*dominant_variant + dominant_variant*four_week_peak + dominant_variant*four_week_valley + (1|state),
            data =covid_hosp_analysis)
summary(me3)
icc(me3)
summ( me3,confint=getOption("summ-confint",TRUE),digits=4)

anova(me2, me3) 

me4 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)

anova(me4, me3) 
cAIC(me2)
cAIC(me3)
cAIC(me4)

me4 <- lmer(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
me5 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
me5v2 <- lmer(capacity_prop_per100thousand ~ year*dominant_variant +  season +  four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
me5v3 <- lmer(capacity_prop_per100thousand ~ year + dominant_variant*season +  four_week_peak + four_week_valley + (1|state),
              data =covid_hosp_analysis)
me5v4 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant*season +  four_week_peak + four_week_valley + (1|state),
              data =covid_hosp_analysis)
me6 <- lmer(capacity_prop_per100thousand ~ year*season + year*dominant_variant + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
me7 <- lmer(capacity_prop_per100thousand ~ year*season + year*dominant_variant + season*dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)

medate <- lmer(capacity_prop_per100thousand ~ date*year*season + dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
anova(me5, me6) 
anova(me5, me7) 
anova(me6, me7) 

cAIC(me4)
cAIC(me5)
cAIC(me5v2)
cAIC(me5v3)
cAIC(me5v4)
cAIC(me6)
cAIC(me7)
cAIC(medate)

anova(me5, medate) 
summary(me5) # THiS IS FINAL MODEL BELOW 
summary(me5v4)
summary(medate)

# check random slopes 
# fails to converge
me5_slope <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant + four_week_peak + four_week_valley + (1 + date |state),
            data =covid_hosp_analysis)
# runs, what is meaning of output
me5_slope <- lmer(capacity_prop_per100thousand ~ date + year*season + dominant_variant + four_week_peak + four_week_valley + (1 + date |state),
                  data =covid_hosp_analysis)
me5_date <- lmer(capacity_prop_per100thousand ~ state + year*season + dominant_variant + four_week_peak + four_week_valley + (1 |date),
                  data =covid_hosp_analysis)
anova(me5, me5_date) 
summary(me5_date)
#### FINAL MODEL TABLES 
final_model <- me5

# Summarize fixed-effects portion of the model with gtsummary
tbl <- tbl_regression(final_model)

# Print the table
tbl
ranef(final_model)

# residuals check
# https://kristopherkyle.github.io/IntroQuantALRM/8_Linear_Mixed_Effects_Models.html
plot(resid(final_model), covid_hosp_analysis$capacity_prop_per100thousand)
library(lattice)

qqmath(final_model)

#install.packages("MuMIn") #if not alread installed
library(MuMIn)
r.squaredGLMM(final_model)

####### models for paper 

mod1<-lmer(capacity_prop_per100thousand~year + season + (1|state), data =covid_hosp_analysis)
# cAIC(mod1)
mod2 <- lmer(capacity_prop_per100thousand ~ year + season + dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
## FINAL MODEL
mod3 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
mod4 <- lmer(capacity_prop_per100thousand ~ year*season + dominant_variant*season +  four_week_peak + four_week_valley + (1|state),
              data =covid_hosp_analysis)
mod5 <- lmer(capacity_prop_per100thousand ~ year*season + year*dominant_variant + four_week_peak + four_week_valley + (1|state),
            data =covid_hosp_analysis)
mod6 <- lmer(capacity_prop_per100thousand ~ year*season + season*dominant_variant + season*dominant_variant + season*four_week_peak + season*four_week_valley + (1|state),
            data =covid_hosp_analysis)


anova(mod1, mod2)
anova(mod2, mod3) 
anova(mod3, mod4) 
anova(mod4, mod5) 

#cAIC(mod5)

summary(mod3)
summ(mod3,confint=getOption("summ-confint",TRUE),digits=4)
tbl_regression(mod3, exponentiate = FALSE,
               show_single_row= c("four_week_peak", "four_week_valley"),
               pvalue_fun = ~ style_pvalue(.x, digits = 2)) %>%
  add_global_p() %>%
  bold_p(t = 0.10) %>%
  bold_labels() %>%
  italicize_levels()

summ(mod3,confint=getOption("summ-confint",TRUE),digits=4)
summary(mod3)

library(ggplot2)

###################################
# residuals 

# Extract predicted values for each state
predicted_values <- predict(mod3, type = "response")  # Replace "response" with appropriate type if not logistic regression

# Create a data frame with actual and predicted values
data <- data.frame(
  Actual = covid_hosp_analysis$capacity_prop_per100thousand,  # Replace with your actual response variable
  Predicted = predicted_values,
  State = covid_hosp_analysis$state  # Replace with your state variable
)

# Plot actual vs. predicted values, grouped by state
ggplot(data, aes(x = Actual, y = Predicted, color = State)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # Add diagonal line for perfect fit
  labs(x = "Actual", y = "Predicted", title = "Actual vs. Predicted Values by State") +
  theme_minimal()
library(sjPlot)
sjPlot::plot_model(mod3,
                   show.values=TRUE, show.p=TRUE,
                   title="Effect of Covars on Estimated Hospitalizatins (per 100,000)")
sjPlot:: tab_model(mod3)


##############################################
# Visualizations of hospitalizations

hosp_viz <- covid_hosp_analysis %>%  
  ggplot(aes(x = date, y = total_hosp_estimate)) + 
  geom_line(aes(y = total_hosp, linetype = "Observed")) +
  geom_line(aes(y = total_hosp_estimate, linetype = "Estimated", color = difference)) +
  labs(x = "Date", y = "Hospitalizations (Observed & Estimated)", color = "Difference (Observed - Expected)") + 
  facet_wrap(~state, ncol = 1) +
  ggtitle("Total Observed vs. Estimated Hospitalizations by State") +
  theme_bw() +
  scale_color_gradient2(low = "red", mid = "forestgreen", high = "red", midpoint = 0) +
  labs(color = "Difference (Observed - Expected)", 
       title = "Time Series of Hospitalization Estimates") +
  scale_linetype_manual(name = "Line Type", 
                        values = c("solid", "dashed"), 
                        labels = c("Estimated Hospitalizations", "Observed Hospitalizations"))
ggsave("hosp_viz.png", plot = hosp_viz, width = 10, height = 8, dpi = 300)

########### make origin pass through 0

mod3_origin <- lmer(capacity_prop_per100thousand ~ 0+ dominant_variant + year*season + four_week_peak + four_week_valley + (1|state),
             data =covid_hosp_analysis)
summary(mod3_origin) 

### 
covid_hosp_analysis_2021 <- covid_hosp_analysis %>% 
  filter(year == 2021, state == "MD") 
unique(covid_hosp_analysis_2021$dominant_variant)


