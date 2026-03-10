#load library
library(fredr)
library(dplyr)
library(tidyr)
library(purrr)
library(zoo) 
library(lubridate)
library(data.table)
library(hdm)
library(glmnet)
library(forecast)
library(readxl)
library(urca)
library(ggplot2)
library(corrplot)

#read in data
file_path = "../data/tech.csv"
df = read.csv(file_path) 
id_cols <- c("caldt")
#change to date format
df = df %>% mutate(caldt = dmy(caldt), first_offer_dt = dmy(first_offer_dt), mgr_dt = dmy(mgr_dt))

#filter out funds that is less than 36 months old and has less than 5m TNA
df <- df %>%
  mutate(
    caldt = as.Date(caldt),
    first_offer_dt = as.Date(first_offer_dt),
    mgr_dt = as.Date(mgr_dt),
    age_months = (year(caldt) - year(first_offer_dt)) * 12 + (month(caldt) - month(first_offer_dt)),
    manager_tenure_months = (year(caldt) - year(mgr_dt)) * 12 + (month(caldt) - month(mgr_dt))
  ) %>% mutate(manager_tenure = manager_tenure_months/12 )

df_filtered = df %>% filter(age_months >= 36) %>% filter(tna_latest >= 5)

#find out which funds are passively managed or is tracking an index
df_filtered = df_filtered %>%  filter(!et_flag %in% c("F", "N"), !index_fund_flag %in% c("B","D","E"))

#filter out funds that do not have at least 70% in equity values over 100 could mean leverage or short selling etc
df_eq70 <- df_filtered %>%
  mutate(
    per_com_c  = pmin(pmax(per_com,  0), 100),
    per_pref_c = pmin(pmax(per_pref, 0), 100),
    equity_pct = coalesce(per_com_c, 0) + coalesce(per_pref_c, 0)
  ) %>%
  filter(equity_pct >= 70)

#extract TNA, expense_ratio, age, manager_tenure, turnover_ratio, value_added (need to join with monthly aggregated results first before calculating)
df_fund_summary = df_eq70 %>% select(crsp_fundno, caldt, age_months, manager_tenure, tna_latest, turn_ratio, exp_ratio)
df_fund_summary = df_fund_summary %>% mutate(
  across(
    .cols = -all_of(id_cols),
    .fns  = ~ readr::parse_number(as.character(.x))
  )) %>% mutate(year = year(caldt))

#store the unique fundno present
unique_fundno <- df_eq70 %>% distinct(crsp_fundno)


#read in monthly returns data
file_path = "../data/monthly_returns.csv"
df_returns = read.csv(file_path) 
df_returns = df_returns %>% mutate(caldt = dmy(caldt)) %>% mutate(caldt = as.Date(caldt))

df_returns_filter = df_returns %>% semi_join(unique_fundno, by = "crsp_fundno")

#read in ff5 + mom data 
file_path = "../data/ff5_mom.csv"
df_ff_mom = read.csv(file_path) 
df_ff_mom = df_ff_mom %>%   mutate(
  dateff = dmy(dateff),       
) %>% mutate( caldt = dateff) %>%
  select(-dateff)




df_merged = df_returns_filter %>% left_join(df_ff_mom ,by = "caldt") %>%    mutate(
  across(
    .cols = -all_of(id_cols),
    .fns  = ~ readr::parse_number(as.character(.x))
  )
)

#compute fund excess return
df_merged = df_merged %>% mutate(rexcess = mret - rf)

#compute flow

#function to winsor
winsor_by_month <- function(x, p = c(0.01, 0.99)) {
  qs <- quantile(x, probs = p, na.rm = TRUE, type = 7)
  pmin(pmax(x, qs[[1]]), qs[[2]])
}

df_with_flow <- df_merged %>%
  arrange(crsp_fundno, caldt) %>%
  group_by(crsp_fundno) %>%
  mutate(
    mtna_lag = lag(mtna),
    flow_m = (mtna - mtna_lag * (1 + mret)) / mtna_lag,
    flow_m = ifelse(is.finite(flow_m), flow_m, NA_real_)
  ) %>%
  ungroup() %>%
  group_by(caldt) %>%
  mutate(
    flow_m_w = winsor_by_month(flow_m, p = c(0.01, 0.99))
  ) %>%
  ungroup()

#agg to annual (including flowvol, need at least 10 months in calendar year)
flow_annual <- df_with_flow %>%
  mutate(year = year(caldt)) %>%
  group_by(crsp_fundno, year) %>%
  summarise(
    n_flow = sum(!is.na(flow_m_w)),
    flow_ann = ifelse(n_flow >= 10, 12 * mean(flow_m_w, na.rm = TRUE), NA_real_),
    flowvol_ann = ifelse(n_flow >= 10, sd(flow_m_w, na.rm = TRUE) * sqrt(12), NA_real_),
    .groups = "drop"
  )



#rolling regression to compute the t stats
factor_cols <- c("mktrf","smb","hml","rmw","cma","umd")

df_rr <- df_merged %>%
  mutate(
    caldt = as.Date(caldt),
    crsp_fundno = as.integer(crsp_fundno)
  ) %>%
  arrange(crsp_fundno, caldt)



#rolling regression helper for ONE fund (36-month window ending at m-1, min 30 complete rows)
factor_cols <- c("mktrf","smb","hml","rmw","cma","umd")

roll_ff_betas_one_fund <- function(d,fund_id, window = 36L, min_obs = 30L, ycol = "rexcess", xcols = factor_cols) {
  
  d <- d[order(d$caldt), ]
  n <- nrow(d) #number of rows for each fund
  
  out <- data.frame(
    caldt = d$caldt,
    crsp_fundno = rep(fund_id, n),
    n_used = rep(NA_integer_, n),
    alpha_hat = rep(NA_real_, n),
    alpha_t = rep(NA_real_, n),
    mktrf_b = rep(NA_real_, n), smb_b = rep(NA_real_, n), hml_b = rep(NA_real_, n),
    rmw_b = rep(NA_real_, n), cma_b = rep(NA_real_, n), umd_b = rep(NA_real_, n),
    mktrf_t = rep(NA_real_, n), smb_t = rep(NA_real_, n), hml_t = rep(NA_real_, n),
    rmw_t = rep(NA_real_, n), cma_t = rep(NA_real_, n), umd_t = rep(NA_real_, n),
    r2 = rep(NA_real_, n)
  )
  
  for (j in seq_len(n)) {
    start_idx <- j - window
    end_idx   <- j - 1
    if (start_idx < 1) next
    
    w <- d[start_idx:end_idx, c(ycol, xcols), drop = FALSE]
    w <- w[complete.cases(w), , drop = FALSE]
    
    #follow the authors rule (min 30 obs)
    if (nrow(w) < min_obs) next
    
    fml <- as.formula(paste(ycol, "~", paste(xcols, collapse = " + ")))
    fit <- lm(fml, data = w)
    s <- summary(fit)
    coefs <- s$coefficients
    
    out$n_used[j] <- nrow(w)
    out$alpha_hat[j] <- coefs["(Intercept)", "Estimate"]
    out$alpha_t[j]   <- coefs["(Intercept)", "t value"]
    out$r2[j] <- s$r.squared
    
    for (x in xcols) {
      out[[paste0(x, "_b")]][j] <- coefs[x, "Estimate"]
      out[[paste0(x, "_t")]][j] <- coefs[x, "t value"]
    }
  }
  
  out
}

#apply to all funds
lst <- df_rr %>% group_split(crsp_fundno)

betas_df <- bind_rows(lapply(lst, function(d) {
  fund_id <- d$crsp_fundno[1]
  roll_ff_betas_one_fund(d, fund_id = fund_id, window = 36L, min_obs = 30L)
}))

#merge 
df_with_betas <- df_rr %>%
  left_join(betas_df, by = c("crsp_fundno","caldt"))

#compute realized alpha
df_with_betas <- df_with_betas %>%
  mutate(alpha_realized = rexcess - (mktrf_b*mktrf + smb_b*smb + hml_b*hml + rmw_b*rmw + cma_b*cma + umd_b*umd))

 
#need to aggregate to annual freq
dfm <- df_with_betas %>%
  mutate(
    year = year(caldt),
    month = month(caldt)
  )

alpha_annual <- dfm %>%
  group_by(crsp_fundno, year) %>%
  summarise(
    n_alpha = sum(!is.na(alpha_realized)),
    alpha_ann = ifelse(n_alpha >= 10, 12 * mean(alpha_realized, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

#for the rest of the features just take dec value 
dec_features <- dfm %>% filter(month == 12) %>% select(crsp_fundno, year,
    alpha_t, r2,
    ends_with("_t"),
    ends_with("_b")
  )

#need to merge this with other fund summary details later
df_ff_annual <- alpha_annual %>% left_join(dec_features, by = c("crsp_fundno", "year")) %>% select(-n_alpha)

# join with annual flow data this df has flow, flowvol, realized alpha, all the t stats and r^2
df_ff_annual <- df_ff_annual %>%
  left_join(flow_annual, by = c("crsp_fundno", "year")) %>% select(-n_flow) %>% select(crsp_fundno, year, alpha_ann, flow_ann, flowvol_ann,
                                                                                       alpha_t, mktrf_t, smb_t, 
                                                                                       hml_t, rmw_t, cma_t, umd_t, r2)

#merge fund summary data and monthly aggregated data
df_final = df_fund_summary %>% 
  left_join(df_ff_annual, by = c("crsp_fundno", "year"))
  

#[value added] match crsp fundno to exp ratio/12 
df_monthly_exp <- df_fund_summary %>%
  arrange(crsp_fundno, caldt) %>%
  group_by(crsp_fundno) %>%
  mutate(monthly_exp = exp_ratio /12) %>%
  mutate(year = year(caldt)) %>%
  ungroup() %>%
  select(crsp_fundno, year, monthly_exp) 

# get monthly fundno and match monthly funds to exp ratio/12 for that year
df_monthly_exp <- df_monthly_exp %>%
  right_join(
    dfm %>%
      select(crsp_fundno, caldt, month, mtna, alpha_realized, year), by = c("crsp_fundno", "year"), relationship = "many-to-many") %>%
  group_by(crsp_fundno) %>%
  arrange(caldt, .by_group = TRUE) %>%
  mutate(tna_lag = lag(mtna, 1L)) %>%
  ungroup() %>%
  mutate(value_added_monthly = (alpha_realized + monthly_exp) * tna_lag)

# agg to annual value added
value_added_ann <- df_monthly_exp %>%
  group_by(crsp_fundno, year) %>%
  summarise(
    n_months = sum(!is.na(value_added_monthly)),
    value_added_ann = ifelse(n_months >= 10, 12 * mean(value_added_monthly, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )
  
# join with other annual features
df_final = df_final %>% 
  left_join(value_added_ann, by = c("crsp_fundno", "year")) %>% select(-year)
  

  
#extract value added annually (not exactly the same as the authors but a good approximation, mention this in report)
#df_final = df_final %>%   group_by(crsp_fundno) %>%
  #arrange(caldt, .by_group = TRUE) %>%
  #mutate(tna_lag = lag(tna_latest, 1L)) %>%
  #ungroup() %>% mutate(value_added = (alpha_ann + exp_ratio) * tna_lag) %>% select(-tna_lag)


#standardise columns (according to author, standardise across funds in 1 year and imputing NA values with 0 is ok)
df_std <- df_final %>%
  mutate(year = year(caldt)) %>%
  group_by(year) %>%
  mutate(across(
    .cols = where(is.numeric) & !any_of(c("year", "crsp_fundno")),   # standardize all numeric except year
    .fns  = ~ {
      mu <- mean(.x, na.rm = TRUE)
      sdv <- sd(.x, na.rm = TRUE)
      z <- (.x - mu) / sdv
      z[is.na(z)] <- 0
      z
    }
  )) %>%
  ungroup() %>% select(-year)

df_std <- df_std %>%
  mutate(year = year(caldt)) %>% select (-n_months)

# lag predictors
predictor_vars <- setdiff(
  names(df_std),
  c("crsp_fundno", "year", "alpha_ann", "caldt")
)

df_ml <- df_std %>%
  arrange(crsp_fundno, year) %>%
  group_by(crsp_fundno) %>%
  mutate(
    # lag alpha_ann
    alpha_ann_lag = lag(alpha_ann, 1),
    # lag all other (_lag)
    across(
      all_of(predictor_vars),
      ~ lag(.x, 1),
      .names = "{.col}_lag"
    )
  ) %>%
  ungroup() %>% select (-year)

# drop first line
df_ml <- df_ml %>%
  filter(!is.na(alpha_ann_lag))

# select
df_ml <- df_ml %>%
  select(
    crsp_fundno,
    caldt,
    alpha_ann,
    ends_with("_lag")
  )


funds_per_year <- df_ml %>%
  mutate(year = year(caldt)) %>%
  group_by(year) %>%
  summarise(n_funds = n_distinct(crsp_fundno)) %>%
  arrange(year)

library(dplyr)
library(ggplot2)

df_plot <- df_ml %>%
  mutate(
    caldt = as.Date(caldt),
    year = as.integer(format(caldt, "%Y"))
  )

alpha_yearly <- df_plot %>%
  group_by(year) %>%
  summarise(
    mean_alpha = mean(alpha_ann, na.rm = TRUE),
    sd_alpha = sd(alpha_ann, na.rm = TRUE),
    n = sum(!is.na(alpha_ann))
  ) %>%
  arrange(year)


ggplot(alpha_yearly, aes(x = year, y = mean_alpha)) +
  geom_line() +
  geom_point() +
  geom_errorbar(
    aes(ymin = mean_alpha - sd_alpha, ymax = mean_alpha + sd_alpha),
    width = 0.2
  ) +
  labs(
    x = "Year",
    y = "Mean alpha_ann",
    title = "Yearly Mean and SD of alpha_ann Across Funds"
  )


