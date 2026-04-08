## INSTALLING FILES AND PACKAGES

## installing the dataset
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lubridate")
install.packages("zoo")
install.packages("maps")
install.packages("purrr")

library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(zoo)
library(maps)
library(purrr)

## Outages dataset
years <- 2015:2024
texas_outages <- lapply(years, function(x){
  read_csv(paste0("~/Downloads/Texas_Outages/texas_outages_",x,".csv"))
})

names(texas_outages) <- years ## assigning year to each dataset
lapply(texas_outages, names) ## checking column names of each dataset

## Medical beneficiaries dataset
texas_medical <- read_csv("~/Desktop/Texas Outage Project/counties-data-power-dependent-devices-dme.csv")

## COMBINING DATA SET

## giving each dataset same column name
texas_outages[["2024"]] <- texas_outages[["2024"]] %>%
  select(-total_customers)

texas_outages[["2022"]] <- texas_outages[["2022"]] %>%
  rename(sum = customers_out)

texas_outages[["2024"]] <- texas_outages[["2024"]] %>%
  rename(sum = customers_out)

## combining the datasets
texas_outages <- texas_outages %>%
  bind_rows(.id = "year") %>%
  mutate(year = as.integer(year)) %>%
  arrange(run_start_time)

## DATA CLEANING

## NA values
sum(is.na(texas_outages)) ## 502335
sum(is.na(texas_outages$sum)) ## All NA values in the 'sum' column
year_NA <- function(df, year = "year") {
  df %>%
    group_by(.data[[year]]) %>%
    summarise(across(everything(), ~ sum(is.na(.)), .names = "na_{.col}"))
}

year_NA(texas_outages) ## NA values are all in 2017-2021

## replacing with NA with mean value
texas_outages <- texas_outages %>% 
  group_by(year) %>%
  mutate(sum = ifelse(is.na(sum), mean(sum, na.rm = TRUE), sum)) %>%
  ungroup()

sum(is.na(texas_outages)) ## 0 NA values left

## Cleaning Medical Beneficiaries Data
sum(is.na(texas_medical)) ## No NA values!

## EXPLORATORY DATA ANALYSIS

## Weekly averages to make data easier to read
texas_outages_weekly <- texas_outages %>%
  mutate(date = as.Date(run_start_time),
         week = floor_date(date, "week")) %>%
  group_by(year, county, state, fips_code, week) %>%
  summarise(avg_sum = mean(sum, na.rm = TRUE), .groups = "drop")

## HARRIS COUNTY (2015-2024)
## Harris County Data
texas_outages_harris <- texas_outages_weekly %>%
  filter(county == "Harris", year >= 2015, year <= 2024) %>%
  mutate(date_no_year = as.Date(format(week, "%m-%d"), "%m-%d"))

## Harris County 2020 focus
texas_outages_harris_2020 <- texas_outages_harris %>%
  filter(year == 2020)

## Plotting Harris County 2020
ggplot(texas_outages_harris_2020, aes(x = date_no_year, y = avg_sum)) +
  geom_line() +
  labs(
    title = "Harris County Outages 2020",
    x = "Date",
    y = "# of Outages"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_minimal()

## Plotting Harris County 2015-2024
ggplot(texas_outages_harris, aes(x = date_no_year, y = avg_sum, color = factor(year))) +
  geom_line() +
  labs(
    title = "Harris county Outages 2015-2024",
    x = "Date",
    y = "# of Outages"
  ) +
  scale_x_date(date_labels = "%b %d", date_breaks = "1 month") +
  theme_minimal()

## Plotting Continuous Harris Country 2015-2024
ggplot(texas_outages_harris, aes(x = week, y = avg_sum)) +
  geom_line() +
  labs(
    title = "Harris county Outages 2015-2024",
    x = "Date",
    y = "# of Outages"
) + 
  scale_x_date(date_labels = "%y %b", date_breaks = "6 month") +
  theme_minimal()

## Medical Beneficiaries Data Analysis

## Ordering the dataset by at-risk
texas_medical <- texas_medical %>%
  rename(at_risk_beneficiaries = `At-Risk Beneficiaries`)
texas_medical <- texas_medical %>%
  arrange(desc(at_risk_beneficiaries))
texas_medical_risk <- texas_medical %>%
  head(5)
counties_at_risk_medical <- c(texas_medical_risk$Geography)

## More graphs for the top 5 medically at-risk counties
counties_at_risk_outage <- texas_outages_weekly %>%
  filter(county %in% counties_at_risk_medical, year >= 2015, year <= 2024)

## Plotting all Counties individually 2015-2024
plots <- lapply(counties_at_risk_medical, function(cnty) {
  df <- counties_at_risk_outage %>% filter(county == cnty)
  ggplot(df, aes(x = week, y = avg_sum)) +
    geom_line(color = "red") +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 1) +
    scale_y_log10() +
    labs(
      title = paste("Outages in", cnty, "2015-2024"),
      x = "Year",
      y = "# of Outages"
    ) +
    scale_x_date(date_labels = "%y %b", date_breaks = "6 month") +
    theme_minimal()
})
plots

counties_at_risk_outage <- counties_at_risk_outage %>%
  mutate(date_no_year = week(week))

plots <- lapply(counties_at_risk_medical, function(cnty) {
  df <- counties_at_risk_outage %>% filter(county == cnty)
  ggplot(df, aes(x = date_no_year, y = avg_sum, color = year, group = year)) +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed", size = 1) +
    scale_y_log10() +
    labs(
      title = paste("Outages in", cnty, "2015-2024"),
      x = "Date",
      y = "# of Outages"
    ) +
    theme_minimal()
})
plots

## Plotting All On One Plot
## Raw
ggplot(counties_at_risk_outage, aes(x = week, y = avg_sum, color = county, group = county)) +
  geom_line() +
  scale_y_log10() +
  labs(
    title = "Outages per County 2015-2024",
    x = "Year",
    y = "# of Outages",
    color = "County"
  ) +
  scale_x_date(date_labels = "%y %b", date_breaks = "6 month") +
  theme_minimal() +
  theme(legend.position = "bottom")

## Smooth
ggplot(counties_at_risk_outage, aes(x = week, y = avg_sum, color = county, group = county)) +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") + 
  scale_y_log10() +
  labs(
    title = "Outages per County 2015-2024",
    x = "Year",
    y = "# of Outages"
  ) +
  scale_x_date(date_labels = "%y %b", date_breaks = "6 month") +
  theme_minimal() +
  theme(legend.position = "bottom")
    
## Day 3 Work

## Filtering Dataset
harris_outages_freeze <- texas_outages %>%
  mutate(date = as.Date(run_start_time)) %>%
  filter(county == "Harris", date >= "2021-02-10", date <= "2021-02-27") %>%
  group_by(year, fips_code, county, state, sum, run_start_time, date) %>%
  arrange(run_start_time)
    
ggplot(harris_outages_freeze, aes(x = run_start_time, y = sum)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(
    title = "Outages Harris County February 10-27 2021",
    x = "Date",
    y = "# of Outages"
  ) +
  theme_minimal()
  
harris_outages_freeze <- harris_outages_freeze %>%
  group_by(date) %>%
  arrange(run_start_time, .by_group = TRUE) %>%
  mutate(
    Ct = rollapply(sum, width = 16, FUN = max,
                        align = "left", fill = NA, na.rm = TRUE),
    Cp = max(Ct, na.rm = TRUE),
    Ts = run_start_time[which(Ct >= 0.25 * Cp)[1]],
    Tpeak = run_start_time[which(Ct >= 0.75 *Cp)[1]],
    Ts_time = format(Ts, "%H:%M:%S"),
    Tpeak_time = format(Tpeak, "%H:%M:%S")
  ) %>%
  ungroup()

ggplot(harris_outages_freeze, aes(x = date, y = Ts_time)) +
  geom_point() +
  labs(
    title = "Ts February 10-27 2021",
    x = "Date",
    y = "Ts"
) +
  theme_minimal()

ggplot(harris_outages_freeze, aes(x = date, y = Tpeak_time)) +
  geom_point() +
  labs(
    title = "Tpeak February 10-27 2021",
    x = "Date",
    y = "Ts"
  ) +
  theme_minimal()

harris_outages_freeze <- harris_outages_freeze %>%
  group_by(date) %>%
  arrange(run_start_time, .by_group = TRUE) %>%
  mutate(
    Ct = rollapply(sum, width = 16, FUN = max,
                   align = "left", fill = NA, na.rm = TRUE),
    Cp = max(Ct, na.rm = TRUE),
    Ts = run_start_time[which(Ct >= 0.25 * Cp)[1]],
    Tpeak = run_start_time[which(Ct >= 0.75 *Cp)[1]],
    Ts_time = format(Ts, "%H:%M:%S"),
    Tpeak_time = format(Tpeak, "%H:%M:%S")
  ) %>%
  ungroup()

ggplot(harris_outages_freeze, aes(x = date, y = Ts_time)) +
  geom_point() +
  labs(
    title = "Ts February 10-27 2021",
    x = "Date",
    y = "Ts"
  ) +
  theme_minimal()

ggplot(harris_outages_freeze, aes(x = Ts_time, y = Tpeak_time)) +
  geom_point() +
  labs(
    title = "Ts vs Tpeak February 10-27 2021",
    x = "Ts",
    y = "Tpeak"
  ) +
  theme_minimal()

T_freeze <- harris_outages_freeze %>%
  group_by(date) %>%
  summarize(
    Cp = max(Ct, na.rm = TRUE),
    Ts_time = format(run_start_time[which(Ct >= 0.25 * Cp)[1]], "%H:%M:%S"), 
    Tpeak_time = format(run_start_time[which(Ct >= 0.75 * Cp)[1]], "%H:%M:%S"), 
    .groups = "drop"
  )

ggplot(T_freeze, aes(x = Cp, y = Ts_time)) +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "Cp vs Ts_time February 10-27 2021",
    x = "Cp",
    y = "Ts_time"
  ) +
  theme_minimal()

ggplot(T_freeze, aes(x = Cp, y = Tpeak_time)) +
  geom_point() +
  scale_x_log10() +
  labs(
    title = "Cp vs Tpeak_time February 10-27 2021",
    x = "Cp",
    y = "Tpeak_time"
  ) +
  theme_minimal()

ggplot(T_freeze, aes(x = Ts_time, y = Tpeak_time)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed")
  labs(
    title = "Ts_time vs Tpeak_time February 10-27 2021",
    x = "Ts_time",
    y = "Tpeak_time"
  ) +
  theme_minimal()
  
## Day 4 Analysis
harris_outages_freeze <- texas_outages %>%
  mutate(date = as.Date(run_start_time)) %>%
  filter(county == "Harris", date >= "2021-02-10", date <= "2021-02-27") %>%
  group_by(year, fips_code, county, state, sum, run_start_time, date) %>%
  arrange(run_start_time)

ggplot(harris_outages_freeze, aes(x = run_start_time, y = sum)) +
  geom_line() +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(
    title = "Outages Harris County February 10-27 2021",
    x = "Date",
    y = "# of Outages"
  ) +
  theme_minimal()

## Creating t_hours column
harris_outages_freeze <- harris_outages_freeze %>% 
  ungroup() %>%
  arrange(run_start_time) %>%
  mutate(
    t_hours = (as.numeric(run_start_time) - as.numeric(first(run_start_time)))/3600
  )

n <- nrow(harris_outages_freeze)

start_idx <- 1:(n - 15)

harris_blocks <- lapply(start_idx, function(i) {
  harris_outages_freeze[i:(i+15),]
})

harris_blocks <- lapply(harris_blocks, function(block) {
  block$Ct <- block$sum
  Cp <- max(block$Ct, na.rm = TRUE)
  Tp <- block$t_hours[which.max(block$Ct)]
  Ts_index <- which(block$Ct >= 0.25 * Cp)[1]
  Ts <- block$t_hours[Ts_index]
  block$Cp <- Cp 
  block$Tp <- Tp 
  block$Ts <- Ts 
  n <- nrow(block)
  A <- numeric(n)
  for(i in 1:n){
    if(block$t_hours[i] < Tp){
      A[i] <- 0
    } else if (block$t_hours[i] == Tp){
      A[i] <- Cp
    } else {
      A[i] <- min(A[i-1], block$Ct[i])
    }
  }
  block$A <- A
  tp_index <- which(block$t_hours == Tp)[1]
  block$monotonic <- all(diff(block$A[tp_index:n]) <= 0)
  shifting_factor <- min((block$Tp - block$Ts) * 0.75, 24)
  block$shifting_factor <- shifting_factor
  block$duration <- block$t_hours - block$Tp + shifting_factor
  customers_restored <- c(NA, -diff(block$A))
  block$customers_restored <- customers_restored
  return(block)
})

harris_blocks_all <-  do.call(rbind, lapply(seq_along(harris_blocks), function(i) {
  block <- harris_blocks[[i]]
  block$block_id <- i
  return(block)
}))

## Day 5 Analysis

target_counties <- c("Harris", "Dallas", "Tarrant", "Bexar", "El Paso")

freeze_results <- lapply(target_counties, function(county_name) {
  df_freeze <- texas_outages %>%
    mutate(date = as.Date(run_start_time)) %>%
    filter(county == county_name, date >= "2021-02-10", date <= "2021-02-27") %>%
    arrange(run_start_time) %>%
    mutate(
      t_hours = as.numeric(difftime(run_start_time, min(run_start_time), units = "hours")),
      elapsed = as.numeric(difftime(run_start_time, min(run_start_time), units = "mins")),
      block_id = floor(elapsed / 240) + 1
    ) %>%
    group_by(block_id) %>%
    mutate(
      Cp = max(sum, na.rm = TRUE),
      Ct = Cp
    ) %>%
    ungroup()
  
  var_name <- paste0(tolower(gsub(" ", "", county_name)), "_outages_freeze")
  
  assign(var_name, df_freeze, envir = .GlobalEnv)
  
  freeze_plot <- ggplot(df_freeze, aes(x = t_hours, y = Ct)) +
    geom_line() +
    geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
    labs(
      title = paste("Outages", county_name, "County February 2021"),
      x = "Hours after Feb. 1st, 0:00",
      y = "# of Outages"
    ) +
    theme_minimal()
  return(list(df_freeze, plot = freeze_plot))
})

ggplot(harris_outages_freeze, aes(x = t_hours, y= Ct)) +
  geom_line() +
  labs(
    title = paste("Outages Harris County February 2021"),
    x = "Hours after Feb. 1st, 0:00",
    y = "# of Outages"
  ) +
  theme_minimal()

names(freeze_results) <- target_counties

lapply(freeze_results, function(results) print(results$plot))

county_peaks <- function(df, county_name) {
  df_roll <- df %>%
    arrange(t_hours) %>%
    mutate(
      Ct_roll = rollmean(Ct, k = 48, fill = NA, align = "center")
    )
  df_blocks_rolling <- df_roll %>%
    group_by(block_id) %>%
    summarize(
      T_max = min(run_start_time),
      Ct_max = if(all(is.na(Ct_roll))) NA_real_ else max(Ct_roll, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(T_max)
  
  df_blocks_rolling <- df_blocks_rolling %>%
    mutate(
      dCt_dt = c(NA, diff(Ct_max) / 4), 
      dCt_dt_2 = c(NA, diff(dCt_dt) / 4), 
      dCt_dt_roll = rollmean(dCt_dt, k = 6, fill = NA, align = "center"), 
      dCt_dt_2_roll = c(NA, diff(dCt_dt_roll) / 4), 
      peak_flag = ifelse(dCt_dt_roll > 0 & lead(dCt_dt_roll) < 0, 1, 0)
    )
  
  local_peaks <- df_blocks_rolling %>%
    filter(peak_flag == 1) %>%
    select(T_max, Ct_max)
  
  plot_peak <- ggplot(df_blocks_rolling, aes(x = T_max, y = Ct_max)) + 
    geom_line(color = "blue") + 
    geom_point(data = local_peaks, aes(x = T_max, y = Ct_max), color = "red", size = 3) + 
    labs( 
      title = paste("Approx. Local Maxima of Outages —", county_name), 
      x = "Block Start Time", 
      y = "Ct_max" 
      ) + 
    theme_minimal() 
  return(list(blocks = df_blocks_rolling, peaks = local_peaks, plot = plot_peak))
}

county_list <- list( 
  Harris = harris_outages_freeze, 
  Tarrant = tarrant_outages_freeze, 
  Dallas = dallas_outages_freeze, 
  Bexar = bexar_outages_freeze, 
  ElPaso = elpaso_outages_freeze 
) 

results <- lapply(names(county_list), function(name) { 
  county_peaks(county_list[[name]], name) 
}) 
names(results) <- names(county_list)

for (name in names(results)) { 
  print(results[[name]]$plot) 
}

all_peaks <- do.call(rbind, lapply(names(results), function(name) { 
  df <- results[[name]]$peaks 
  df$county <- name 
  df 
}))

harris_peaks <- all_peaks %>%
  filter(county == "Harris")

dallas_peaks <- all_peaks %>%
  filter(county == "Dallas")

tarrant_peaks <- all_peaks %>%
  filter(county == "Tarrant")

bexar_peaks <- all_peaks %>%
  filter(county == "Bexar")

elpaso_peaks <- all_peaks %>%
  filter(county == "ElPaso")

## Day 12 Analysis
## FINAL REVISIONS ON THE LOOP FUNCTION

texas_outages_all <- texas_outages %>%
  arrange(run_start_time) %>% 
  mutate(
    date = as.Date(run_start_time),
    t_hours = as.numeric(difftime(run_start_time, first(run_start_time), units = "hours")),
    elapsed = hour(run_start_time) + minute(run_start_time) / 60 + second(run_start_time) / 3600
  ) %>%
  
  mutate(
    block_id = floor(t_hours / 4)
  ) %>%
  
  group_by(fips_code, date) %>%
  mutate(
    Cp = first(sum),
    Ct = sum - Cp
  ) %>%
  
  ungroup()

harris_outages_all <- texas_outages_all %>% filter(county == "Harris")
tarrant_outages_all <- texas_outages_all %>% filter(county == "Tarrant")
dallas_outages_all <- texas_outages_all %>% filter(county == "Dallas")
bexar_outages_all <- texas_outages_all %>% filter(county == "Bexar")
elpaso_outages_all <- texas_outages_all %>% filter(county == "El Paso")

## NOTE: Date in the 'analyze_peaks' function can be modified in order to suit the dates of interest
## NOTE: You also have to alter the date in the 'results' call as well as the inflection points function after 
## to change the date of interest
analyze_peaks <- function(df, county_name, start_date = "2021-02-10", end_date = "2021-02-27") {
  df <- df %>%
    filter(run_start_time >= as.POSIXct(start_date),
           run_start_time <= as.POSIXct(end_date)) %>%
    mutate(t_hours = as.numeric(difftime(run_start_time, min(run_start_time), units="hours"))) %>% 
    arrange(t_hours) %>%
    mutate(Ct_roll = rollmean(Ct, k = 48, fill = NA, align = "center"))
  if(nrow(df) < 48) return(NULL)
  
  df_blocks <- df %>%
    group_by(block_id) %>%
    summarize(
      T_max = min(run_start_time),
      Ct_max = if(all(is.na(Ct_roll))) NA_real_ else max(Ct_roll, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(T_max) %>%
    mutate(
      dCt_dt = c(NA, diff(Ct_max) / 4),
      dCt_dt_roll = rollmean(dCt_dt, k = 6, fill = NA, align = "center"),
      peak = ifelse(dCt_dt_roll > 0 & lead(dCt_dt_roll) < 0, 1, 0)
    )
  
  local_peaks <- df_blocks %>%
    filter(peak == 1) %>%
    select(T_max, Ct_max)
  
  peaks_base <- local_peaks %>% arrange(T_max)
  if(nrow(peaks_base) == 0) return(NULL)
  
  minima_list <- list()
  for(i in 1:(nrow(peaks_base) - 1)) {
    T_max_start <- peaks_base$T_max[i]
    T_max_end <- peaks_base$T_max[i + 1]
    
    min_data <- df %>%
      filter(run_start_time > T_max_start, run_start_time < T_max_end) %>%
      filter(!is.na(Ct_roll)) %>%
      slice_min(Ct_roll, n = 1, with_ties = FALSE)
    
    if(nrow(min_data) > 0) {
      minima_list[[i]] <- data.frame(
        T_max_ref = T_max_start,
        T_min_next = min_data$run_start_time[1],
        Ct_min_next = min_data$Ct_roll[1]
      )
    }
  }
  
  minima_df <- bind_rows(minima_list)
  
  peaks_constrained <- peaks_base %>%
    left_join(minima_df, by = c("T_max" = "T_max_ref")) %>%
    mutate(
      T_min_prev = lag(T_min_next),
      Ct_min_prev = lag(Ct_min_next)
    )
  
  if(nrow(peaks_constrained) > 0 && is.na(peaks_constrained$T_min_prev[1])) {
    peaks_constrained$T_min_prev[1] <- min(df$run_start_time, na.rm = TRUE)
  }
  
  if(nrow(peaks_constrained) > 0 && is.na(peaks_constrained$T_min_next[nrow(peaks_constrained)])) {
    peaks_constrained$T_min_next[nrow(peaks_constrained)] <- max(df$run_start_time, na.rm = TRUE)
  }
  
  peaks <- peaks_constrained %>%
    rowwise() %>%
    mutate(
      thr_Ts = 0.1 * Ct_max,
      thr_T_end = exp(-4) * Ct_max,
      
      Ts = {
        search_window <- df %>%
          filter(run_start_time >= T_min_prev, run_start_time <= T_max)
        Ts_candidate <- search_window %>%
          filter(Ct_roll <= thr_Ts) %>%
          slice_tail(n = 1) %>%
          pull(run_start_time)
        found <- if(length(Ts_candidate) > 0) Ts_candidate[1] else NA
        if(is.na(found)) found <- T_min_prev
        found
      },
      
      T_end = {
        search_window <- df %>%
          filter(run_start_time >= T_max, run_start_time <= T_min_next)
        T_end_candidate <- search_window %>%
          filter(Ct_roll <= thr_T_end) %>%
          slice_head(n = 1) %>%
          pull(run_start_time)
        found <- if(length(T_end_candidate) > 0) T_end_candidate[1] else NA
        if(is.na(found)) found <- T_min_next
        found
      }
    ) %>%
    ungroup() %>%
    mutate(
      duration = as.numeric(difftime(T_end, Ts, units = "hours")),
      outage_type = case_when(
        duration < 24 ~ "Type 1 (<24h)",
        duration >= 24  & duration < 48  ~ "Type 2 (24-48h)",
        duration >= 48  & duration < 72  ~ "Type 3 (48-72h)",
        duration >= 72  & duration < 120 ~ "Type 4 (72-120h)",
        duration >= 120 ~ "Type 5 (>120h)",
        TRUE ~ NA_character_
      )
    )
  
  type_levels <- c("Type 1 (<24h)", "Type 2 (24-48h)", "Type 3 (48-72h)", "Type 4 (72-120h)", "Type 5 (>120h)")
  peaks$outage_type <- factor(peaks$outage_type, levels = type_levels)
  
  plot3 <- ggplot(peaks, aes(x = outage_type, fill = outage_type)) +
    geom_bar(color = "black") +
    scale_fill_brewer(palette = "Blues") +
    labs(title = paste(county_name, "Outage Frequency by Type"), x = "Category", y = "Events") +
    theme_minimal() + theme(legend.position = "none")
  
  outage_events <- nrow(peaks)
  valid_data <- peaks %>%
    mutate(Ct_max = as.numeric(Ct_max), duration = as.numeric(duration)) %>%
    filter(!is.na(Ct_max), !is.na(duration), is.finite(Ct_max), is.finite(duration))
  
  plot1 <- ggplot(peaks, aes(x = Ct_max, y = duration)) +
    geom_point(color = "red", size = 3) +
    geom_smooth(method = "loess", span = 1.0, se = FALSE, color = "blue", linetype = "dashed") +
    scale_x_log10() +
    labs(title = paste(county_name, "Peak Size vs Duration"), x = "Logarithmic Size", y = "Duration") +
    theme_minimal()
  
  inflection_points <- data.frame()
  valid_data <- valid_data %>% filter(Ct_max > 0)
  
  if (nrow(valid_data) >= 3) {
    fit_model <- loess(duration ~ log10(Ct_max), data = valid_data, span = 1.0)
    
    grid_clean <- data.frame(
      Ct_max = seq(min(valid_data$Ct_max), max(valid_data$Ct_max), length.out = 500)
    )
    
    grid_clean$fit <- predict(fit_model, newdata = grid_clean)
    
    grid_clean <- grid_clean %>%
      filter(!is.na(fit)) %>%
      mutate(
        d1 = c(NA, diff(fit)),
        d2 = c(NA, diff(d1))
      ) %>%
      filter(!is.na(d2))
    
    inflection_idx <- which(diff(sign(grid_clean$d2)) != 0 & grid_clean$d1[1:(nrow(grid_clean)-1)] > 0)
    
    if(length(inflection_idx) > 0) {
      inflection_points <- grid_clean[inflection_idx, ]
    }
    
    if(nrow(inflection_points) > 0) {
      plot1 <- plot1 +
        geom_point(data = inflection_points, aes(x = Ct_max, y = fit), color = "purple", size = 4) +
        geom_text(data = inflection_points, aes(x = Ct_max, y = fit), 
                  label = "Inflection", color = "purple", vjust = -1)
    }
  }
  
  plot2 <- ggplot(df, aes(x = run_start_time, y = Ct_roll)) +
    geom_line(linewidth = 1.2) +
    geom_point(data = peaks, aes(x = T_max, y = Ct_max), color = "red", size = 4) +
    geom_point(data = peaks %>% left_join(df, by = c("Ts" = "run_start_time")), aes(x = Ts, y = Ct_roll), color = "green", size = 4) +
    geom_point(data = peaks %>% left_join(df, by = c("T_end" = "run_start_time")), aes(x = T_end, y = Ct_roll), color = "blue", size = 4) +
    labs(title = paste(county_name, "Outages Over Time"), x = "Time", y = "Rolling Outages") +
    theme_minimal()
  
  return(list(
    county = county_name,
    peaks = peaks,
    outage_events = outage_events,
    inflection_points = inflection_points,
    plot1 = plot1,
    plot2 = plot2,
    plot3 = plot3
  ))
}

county_list <- list(
  Harris = harris_outages_all,
  Tarrant = tarrant_outages_all,
  Dallas = dallas_outages_all,
  Bexar = bexar_outages_all,
  ElPaso = elpaso_outages_all
)


results <- lapply(names(county_list), function(name) {
  analyze_peaks(county_list[[name]], name, 
                start_date = "2021-02-10", 
                end_date = "2021-02-27") ## Change date if needed here
})
names(results) <- names(county_list)

print(results$Harris$plot1)
print(results$Harris$plot2)
print(results$Harris$plot3)

print(results$Tarrant$plot1)
print(results$Tarrant$plot2)
print(results$Tarrant$plot3)

print(results$Dallas$plot1)
print(results$Dallas$plot2)
print(results$Dallas$plot3)

print(results$Bexar$plot1)
print(results$Bexar$plot2)
print(results$Bexar$plot3)

print(results$ElPaso$plot1)
print(results$ElPaso$plot2)
print(results$ElPaso$plot3)

inflection_points <- bind_rows(lapply(names(results), function(name) {
  current_county <- results[[name]]
  if (is.null(current_county) || is.null(current_county$inflection_points)) return(NULL)
  if (!"Ct_max" %in% names(current_county$inflection_points)) return(NULL)
  county_raw_data <- texas_outages_all %>% 
    filter(county == name, 
           run_start_time >= as.POSIXct("2021-02-10"), 
           run_start_time <= as.POSIXct("2021-02-27")) ## Change date if needed here
  
  if(nrow(county_raw_data) == 0) return(NULL)
  
  current_county$inflection_points %>%
    rowwise() %>%
    mutate(
      idx = which.min(abs(county_raw_data$sum - Ct_max)),
      inflection_time = county_raw_data$run_start_time[idx],
      county = name,
      max_outage_time = if(!is.null(current_county$peaks)) {
        current_county$peaks$T_max[which.max(current_county$peaks$Ct_max)]
      } else { as.POSIXct(NA) },
      time_diff_hours = as.numeric(difftime(inflection_time, max_outage_time, units = "hours"))
    ) %>%
    ungroup() %>%
    select(-idx)
}))

print(head(inflection_points))

all_county_peaks <- bind_rows(lapply(results, function(x) {
  x$peaks %>% mutate(county = x$county)
}))

summary_stats <- all_county_peaks %>%
  group_by(county) %>%
  mutate(total_outage_events = n()) %>% 
  group_by(county, outage_type) %>%
  summarize(
    total_events = first(total_outage_events),
    count_type = n(),
    likelihood = (count_type / total_events) * 100,
    avg_impacted = mean(Ct_max, na.rm = TRUE),
    .groups = "drop"
  )

type_risk_factors <- summary_stats %>%
  mutate(risk_factor = likelihood * avg_impacted) %>%
  mutate(county = gsub("([a-z])([A-Z])", "\\1 \\2", county))

combined_risk_ranking <- type_risk_factors %>%
  group_by(county) %>%
  summarize(combined_risk_factor = sum(risk_factor, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(combined_risk_factor))

tx_counties <- map_data("county", "texas") %>%
  mutate(county = stringr::str_to_title(subregion))

map_data_final <- tx_counties %>%
  left_join(combined_risk_ranking, by = "county")

ggplot(data = map_data_final, aes(x = long, y = lat, group = group, fill = combined_risk_factor)) +
  geom_polygon(color = "white", linewidth = 0.1) + 
  scale_fill_gradient(
    low = "#fb9a99", 
    high = "#a50f15", 
    na.value = "#9ecae1", 
    name = "Risk Factor"
  ) +
  coord_map() +
  theme_void() +
  labs(
    title = "Combined Human Impact Risk Factor by County",
    subtitle = "Analysis of Target Counties (Non-target counties in light blue)"
  )

map_data_faceted <- tx_counties %>%
  inner_join(type_risk_factors, by = "county")

ggplot() +
  geom_polygon(data = tx_counties, aes(x = long, y = lat, group = group), 
               fill = "#9ecae1", color = "white", linewidth = 0.1) +
  geom_polygon(data = map_data_faceted, aes(x = long, y = lat, group = group, fill = risk_factor), 
               color = "white", linewidth = 0.1) +
  facet_wrap(~outage_type, ncol = 2) +
  scale_fill_gradient(
    low = "#fb9a99", 
    high = "#a50f15", 
    name = "Risk Factor"
  ) +
  coord_map() +
  theme_void() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    panel.spacing = unit(1, "lines")
  ) +
  labs(
    title = "Risk Factor Breakdown by Outage Type",
    subtitle = "Red = Target Counties | Light Blue = Not in Target Analysis"
  )

all_counties <- unique(texas_outages_all$county)
safe_analyze <- possibly(analyze_peaks, otherwise = NULL)

statewide_results <- lapply(all_counties, function(name) {
  message(paste("Processing:", name))
  county_df <- texas_outages_all %>% filter(county == name)
  safe_analyze(county_df, name)
})
names(statewide_results) <- all_counties

all_state_peaks <- bind_rows(lapply(names(statewide_results), function(name) {
  res <- statewide_results[[name]]
  if (is.null(res) || is.null(res$peaks)) return(NULL)
  res$peaks %>% mutate(county = name)
}))

statewide_stats <- all_state_peaks %>%
  group_by(county) %>%
  mutate(total_outage_events = n()) %>% 
  group_by(county, outage_type) %>%
  summarize(
    total_events = first(total_outage_events),
    count_type = n(),
    likelihood = (count_type / total_events) * 100,
    avg_impacted = mean(Ct_max, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(risk_factor = likelihood * avg_impacted)

statewide_ranking <- statewide_stats %>%
  group_by(county) %>%
  summarize(combined_risk_factor = sum(risk_factor, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(combined_risk_factor)) %>%
  mutate(county = stringr::str_to_title(gsub("([a-z])([A-Z]| County)", "\\1 \\2", county)))

texas_medical_clean <- texas_medical %>%
  rename(county = Geography) %>%
  mutate(county = stringr::str_to_title(gsub("([a-z])([A-Z]| County)", "\\1 \\2", county)),
         county = gsub(" County", "", county)) %>%
  mutate(medical_scale = at_risk_beneficiaries / max(at_risk_beneficiaries, na.rm = TRUE))

tx_map_base <- map_data("county", "texas") %>%
  mutate(county = stringr::str_to_title(subregion))

final_map_data <- tx_map_base %>%
  left_join(statewide_ranking, by = "county") %>%
  left_join(texas_medical_clean, by = "county")

correlation_map_data <- statewide_stats %>%
  mutate(county = stringr::str_to_title(gsub("([a-z])([A-Z]| County)", "\\1 \\2", county))) %>%
  left_join(texas_medical_clean %>% select(county, medical_scale), by = "county") %>%
  mutate(priority_impact = risk_factor * medical_scale) %>%
  inner_join(tx_map_base, by = "county")

ggplot(data = correlation_map_data, aes(x = long, y = lat, group = group, fill = priority_impact)) +
  geom_polygon(data = tx_map_base, aes(x = long, y = lat, group = group), 
               fill = "#bdbdbd", color = "white", linewidth = 0.05) +
  geom_polygon(color = "white", linewidth = 0.05) +
  facet_wrap(~outage_type, ncol = 2) +
  scale_fill_gradient(low = "#9ecae1", high = "#08306b", trans = "sqrt", name = "Priority") +
  coord_map() + theme_void() +
  labs(title = "Correlation: Infrastructure Risk vs Medical Vulnerability",
       subtitle = "Darker = Highest priority for emergency intervention")

ggplot(data = final_map_data, aes(x = long, y = lat, group = group, fill = combined_risk_factor)) +
  geom_polygon(color = "white", linewidth = 0.05) + 
  scale_fill_gradient(
    low = "#9ecae1", 
    high = "#08306b", 
    na.value = "#bdbdbd", 
    trans = "sqrt",
    name = "Risk Score"
  ) +
  coord_map() +
  theme_void() +
  labs(
    title = "Texas Statewide Power Grid Risk Assessment",
    subtitle = "Combined Human Impact Factor (All 254 Counties)"
  )

ggplot(data = final_map_data, aes(x = long, y = lat, group = group, fill = medical_scale)) +
  geom_polygon(color = "white", linewidth = 0.05) + 
  scale_fill_gradient(
    low = "#e5f5e0", 
    high = "#00441b", 
    name = "Medical Index (0-1)"
  ) +
  coord_map() + 
  theme_void() +
  labs(
    title = "Scaled Medically At-Risk Index by County",
    subtitle = "1.0 = Highest Risk in Texas | 0.0 = Lowest Risk",
    caption = "Grey indicates counties with no reported data"
  )