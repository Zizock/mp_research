# process US data for estimation: full diff series
rm(list=ls())
library(tidyverse)
library(reshape2)
library(zoo)
library(psych)
library(here)

# ==============================================================================
# ==== read raw data files ====
us_gdp <- read.csv(here("data", "us_realgdp_percapita_q.csv"))
# US gdp, real, in level, per capita, quarterly, seasonally adjusted

us_consumption <- read.csv(here("data", "us_realconsumption_percapita_q.csv"))
# US consumption, real, in level, per capita, quarterly, seasonally adjusted

us_inflation <- read.csv(here("data", "us_inflation_q.csv"))
# measure: GDP implicit price deflator, quarterly, percentage change Q-to-Q, seasonally adjusted

us_ssr <- read_excel(here("data", "us_ssr_monthly.xlsx", sheet="Sheet1"))
# monthly, no need for seasonally adjusted

us_lfpr <- read.csv(here("data", "us_LFPR_q.csv"))
# quarterly, seasonally adjusted

us_unemployment <- read.csv(here("data", "us_unemployment_q.csv"))
# quarterly, seasonally adjusted

us_population <- read.csv(here("data", "us_population_all_q.csv"))
# total population, quarterly, not seasonally adjusted, thousands

us_population16above <- read.csv(here("data", "us_population_16above_q.csv"))
# total population 16+, quarterly, not seasonally adjusted, thousands

# ==============================================================================
# make DATE a date variable
us_gdp$DATE <- as.Date(us_gdp$DATE)
us_consumption$DATE <- as.Date(us_consumption$DATE)
us_inflation$DATE <- as.Date(us_inflation$DATE)
us_ssr$DATE <- as.Date(us_ssr$DATE)
us_lfpr$DATE <- as.Date(us_lfpr$DATE)
us_unemployment$DATE <- as.Date(us_unemployment$DATE)
us_population$DATE <- as.Date(us_population$DATE)
us_population16above$DATE <- as.Date(us_population16above$DATE)

# ==============================================================================
#calculate quarterly average ssr
us_ssr$quarter <- floor_date(us_ssr$DATE, "quarter")
us_ssr_q <- us_ssr %>%
  group_by(quarter) %>% summarize(ssr_q = mean(ssr)) %>%
  rename(DATE = quarter)

# ==============================================================================
#based on availability length of ssr, cut other variables
#and merge together
start_date <- min(us_ssr_q$DATE)
end_date <- max(us_ssr_q$DATE)

us_gdp_filtered <- us_gdp %>%
  filter(DATE >= start_date & DATE <= end_date)

us_consumption_filtered <- us_consumption %>%
  filter(DATE >= start_date & DATE <= end_date)

us_inflation_filtered <- us_inflation %>%
  filter(DATE >= start_date & DATE <= end_date)

us_lfpr_filtered <- us_lfpr %>%
  filter(DATE >= start_date & DATE <= end_date)

us_unemployment_filtered <- us_unemployment %>%
  filter(DATE >= start_date & DATE <= end_date)

us_population_filtered <- us_population %>%
  filter(DATE >= start_date & DATE <= end_date)

us_population16above_filtered <- us_population16above %>%
  filter(DATE >= start_date & DATE <= end_date)

merged_data <- merge(us_gdp_filtered, us_consumption_filtered, by = "DATE")
merged_data <- merge(merged_data, us_inflation_filtered, by = "DATE")
merged_data <- merge(merged_data, us_lfpr_filtered, by = "DATE")
merged_data <- merge(merged_data, us_unemployment_filtered, by = "DATE")
merged_data <- merge(merged_data, us_population_filtered, by = "DATE")
merged_data <- merge(merged_data, us_population16above_filtered, by = "DATE")
merged_data <- merge(merged_data, us_ssr_q, by = "DATE")

# ==============================================================================
#scale data to meet the definition in the model by population
merged_data$gdp <- as.numeric(as.character(merged_data$gdp))
merged_data$population <- as.numeric(as.character(merged_data$population))
merged_data$population16 <- as.numeric(as.character(merged_data$population16))

# for GDP and consumption: rescale from total population
# to the population above 15: fit definition of LFPR
merged_data$gdp = merged_data$gdp * merged_data$population / merged_data$population16
merged_data$consumption = merged_data$consumption * merged_data$population / merged_data$population16

# ==============================================================================
#now the raw data at level is done
write.csv(merged_data, file=here("data", "us_level_data.csv"), row.names = FALSE)

# process the series to match the terms in observation equations

#step: log
merged_data$gdp = log(merged_data$gdp)
merged_data$consumption = log(merged_data$consumption)
merged_data$inflation = log(1 + merged_data$inflation/100)
merged_data$ssr_q = log(1 + merged_data$ssr_q/400)
merged_data$lfpr = log(merged_data$lfpr/100)
merged_data$employment = log(1 - merged_data$unemployment/100)

# gen X_obs by differencing
merged_data$Y_obs = c(NA, diff(merged_data$gdp))
merged_data$C_obs = c(NA, diff(merged_data$consumption))
merged_data$infl_obs = c(NA, diff(merged_data$inflation))
merged_data$ii_obs = c(NA, diff(merged_data$ssr_q))
merged_data$L_obs = c(NA, diff(merged_data$lfpr))
merged_data$H_obs = c(NA, diff(merged_data$employment))

#select the needed ob variables to export the final file
#full diff
# us_obs <- merged_data %>%
#   select(Y_obs, C_obs, infl_obs, ii_obs, L_obs, H_obs)
# write.csv(us_obs, file="us_obs_file_full_diff.csv", row.names = FALSE)

#partial diff
us_obs <- merged_data %>%
  select(Y_obs, C_obs, infl_obs, ii_obs, lfpr, employment)
write.csv(us_obs, file=here("data", "us_obs_file_partial_diff.csv"), row.names = FALSE)



plot(merged_data$DATE, us_obs$y_obs, type="l")
lines(merged_data$DATE, us_obs$c_obs, type="l", col="red")

plot(merged_data$DATE, us_obs$ssr_q, type="l")













