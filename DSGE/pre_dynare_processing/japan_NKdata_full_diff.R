# process Japan data for estimation: diff series for Y, C, infl and ii
rm(list=ls())
library(tidyverse)
library(reshape2)
library(zoo)
library(psych)
library(here)

# read raw data series: japan
japan_gdp <- read.csv(here("data", "japan_realgdp_q.csv"))

japan_consumption <- read.csv(here("data", "japan_realconsumption_q.csv"))

japan_inflation <- read.csv(here("data", "japan_inflation_q.csv"))

japan_ssr <- read_excel(here("data", "japan_ssr_monthly.xlsx", sheet="Sheet1"))

japan_lfpr <- read.csv(here("data", "japan_LFPR_q.csv"))

japan_unemployment <- read.csv(here("data", "japan_unemployment_q.csv"))

japan_employed_person <- read.csv(here("data", "japan_employed_person.csv"))

japan_gdp$DATE <- as.Date(japan_gdp$DATE)
japan_consumption$DATE <- as.Date(japan_consumption$DATE)
japan_inflation$DATE <- as.Date(japan_inflation$DATE)
japan_ssr$DATE <- as.Date(japan_ssr$DATE)
japan_lfpr$DATE <- as.Date(japan_lfpr$DATE)
japan_unemployment$DATE <- as.Date(japan_unemployment$DATE)
japan_employed_person$DATE <- as.Date(japan_employed_person$DATE)

#calculate quarterly average ssr
japan_ssr$quarter <- floor_date(japan_ssr$DATE, "quarter")
japan_ssr_q <- japan_ssr %>%
  group_by(quarter) %>% summarize(ssr_q = mean(ssr)) %>%
  rename(DATE = quarter)

#based on availability length of ssr, cut other variables
#and merge together
start_date <- min(japan_ssr_q$DATE)
end_date <- max(japan_ssr_q$DATE)

japan_gdp_filtered <- japan_gdp %>%
  filter(DATE >= start_date & DATE <= end_date)

japan_consumption_filtered <- japan_consumption %>%
  filter(DATE >= start_date & DATE <= end_date)

japan_inflation_filtered <- japan_inflation %>%
  filter(DATE >= start_date & DATE <= end_date)

japan_lfpr_filtered <- japan_lfpr %>%
  filter(DATE >= start_date & DATE <= end_date)

japan_unemployment_filtered <- japan_unemployment %>%
  filter(DATE >= start_date & DATE <= end_date)

japan_employed_person_filtered <- japan_employed_person %>%
  filter(DATE >= start_date & DATE <= end_date)

merged_data <- merge(japan_gdp_filtered, japan_consumption_filtered, by = "DATE")
merged_data <- merge(merged_data, japan_inflation_filtered, by = "DATE")
merged_data <- merge(merged_data, japan_lfpr_filtered, by = "DATE")
merged_data <- merge(merged_data, japan_unemployment_filtered, by = "DATE")
merged_data <- merge(merged_data, japan_employed_person_filtered, by = "DATE")
merged_data <- merge(merged_data, japan_ssr_q, by = "DATE")

#scale data to meet the definition in the model
merged_data$gdp <- as.numeric(as.character(merged_data$gdp))
merged_data$lfpr <- as.numeric(as.character(merged_data$lfpr))
merged_data$unemployment <- as.numeric(as.character(merged_data$unemployment))

#first: seasonally adjust the employed_person data
library(seasonal)
employed_ts <- ts(merged_data$employed_person, start=c(1995,1), frequency=4)
employed_seas <- seas(employed_ts)
merged_data$employed_person <- final(employed_seas)

merged_data$population16 <- (merged_data$employed_person / (1-merged_data$unemployment/100) ) / (merged_data$lfpr/100)

#scale total gdp and consumption to per capita for working age population
merged_data$gdp = merged_data$gdp / merged_data$population16
merged_data$consumption = merged_data$consumption / merged_data$population16

#now the raw data at level is done
write.csv(merged_data, file=here("data", "japan_level_data.csv"), row.names = FALSE)



#step: log
merged_data$gdp = log(merged_data$gdp)
merged_data$consumption = log(merged_data$consumption)
merged_data$inflation = log(merged_data$inflation/100 + 1)
merged_data$ssr_q = log(1 + merged_data$ssr_q/400)
merged_data$lfpr = log(merged_data$lfpr/100)
merged_data$employment = log(1 - merged_data$unemployment/100)

# gen X_obs by differencing
merged_data$Y_obs = c(NA, diff(merged_data$gdp))
merged_data$C_obs = c(NA, diff(merged_data$consumption))
merged_data$infl_obs = c(NA, diff(merged_data$inflation))
merged_data$ii_obs = c(NA, diff(merged_data$ssr_q))
merged_data$L_obs = merged_data$lfpr
merged_data$H_obs = merged_data$employment

#select the needed ob variables to export the final file
#full diff
# us_obs <- merged_data %>%
#   select(Y_obs, C_obs, infl_obs, ii_obs, L_obs, H_obs)
# write.csv(us_obs, file="us_obs_file_full_diff.csv", row.names = FALSE)

#partial diff
us_obs <- merged_data %>%
  select(Y_obs, C_obs, infl_obs, ii_obs, L_obs, H_obs)
write.csv(us_obs, file=here("data", "japan_obs_file_partial_diff.csv"), row.names = FALSE)


# 
# plot(merged_data$DATE, us_obs$y_obs, type="l")
# lines(merged_data$DATE, us_obs$c_obs, type="l", col="red")
# 
# plot(merged_data$DATE, us_obs$ssr_q, type="l")







