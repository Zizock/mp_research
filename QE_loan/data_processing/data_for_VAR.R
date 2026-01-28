# prepare data for aggregate VAR and panel VAR
# this script is for all log diff series

rm(list=ls())
library(tidyverse)
library(writexl)
library(fastDummies)
library(here)

# ==== Loan, GDP and inflation ====
file_name <- "raw_data_collection.xlsx"

all_sheet <- excel_sheets(here("data", file_name))

# A sample process: write a sample block before looping
# raw_loan_total <- read_excel(here("data", file_name), sheet = all_sheet[1], na = "NA")
#
# # Generate date: the raw Date variable has format as 2000q1
# raw_loan_total <- raw_loan_total |> mutate(
#   year = as.numeric(str_sub(Date, 1, 4)),
#   quarter = as.numeric(str_sub(Date, 6, 6)),
#   month = (quarter-1)*3+1,
#   Date = as.Date(paste(year, month, "01", sep = "-"))
# ) |> select(Date:O)
# 
# # Calculate YoY change and keep the needed part into new data
# cleaned_loan_total <- raw_loan_total |> 
#   arrange(Date) |> 
#   mutate(across(
#     .cols = -Date,
#     .fns = ~ log(.) -lag(log(.), 4),
#     .names = "{.col}"
#   )) |> filter(
#     Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
#   )

# loop for multiple raw data files
for (i in 1:6){
  sheet = all_sheet[i]
  
  temp_data <- read_excel(here("data", file_name), sheet = sheet, na="NA")
  
  # Generate date
  temp_data <- temp_data |> mutate(
    year = as.numeric(str_sub(Date, 1, 4)),
    quarter = as.numeric(str_sub(Date, 6, 6)),
    month = (quarter-1)*3 +1,
    
    Date = as.Date(paste(year, month, "1", sep="-"))
  ) |> select(Date:O)
  
  # Calculate YoY change and keep the needed parts
  temp_data <- temp_data |> 
    arrange(Date) |> 
    mutate(
      across(
        .cols = - Date,
        .fns = ~ log(.) - lag(log(.), 4),
        .names = "{.col}"
      )
    ) |> filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) |> mutate(across(
      .cols = -Date,
      .fns = ~ ifelse(is.finite(.), ., 0)
    ))
  
  # Save to a new data set
  assign(
    paste0("cleaned_", sheet),
    temp_data
  )
}

# ==== QE indicators ====

raw_qe <- read_excel(here("data", file_name), sheet = "qe_indicators")

raw_qe <- raw_qe |> mutate(
  year = as.numeric(str_sub(Date, 1, 4)),
  quarter = as.numeric(str_sub(Date, 6, 6)),
  month = (quarter-1)*3 +1,
  Date = as.Date(paste(year, month, "01", sep = "-"))
) |> select(Date, JGB_level, MB_level)

cleaned_qe <- raw_qe |>
  arrange(Date) |> mutate(
    across(
      .cols = -Date,
      .fns = ~ log(.) - lag(log(.), 4),
      .names = "{.col}"
    )
  ) |> rename_with(
    .fn = ~str_extract(., "^[^_]+"),
    .cols = -Date
  ) |> filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  )

# ==== Daily finance data ====
# Import TOPIX stock index data
file_name = "raw10_eikondata250326_TOPIXby_sector.xlsx"

raw_TOPIX <- read_excel(here("data", file_name))

# Calculate daily change
cleaned_TOPIX <- raw_TOPIX |> mutate(
  Date = as.Date(Date)
) |> arrange(Date) |> mutate(
  across(
    .cols = -Date,
    .fns = ~log(.) - lag(log(.), 1),
    .names = paste0("TOPIX_", "{.col}")
  )
) |> select(Date, TOPIX_1:TOPIX_total) |> 
  filter(
    is.na(TOPIX_1) == 0
  )

# Import VIX datq
file_name = "raw11_nikkeiVIX0125.xlsx"
all_sheet = excel_sheets(here("data", file_name))

# read all sheets in to a list
vix_list <- all_sheet |> 
  map(~ read_excel(here("data", file_name), sheet = .x) |> 
        mutate(Sheet = .x)
      )

# combine all sheets into one data set
raw_VIX <- bind_rows(vix_list) |> rename(
  price = 'Last Price'
) |> select(Date:price) |> arrange(Date) |> 
  mutate(
    price = log(price) # Use log VIX, no diff required.
  )

# merge cleaned_TOPIX and cleaned_VIX by Date
cleaned_TOPIX_VIX <- cleaned_TOPIX |>
  inner_join(raw_VIX, by = "Date")

# rolling regression: sector_TOPIX and overall_TOPIX
# prepare data for rolling regression
data_for_beta <- cleaned_TOPIX_VIX |> 
  select(Date, TOPIX_total, TOPIX_1:TOPIX_17) |> 
  pivot_longer(
    cols = c(-Date, -TOPIX_total),
    names_to = "sector",
    values_to = "sector_TOPIX"
  )

# estimate and collect rolling beta
rolling_beta <- data_for_beta |> 
  group_by(sector) |> 
  arrange(Date) |> 
  mutate(
    beta = rollapply(
      data = cbind(sector_TOPIX, TOPIX_total),
      width = 60,
      FUN = function(x) coef(lm(x[,1] ~ x[,2]))[[2]],
      by.column = FALSE,
      align = "right",
      fill = NA
    )
  ) |> ungroup()

# pivot rolling_beta back to wide format
rolling_beta_wide <- rolling_beta |>
  select(Date, sector, beta) |>
  #distinct() |>  # just in case because TOPIX_total has some floating point error
  pivot_wider(
    names_from = sector,
    values_from = beta
  )
# now I get betas per sector

# next: scale market VIX to by sector VIX
sector_VIX <- rolling_beta_wide |>
  arrange(Date) |> 
  mutate(
    across(
      .cols = -Date,
      .fns = ~ . * cleaned_TOPIX_VIX$price,
      .names = paste0("{.col}")
    )
  ) |> rename_with(
    ~str_replace(., "TOPIX_", "VIX_")
  )

# average to quarterly data
quarter_VIX <- sector_VIX |> 
  mutate(
    year = year(Date),
    quarter = quarter(Date)
  ) |> 
  group_by(year, quarter) |> 
  summarise(
    across(
      .cols = -Date,
      .fns = mean,
      .names = "{.col}"
    )
  ) |> ungroup()

# re-format the date variable
quarter_VIX <- quarter_VIX |> 
  mutate(
    Date = as.Date(paste(year, quarter*3-2, "01", sep = "-"))
  ) |> select(Date, VIX_1:VIX_17)

# combine data sets and export usable data for panel BVAR

code_1 <- colnames(cleaned_loan_total)[2:ncol(cleaned_loan_total)]
code_2 <- colnames(quarter_VIX)[2:ncol(quarter_VIX)]

# 1, define mapping rule
# ==== pick one: ====
qe_indicator = "JGB"
#qe_indicator = "MB"

sector_mapping <- tibble(
  sector = code_1,
  
  gdp_var = code_1,
  inflation_var = code_1,
  loan_var = code_1,
  qe_var = rep(qe_indicator, length(code_1)),
  vix_var = c("VIX_1", "VIX_1", "VIX_4", "VIX_4", "VIX_4", "VIX_2", # C and C1-5
              "VIX_3", "VIX_7", "VIX_7", "VIX_3", "VIX_9", # C6-10
              "VIX_6", "VIX_8", # C11-12
              "VIX_1", "VIX_2", "VIX_3", "VIX_11", "VIX_10", # A B D E F
              "VIX_12", "VIX_13", "VIX_14", "VIX_16", "VIX_17", # G H I J K
              "VIX_10", "VIX_10", "VIX_10", "VIX_10" # L M N O
  )
)

overall_data <- list(
  cleaned_gdp,
  cleaned_inflation,
  cleaned_loan_total,
  cleaned_qe,
  quarter_VIX
)

# 2, define function for extracting and combining variables
get_sector_df <- function(sector, gdp_var, inflation_var, loan_var, qe_var, vix_var) {
  out <- list(
    cleaned_gdp |> select(Date, !!sym(gdp_var)) |> 
      rename(!!sym("gdp") := !!sym(gdp_var)),
    
    cleaned_inflation |> select(Date, !!sym(inflation_var)) |> 
      rename(!!sym("inflation") := !!sym(inflation_var)),
    
    cleaned_loan_total |> select(Date, !!sym(loan_var)) |> 
      rename(!!sym("loan") := !!sym(loan_var)),
    
    cleaned_qe |> select(Date, !!sym(qe_var)) |> 
      rename(!!sym("qe") := !!sym(qe_var)),
    
    quarter_VIX |> select(Date, !!sym(vix_var)) |> 
      rename(!!sym("VIX") := !!sym(vix_var))
  )
  
  temp_df <- reduce(out, inner_join, by = "Date") |> 
    mutate(
      Date = paste0(year(Date), "q", quarter(Date)), # BVAR needs date format as 2004q1
      year = str_sub(Date, 1,4), # for year dummy
    ) |> # generate year dummies
    dummy_cols( select_columns = "year", remove_selected_columns = TRUE)
  
  sector_dummy <- map_dfc(code_1, function(code){
    tibble(!!code := as.integer(code == sector))
  })
  
  bind_cols(temp_df, sector_dummy[rep(1, nrow(temp_df)), ])

}

# 3, rolling for all sectors using parallel map
sector_data <- pmap(
  sector_mapping,
  get_sector_df
)

# 4, name the dataframes according to the sector codes
names(sector_data) <- sector_mapping$sector

# 5, export to excel sheets
write_xlsx(sector_data, here("data", "bvar_data.xlsx"))



