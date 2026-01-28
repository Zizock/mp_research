# split all sectors into three parts:
# group1: all manufacturing
# group2: selected groups
# group3: unselected services

rm(list=ls())
library(tidyverse)
library(writexl)
library(fastDummies)
library(here)

# read data

file_name <- "raw_data_collection.xlsx"

all_sheet <- excel_sheets(here("data", file_name))

# avoid looping. do it one by one

# ==== define groups ====

group_1 <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10", "C11", "C12")

group_2 <- c("D", "H", "I", "J", "K")

group_3 <- LETTERS[which(LETTERS == "D"):which(LETTERS == "O")] %>% setdiff(group_2)


# ==== loan_total ====
# 1=total, 2=small, 3=medium, 4=large
loan_total <- read_excel(here("data", file_name), sheet = all_sheet[1], na="NA")

# Generate date
loan_total <- loan_total |> mutate(
  year = as.numeric(str_sub(Date, 1, 4)),
  quarter = as.numeric(str_sub(Date, 6, 6)),
  month = (quarter-1)*3 +1,
  
  Date = as.Date(paste(year, month, "1", sep="-"))
) |> select(Date:O)

# loan growth rate
loan_growth <- loan_total |> 
  arrange(Date) |> 
  mutate(across(
    .cols = -Date,
    .fns = ~ log(.) -lag(log(.), 4),
    .names = "{.col}"
  )) |> filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  ) |> mutate(
    across(
      .cols = -Date,
      .fns = ~ ifelse(is.finite(.), ., 0)
    )
  )

# weighted loan growth function
make_loan_growth <- function(df, group) {
  df %>%
    select(Date, all_of(group)) %>%
    mutate(
      total_sum = rowSums(across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ (. - lag(., 4)) / lag(total_sum, 4) * 100,
        .names = "{.col}" # this is default
      )
    ) %>%
    mutate(
      weighted_loan_growth = rowSums(across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>%
    mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0),
        .names = "{.col}"
      )
    ) %>%
    select(Date, weighted_loan_growth)
}

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  assign(
    paste0("loan_growth_", i),
    make_loan_growth(loan_total, group)
  )
}

# ==== gdp_total ====
gdp_total <- read_excel(here("data", file_name), sheet = all_sheet[5], na="NA")

# Generate date
gdp_total <- gdp_total |> mutate(
  year = as.numeric(str_sub(Date, 1, 4)),
  quarter = as.numeric(str_sub(Date, 6, 6)),
  month = (quarter-1)*3 +1,
  
  Date = as.Date(paste(year, month, "1", sep="-"))
) |> select(Date:O)

# gdp growth rate
gdp_growth <- gdp_total |> 
  arrange(Date) |> 
  mutate(across(
    .cols = -Date,
    .fns = ~ log(.) -lag(log(.), 4),
    .names = "{.col}"
  )) |> filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  ) |> mutate(
    across(
      .cols = -Date,
      .fns = ~ ifelse(is.finite(.), ., 0)
    )
  )

# weighted gdp growth function
make_gdp_growth <- function(df, group) {
  df %>%
    select(Date, all_of(group)) %>%
    mutate(
      total_sum = rowSums(across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ (. - lag(., 4)) / lag(total_sum, 4) * 100,
        .names = "{.col}" # this is default
      )
    ) %>%
    mutate(
      weighted_gdp_growth = rowSums(across(where(is.numeric)), na.rm = TRUE)
    ) %>%
    filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>%
    mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0),
        .names = "{.col}"
      )
    ) %>%
    select(Date, weighted_gdp_growth)
}

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  assign(
    paste0("gdp_growth_", i),
    make_gdp_growth(gdp_total, group)
  )
}

# ==== inflation ====

# calculate weighting matrix function
calculate_weight <- function(df, group) {
  df %>% 
    select(Date, all_of(group)) %>% 
    mutate(
      across(
        .cols = where(is.numeric),
        .fns = ~ abs(. - lag(., 4)),
        .names = "delta_{.col}"
      ),
      abs_acum_loan = rowSums(across(starts_with("delta_")), na.rm = TRUE)
    ) %>% 
    mutate(
      across(
        .cols = all_of(group),
        .fns = ~ abs(. - lag(., 4)) / abs_acum_loan,
        .names = "{.col}"
      )
    ) %>% 
    select(Date, all_of(group)) %>% 
    filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    )
}

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  assign(
    paste0("weight_matrix_", i),
    calculate_weight(loan_total, group)
  )
}

# read inflation data
inflation_total <- read_excel(here("data", file_name), sheet = all_sheet[6], na="NA")
# Generate date
inflation_total <- inflation_total |> mutate(
  year = as.numeric(str_sub(Date, 1, 4)),
  quarter = as.numeric(str_sub(Date, 6, 6)),
  month = (quarter-1)*3 +1,
  
  Date = as.Date(paste(year, month, "1", sep="-"))
) |> select(Date:O)

inflation_growth <- inflation_total |> 
  arrange(Date) |> 
  mutate(across(
    .cols = -Date,
    .fns = ~ log(.) -lag(log(.), 4),
    .names = "{.col}"
  )) %>%
  filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  ) |> mutate(
    across(
      .cols = -Date,
      .fns = ~ ifelse(is.finite(.), ., 0)
    )
  )

# weight inflation function
weight_inflation <- function(df, weight_matrix, group) {
  df %>%
    select(Date, all_of(group)) %>%
    mutate(
      across(
        .cols = all_of(group),
        .fns = ~ .x * weight_matrix[[cur_column()]],
        .names = "{.col}"
      ),
      weighted_inflation = rowSums(across(where(is.numeric)), na.rm = TRUE) * 100
    ) %>% select(Date, weighted_inflation) %>%
    mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0),
        .names = "{.col}"
      )
    ) %>% 
    filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    )
}

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  weight_matrix <- get(paste0("weight_matrix_", i))
  assign(
    paste0("inflation_growth_", i),
    weight_inflation(inflation_growth, weight_matrix, group)
  )
}


# ==== calculate VIX ====
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
  ) |> select(Date, VIX_1:VIX_17) %>% 
  filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  )

# combine data sets and export usable data for panel BVAR

# code_1 collects all sector codes from macro variables
code_1 <- colnames(loan_total)[2:ncol(loan_total)]
# code_2 collects all sector codes from VIX series
code_2 <- colnames(quarter_VIX)[2:ncol(quarter_VIX)]

matched_VIX <- quarter_VIX %>% select(Date) %>% mutate(
  C = quarter_VIX$VIX_1,
  C1 = quarter_VIX$VIX_1,
  C2 = quarter_VIX$VIX_4,
  C3 = quarter_VIX$VIX_4,
  C4 = quarter_VIX$VIX_4,
  C5 = quarter_VIX$VIX_2,
  C6 = quarter_VIX$VIX_3,
  C7 = quarter_VIX$VIX_7,
  C8 = quarter_VIX$VIX_7,
  C9 = quarter_VIX$VIX_3,
  C10 = quarter_VIX$VIX_9,
  C11 = quarter_VIX$VIX_6,
  C12 = quarter_VIX$VIX_8,
  
  A = quarter_VIX$VIX_1,
  B = quarter_VIX$VIX_2,
  D = quarter_VIX$VIX_3,
  E = quarter_VIX$VIX_11,
  F = quarter_VIX$VIX_10,
  G = quarter_VIX$VIX_12,
  H = quarter_VIX$VIX_13,
  I = quarter_VIX$VIX_14,
  J = quarter_VIX$VIX_16,
  K = quarter_VIX$VIX_17,
  L = quarter_VIX$VIX_10,
  M = quarter_VIX$VIX_10,
  N = quarter_VIX$VIX_10,
  O = quarter_VIX$VIX_10
)

# weighted VIX function
weight_vix <- function(df, weight_matrix, group) {
  df %>%
    select(Date, all_of(group)) %>%
    mutate(
      across(
        .cols = all_of(group),
        .fns = ~ .x * weight_matrix[[cur_column()]],
        .names = "{.col}"
      ),
      weighted_vix = rowSums(across(where(is.numeric)), na.rm = TRUE)
    ) %>% select(Date, weighted_vix) %>%
    mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0),
        .names = "{.col}"
      )
    ) %>% 
    filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    )
}

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  weight_matrix <- get(paste0("weight_matrix_", i))
  assign(
    paste0("vix_", i),
    weight_vix(matched_VIX, weight_matrix, group)
  )
}

# ==== combine all data ====

# join data function
join_data <- function(df1, df2, df3, df4) {
  df1 %>%
    inner_join(df2, by = "Date") %>%
    inner_join(df3, by = "Date") %>%
    inner_join(df4, by = "Date")
} %>% rename(
  gdp = weighted_gdp_growth,
  inflation = weighted_inflation,
  loan = weighted_loan_growth,
  VIX = weighted_vix
)

# apply to 3 groups
for (i in 1:3) {
  group <- get(paste0("group_", i))
  assign(
    paste0("data_", i),
    join_data(
      get(paste0("gdp_growth_", i)),
      get(paste0("inflation_growth_", i)),
      get(paste0("loan_growth_", i)),
      get(paste0("vix_", i))
    )
  )
}

write_list <- list(
  Sheet1 = data_1,
  Sheet2 = data_2,
  Sheet3 = data_3
)

write_xlsx(
  write_list,
  path = here("data", "by_group_data.xlsx"),
  col_names = TRUE,
  format_headers = TRUE
)