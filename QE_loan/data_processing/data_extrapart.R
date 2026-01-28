# extra parts of chapter 3:
# dig deeper into details, from meeting on Apr 22
# use sector version from March version

rm(list=ls())

library(tidyverse)
library(readxl)
library(writexl)
library(fastDummies)
library(here)

# read data

file_name <- "raw_data_collection.xlsx"

all_sheet <- excel_sheets(here("data", file_name))

# ==== loop read sheets ====

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
  
  # Calculate contributions of sectors
  temp_contrib <- temp_data %>% mutate(
    total_loan = rowSums(across(C1:O), na.rm = TRUE)
  ) 
  
  temp_contrib <- temp_contrib %>% 
    arrange(Date) %>%
    mutate(
      across(
        .cols = C1:O, 
        .fns = ~ (. - lag(., 4)) / (total_loan - lag(total_loan, 4)) * 100,
        .names = "{.col}"
        )
    ) %>% filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>% mutate(
      across(
      .cols = -Date,
      .fns = ~ ifelse(is.finite(.), ., 0)
    )
    ) %>% mutate(
      contrib_sum = rowSums(across(C1:O), na.rm = TRUE)
    )
    
  # Save to a new data set
  assign(
    paste0("level_", sheet),
    temp_data
  )
  
  assign(
    paste0("contrib_", sheet),
    temp_contrib
  )
}

# draw plot
loan_long <- contrib_loan_total %>% 
  pivot_longer(
    cols = c(C1:O),
    names_to = "Sector",
    values_to = "Share"
  )

ggplot(loan_long, aes(x = Date, y = Share, fill = Sector)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  labs(
    title = "",
    y = "Contribution to loan growth",
    x = "Date"
  ) +
  scale_fill_brewer(palette = "Paired") +
  theme_minimal() +
  theme(legend.position = "bottom")

top_sectors <- loan_long %>%
  group_by(Sector) %>%
  summarize(
    avg_contrib = mean(abs(Share), na.rm = TRUE)
    ) %>%
  slice_max(avg_contrib, n = 8) %>%
  pull(Sector)

loan_long <- loan_long %>%
  mutate(
    Sector_grouped = if_else(Sector %in% top_sectors, Sector, "Other")
    )

ggplot(loan_long, aes(x = Date, y = Share, fill = Sector_grouped)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal()

# ==== another version: C combined C1-12 ====

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
  
  # Calculate contributions of sectors
  temp_contrib <- temp_data %>% mutate(
    total_loan = rowSums(across(C1:O), na.rm = TRUE)
  ) 
  
  temp_contrib <- temp_contrib %>% 
    arrange(Date) %>%
    mutate(
      across(
        .cols = c(C, A:O), 
        .fns = ~ (. - lag(., 4)) / (total_loan - lag(total_loan, 4)),
        .names = "{.col}"
      )
    ) %>% filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>% mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0)
      )
    ) %>% mutate(
      contrib_sum = rowSums(across(c(C, A:O)), na.rm = TRUE)
    ) %>% select(Date, C, A:O)
  
  # Save to a new data set
  assign(
    paste0("level_", sheet),
    temp_data
  )
  
  assign(
    paste0("contrib_", sheet),
    temp_contrib
  )
}

loan_long <- contrib_loan_total %>% 
  pivot_longer(
    cols = c(C:O),
    names_to = "Sector",
    values_to = "Share"
  )

top_sectors <- loan_long %>%
  group_by(Sector) %>%
  summarize(
    avg_contrib = mean(abs(Share), na.rm = TRUE)
  ) %>%
  slice_max(avg_contrib, n = 8) %>%
  pull(Sector)

loan_long <- loan_long %>%
  mutate(
    Sector_grouped = if_else(Sector %in% top_sectors, Sector, "Other")
  )

ggplot(loan_long, aes(x = Date, y = Share, fill = Sector_grouped)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "",
    y = "Contribution to loan growth",
    x = "Date"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust =1))

# ==== another version: C combined C1-12, total levels, not shares ====
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
  
  # Calculate contributions of sectors
  temp_contrib <- temp_data %>% mutate(
    total_loan = rowSums(across(C1:O), na.rm = TRUE)
  ) 
  
  temp_contrib <- temp_contrib %>% 
    arrange(Date) %>%
    mutate(
      across(
        .cols = c(C, A:O), 
        .fns = ~ (. - lag(., 4)),
        .names = "{.col}"
      )
    ) %>% filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>% mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0)
      )
    ) %>% mutate(
      contrib_sum = rowSums(across(c(C, A:O)), na.rm = TRUE)
    ) %>% select(Date, C, A:O)
  
  # Save to a new data set
  assign(
    paste0("level_", sheet),
    temp_data
  )
  
  assign(
    paste0("contrib_", sheet),
    temp_contrib
  )
}

loan_long <- contrib_loan_total %>% 
  pivot_longer(
    cols = c(C:O),
    names_to = "Sector",
    values_to = "Share"
  )

top_sectors <- loan_long %>%
  group_by(Sector) %>%
  summarize(
    avg_contrib = mean(abs(Share), na.rm = TRUE)
  ) %>%
  slice_max(avg_contrib, n = 8) %>%
  pull(Sector)

loan_long <- loan_long %>%
  mutate(
    Sector_grouped = if_else(Sector %in% top_sectors, Sector, "Other")
  )

ggplot(loan_long, aes(x = Date, y = Share, fill = Sector_grouped)) +
  geom_bar(stat = "identity", position = "stack") +
  #geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()



# ==== draw loan by sector overtime chart ====
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
  
  # Calculate contributions of sectors
  temp_contrib <- temp_data %>% mutate(
    total_loan = rowSums(across(C1:O), na.rm = TRUE)
  ) 
  
  temp_contrib <- temp_contrib %>% 
    arrange(Date) %>%
    mutate(
      across(
        .cols = c(C, A:O), 
        .fns = ~ (. - lag(., 4)),
        .names = "{.col}"
      )
    ) %>% filter(
      Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
    ) %>% mutate(
      across(
        .cols = -Date,
        .fns = ~ ifelse(is.finite(.), ., 0)
      )
    ) %>% mutate(
      contrib_sum = rowSums(across(c(C, A:O)), na.rm = TRUE)
    ) %>% select(Date, C, A:O)
  
  # Save to a new data set
  assign(
    paste0("level_", sheet),
    temp_data
  )
  
  assign(
    paste0("contrib_", sheet),
    temp_contrib
  )
}

loan_long <- contrib_loan_total %>% 
  pivot_longer(
    cols = c(C:O),
    names_to = "Sector",
    values_to = "Share"
  )

top_sectors <- loan_long %>%
  group_by(Sector) %>%
  summarize(
    avg_contrib = mean(abs(Share), na.rm = TRUE)
  ) %>%
  slice_max(avg_contrib, n = 3) %>%
  pull(Sector)

loan_long <- loan_long %>%
  mutate(
    Sector_grouped = if_else(Sector %in% top_sectors, Sector, "Other")
  )

ggplot(loan_long, aes(x = Date, y = Share, fill = Sector_grouped)) +
  geom_col(position = "stack") +
  geom_hline(yintercept = 0, color = "black") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()

# filter the data
level_loan_total <- level_loan_total %>% 
  filter(
    Date >= as.Date("2004-01-01") & Date < as.Date("2020-01-01")
  )

# to long
loan_long <- level_loan_total %>% 
  pivot_longer(
    cols = c(C, A:O),
    names_to = "Sector",
    values_to = "Total_loan"
  )

# create an index for merge small sectors
top_sectors <- loan_long %>%
  group_by(Sector) %>% 
  summarize(avg_total = mean(Total_loan, na.rm = TRUE)) %>% 
  slice_max(avg_total, n = 8) %>% 
  pull(Sector)

# merge those sectors
loan_grouped <- loan_long %>% 
  mutate(
    Sector_grouped = if_else(Sector %in% top_sectors, Sector, "Other")
  ) %>%
  group_by(Date, Sector_grouped) %>%
  summarize(Total_loan = sum(Total_loan, na.rm = TRUE), .groups = "drop")  # collapse to 8 groups

# calculate share
loan_share <- loan_grouped %>%
  group_by(Date) %>%
  mutate(Total_all = sum(Total_loan, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Share = 100 * Total_loan / Total_all)

# plot
ggplot(loan_share, aes(x = Date, y = Share, fill = Sector_grouped)) +
  geom_area(position = "stack") +
  scale_fill_brewer(palette = "Set1") +
  labs(
    title = "",
    y = "Share of Total Loans by Sector (%)",
    x = "Date",
    fill = "Sector"
  ) +
  theme_minimal() +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust =1))
