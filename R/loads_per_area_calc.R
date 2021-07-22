# Tidal Wetlands Hg Journal Article
# Purpose: Calculate daily loads per area for all Hg parameters and wetlands using both
  # event and monthly loads
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code and Functions -----------------------------------------------

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(magrittr)

# Source functions
source("R/global_funcs.R")

# Define file path for the load data on the Tidal Wetlands Hg Journal Article SharePoint site
fp_load_data <- twhg_abs_sp_path("Data")


# 2. Import Load data -----------------------------------------------------

# Event Loads
df_load_event_orig <- read_excel(
  file.path(fp_load_data, "Consolidated_Hg_Event_Loads_All_Wetlands.xlsx")
)

# Monthly Loads
l_load_month_orig <- map(
  c("THg", "MeHg"),
  ~read_excel(
    file.path(fp_load_data, "Consolidated_Hg_Monthly_Loads_All_Wetlands.xlsx"),
    sheet = .x
  )
)


# 3. Prepare Load data ----------------------------------------------------

# Event Loads - convert dttm to date, remove flow per tide variable, and rename Hg load variables
df_load_event_clean <- df_load_event_orig %>%
  mutate(tide_date = as_date(tide_date)) %>%
  select(-flow_L_per_tide)

# Monthly Loads - convert dttm to date, select variables to keep, and rename Hg load variables
l_load_month_clean <- l_load_month_orig %>%
  map(
    ~select(.x, Wetland, Date, contains("Hg_"), total_n_days_in_month) %>%
      mutate(Date = as_date(Date))
  )

# Combine monthly load data
df_load_month <-
  left_join(l_load_month_clean[[1]], l_load_month_clean[[2]], by = c("Wetland", "Date"))

# Check if total_n_days_in_month variables are the same
all(df_load_month$total_n_days_in_month.x == df_load_month$total_n_days_in_month.y)
# Yes, they are

# We will use total_n_days_in_month.x since they are both the same
df_load_month_clean <- df_load_month %>%
  select(-total_n_days_in_month.y) %>%
  rename(total_n_days = total_n_days_in_month.x)


# 4. Calculate Load per area ----------------------------------------------

# Convert event loads to daily loads (event loads are for a 25-hour period)
df_load_event_clean2 <- df_load_event_clean %>%
  mutate(across(contains("Hg_"), ~.x/25 * 24)) %>%
  rename_with(~str_remove(.x, "_g_per_tide$"), ends_with("tide"))

# Convert monthly loads to daily loads (divide by total_n_days)
df_load_month_clean2 <- df_load_month_clean %>%
  mutate(across(contains("Hg_"), ~.x/total_n_days)) %>%
  select(-total_n_days) %>%
  rename_with(~str_remove(.x, "_g_per_month$"), ends_with("per_month"))

# Create a data frame of wetland acreages
df_wetland_area <- tibble(
  Wetland = sort(unique(df_load_event_clean2$Wetland)),
  area_acres = c(61.9, 22.5, 24.8, 31.3)
)

# Convert acreage to square meters to be consistent with Mitchell & Gilmour, 2012
df_wetland_area %<>%
  mutate(area_m2 = area_acres * 4046.8564) %>%
  select(-area_acres)

# Calculate daily loads per square meter for both methods (event and monthly)
l_load_per_area <-
  list("event" = df_load_event_clean2, "monthly" = df_load_month_clean2) %>%
  map(
    ~left_join(.x, df_wetland_area) %>%
      mutate(across(ends_with("Hg"), ~.x/area_m2)) %>%
      select(-area_m2) %>%
      # Convert to ng to be consistent with Mitchell & Gilmour, 2012
      mutate(across(where(is.numeric), ~.x * 10^9)) %>%
      # Add a variable for load units
      mutate(Units = "ng/day/m2")
  ) %>%
  # Don't include the last 3 sampling events for Cosumnes, since the wetland flooded
    # and was out of bank
  map_at(vars("event"), ~filter(.x, !(Wetland == "Cosumnes" & tide_date > "2019-01-01"))) %>%
  map_at(vars("monthly"), ~filter(.x, !(Wetland == "Cosumnes" & Date > "2019-01-01")))

# Export calculated loads
iwalk(
  l_load_per_area,
  ~write_excel_csv(.x, file = file.path("output", paste0("loads_per_area_", .y, ".csv")))
)

