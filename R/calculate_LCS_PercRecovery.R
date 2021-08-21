# Tidal Wetlands Hg Journal Article
# Purpose: Calculate Average Percent recovery of LCS samples for MeHg, THg, and Organic Carbon
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# Load packages
library(tidyverse)
library(readxl)

# Source functions
source("R/global_funcs.R")

# Define file path for the QA sample data on the Tidal Wetlands Hg Journal Article SharePoint site
fp_data <- twhg_abs_sp_path("Data")

# Import QA sample data for MeHg samples analyzed by MLML
qa_samp_mlml <-
  read_excel(
    file.path(fp_data, "Tidal_Wetlands_Conc_Data_10-07-2019.xlsx"),
    sheet = "QA Data- MLML"
  )

# Calculate minimum, maximum and average of LCS percent recovery for MeHg samples
lcs_mehg_summ <- qa_samp_mlml %>%
  filter(SampleTypeCode == "LCS") %>%
  mutate(perc_recovery = Result/ExpectedValue * 100) %>%
  group_by(SampleTypeCode) %>%
  summarize(
    N = n(),
    min_per_r = min(perc_recovery),
    max_per_r = max(perc_recovery),
    avg_per_r = round(mean(perc_recovery), 1)
  )

lcs_mehg_summ
# Average LCS Percent Recovery value for MeHg: 99.0% for 68 samples

