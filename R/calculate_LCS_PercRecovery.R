# Tidal Wetlands Hg Journal Article
# Purpose: Calculate Average Percent recovery of LCS samples for MeHg, THg, and Organic Carbon
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# Load packages
library(tidyverse)
library(readxl)
library(lubridate)

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
  summarize(
    N = n(),
    min_per_r = min(perc_recovery),
    max_per_r = max(perc_recovery),
    avg_per_r = round(mean(perc_recovery), 1)
  )

lcs_mehg_summ
# Average LCS Percent Recovery value for MeHg: 99.0% for 68 samples


# Import QA sample data for Hg and organic carbon samples analyzed by Bryte Lab
# This file is already cleaned and has duplicates removed. There was no consistent way
  # of doing this, so we deleted duplicate records manually in the file.
qa_samp_bryte <- read_excel("data/Bryte Instrument QC Data by Date_Measure_Analyte Q_DBoz_MHead_20210930.xlsx")

# Calculate minimum, maximum and average of LCS percent recovery for Hg and organic carbon samples
lcs_hg_oc_summ <- qa_samp_bryte %>%
  mutate(QCR_PERCENT_RECOVERY = QCR_PERCENT_RECOVERY * 100) %>%
  group_by(ANL_ANALYTE_NAME) %>%
  summarize(
    N = n(),
    min_per_r = min(QCR_PERCENT_RECOVERY),
    max_per_r = max(QCR_PERCENT_RECOVERY),
    avg_per_r = round(mean(QCR_PERCENT_RECOVERY), 1)
  )

lcs_hg_oc_summ
# Average LCS Percent Recovery value for THg: 98.8% for 113 samples
# Average LCS Percent Recovery value for Organic Carbon: 99.5% for 67 samples

