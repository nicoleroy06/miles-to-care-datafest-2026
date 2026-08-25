###############################################################################
# Miles to Care: Transportation Barriers & Patient Journeys
# ASA DataFest 2026
#
# Cohort Building & Feature Engineering
#
# This script prepares encounter-level healthcare data for analysis of
# transportation barriers and patient journeys.
#
# The original ASA DataFest healthcare datasets are not included in this
# repository. Users should place authorized source files in a local data folder.
###############################################################################

library(tidyverse)
library(data.table)
library(lubridate)

# Set the path to the folder containing the authorized DataFest source files.
# Example:
# data_dir <- "data/raw"

data_dir <- "data/raw"
