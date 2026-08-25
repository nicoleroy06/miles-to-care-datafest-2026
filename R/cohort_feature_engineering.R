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

# ── 1. LOAD SOURCE DATA ──────────────────────────────────────────────────────

message("Loading source datasets...")

encounters <- fread(file.path(data_dir, "encounters.csv"))
patients <- fread(file.path(data_dir, "patients.csv"))
diagnosis <- fread(file.path(data_dir, "diagnosis.csv"))
social_determinants <- fread(file.path(data_dir, "social_determinants.csv"))
departments <- fread(file.path(data_dir, "departments.csv"))

message("Source datasets loaded successfully.")

# ── 2. IDENTIFY TRANSPORTATION-BURDENED PATIENTS ─────────────────────────────

# Keep only social determinant responses related to transportation needs.
transport_responses <- social_determinants %>%
  filter(Domain == "Transportation Needs") %>%
  select(
    PatientDurableKey,
    EncounterKey,
    DisplayName,
    AnswerText
  )

# Flag a patient as transportation-burdened if they answered "Yes"
# to a transportation-related screening question at least once.
transport_patient_flag <- transport_responses %>%
  group_by(PatientDurableKey) %>%
  summarise(
    transport_burdened = as.integer(any(AnswerText == "Yes")),
    n_transport_screens = n_distinct(EncounterKey),
    .groups = "drop"
  )

# Review the number of burdened and non-burdened patients.
table(transport_patient_flag$transport_burdened)

# ── 3. CREATE ENCOUNTER-LEVEL TRANSPORTATION FLAG ─────────────────────────────

# Create an encounter-level indicator showing whether transportation burden
# was reported during a specific healthcare encounter.
transport_encounter_flag <- transport_responses %>%
  group_by(EncounterKey) %>%
  summarise(
    transport_burden_at_encounter = as.integer(any(AnswerText == "Yes")),
    .groups = "drop"
  )

# ── 4. CREATE ADDITIONAL SOCIAL DETERMINANT FLAGS ─────────────────────────────

# Define social determinant domains with yes/no responses.
yes_no_domains <- c(
  "Food insecurity",
  "Financial Resource Strain",
  "Housing Stability",
  "intimate partner violance",
  "Utilities"
)

# Create patient-level indicators for whether each social determinant
# was ever reported as positive.
sdoh_yes_no <- social_determinants %>%
  filter(Domain %in% yes_no_domains) %>%
  group_by(PatientDurableKey, Domain) %>%
  summarise(
    positive = as.integer(any(AnswerText == "Yes")),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = Domain,
    values_from = positive,
    values_fill = 0L,
    names_prefix = "sdoh_"
  )

# Standardize column names for easier downstream analysis.
names(sdoh_yes_no) <- names(sdoh_yes_no) %>%
  str_replace_all(" ", "_") %>%
  str_to_lower() %>%
  { ifelse(. == "patientdurablekey", "PatientDurableKey", .) }

# Create a separate stress indicator because stress responses
# include several positive-response categories.
stress_flag <- social_determinants %>%
  filter(Domain == "stress") %>%
  group_by(PatientDurableKey) %>%
  summarise(
    sdoh_stress = as.integer(
      any(AnswerText %in% c("Yes", "A lot", "Somewhat", "Quite a bit"))
    ),
    .groups = "drop"
  )

# ── 5. PREPARE ENCOUNTER DATA ────────────────────────────────────────────────

# Parse encounter dates and retain variables needed for patient-journey analysis.
encounters_clean <- encounters %>%
  mutate(
    encounter_date = mdy(Date)
  ) %>%
  select(
    EncounterKey,
    PatientDurableKey,
    encounter_date,
    Type,
    VisitType,
    VisitTypeDescription,
    DepartmentKey,
    PrimaryDiagnosisKey,
    AttendingProviderDurableKey,
    IsEdVisit,
    IsHospitalAdmission,
    IsHospitalOutpatientVisit,
    IsInpatientAdmission,
    IsObservation,
    IsOutpatientFaceToFaceVisit,
    AdmissionSource,
    AdmissionType
  )

# ── 6. ATTACH DIAGNOSIS INFORMATION ──────────────────────────────────────────

# Keep one record per diagnosis key to avoid duplicate joins.
diagnosis_unique <- diagnosis %>%
  distinct(DiagnosisKey, .keep_all = TRUE)

# Add diagnosis code and high-level diagnosis group to each encounter.
encounters_clean <- encounters_clean %>%
  left_join(
    diagnosis_unique %>%
      select(
        DiagnosisKey,
        DiagnosisValue,
        GroupName,
        GroupCode
      ),
    by = c("PrimaryDiagnosisKey" = "DiagnosisKey")
  )

# ── 7. ATTACH PATIENT DEMOGRAPHICS ───────────────────────────────────────────

# Add demographic and patient-level characteristics to each encounter.
encounters_clean <- encounters_clean %>%
  left_join(
    patients %>%
      select(
        DurableKey,
        OmbRace,
        OmbEthnicity,
        SexAssignedAtBirth,
        PatientBirthYearBin,
        SmokingStatus,
        MyChartStatus,
        MaritalStatus,
        CensusBlockGroupFipsCode,
        VitalStatus
      ),
    by = c("PatientDurableKey" = "DurableKey")
  )

# Approximate patient age at the time of each encounter.
encounters_clean <- encounters_clean %>%
  mutate(
    approx_age = year(encounter_date) - as.numeric(PatientBirthYearBin)
  )

# ── 8. ATTACH DEPARTMENT INFORMATION ─────────────────────────────────────────

# Add department characteristics to each encounter.
encounters_clean <- encounters_clean %>%
  left_join(
    departments %>%
      select(
        DepartmentKey,
        DepartmentName,
        DepartmentSpecialty,
        DepartmentType,
        City,
        County
      ),
    by = "DepartmentKey"
  )

# ── 9. BUILD TRANSPORTATION-SCREENED PATIENT COHORT ──────────────────────────

# Identify patients who received at least one transportation screening.
screened_patients <- transport_patient_flag$PatientDurableKey

# Retain all encounters for transportation-screened patients so their
# complete healthcare journey can be analyzed.
cohort <- encounters_clean %>%
  filter(PatientDurableKey %in% screened_patients)

# Review cohort size.
message("Cohort encounters: ", nrow(cohort))
message(
  "Unique patients in cohort: ",
  n_distinct(cohort$PatientDurableKey)
)

# Attach patient-level and encounter-level transportation indicators.
cohort <- cohort %>%
  left_join(
    transport_patient_flag,
    by = "PatientDurableKey"
  ) %>%
  left_join(
    transport_encounter_flag,
    by = "EncounterKey"
  )

# ── 10. ATTACH ADDITIONAL SOCIAL DETERMINANT FLAGS ───────────────────────────

# Add the broader social determinant indicators to the analysis cohort.
cohort <- cohort %>%
  left_join(
    sdoh_yes_no,
    by = "PatientDurableKey"
  ) %>%
  left_join(
    stress_flag,
    by = "PatientDurableKey"
  ) %>%
  mutate(
    across(
      starts_with("sdoh_"),
      ~ replace_na(.x, 0L)
    )
  )
