# ==== Install if needed ====
if (!requireNamespace("haven", quietly = TRUE)) {
  install.packages("haven")
}
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("stringr", quietly = TRUE)) {
  install.packages("stringr")
}

# ==== Load required packages ====
library(haven)      # for read_sav() and write_sav()
library(dplyr)      # for select(), filter(), pipe
library(stringr)    # for string helpers inside select()

# Helper to prefer *_r over non-recoded duplicates
prefer_recoded_columns <- function(df) {
  
  # Get column names that have a recoded counterpart
  recoded_columns <- df |> 
    colnames() |> 
    # suffix "_r" means they are recoded 
    str_subset(pattern = "_r$") |>  
    # remove the suffix again to get the non-recoded column names
    str_remove(pattern = "_r$") 
  
  # Remove non-recoded columns that have a recoded counterpart
  df <- df |> select(!any_of(recoded_columns))
  
  return(df)
}

# ==== Load dataset ====
data_brabant <- read_sav(
  "O:/fsw/Data FSW/MedPsy/Brabant Studie/Data merges/DATA CLEANING/Merge September 2023/MERGES/PREGNANCY_FOLLOWUP_OBSTETRIC_FATHERDATA_MERGE.sav"
)

dataset_id_test_2 <- data_brabant |>
  select(
    # Category: Biological
    # Subscale: Medication
    # Medication usage
    matches("^Medication_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    # Medication without doctor's prescription
    matches("^Medication_unreg_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    # Subscale: Doctor Visits
    # GP visit (Yes/No)
    matches("^Doctorvisit_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    # Category: (Post-) Delivery information
    # Subscale: Hospital and Consultations
    # Hospital visits or stays
    matches("^Hospital_.*(?<!_F_)(?<=_)6mPP(?:_r)?$", perl = TRUE),
    matches("^Hospital_.*(?<!_F_)(?<=_)2yPP(?:_r)?$", perl = TRUE),
    # Days in hospital
    matches("^Dayshospital_.*(?<!_F_)(?<=_)1yPP(?:_r)?$", perl = TRUE),
    # Category: Demographic
    # Subscale: Language & Migration Status
    # Language understanding of the child
    matches("^Language_.*(?<!_F_)(?<=_)2yPP(?:_r)?$", perl = TRUE),
    # Migration background
    matches("^Migration_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    # Category: Psychological
    # Subscale: Depressive Symptoms
    # Edinburgh Depression Scale
    matches("^EDS_.*(?<!_F_)(?<=_)2y6mPP(?:_r)?$", perl = TRUE),
    # Subscale: Anxiety
    # Symptom Check-List-90 - Subscale anxiety
    matches("^SCL90_.*(?<!_F_)(?<=_)3y6mPP(?:_r)?$", perl = TRUE),
    # Category: Social-Relational
    # Subscale: Work and Absenteeism
    # Childcare in the organization
    matches("^Childcare_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    matches("^Childcare_.*(?<!_F_)(?<=_)3yPP(?:_r)?$", perl = TRUE),
    # Compressed workweek (4x9 hours)
    matches("^Compressedwork_.*(?<!_F_)(?<=_)2yPP(?:_r)?$", perl = TRUE),
    # Family Supportive Supervisor Behaviors - Short Form
    matches("^FSSB_SF_.*(?<!_F_)(?<=_)6mPP(?:_r)?$", perl = TRUE),
    # Individual Work Performance Questionnaire
    matches("^IWPQ_.*(?<!_F_)(?<=_)1y6mPP(?:_r)?$", perl = TRUE),
    # Category: Biological - Father
    # Subscale: Chronic conditions father
    # Diagnosis of diabetes - father
    matches("^Diabetes_.*(?<=_)F_28(?:_r)?$", perl = TRUE),
    # Diagnosis of eczema - father
    matches("^Eczema_.*(?<=_)F_28(?:_r)?$", perl = TRUE),
    # Diagnosis of hypertension - father
    matches("^Hypertension_.*(?<=_)F_28(?:_r)?$", perl = TRUE),
    # Diagnosis of rheumatism - father
    matches("^Rheumatism_.*(?<=_)F_28(?:_r)?$", perl = TRUE),
    # Diagnosis of other chronic disease(s) - father
    matches("^Chronicdisease_other_.*(?<=_)F_28(?:_r)?$", perl = TRUE),
    # Category: Obstetric data
    # Subscale: Breastfeeding & Nutrition
    # Number of pregnancies the participant has had
    matches("^Graviditeit_.*(?<!_F_)(?<=_)OBS(?:_r)?$", perl = TRUE),
    # Maternal status before delivery
    matches("^Mater_voor_partus_.*(?<!_F_)(?<=_)OBS(?:_r)?$", perl = TRUE),
    # Number of times the participant has given birth
    matches("^Pariteit_.*(?<!_F_)(?<=_)OBS(?:_r)?$", perl = TRUE),
    # Subscale: Previous Obstetric Complications
    # Indicates if there was a history of gestational diabetes
    matches("^OVG_diabetes_gravidarum_.*(?<!_F_)(?<=_)OBS(?:_r)?$", perl = TRUE)
  ) |>
  # favor recoded columns if non-recoded column is present
  prefer_recoded_columns()

# preview
dataset_id_test_2

dataset_id_test_2 |> write_sav("C:/Users/u702065/data warehouse repository/dataset_id_test.sav")
