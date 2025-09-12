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

# ==== Load dataset ====
data_brabant <- read_sav(
  "O:/fsw/Data FSW/MedPsy/Brabant Studie/Data merges/DATA CLEANING/Merge September 2023/MERGES/PREGNANCY_FOLLOWUP_OBSTETRIC_FATHERDATA_MERGE.sav"
)

dataset_id_test_subset <- data_brabant |>
  select(
    # Category: Biological
    # Subscale: Medication
    # Medication usage
    starts_with("Medication_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    # Medication without doctor's prescription
    starts_with("Medication_unreg_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    # Subscale: Doctor Visits
    # GP visit (Yes/No)
    starts_with("Doctorvisit_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    # Category: (Post-) Delivery information
    # Subscale: Hospital and Consultations
    # Hospital visits or stays
    starts_with("Hospital_") & (ends_with("_6mPP") | ends_with("_6mPP_r")) & !matches("_F_6mPP(_r)?$"),
    starts_with("Hospital_") & (ends_with("_2yPP") | ends_with("_2yPP_r")) & !matches("_F_2yPP(_r)?$"),
    # Days in hospital
    starts_with("Dayshospital_") & (ends_with("_1yPP") | ends_with("_1yPP_r")) & !matches("_F_1yPP(_r)?$"),
    # Category: Demographic
    # Subscale: Language & Migration Status
    # Language understanding of the child
    starts_with("Language_") & (ends_with("_2yPP") | ends_with("_2yPP_r")) & !matches("_F_2yPP(_r)?$"),
    # Migration background
    starts_with("Migration_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    # Category: Psychological
    # Subscale: Depressive Symptoms
    # Edinburgh Depression Scale
    starts_with("EDS_") & (ends_with("_2y6mPP") | ends_with("_2y6mPP_r")) & !matches("_F_2y6mPP(_r)?$"),
    # Subscale: Anxiety
    # Symptom Check-List-90 - Subscale anxiety
    starts_with("SCL90_") & (ends_with("_3y6mPP") | ends_with("_3y6mPP_r")) & !matches("_F_3y6mPP(_r)?$"),
    # Category: Social-Relational
    # Subscale: Work and Absenteeism
    # Childcare in the organization
    starts_with("Childcare_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    starts_with("Childcare_") & (ends_with("_3yPP") | ends_with("_3yPP_r")) & !matches("_F_3yPP(_r)?$"),
    # Compressed workweek (4x9 hours)
    starts_with("Compressedwork_") & (ends_with("_2yPP") | ends_with("_2yPP_r")) & !matches("_F_2yPP(_r)?$"),
    # Family Supportive Supervisor Behaviors - Short Form
    starts_with("FSSB_SF_") & (ends_with("_6mPP") | ends_with("_6mPP_r")) & !matches("_F_6mPP(_r)?$"),
    # Individual Work Performance Questionnaire
    starts_with("IWPQ_") & (ends_with("_1y6mPP") | ends_with("_1y6mPP_r")) & !matches("_F_1y6mPP(_r)?$"),
    # Category: Biological - Father
    # Subscale: Chronic conditions father
    # Diagnosis of diabetes - father
    starts_with("Diabetes_") & (ends_with("_F_F_28") | ends_with("_F_F_28_r")),
    # Diagnosis of eczema - father
    starts_with("Eczema_") & (ends_with("_F_F_28") | ends_with("_F_F_28_r")),
    # Diagnosis of hypertension - father
    starts_with("Hypertension_") & (ends_with("_F_F_28") | ends_with("_F_F_28_r")),
    # Diagnosis of rheumatism - father
    starts_with("Rheumatism_") & (ends_with("_F_F_28") | ends_with("_F_F_28_r")),
    # Diagnosis of other chronic disease(s) - father
    starts_with("Chronicdisease_other_") & (ends_with("_F_F_28") | ends_with("_F_F_28_r")),
    # Category: Obstetric data
    # Subscale: Breastfeeding & Nutrition
    # Number of pregnancies the participant has had
    starts_with("Graviditeit_") & (ends_with("_OBS") | ends_with("_OBS_r")) & !matches("_F_OBS(_r)?$"),
    # Maternal status before delivery
    starts_with("Mater_voor_partus_") & (ends_with("_OBS") | ends_with("_OBS_r")) & !matches("_F_OBS(_r)?$"),
    # Number of times the participant has given birth
    starts_with("Pariteit_") & (ends_with("_OBS") | ends_with("_OBS_r")) & !matches("_F_OBS(_r)?$"),
    # Subscale: Previous Obstetric Complications
    # Indicates if there was a history of gestational diabetes
    starts_with("OVG_diabetes_gravidarum_") & (ends_with("_OBS") | ends_with("_OBS_r")) & !matches("_F_OBS(_r)?$")
  ) |>
  # favor recoded columns if non-recoded column is present
  prefer_recoded_columns()

dataset_id_test_subset |> write_sav("RDdata/dataset_id_test_subset.sav")

# preview
dataset_id_test_subset
