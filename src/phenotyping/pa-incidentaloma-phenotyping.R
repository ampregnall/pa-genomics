library(data.table)

# Load PMBB demographics data and subset to key variables
dt <- fread("/static/PMBB/PMBB-Release-2026-4.0/Phenotype/4.0/PMBB-Release-2026-4.0_phenotype_covariates.txt")
dt <- dt[, .(person_id, sequenced_gender, sample_date, sample_age, population = Class)]

# Identify cases of people with adrenal adenomas
ADENOMA_CODES <- c("D35.0", "D35.00", "D35.01", "D35.02")

dt_diagnoses <- fread(
  "/static/PMBB/PMBB-Release-2026-4.0/Phenotype/4.0/PMBB-Release-2026-4.0_phenotype_conditions_icd10.txt"
)

dt_diagnoses <- dt_diagnoses[, 
.(adenoma_icd=as.integer(any(grepl(paste(ADENOMA_CODES, collapse = "|"), condition_source_value)))), 
by = person_id]

dt <- merge(
  dt,
  dt_diagnoses,
  by = "person_id",
  all.x = TRUE
)

dt_imaging <- fread("data/phenotyping/pmbb-adrenal-incidentaloma-imaging.txt")
dt <- dt[, adenoma_imaging := as.numeric(person_id %in% dt_imaging$person_id)]
dt <- dt[, genetics_consent := TRUE]
dt <- dt[adenoma_icd == 1 | adenoma_imaging == 1]

dt_model <- merge(dt, dt_imaging, by = "person_id", all = TRUE)

dt_model[, `:=`(
  serum_consent = TRUE,
  genetics_consent      = fcoalesce(genetics_consent, FALSE),
  adenoma_imaging = fcoalesce(adenoma_imaging, 1)
)]

# load phenotyped functional testing data
dt_cortisol <- fread("data/phenotyping/pmbb-cortisol-labs-cleaned.txt")
dt_dheas <- fread("data/phenotyping/pmbb-dhea-s-labs-cleaned.txt")
dt_metanephrines <- fread("data/phenotyping/pmbb-metanephrine-labs-cleaned.txt")
dt_normetanephrines <- fread("data/phenotyping/pmbb-normetanephrine-labs-cleaned.txt")
dt_adrenalectomies <- fread("/project/damrauer_shared/Phenotypes/PMBB_Adrenalectomy_cases_090826.csv")

dt_model <- merge(dt_model, dt_cortisol, by.x = "person_id", by.y = "PMBB_ID", all.x = TRUE)
dt_model <- merge(dt_model, dt_dheas, by.x = "person_id", by.y = "PMBB_ID", all.x = TRUE)
dt_model <- merge(dt_model, dt_metanephrines, by = "person_id", all.x = TRUE)
dt_model <- merge(dt_model, dt_normetanephrines, by = "person_id", all.x = TRUE)
dt_model <- merge(dt_model, dt_adrenalectomies, by.x = "person_id", by.y = "PMBB_ID", all.x = TRUE)

fwrite(dt_model, file = "data/phenotyping/pmbb-incidentaloma-selection-cohort.txt", sep = "\t")
