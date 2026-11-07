library(data.table)

dt_dhea <- fread("data/phenotyping/AP_PMBB_DHEA_Labpull_090426.csv")

# Subset data to DHEA-S testing only
dt_dhea <- dt_dhea[
    LabName ==
        "Dehydroepiandrosterone sulfate (DHEA-S) [Mass/volume] in Serum or Plasma"
]

# How many patients had clinical DHEA-S testing done
nrow(dt_dhea) # 10,195 test results
length(unique(dt_dhea$PMBB_ID)) # 7,647 unique patients

# Evaluate test name-unit pairings
dt_dhea[, .N, by = c("LabName", "Unit")]

#     Unit     N
# 1: mcg/dL  1063 --> standardizes to mcg/dl
# 2:  ug/dL  8346 --> standardizes to mcg/dl
# 3:  ng/mL   748 --> scale of data is inaccurate
# 4:   null    27 --> unusable without units
# 5:  ng/dL     5 --> mislabeled mcg/dl; standardizes to mcg/dl
# 6: MCG/DL     4 --> standardizes to mcg/dl
# 7:  ug/mL     2 --> mislabeled mcg/dl

EXCLUDED_UNITS <- c(
    "null",
    "ng/mL"
)

# remove
dt_dhea <- dt_dhea[,
    LabValue := suppressWarnings(as.numeric(gsub(
        pattern = "<|>",
        replacement = "",
        x = LabValue
    )))
]
dt_dhea <- dt_dhea[!Unit %in% EXCLUDED_UNITS | !is.na(LabValue)]

# How many patients had clinical DHEA-S testing done after
nrow(dt_dhea) # 9,420 test results
length(unique(dt_dhea$PMBB_ID)) # 7,198 unique patients

# Identify DHEA-S positive patients
setorder(dt_dhea, PMBB_ID, ResultDate)
dt_dhea <- dt_dhea[
    !is.na(LabValue),
    {
        i <- which.max(LabValue)
        .(
            dhea_level = LabValue[i],
            dhea_positive = LabValue[i] > 600,
            dhea_result_date = ResultDate[i]
        )
    },
    by = PMBB_ID
]

dt_dhea[, .N, by = dhea_positive] # 39 positive

fwrite(
    x = dt_dhea,
    file = "data/phenotyping/pmbb-dhea-s-labs-cleaned.txt",
    sep = "\t"
)
