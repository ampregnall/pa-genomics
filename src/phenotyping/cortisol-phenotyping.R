library(data.table)

dt_cortisol <- fread("data/phenotyping/AP_PMBB_Cortisol_Labpull_090426.csv")

CORTISOL_TESTS <- c(
    "Cortisol [Mass/volume] in Saliva (oral fluid)",
    "Cortisol [Mass/volume] in Serum or Plasma",
    "Cortisol [Mass/volume] in Serum or Plasma --8 hours post 1 mg dexamethasone PO overnight",
    "Cortisol Free [Mass/time] in 24 hour Urine"
)

dt_cortisol <- dt_cortisol[LabName %in% CORTISOL_TESTS]

# How many patients had clinical cortisol testing done
nrow(dt_cortisol) # 53643
length(unique(dt_cortisol$PMBB_ID)) # 19,409

dt_cortisol <- dt_cortisol[,
    LabValue := suppressWarnings(as.numeric(gsub(
        pattern = "<|>",
        replacement = "",
        x = LabValue
    )))
]

dt_cortisol <- dt_cortisol[!is.na(LabValue)]

nrow(dt_cortisol) # 53275
length(unique(dt_cortisol$PMBB_ID)) # 19,356

lab_names <- c(
    urine_cortisol              = "Cortisol Free [Mass/time] in 24 hour Urine",
    plasma_cortisol             = "Cortisol [Mass/volume] in Serum or Plasma",
    post_dexamethasone_cortisol = "Cortisol [Mass/volume] in Serum or Plasma --8 hours post 1 mg dexamethasone PO overnight",
    midnight_cortisol           = "Cortisol [Mass/volume] in Saliva (oral fluid)"
)

thresholds <- c(
    urine_cortisol              = 200,
    plasma_cortisol             = 41,
    post_dexamethasone_cortisol = 1.8,
    midnight_cortisol           = 2.0
)
date_col <- "ResultDate"

# Returns: <prefix> (any > threshold), max_<prefix>, max_<prefix>_date
lab_summary <- function(value, date, keep, threshold, prefix) {
    v <- value[keep]
    d <- date[keep]
    i <- which.max(v)
    if (!length(i)) i <- NA_integer_  # no tests (or all NA) -> NA, not -Inf
    setNames(
        list(any(v > threshold, na.rm = TRUE), v[i], d[i]),
        c(prefix, paste0("max_", prefix), paste0("max_", prefix, "_date"))
    )
}

dt_cortisol <- dt_cortisol[,
    do.call(c, lapply(names(lab_names), function(p) {
        lab_summary(
            LabValue, get(date_col), LabName == lab_names[[p]],
            thresholds[[p]], p
        )
    })),
    by = .(PMBB_ID)
]

fwrite(dt_cortisol, file = "data/phenotyping/pmbb-cortisol-labs-cleaned.txt", sep = "\t")
