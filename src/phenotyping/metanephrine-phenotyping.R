library(data.table)

dt <- fread(
    "/static/PMBB/PMBB-Release-2026-4.0/Phenotype/4.0/PMBB-Release-2026-4.0_phenotype_labs_metanephrine.txt"
)

# How many patients had clinical metanephrine testing
nrow(dt) # 3663
length(unique(dt$person_id)) # 1709 patients

dt <- dt[unit_converted == "nmol/L" & !is.na(value_converted)]

nrow(dt) # 2150 tests
length(unique(dt$person_id)) # 1188 patients

setorder(dt, person_id, measurement_date)

dt <- dt[
    !is.na(value_converted),
    {
        i <- which.max(value_converted)
        .(
            metanephrine_level = value_converted[i],
            metanephrine_positive = value_converted[i] > 1.1,
            metanephrine_result_date = value_converted[i]
        )
    },
    by = person_id
]

fwrite(
    x = dt,
    file = "data/phenotyping/pmbb-metanephrine-labs-cleaned.txt",
    sep = "\t"
)
