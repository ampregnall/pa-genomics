library(data.table)

dt <- fread(
    "/static/PMBB/PMBB-Release-2026-4.0/Phenotype/4.0/PMBB-Release-2026-4.0_phenotype_labs_normetanephrine.txt"
)

# How many patients had clinical metanephrine testing
nrow(dt) # 3433
length(unique(dt$person_id)) # 1685 patients

dt <- dt[unit_converted == "nmol/L" & !is.na(value_converted)]

nrow(dt) # 3194 tests
length(unique(dt$person_id)) # 1608 patients

setorder(dt, person_id, measurement_date)

dt <- dt[
    !is.na(value_converted),
    {
        i <- which.max(value_converted)
        .(
            normetanephrine_level = value_converted[i],
            normetanephrine_positive = value_converted[i] > 2.2,
            normetanephrine_result_date = value_converted[i]
        )
    },
    by = person_id
]

fwrite(
    x = dt,
    file = "data/phenotyping/pmbb-normetanephrine-labs-cleaned.txt",
    sep = "\t"
)
