library(mgcv)
library(dplyr)
library(tidyr)
library(gratia)

hgam_fit_per_brainage_model_cohort <- function(data, abrainage_model) {
    data_bam <- data |>
        select(all_of(c("uncorrected_brainage_gap", "group_gest", "brainage_model",
                        "time_to_parturition_approached_ses1_weeks", "age",
                        "scanning_site", "participant_id"))) |>
        filter(brainage_model == abrainage_model) |>
        mutate(age_centered = age - mean(age)) |>
        select(-age) |>
        rename(age = age_centered) |>
        drop_na()

    gam_fit <- gam(uncorrected_brainage_gap ~ group_gest + age*scanning_site +
                        s(time_to_parturition_approached_ses1_weeks, by=group_gest, k=10, bs="tp") +
                        s(participant_id, bs='re'),
                    data = data_bam,
                    method = "REML",
                    family = "gaussian")

    gam_fit_sm <- smooth_estimates(gam_fit) |>
    add_confint()

    data_bam <- data_bam |>
    add_partial_residuals(gam_fit)

    data_bam_gest <- data_bam |>
        filter(group_gest == "gestational_mother")

    data_bam_ngest <- data_bam |>
        filter(group_gest == "non_gestational_mother")

    data_bam_null <- data_bam |>
        filter(group_gest == "nulliparous_women")
    return(list(gam_fit = gam_fit,
                gam_fit_sm = gam_fit_sm,
                data_bam = data_bam,
                data_bam_gest = data_bam_gest,
                data_bam_ngest = data_bam_ngest,
                data_bam_null = data_bam_null))
}