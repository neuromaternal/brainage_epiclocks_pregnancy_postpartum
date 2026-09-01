library(mgcv)
library(dplyr)
library(tidyr)
library(gratia)

hgam_fit_per_brainage_model_dense <- function(data, abrainage_model) {
    data_bam <- data |>
        select(all_of(c("brainage_gap", "time_to_parturition_weeks", "brainage_model",
                        "scanning_site", "participant_id"))) |>
        filter(brainage_model == abrainage_model) |>
        drop_na()

    gam_fit <- gam(brainage_gap ~ scanning_site +
                                  s(time_to_parturition_weeks, bs = "tp") +
                                  s(participant_id, bs='re'),
                    data = data_bam,
                    method = "REML",
                    family = "gaussian")

    gam_fit_sm <- smooth_estimates(gam_fit) |>
    add_confint()

    data_bam <- data_bam |>
    add_partial_residuals(gam_fit)

    return(list(gam_fit = gam_fit,
                gam_fit_sm = gam_fit_sm,
                data_bam = data_bam))
}