library(mgcv)
library(dplyr)
library(tidyr)
library(emmeans)

lme_fit_per_brainage_model_cohort <- function(data, abrainage_model) {
    data_bam <- data |>
        select(all_of(c("uncorrected_brainage_gap", "group_gest", "brainage_model",
                        "session", "age",
                        "scanning_site", "participant_id"))) |>
        filter(brainage_model == abrainage_model) |>
        mutate(age_centered = age - mean(age)) |>
        select(-age) |>
        rename(age = age_centered) |>
        drop_na()

    lme_fit <- gam(uncorrected_brainage_gap ~ group_gest*session +
                                              s(participant_id, bs='re') + 
                                              age*scanning_site,
                    data = data_bam,
                    method = "REML", family = "gaussian")

    lme_fit_emmeans <- emmeans(lme_fit, ~ session | group_gest)

    # Performing within-group longitudinal comparisons, preserving only those of interest
    lme_fit_emmeans_pairs <- lme_fit_emmeans |> 
        pairs(reverse = TRUE)

    lme_fit_emmeans_tests <- emmeans::test(lme_fit_emmeans_pairs, by = NULL, adjust = 'none') |> 
        relocate(group_gest, .before = contrast) |>
        filter(str_detect(contrast, paste(c(str_escape("- (ses-1)"), str_escape("- (ses-3)")), collapse = "|"))) |>
        mutate(sesref = str_extract(contrast, "ses-\\d+\\)$")) |> # For ordering the contrasts
        arrange(group_gest, sesref, contrast) |>
        select(-sesref)

    lme_fit_emmeans_tests$pFDR <- p.adjust(lme_fit_emmeans_tests$p.value, method = "fdr") 
    
    colnames(lme_fit_emmeans_tests) <- c("Group", "Contrast", "Estimate", "SE",
                                                    "df", "t.ratio", "p.value", "pFDR")
    
    return(list(lme_fit = lme_fit,
                emmeans_tests = lme_fit_emmeans_tests))
}