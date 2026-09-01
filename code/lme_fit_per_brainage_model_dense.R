library(mgcv)
library(dplyr)
library(tidyr)
library(emmeans)

lme_fit_per_brainage_model_dense <- function(data, abrainage_model) {
    data_bam <- data |>
        mutate(gestation_postpartum_period = case_when(
                                                trimester == "pre"  ~ "pre",
                                                trimester == "first" ~ "first",
                                                trimester == "second" ~ "second",
                                                trimester == "third" ~ "third",
                                                trimester == "post" & gestation_week < 48 ~ "early_post",
                                                trimester == "post" & gestation_week >= 48 & gestation_week < 74 ~ "6m_post",
                                                trimester == "post" & gestation_week >= 74 ~ "year_post")
              )

    data_bam$gestation_postpartum_period <- factor(data_bam$gestation_postpartum_period,
                                                   levels = c("pre", "first", "second", "third", "early_post", "6m_post", "year_post"))

    data_bam <- data_bam |> 
        filter(brainage_model == abrainage_model) |>
        select(all_of(c("brainage_gap", "gestation_postpartum_period", "scanning_site", "participant_id", "brainage_model"))) |>
        drop_na()

    lme_fit <- gam(brainage_gap ~ gestation_postpartum_period + 
                                              scanning_site +
                                              s(participant_id, bs='re'),
                                data = data_bam,
                                method = "REML", family = "gaussian")

    lme_fit_emmeans <- emmeans(lme_fit, ~ gestation_postpartum_period)

    # Performing within-group longitudinal comparisons, preserving only those of interest
    lme_fit_emmeans_pairs <- lme_fit_emmeans |>
        pairs(reverse = TRUE)

    lme_fit_emmeans_tests <- emmeans::test(lme_fit_emmeans_pairs, by = NULL, adjust = 'none') 

    lme_fit_emmeans_tests_pre <- lme_fit_emmeans_tests |>
        filter(str_detect(contrast, "pre"))

    lme_fit_emmeans_tests_third <- lme_fit_emmeans_tests |>
        filter(str_detect(contrast, str_escape("- third")))

    lme_fit_emmeans_tests <- rbind(lme_fit_emmeans_tests_pre, lme_fit_emmeans_tests_third) 
    
    lme_fit_emmeans_tests$pFDR <- p.adjust(lme_fit_emmeans_tests$p.value, method = "fdr")

    colnames(lme_fit_emmeans_tests) <- c("Contrast", "Estimate", "SE",
                                         "df", "t.ratio", "p.value", "pFDR")
    
    return(list(lme_fit = lme_fit,
                emmeans_tests = lme_fit_emmeans_tests))
}