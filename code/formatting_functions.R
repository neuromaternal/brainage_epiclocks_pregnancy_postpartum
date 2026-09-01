library(flextable)
library(officer)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(emmeans)

create_hgam_summary_table_docx <- function(model_summary, 
                                      stable_substitutions,
                                      ptable_substitutions,
                                      docx_filename) {
  ptable <- data.frame(model_summary$p.table) |>
    rownames_to_column("Coefficient")
  colnames(ptable)[2:5] <- c("Estimate", "Std.Error", "t.value", "p.value")

  stable <- data.frame(model_summary$s.table) |>
    rownames_to_column("Smooth") |>
    mutate(p.FDR = NA)

  stable$p.FDR[seq_len(nrow(stable)-1)] <- p.adjust(stable$p.value[seq_len(nrow(stable)-1)],
                                             method = "fdr")

  stable$Smooth <- str_replace_all(stable$Smooth, stable_substitutions)
  colnames(stable)[1] <- "Smooth Term"
  
  ptable$Coefficient <- str_replace_all(ptable$Coefficient, ptable_substitutions)

  ptable_aux <- ptable |>
  flextable() |>
  width(width = 3, j = 1) |>
  width(width = 1, j = 2:5) |>
  set_formatter(  
                  "Estimate" = function(x) sprintf("%.3f", x),
                  "Std.Error" = function(x) sprintf("%.3f", x),
                  "t.value" = function(x) sprintf("%.3f", x),
                  "p.value" = function(x) ifelse(x < 0.001, "<0.001",
                                                            sprintf("%.3f", x)),
                  "p.FDR" = function(x) ifelse(is.na(x), "-", 
                                                        ifelse(x < 0.001, "<0.001",
                                                                         sprintf("%.3f", x)))
                )

  stable_aux <- stable |>
  flextable() |>
  width(width = 3, j = 1) |>
  width(width = 0.85, j = 2:6) |>
  set_formatter(
                "edf" = function(x) sprintf("%.2f", x),
                "Ref.df" = function(x) sprintf("%.2f", x),
                "F" = function(x) sprintf("%.3f", x),
                "p.value" = function(x) ifelse(x < 0.001, "<0.001",
                                                          sprintf("%.3f", x)),
                "p.FDR" = function(x) ifelse(is.na(x), "-", 
                                                       ifelse(x < 0.001, "<0.001",
                                                                         sprintf("%.3f", x)))
                )

  table_doc <- read_docx() |>
    body_add_par("Table with HGAM parametric coefficients") |>
    body_add_flextable(ptable_aux) |>
    body_add_par("Table with HGAM smooth terms") |>
    body_add_flextable(stable_aux) |>
    body_add_par(sprintf("Deviance explained = %.3f, R²(Adjusted) = %.3f, n = %d",
                         model_summary$dev.expl, model_summary$r.sq, model_summary$n)) |>
    print(docx_filename)

  return_list <- list(ptable = ptable,
                      stable = stable,
                      tables_doc = table_doc)
  return(return_list)
}

create_lme_fit_tables_docx <- function(lme_fit,
                                       em_comparisons,
                                       ptable_substitutions,
                                       stable_substitutions,
                                       emtable_substitutions,
                                       type_dataset,
                                       docx_filename) {
  summ_lme_fit <- summary(lme_fit)                                      
  ptable <- data.frame(summ_lme_fit$p.table) |>
    rownames_to_column("Coefficient")
  colnames(ptable) <- c("Coefficient",
                        "Estimate",
                        "Std.Error",
                        "t.value",
                        "p.value")

  ptable$Coefficient <- str_replace_all(ptable$Coefficient, ptable_substitutions)

  ptable_formated <- ptable |>
    flextable() |>
    width(width = 3, j = 1) |>
    width(width = 1, j = 2:5) |>
    set_formatter(
      "Estimate" = function(x) sprintf("%.3f", x),
      "Std.Error" = function(x) sprintf("%.3f", x),
      "t.value" = function(x) sprintf("%.3f", x),
      "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    )

  stable <- data.frame(summ_lme_fit$s.table) |> 
    rownames_to_column(var = "Smooth") 
  stable$Smooth <- str_replace_all(stable$Smooth, stable_substitutions)
  stable_formated <- stable |> 
    flextable() |> 
    width(width = 3, j = 1) |>
    width(width = 1, j = 2:5) |> 
    set_formatter(
      "edf" = function(x) sprintf("%.2f", x),
      "Ref.df" = function(x) sprintf("%.2f", x),
      "F" = function(x) sprintf("%.3f", x),
      "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
    )

  if (type_dataset == "cohort") {
    
    emtable <- em_comparisons |>
      mutate(Group = str_replace_all(Group, emtable_substitutions)) 
      
    emtable_formated <- emtable |>
      flextable() |> 
      width(width = 1.4, j = 1:2) |> 
      width(width = 0.85, j = 3:8) |>
      set_formatter(
        "Estimate" = function(x) sprintf("%.3f", x),
        "SE" = function(x) sprintf("%.3f", x),
        "df" = function(x) sprintf("%.2f", x),
        "t.ratio" = function(x) sprintf("%.3f", x),
        "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)),
        "pFDR" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
        )
  } else if (type_dataset == "dense") {
    
    emtable <- em_comparisons |>
      mutate(Contrast = str_replace_all(Contrast, emtable_substitutions))
    
    emtable_formated <- emtable |>
      flextable() |> 
      width(width = 1.4, j = 1) |> 
      width(width = 0.85, j = 2:7) |>
      set_formatter(
        "Estimate" = function(x) sprintf("%.3f", x),
        "SE" = function(x) sprintf("%.3f", x),
        "df" = function(x) sprintf("%.2f", x),
        "t.ratio" = function(x) sprintf("%.3f", x),
        "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)),
        "pFDR" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
        )
  } 
  else if (type_dataset == "cohort_birth_type") {
    
    emtable <- em_comparisons |>
      mutate(`Birth Type` = str_replace_all(`Birth Type`, emtable_substitutions)) 
      
    emtable_formated <- emtable |>
      flextable() |> 
      width(width = 1.4, j = 1:2) |> 
      width(width = 0.85, j = 3:8) |>
      set_formatter(
        "Estimate" = function(x) sprintf("%.3f", x),
        "SE" = function(x) sprintf("%.3f", x),
        "df" = function(x) sprintf("%.2f", x),
        "t.ratio" = function(x) sprintf("%.3f", x),
        "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)),
        "pFDR" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
        )
  }
  else {
    stop("Invalid type_dataset. Must be either 'cohort', 'cohort_birth_type' or 'dense'.")
  }
  
  tables_doc <- read_docx() |>
    body_add_par("Table with LME parametric coefficients") |>
    body_add_flextable(ptable_formated) |>
    body_add_par("Table with LME smooth terms") |>
    body_add_flextable(stable_formated) |>
    body_add_par(sprintf("Deviance explained = %.3f, R²(Adjusted) = %.3f, n = %d",
                          summ_lme_fit$dev.expl, summ_lme_fit$r.sq, summ_lme_fit$n)) |>
    body_add_par("Table with LME estimated marginal means") |>
    body_add_flextable(emtable_formated) |>
    print(target = docx_filename)

  return_list <- list(ptable = ptable,
                      stable = stable,
                      emtable = emtable,
                      tables_doc = tables_doc)  
  return(return_list)
}

create_lme_fit_additional_em_tables_docx <- function(em_comparisons_cross,
                                                     em_comparisons_long,
                                                     em_cross_table_substitutions,
                                                     em_long_table_substitutions,
                                                     docx_filename) {
  
  em_cross_table <- em_comparisons_cross |>
    mutate(Contrast = str_replace_all(Contrast, em_cross_table_substitutions))
      
  em_long_table <- em_comparisons_long |>
    mutate(Contrast = str_replace_all(Contrast, em_long_table_substitutions))

  em_cross_table_formated <- em_cross_table |>
    flextable() |> 
    width(width = 1.4, j = 1:2) |> 
    width(width = 0.85, j = 3:8) |>
    set_formatter(
      "Estimate" = function(x) sprintf("%.3f", x),
      "SE" = function(x) sprintf("%.3f", x),
      "df" = function(x) sprintf("%.2f", x),
      "t.ratio" = function(x) sprintf("%.3f", x),
      "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)),
      "pFDR" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
      )
  
  em_long_table_formated <- em_long_table |>
    flextable() |> 
    width(width = 1.4, j = 1) |> 
    width(width = 0.85, j = 2:7) |>
    set_formatter(
      "Estimate" = function(x) sprintf("%.3f", x),
      "SE" = function(x) sprintf("%.3f", x),
      "df" = function(x) sprintf("%.2f", x),
      "t.ratio" = function(x) sprintf("%.3f", x),
      "p.value" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x)),
      "pFDR" = function(x) ifelse(x < 0.001, "<0.001", sprintf("%.3f", x))
      )
  
  tables_doc <- read_docx() |>
    body_add_par("Table with LME estimated marginal means | Cross-sectional comparisons") |>
    body_add_flextable(em_cross_table_formated) |>
    body_add_par("Table with LME estimated marginal means | Longitudinal comparisons") |>
    body_add_flextable(em_long_table_formated) |>
    print(target = docx_filename)

  return_list <- list(em_cross_table = em_cross_table,
                      em_long_table = em_long_table,
                      tables_doc = tables_doc)
  return(return_list)
}

# Defining recurrent substitutions for table formatting

substitutions_stable_hgam_cohort <- c("group_gest" = "Group ",
                                      "time_to_parturition_approached_ses1_weeks" = "Time to Parturition",
                                      "non_" = "Non-",
                                      "gestational_mother" = "Gestational Mother",
                                      "nulliparous_women" = "Nulliparous Women",
                                      " Gestational Mother" = " [Gestational Mother]",
                                      "Nulliparous Women" = " [Nulliparous Women]",
                                      "Non-Gestational Mother" = " [Non-Gestational Mother]",
                                      "participant_id" = "Participant",
                                      ":" = " : ")

substitutions_ptable_hgam_cohort <- c("group_gest" = "Group ", 
                                      "session" = "Session ",
                                      "scanning_site" = "Scanning Site ",
                                      "age" = "Age ",
                                      "non_" = "Non-",
                                      "gestational_mother" = "Gestational Mother",
                                      "nulliparous_women" = "Nulliparous Women",
                                      " Gestational Mother" = "[Gestational Mother]",
                                      "Nulliparous Women" = "[Nulliparous Women]",
                                      "Non-Gestational Mother" = "[Non-Gestational Mother]",
                                      "Barcelona" = "[Barcelona]",
                                      "Madrid" = "[Madrid]",
                                      ":" = " : ")

                                      substitutions_stable_hgam_dense <- c("time_to_parturition_weeks" = "Time to Parturition",
                                      "participant_id" = "Participant"
                                      )

substitutions_ptable_hgam_dense <- c("scanning_site" = "Scanning Site ",                                    
                                      "UCSB" = "[UCSB]")

substitutions_ptable_lme_fit_cohort <- c("group_gest" = "Group ",
                                        "session" = "Session ",
                                        "scanning_site" = "Scanning Site ",
                                        "age" = "Age ",
                                        "non_" = "Non-",
                                        "gestational_mother" = "Gestational Mother",
                                        "nulliparous_women" = "Nulliparous Women",
                                        "ses-1" = "[ses-1]",
                                        "ses-2" = "[ses-2]",
                                        "ses-3" = "[ses-3]",
                                        "ses-4" = "[ses-4]",
                                        "ses-5" = "[ses-5]",
                                        "ses-6" = "[ses-6]",
                                        " Gestational Mother" = " [Gestational Mother]",
                                        "Nulliparous Women" = " [Nulliparous Women]",
                                        "Non-Gestational Mother" = " [Non-Gestational Mother]",
                                        "Barcelona" = " [Barcelona]",
                                        "Madrid" = " [Madrid]",
                                        ":" = " : ")

substitutions_stable_lme_fit_cohort <- c("participant_id" = "Participant ID")
                                        

substitutions_emtable_test_cohort <- c("gestational_mother" = "Gestational Mother",
                                       "nulliparous_women" = "Nulliparous Women",
                                       "non_" = "Non-")

                                       substitutions_ptable_lme_fit_dense <- c("gestation_postpartum_period" = "Gestation/Postpartum ",
                                        "scanning_site" = "Scanning Site ",
                                        "first" = "[1st Trimester]",
                                        "second" = "[2nd Trimester]",
                                        "third" = "[3rd Trimester]",
                                        "early_post" = "[Early Postpartum]",
                                        "6m_post" = "[~6 Months Postpartum]",
                                        "year_post" = "[>1 Year Postpartum]",
                                        "UCSB" = "[UCSB]")

substitutions_stable_lme_fit_dense <- c("participant_id" = "Participant ID")

substitutions_emtable_test_dense <- c("pre" = str_escape("Pre-conception"),
                                      "first" = "1st Trimester",
                                      "second" = "2nd Trimester",
                                      "third" = "3rd Trimester",
                                      "early_post" = "Early Postpartum",
                                      "6m_post" = str_escape("6 Months Postpartum"),
                                      "year_post" = str_escape("1 Year Postpartum"))

substitutions_ptable_lme_fit_birth_type <- c("birth_type" = "Birth Type ",
                                        "session" = "Session ",
                                        "scanning_site" = "Scanning Site ",
                                        "age" = "Age ",
                                        "vaginal" = "Vaginal Birth",
                                        "c-section emergency" = "Emergency C-Section",
                                        "c-section scheduled" = "Scheduled C-Section",
                                        "control" = "Control",
                                        "ses-1" = "[ses-1]",
                                        "ses-2" = "[ses-2]",
                                        "ses-3" = "[ses-3]",
                                        "ses-4" = "[ses-4]",
                                        "ses-5" = "[ses-5]",
                                        "ses-6" = "[ses-6]",
                                        "Vaginal Birth" = " [Vaginal Birth]",
                                        "Emergency C-Section" = " [Emergency C-Section]",
                                        "Scheduled C-Section" = " [Scheduled C-Section]",
                                        "Control" = " [Control]",
                                        "Barcelona" = " [Barcelona]",
                                        "Madrid" = " [Madrid]",
                                        ":" = " : ")

substitutions_stable_lme_fit_birth_type <- c("participant_id" = "Participant ID")
                                        

substitutions_emtable_test_birth_type <- c("vaginal" = "Vaginal Birth",
                                        "c-section emergency" = "Emergency C-Section",
                                        "c-section scheduled" = "Scheduled C-Section",
                                        "control" = "Control")

substitutions_em_ses_table_birth_type <- c("vaginal" = "Vaginal",
                                "c-section emergency" = "Emergency CS",
                                "c-section scheduled" = "Scheduled CS",
                                "control" = "Control")
                                
substitutions_em_long_table_birth_type <- c("vaginal" = "(Vaginal",
                                 "c-section emergency" = "(Emergency CS",
                                 "c-section scheduled" = "(Scheduled CS",
                                 "control" = "(Control")