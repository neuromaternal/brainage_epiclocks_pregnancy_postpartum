library(ggplot2)
library(dplyr)
library(tidyr)

source("code/basic_plot_configuration.R")

create_plots_hgam_fit_per_brainage_model_cohort <- function(gam_fit_bam,
                                                            fig_subfig_xlim = c(-48, 108),
                                                            fig_subfig_ylim = c(-10, 10)) {
    fig_p1 <- gam_fit_bam$gam_fit_sm  |>                                                        
        filter(.smooth == 's(time_to_parturition_approached_ses1_weeks):group_gestgestational_mother') |>
        ggplot() +
        geom_rug(aes(x = time_to_parturition_approached_ses1_weeks),
                 data = gam_fit_bam$data_bam_gest,
                 sides = "b", length = grid::unit(0.02, "npc")
        ) +
        geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = time_to_parturition_approached_ses1_weeks),
                    alpha = 0.2
        ) +
        geom_point(aes(x = time_to_parturition_approached_ses1_weeks, y = `s(time_to_parturition_approached_ses1_weeks):group_gestgestational_mother`),
                   data = gam_fit_bam$data_bam_gest, cex = 2, alpha = 0.6, color = colorblind_colors[6]
        ) +
        geom_line(aes(x = time_to_parturition_approached_ses1_weeks, y = .estimate), lwd = 0.8) +
        labs(y = "Partial effect on brainage-gap (years)", title = "Gestational mothers", x = "Time to parturition (weeks)"
        ) +
        theme_paper +
        coord_cartesian(ylim = fig_subfig_ylim, xlim = fig_subfig_xlim) + 
        geom_vline(xintercept = c(-40, 0), linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = 12)) + 
        scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = 1)) +
        theme(legend.position = c(0.9, 0.89))
    
    fig_p2  <- gam_fit_bam$gam_fit_sm  |> 
        filter(.smooth == 's(time_to_parturition_approached_ses1_weeks):group_gestnulliparous_women') |>
        ggplot() +
        geom_rug(aes(x = time_to_parturition_approached_ses1_weeks),
                 data = gam_fit_bam$data_bam_ngest,
                 sides = "b", length = grid::unit(0.02, "npc")
        ) +
        geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = time_to_parturition_approached_ses1_weeks),
                    alpha = 0.2
        ) +
        geom_point(aes(x = time_to_parturition_approached_ses1_weeks, y = `s(time_to_parturition_approached_ses1_weeks):group_gestnon_gestational_mother`),
                   data = gam_fit_bam$data_bam_ngest, cex = 2, alpha = 0.6, color = colorblind_colors[7]
        ) +
        geom_line(aes(x = time_to_parturition_approached_ses1_weeks, y = .estimate), lwd = 0.8) +
        labs(y = "Partial effect on brainage-gap (years)", title = "Non-gestational mothers", x = "Time to parturition (weeks)"
        ) +
        theme_paper +
        coord_cartesian(ylim = fig_subfig_ylim, xlim = fig_subfig_xlim) + 
        geom_vline(xintercept = c(-40, 0), linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = 12)) + 
        scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = 1)) +
        theme(legend.position = c(0.9, 0.89))

    fig_p3 <- gam_fit_bam$gam_fit_sm  |> 
        filter(.smooth == 's(time_to_parturition_approached_ses1_weeks):group_gestnulliparous_women') |>
        ggplot() +
        geom_rug(aes(x = time_to_parturition_approached_ses1_weeks),
                 data = gam_fit_bam$data_bam_null,
                 sides = "b", length = grid::unit(0.02, "npc")
        ) +
        geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = time_to_parturition_approached_ses1_weeks),
                    alpha = 0.2
        ) +
        geom_point(aes(x = time_to_parturition_approached_ses1_weeks, y = `s(time_to_parturition_approached_ses1_weeks):group_gestnulliparous_women`),
                   data = gam_fit_bam$data_bam_null, cex = 2, alpha = 0.6, color = colorblind_colors[8]
        ) +
        geom_line(aes(x = time_to_parturition_approached_ses1_weeks, y = .estimate), lwd = 0.8) +
        labs(y = "Partial effect on brainage-gap (years)", title = "Nulliparous women", x = "Time to parturition (weeks)"
        ) +
        theme_paper +
        coord_cartesian(ylim = fig_subfig_ylim, xlim = fig_subfig_xlim) + 
        geom_vline(xintercept = c(-40, 0), linetype = "dashed") +
        geom_hline(yintercept = 0, linetype = "dashed") +
        scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = 12)) + 
        scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = 1)) +
        theme(legend.position = c(0.9, 0.89))

        return(list(fig_p1 = fig_p1, fig_p2 = fig_p2, fig_p3 = fig_p3))
}