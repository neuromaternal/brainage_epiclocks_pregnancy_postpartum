library(ggplot2)
library(dplyr)

source("code/basic_plot_configuration.R")

create_plot_hgam_fit_per_brainage_model_dense <- function(gam_fit_bam, 
                                                          fig_subfig_title,
                                                          fig_subfig_ylim = c(-7, 7),
                                                          fig_subfig_xlim = c(-60, 132)) {
    fig_subfig_alpha <- 0.8
    fig_subfig_cex <- 2

    fig_p1 <- gam_fit_bam$gam_fit_sm |> 
    filter(.smooth == 's(time_to_parturition_weeks)') |>
    ggplot() +
    geom_rug(aes(x = time_to_parturition_weeks, colour = participant_id),
             data = gam_fit_bam$data_bam,
             sides = "b", length = grid::unit(0.02, "npc")
    ) +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = time_to_parturition_weeks),
                alpha = 0.2
    ) +
    geom_point(aes(x = time_to_parturition_weeks, y = `s(time_to_parturition_weeks)`, colour = participant_id),
               data = gam_fit_bam$data_bam, alpha = fig_subfig_alpha, cex = fig_subfig_cex,
    ) +
    geom_line(aes(x = time_to_parturition_weeks, y = .estimate), lwd = 0.8) +
    labs(y = "Partial effect on brainage-gap (years)", title = fig_subfig_title, x = "Time to parturition (weeks)"
    ) +
    theme_paper +
    coord_cartesian(ylim = fig_subfig_ylim, xlim = fig_subfig_xlim) +
    geom_vline(xintercept = c(-40, 0), linetype = "dashed") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = 12)) + 
    scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = 1)) +
    theme(legend.position = c(0.8, 0.76)) +
    labs(color = "Participant") +
    scale_color_manual(labels = c("S01", "S02", "S03"), values = colorblind_colors[2:4])
    return(fig_p1)
}