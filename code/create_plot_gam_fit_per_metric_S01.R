library(ggplot2)
library(dplyr)

source("code/basic_plot_configuration.R")

create_plot_gam_fit_per_metric_s01 <- function(data_metric,
                                               gam_fit_sm,
                                               fig_subfig_title,
                                               fig_subfig_xlim,
                                               fig_subfig_ylim,
                                               fig_subfig_yby,
                                               fig_subfig_ylabel,
                                               fig_subfig_cex,
                                               mark_epi_range = FALSE) {
    fig_subfig_alpha <- 1.0

    fig_p1 <- gam_fit_sm |> 
    filter(.smooth == 's(time_to_parturition_weeks)') |>
    ggplot() +
    geom_ribbon(aes(ymin = .lower_ci, ymax = .upper_ci, x = time_to_parturition_weeks),
                alpha = 0.2
    ) +
    geom_line(aes(x = time_to_parturition_weeks, y = .estimate), lwd = 0.8) +
    labs(x = "Time to parturition (weeks)",y = fig_subfig_ylabel, title = fig_subfig_title 
    ) +
    geom_point(aes(x = time_to_parturition_weeks, y = `s(time_to_parturition_weeks)`),
               data = data_metric, alpha = fig_subfig_alpha, cex = fig_subfig_cex,
               colour = colorblind_colors[2]
    ) +
    scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = 8)) + 
    scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = fig_subfig_yby)) +
    geom_vline(xintercept = c(-40, 0), linetype = "dashed") + theme_paper
    if (mark_epi_range) {
        fig_p1 <- fig_p1 + annotate("rect", xmin = -41, xmax = 54, ymin = -9, ymax = 7, alpha = 0.1, fill=colorblind_colors[6])
    }     
    return(fig_p1)
}