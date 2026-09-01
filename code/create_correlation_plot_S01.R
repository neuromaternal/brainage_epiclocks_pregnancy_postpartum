library(ggplot2)
library(dplyr)
library(tidyr)

source("code/basic_plot_configuration.R")

get_annot_coord <- function(ggfig, perc_x, perc_y) {
  xrange <- layer_scales(ggfig)$x$range$range
  yrange <- layer_scales(ggfig)$y$range$range
  annot_x <- xrange[1] + (xrange[2] - xrange[1]) * perc_x
  annot_y <- yrange[1] + (yrange[2] - yrange[1]) * perc_y
  return(list(x = annot_x, y = annot_y))
}

create_correlation_plot_S01 <- function(data_metrics,
                                        x_metric,
                                        y_metric, 
                                        corr_metrics,
                                        fig_subfig_title,
                                        fig_subfig_xlabel,
                                        fig_subfig_ylabel,
                                        fig_subfig_xlim,
                                        fig_subfig_xby,
                                        fig_subfig_ylim,
                                        fig_subfig_yby,
                                        annot_perc_x = 0.4,
                                        annot_perc_y = 0.1
                                        ) {
    # Filter the correlation results for the specific metrics
    fig_corr_metric <- corr_metrics |>
        filter(metric1 == x_metric & metric2 == y_metric)
    
    # Create the scatter plot with linear regression line
    fig_p1 <- ggplot(data_metrics, aes(x = .data[[x_metric]], y = .data[[y_metric]])) +
    geom_smooth(method = "lm", formula = y ~ x, se = TRUE, colour = "#000000") +
    geom_point(data = data_metrics, cex = 2, colour = colorblind_colors[2]
    ) +
    labs(x = fig_subfig_xlabel, y = fig_subfig_ylabel, title = fig_subfig_title
    ) +
    theme_paper +
    scale_x_continuous(breaks = seq(fig_subfig_xlim[1], fig_subfig_xlim[2], by = fig_subfig_xby)) + 
    scale_y_continuous(breaks = seq(fig_subfig_ylim[1], fig_subfig_ylim[2], by = fig_subfig_yby)) +
    theme(legend.position = "none") +
    coord_cartesian(ylim = fig_subfig_ylim)
    annot_coords <- get_annot_coord(fig_p1, perc_x = annot_perc_x, perc_y = annot_perc_y)
    fig_p1 <- fig_p1 +
    annotate("text",
             x = annot_coords$x,
             y = annot_coords$y,
             label = sprintf("r = %0.3f\npFDR = %0.1e",
                             fig_corr_metric$R, fig_corr_metric$p.FDR),
             size = 6,
             hjust = 0)
    return(fig_p1)
}