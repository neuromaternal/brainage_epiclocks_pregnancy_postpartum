library(ggplot2)

colorblind_colors <- c("#000000", "#E69F00",
                       "#56B4E9", "#009E73",
                       "#F0E442", "#0072B2",
                       "#D55E00", "#CC79A7")

theme_paper <- theme_minimal(base_size = 18) +  # Use a minimal theme with larger base font size
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),              # Bold axis titles
    axis.text = element_text(size = 14),                   # Adjust axis text size
    #text = element_text(family = "DejaVu Sans"),   # Use Bitstream Vera Sans font for all text
    legend.position = "top",                               # Place legend at the top
    legend.title = element_text(face = "bold"),            # Bold legend title
    panel.grid.major = element_line(color = "gray95"),     # Light gray grid lines
    panel.grid.minor = element_blank(),                     # Remove minor grid lines
  )