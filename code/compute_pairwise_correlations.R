library(dplyr)
library(tidyr)
library(stats)


compute_pairwise_correlations <- function(data, metrics) {
    data_correlations <- data |>
        select(all_of(metrics))
    
    corr_results <- data.frame(metric1 = character(),
                               metric2 = character(),
                               R = numeric(),
                               df = integer(),
                               p.value = numeric()
                               )

    for (i in 1:(length(metrics)-1)) {
        for (j in (i+1):length(metrics)) {
            metric1 <- metrics[i]
            metric2 <- metrics[j]

            ij_data <- data_correlations |>
                select(all_of(c(metric1, metric2))) |>
                drop_na()
            ij_data <- ij_data[!duplicated(ij_data), ]

            corr_test <- cor.test(ij_data[[metric1]], ij_data[[metric2]], method = "pearson")
            r_value <- corr_test$estimate
            p_value <- corr_test$p.value
            df <- corr_test$parameter
            corr_results <- corr_results |>
                add_row(metric1 = metric1,
                        metric2 = metric2,
                        R = r_value,
                        df = df,
                        p.value = p_value
                        )
        }
    }
    corr_results$p.FDR <- p.adjust(corr_results$p.value, method = "fdr")
    return(corr_results)
}