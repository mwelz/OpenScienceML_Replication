# -------------------------------------
# Authors: Max Welz and Andreas Alfons
#          Erasmus University Rotterdam
# -------------------------------------


## load relevant packages
library("ggplot2")

## directories to load and save results in
dir_load <- "simulations/analyzed/"
dir_save <- "simulations/plots/"
prefix <- "recall"

## simulation settings to loop over
designs <- c("idealized", "high", "moderate", "low", "grouped")
careless_types <- c("random", "extreme", "pattern", "straight")
careless_onsets <- c("throughout", "late")

## number of replications
R <- 1000L


## loop over different settings
for (design in designs) {
  for (careless_type in careless_types) {
    for (careless_onset in careless_onsets) {

      ## load data
      file_load <- paste0(dir_load, prefix, "_", design, "_", careless_type,
                          "_", careless_onset, "_R=", R, ".Rdata")
      if (file.exists(file_load)) {
        load(file_load)

        # make sure we have factors in the data frame
        df_mean_recall$Method <- factor(df_mean_recall$Method, levels = methods)

        # construct ideal recall curve
        ideal_recall_list <- lapply(prop_careless, function(p, seq_n) {
          # compute ideal recall curve
          x <- seq_n / (p*n)
          recall <- ifelse(x < 1, x, 1)
          # construct data frame
          data.frame(Prevalence = p,
                     Identified = seq_n,
                     Recall = recall)
        }, seq_n = seq(from = 0, to = n))
        # put data frames together
        df_ideal_recall <- do.call(rbind, ideal_recall_list)

        # construct nice labels
        # expressions are needed for legend
        method_labels <- c(autoencoder = expression(Autoencoder),
                           mahalanobis = expression(Mahalanobis),
                           reliability = expression(Reliability),
                           synonym = expression(Synonym),
                           irv = expression(IRV),
                           lz = expression(l[z]))
        # labeller function is needed for facets
        label_prevalence <- function(labels, multi_line = FALSE) {
          lapply(labels, function(label) {
            paste0(format(100 * label, trim = TRUE), "% prevalence")
          })
        }

        # colors and line types for plots
        colors <- c("#00BFC4", "#F8766D", "#C49A00",
                    "#619CFF", "#A58AFF", "#FB61D7")
        linetypes <- c("solid", "42", "22", "1242", "12", "5232")

        # create plot with colors
        p <- ggplot() +
          theme_bw() +
          geom_line(data = df_ideal_recall,
                    mapping = aes(x = Identified, y = Recall),
                    color = "black", linetype = "solid") +
          geom_line(data = df_mean_recall,
                    mapping = aes(x = Identified, y = Recall,
                                  color = Method, linetype = Method)) +
          scale_color_manual(values = colors, labels = method_labels) +
          scale_linetype_manual(values = linetypes,
                                labels = method_labels[methods]) +
          facet_wrap(facets = vars(Prevalence), nrow = 2, ncol = 3,
                     labeller = label_prevalence) +
          theme(legend.position = "top",
                legend.key.width = unit(23, "bigpts"),
                legend.title = element_blank(),
                legend.text = element_text(size = 10)) +
          guides(color = guide_legend(nrow = 1, byrow = TRUE),
                 linetype = guide_legend(nrow = 1, byrow = TRUE)) +
          xlab("Number of respondents identified as careless") +
          ylab("Percentage of true careless respondents recovered")

        ## save the plot
        file_save <- paste0(prefix, "_", design, "_", careless_type, "_",
                             careless_onset, "_R=", R, ".pdf")
        ggsave(filename = file_save, plot = p, device = "pdf",
               path = dir_save, width = 8.5 * 0.85, height = 6 * 0.85)

        ## save the plot as png for online supplement
        file_save <- paste0(prefix, "_", design, "_", careless_type, "_",
                            careless_onset, "_R=", R, ".png")
        ggsave(filename = file_save, plot = p, device = "png",
               path = dir_save, width = 8.5 * 0.85, height = 6 * 0.85)

        # create plot in grayscale
        p_bw <- ggplot() +
          theme_bw() +
          geom_line(data = df_ideal_recall,
                    mapping = aes(x = Identified, y = Recall),
                    color = gray(0.6), linetype = "solid") +
          geom_line(data = df_mean_recall,
                    mapping = aes(x = Identified, y = Recall,
                                  linetype = Method),
                    color = "black") +
          scale_linetype_manual(values = linetypes,
                                labels = method_labels[methods]) +
          facet_wrap(facets = vars(Prevalence), nrow = 2, ncol = 3,
                     labeller = label_prevalence) +
          theme(legend.position = "top",
                legend.key.width = unit(23, "bigpts"),
                legend.title = element_blank(),
                legend.text = element_text(size = 10)) +
          guides(linetype = guide_legend(nrow = 1, byrow = TRUE)) +
          xlab("Number of respondents identified as careless") +
          ylab("Percentage of true careless respondents recovered")

        ## save the plot
        file_save <- paste0(prefix, "_", design, "_", careless_type, "_",
                            careless_onset, "_R=", R, "_bw.pdf")
        ggsave(filename = file_save, plot = p_bw, device = "pdf",
               path = dir_save, width = 8.5 * 0.85, height = 6 * 0.85)

      }
    }
  }
}
