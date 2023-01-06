# --------------------------------------------------------------------------------------------------------------------
# Here we plot and save the recall curves (averaged over 1,000 repetitions)
#
# Last changed on December 12, 2022
# --------------------------------------------------------------------------------------------------------------------
library("ggplot2")

# load output of analyzer script
load("simulation_analyzer.Rdata")


## get the data in long format ----
methods <- names(recall_ave)
ggdata <- NULL

for(j in seq_along(methods))
{
  ggdata <- rbind(ggdata,
                  data.frame(recall_ave[[j]], 
                             Contamination = rownames(recall_ave[[j]]), 
                             Method = methods[j]))
}

# ideal recall line
ideal <- outer(prop_careless_arr, 1:n, function(i,j) {
  x <- j / (i*n)
  ifelse(x < 1, x, 1)
})

# combine with ggdata
ggdata <- rbind(ggdata,
                data.frame(ideal, 
                           Contamination = prop_careless_arr,
                           Method = "ideal"))
ggdata$Method <- factor(ggdata$Method)
ggdata$Contamination <- as.numeric(ggdata$Contamination)
colnames(ggdata)[1:n] <- 1:n


# get data in final long format and adjust variable naming
df <- reshape2::melt(data = ggdata, 
                     id.vars = c("Contamination", "Method"),
                     variable.name = "Recall_size")
df$Recall_size <- as.numeric(df$Recall_size)
df$Method <- as.character(df$Method)
df$Method[df$Method == "mah2_ord"] <- "Mahalanobis"
df$Method[df$Method == "RE"]       <- "Autoencoder"
df$Method[df$Method == "reliability"] <- "Reliability"
df$Method[df$Method == "synonym"] <- "Synonym"
df$Method[df$Method == "antonym"] <- "Antonym"
df$Method <- factor(df$Method, levels = c("Autoencoder", "Mahalanobis", "Reliability", "Synonym", "Antonym", "ideal"))

# adjust level naming
df$Contamination[df$Contamination == 0.05] <- "5% prevalence"
df$Contamination[df$Contamination == 0.1]  <- "10% prevalence"
df$Contamination[df$Contamination == 0.15] <- "15% prevalence"
df$Contamination[df$Contamination == 0.2]  <- "20% prevalence"
df$Contamination[df$Contamination == 0.25] <- "25% prevalence"
df$Contamination[df$Contamination == 0.3]  <- "30% prevalence"
df$Contamination <- factor(df$Contamination, levels = paste0(c(5,10,15,20,25,30), "% prevalence"))

# obtain the final plot
p <- ggplot(df[df$Method != "ideal",], mapping = aes(x = Recall_size, y = value, color = Method)) +
  theme_bw() +
  geom_line(data = df[df$Method == "ideal",], mapping = aes(x = Recall_size, y = value)) +
  geom_line() +
  scale_color_manual(values = c(scales::hue_pal()(6)[c(4, 1, 2, 5, 6)], "darkgray"), 
                     breaks = levels(df$Method)[-6]) + # drop 'ideal' from legend
  facet_wrap(facets = vars(Contamination), nrow = 2, ncol = 3) + 
  theme(legend.position = "top", 
        legend.title = element_blank(),
        legend.text = element_text(size = 10)) +
  guides(linetype = guide_legend(nrow = 1, byrow = TRUE)) +
  xlab("Number of respondents identified as careless") +
  ylab("Percentage of true careless respondents recovered")

# save the plot
ggsave(filename = "simulation_plot.pdf", 
       plot = p,
       device = "pdf",
       width = 8.5 * 0.85, 
       height = 6 * 0.85) 
