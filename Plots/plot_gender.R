
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 25 Apr 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggpubr")){install.packages("ggpubr", dependencies = TRUE); require("ggpubr")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")} # Install XQuartz for this

# Load all data frames ---------------------------------------------------------

load('all_models.RData')

# Define function to generate gender plots -------------------------------------

gender_plot <- function(model){
  
  plot <- ggplot(model, aes(x = gender, y = cosine, color = gender)) + 
    geom_hline(yintercept = 0.0, linetype = "dashed") + 
    geom_point(stat = "summary", fun = "mean", size = 2, 
               position = position_dodge(0.5)) + 
    theme_bw() + 
    theme(legend.position = "none",
          strip.text.x = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 10)) + 
    labs(x = "Gender Groups", 
         y = "Standardized Cosine Similarity", 
         color = "Gender Groups") + 
    coord_cartesian(ylim = c(-0.40, 0.30)) +
    scale_color_aaas()
  
  return(plot)
}

# Generate plot for BERT-2 -----------------------------------------------------

gender_plot(bert2)
ggsave("Figures/bert2_gender.pdf", width = 6, height = 2.5, 
       dpi = "retina", device = cairo_pdf)

