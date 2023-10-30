## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 15 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggpubr")){install.packages("ggpubr", dependencies = TRUE); require("ggpubr")}

# Load All Models --------------------------------------------------------------

load('all_models.RData')

# Define Function to Generate Gender Plots -------------------------------------

gender_plot <- function(model){
  
  plot <- ggplot(model, aes(x = gender, y = cosine, color = gender)) + 
    geom_hline(yintercept = 0.0, linetype = "dashed") + 
    geom_point(stat = "summary", fun = "mean", size = 2, 
               position = position_dodge(0.5)) + 
    theme_bw() + 
    theme(legend.position = "none",
          strip.text.x = element_blank()) + 
    labs(x = "Gender Groups", 
         y = "Standardized Cosine Similarity", 
         color = "Gender Groups") + 
    coord_cartesian(ylim = c(-0.40, 0.30)) +
    scale_color_aaas()
  
  return(plot)
}

# Generate Plot for BERT-2 -----------------------------------------------------

gender_plot(main_cosine_std)
# ggsave("cosine_gender_main.jpg", width = 8, height = 3, dpi = "retina")
ggsave("cosine_gender_main.pdf", width = 6, height = 3, dpi = "retina")

# Alternative Plots ------------------------------------------------------------

ggplot(all_models, aes(x = model, y = cosine, color = gender)) + 
  geom_point(stat = "summary", fun = "mean", size = 2,
             position = position_dodge(1)) + 
  facet_grid(.~model, scales = "free_x") + 
  theme_bw() + 
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(x = "Model Specification", 
       y = "Standardized Cosine Similarity", 
       color = "Gender Group") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

ggsave("cosine_gender_seven.jpg", width = 9, height = 4, dpi = "retina")
