## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 9 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggpubr")){install.packages("ggpubr", dependencies = TRUE); require("ggpubr")}

# Load All Models --------------------------------------------------------------

load('all_models.RData')

# Define Function to Generate Interaction Effect Plots -------------------------

interaction_plot <- function(model){
  
  plot <- ggplot(model, aes(x = race, y = cosine, color = gender)) + 
    geom_point(stat = "summary", fun = "mean", 
               position = position_dodge(0.5)) + 
    geom_errorbar(stat = "summary", fun.data = "mean_se", 
                  fun.args = list(mult = 1.96), 
                  width = 0.15, position = position_dodge(0.5)) +
    facet_grid(.~race, scales = "free_x") + 
    theme_bw() + 
    theme(legend.position = "top",
          strip.text.x = element_blank()) + 
    labs(x = "Racial/Ethnic Groups", 
         y = "Cosine Similarity", 
         color = "Gender Groups") + 
    coord_cartesian(ylim = c(-0.40, 0.30)) +
    scale_color_aaas() + 
    scale_x_discrete(labels = c("White Americans" = "White\nAmericans", 
                                "African Americans" = "African\nAmericans", 
                                "Asian Americans" = "Asian\nAmericans", 
                                "Hispanic Americans" = "Hispanic\nAmericans"))
  
  return(plot)
}

# Generate Plots for Each of the Models ----------------------------------------

int_1 <- interaction_plot(main_cosine_std)
int_2 <- interaction_plot(s1_layer_cosine_std)
int_3 <- interaction_plot(s2_roberta_cosine_std)
int_4 <- interaction_plot(s3_roberta_layer_cosine_std)
int_5 <- interaction_plot(s4_mpnetbase_cosine_std)
int_6 <- interaction_plot(s4_distilroberta_cosine_std)
int_7 <- interaction_plot(s4_allminilm_cosine_std)

# Arrange and Save Plot(s) -----------------------------------------------------

ggarrange(int_1, int_2, int_3, int_4, int_5, int_6, int_7,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 2, nrow = 4, 
          common.legend = T)

ggsave("cosine_interaction.jpg", width = 8, height = 9, dpi = "retina")
ggsave("cosine_interaction.pdf", width = 8, height = 9, dpi = "retina")
