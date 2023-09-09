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

race_plot <- function(model){
  
  plot <- ggplot(model, aes(x = race, y = cosine, color = race)) + 
    geom_point(stat = "summary", fun = "mean", 
               position = position_dodge(0.5)) + 
    geom_errorbar(stat = "summary", fun.data = "mean_se", 
                  fun.args = list(mult = 1.96), 
                  width = 0.15, position = position_dodge(0.5)) +
    theme_bw() + 
    theme(legend.position = "none",
          strip.text.x = element_blank()) + 
    labs(x = "Racial/Ethnic Groups", 
         y = "Cosine Similarity", 
         color = "Racial/Ethnic Groups") + 
    coord_cartesian(ylim = c(-0.40, 0.30)) +
    scale_color_aaas() + 
    scale_x_discrete(labels = c("White Americans" = "White\nAmericans", 
                                "African Americans" = "African\nAmericans", 
                                "Asian Americans" = "Asian\nAmericans", 
                                "Hispanic Americans" = "Hispanic\nAmericans"))
  
  return(plot)
}

# Generate Plots for Each of the Models ----------------------------------------

race_1 <- race_plot(main_cosine_std)
race_2 <- race_plot(s1_layer_cosine_std)
race_3 <- race_plot(s2_roberta_cosine_std)
race_4 <- race_plot(s3_roberta_layer_cosine_std)
race_5 <- race_plot(s4_mpnetbase_cosine_std)
race_6 <- race_plot(s4_distilroberta_cosine_std)
race_7 <- race_plot(s4_allminilm_cosine_std)

# Arrange and Save Plot(s) -----------------------------------------------------

ggarrange(race_1, race_2, race_3, race_4, race_5, race_6, race_7,
          labels = c("A", "B", "C", "D", "E", "F", "G"),
          ncol = 2, nrow = 4, 
          common.legend = T)

ggsave("cosine_race.jpg", width = 8, height = 9, dpi = "retina")
ggsave("cosine_race.pdf", width = 8, height = 9, dpi = "retina")
