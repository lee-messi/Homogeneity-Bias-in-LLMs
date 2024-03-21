
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 21 Mar 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggpubr")){install.packages("ggpubr", dependencies = TRUE); require("ggpubr")}

# Load all data frames ---------------------------------------------------------

load('all_models.RData')

# Create a new column specifying the model in which the data is from
bert2 <- bert2 %>% mutate(model = "BERT-2")
bert3 <- bert3 %>% mutate(model = "BERT-3")
roberta2 <- roberta2 %>% mutate(model = "RoBERTa-2")
roberta3 <- roberta3 %>% mutate(model = "RoBERTa-3")
mpnetbase <- mpnetbase %>% mutate(model = "all-mpnet-base-v2")
distilroberta <- distilroberta %>% mutate(model = "all-distilroberta-v1")
minilm <- minilm %>% mutate(model = "all-MiniLM-L12-v2")

# Concatenate the seven data frames into a single large data frame
all_models <- rbind(bert2, bert3, roberta2, roberta3, 
                    mpnetbase, distilroberta, minilm)

# Set model and race as factor variables and re-level them in specific order
all_models <- all_models %>% 
  mutate(model = as.factor(model)) %>%
  mutate(model = fct_relevel(model, c("BERT-2", "BERT-3", 
                                      "RoBERTa-2", "RoBERTa-3",
                                      "all-mpnet-base-v2",
                                      "all-distilroberta-v1",
                                      "all-MiniLM-L12-v2"))) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = fct_relevel(race, c("African Americans", 
                                    "Asian Americans", 
                                    "Hispanic Americans", 
                                    "White Americans"))) %>%
  arrange(model)

# Interaction Plot for BERT-2 --------------------------------------------------

ggplot(bert2, aes(x = race, y = cosine, color = gender)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  geom_point(size = 2, stat = "summary", fun = "mean", 
             position = position_dodge(0.75)) + 
  theme_bw() + 
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  theme(legend.position = c(0.5, 0.9), 
        legend.direction = "horizontal",
        legend.key.height = unit(0.4, "cm"),  # Adjust key height
        legend.spacing.y = unit(0.2, "cm"),   # Adjust vertical spacing
        legend.box.background = element_rect(fill = "white", color = "black"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10),
        legend.title = element_text(size = 9)) + 
  labs(x = "Racial/Ethnic Groups", 
       y = "Standardized Cosine Similarity", 
       color = "Gender Groups") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

# Save plot
ggsave("Figures/bert2_interaction.pdf", width = 6, height = 2.5, dpi = "retina")

# Interaction plot for all model specifications --------------------------------

ggplot(all_models, aes(x = model, y = cosine, color = gender)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  geom_point(stat = "summary", fun = "mean", 
             position = position_dodge(0.5)) + 
  facet_grid(race~model, scales = "free_x") + 
  theme_bw() + 
  theme(legend.position = "top",
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11),
        legend.box.spacing = unit(0.5, "pt"),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 13), 
        plot.margin = margin(t = 0, l = 1, r = 1)) + 
  labs(x = "Model Specification", 
       y = "Standardized Cosine Similarity", 
       color = "Gender Groups") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

# Save plot
ggsave("Figures/all_interactions.pdf", width = 10, height = 6, dpi = "retina")

