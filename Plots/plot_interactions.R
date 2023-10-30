## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 26 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggpubr")){install.packages("ggpubr", dependencies = TRUE); require("ggpubr")}

# Load All Models --------------------------------------------------------------

load('all_models.RData')

# Create a new column specifying the model in which the data is from
main_cosine_std <- main_cosine_std %>% mutate(model = "BERT-2")
s1_layer_cosine_std <- s1_layer_cosine_std %>% mutate(model = "BERT-3")
s2_roberta_cosine_std <- s2_roberta_cosine_std %>% mutate(model = "RoBERTa-2")
s3_roberta_layer_cosine_std <- s3_roberta_layer_cosine_std %>% mutate(model = "RoBERTa-3")
s4_mpnetbase_cosine_std <- s4_mpnetbase_cosine_std %>% mutate(model = "all-mpnet-base-v2")
s4_distilroberta_cosine_std <- s4_distilroberta_cosine_std %>% mutate(model = "all-distilroberta-v1")
s4_allminilm_cosine_std <- s4_allminilm_cosine_std %>% mutate(model = "all-MiniLM-L12-v2")

# Concatenate the seven data frames into a single large data frame
all_models <- rbind(main_cosine_std, s1_layer_cosine_std,
                    s2_roberta_cosine_std, s3_roberta_layer_cosine_std,
                    s4_mpnetbase_cosine_std, s4_distilroberta_cosine_std,
                    s4_allminilm_cosine_std)

unique(all_models$race)

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

# Interaction Plot for All Model Specifications --------------------------------

ggplot(all_models, aes(x = model, y = cosine, color = gender)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  geom_point(stat = "summary", fun = "mean", 
             position = position_dodge(0.75)) + 
  facet_grid(race~model, scales = "free_x") + 
  theme_bw() + 
  theme(legend.position = "top",
        axis.text.x = element_blank(),
        axis.title.x = element_blank()) + 
  labs(x = "Model Specification", 
       y = "Standardized Cosine Similarity", 
       color = "Gender Groups") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

# Save plot
ggsave("all_interactions.pdf", width = 10, height = 6, dpi = "retina")

# Interaction Plot for BERT-2 --------------------------------------------------

ggplot(main_cosine_std, aes(x = race, y = cosine, color = gender)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  geom_point(size = 2, stat = "summary", fun = "mean", 
             position = position_dodge(0.75)) + 
  theme_bw() + 
  theme(legend.position = "top",
        axis.title.x = element_blank()) + 
  labs(x = "Racial/Ethnic Groups", 
       y = "Standardized Cosine Similarity", 
       color = "Gender Groups") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

# Save plot
ggsave("cosine_interaction.pdf", width = 6, height = 3, dpi = "retina")

