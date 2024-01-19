
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 8 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}

# Initilize the text package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define preprocessing steps ---------------------------------------------------

simple_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load Stories -----------------------------------------------------------------

stories <- read.csv('Original/suppression_study_1.csv') %>%
  mutate(text = simple_prep(text))

# Count the number of texts containing adversity-related keywords
keywords <- c("adversity", "barrier")

stories %>% filter(race %in% c("African", "Asian", "Hispanic")) %>% 
  mutate(contains = str_detect(text, paste(keywords, collapse = "|"))) %>% 
  pull(contains) %>% sum()

# Total number of texts about African, Asian, and Hispanic Americans
stories %>% filter(race %in% c("African", "Asian", "Hispanic")) %>% nrow()

# Separate stories by gender, racial/ethnic group, and text format -------------

black_males <- stories %>% 
  filter(gender == "man" & race == "African") %>% 
  mutate(gender = "Man") %>% mutate(race = "African Americans") %>% 
  group_split(format)

black_females <- stories %>% 
  filter(gender == "woman" & race == "African") %>% 
  mutate(gender = "Woman") %>% mutate(race = "African Americans") %>% 
  group_split(format)

asian_males <- stories %>% 
  filter(gender == "man" & race == "Asian") %>% 
  mutate(gender = "Man") %>% mutate(race = "Asian Americans") %>% 
  group_split(format)

asian_females <- stories %>% 
  filter(gender == "woman" & race == "Asian") %>% 
  mutate(gender = "Woman") %>% mutate(race = "Asian Americans") %>% 
  group_split(format)

hispanic_males <- stories %>% 
  filter(gender == "man" & race == "Hispanic") %>% 
  mutate(gender = "Man") %>% mutate(race = "Hispanic Americans") %>% 
  group_split(format)

hispanic_females <- stories %>% 
  filter(gender == "woman" & race == "Hispanic") %>% 
  mutate(gender = "Woman") %>% mutate(race = "Hispanic Americans") %>% 
  group_split(format)

white_males <- stories %>% 
  filter(gender == "man" & race == "White") %>% 
  mutate(gender = "Man") %>% mutate(race = "White Americans") %>% 
  group_split(format)

white_females <- stories %>% 
  filter(gender == "woman" & race == "White") %>% 
  mutate(gender = "Woman") %>% mutate(race = "White Americans") %>% 
  group_split(format)

# Calculate cosine similarity between sentence embeddings for each text format -

bmc <- bfc <- amc <- afc <- hmc <- hfc <- wmc <- wfc <- list()

for (i in black_males){
  bm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bm.embeds <- as.matrix(bm.embeddings[['texts']]$text)
  bm.cosines <- sim2(bm.embeds)
  bmc <- append(bmc, list(bm.cosines[upper.tri(bm.cosines)]))
}

for (i in black_females){
  bf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bf.embeds <- as.matrix(bf.embeddings[['texts']]$text)
  bf.cosines <- sim2(bf.embeds)
  bfc <- append(bfc, list(bf.cosines[upper.tri(bf.cosines)]))
}

for (i in asian_males){
  am.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  am.embeds <- as.matrix(am.embeddings[['texts']]$text)
  am.cosines <- sim2(am.embeds)
  amc <- append(amc, list(am.cosines[upper.tri(am.cosines)]))
}

for (i in asian_females){
  af.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  af.embeds <- as.matrix(af.embeddings[['texts']]$text)
  af.cosines <- sim2(af.embeds)
  afc <- append(afc, list(af.cosines[upper.tri(af.cosines)]))
}

for (i in hispanic_males){
  hm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hm.embeds <- as.matrix(hm.embeddings[['texts']]$text)
  hm.cosines <- sim2(hm.embeds)
  hmc <- append(hmc, list(hm.cosines[upper.tri(hm.cosines)]))
}

for (i in hispanic_females){
  hf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hf.embeds <- as.matrix(hf.embeddings[['texts']]$text)
  hf.cosines <- sim2(hf.embeds)
  hfc <- append(hfc, list(hf.cosines[upper.tri(hf.cosines)]))
}

for (i in white_males){
  wm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  wm.embeds <- as.matrix(wm.embeddings[['texts']]$text)
  wm.cosines <- sim2(wm.embeds)
  wmc <- append(wmc, list(wm.cosines[upper.tri(wm.cosines)]))
}

for (i in white_females){
  wf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  wf.embeds <- as.matrix(wf.embeddings[['texts']]$text)
  wf.cosines <- sim2(wf.embeds)
  wfc <- append(wfc, list(wf.cosines[upper.tri(wf.cosines)]))
}

gc()

# Create a data frame with all cosine similarity measurements ------------------

group.length <- length(unlist(bmc, recursive = FALSE))
format.length <- length(bmc[[1]])

race_list <- rep(c('African Americans', 'Asian Americans', 
                   'Hispanic Americans', 'White Americans'), each = group.length * 2)

gender_list <- rep(rep(c("Man", "Woman"), each = group.length), 4)

format_list <- rep(rep(c('story about', 'character description of', 'biography of',
                         'introduction of', 'social media profile of', 'synopsis for', 
                         'narrative of', 'self-introduction of', 'tragic story about', 
                         'funny story about', 'romantic story about', 
                         'horror story about', 'dramatic story about'), each = format.length), 8)

cosine_list <- c(unlist(bmc, recursive = FALSE), unlist(bfc, recursive = FALSE),
                 unlist(amc, recursive = FALSE), unlist(afc, recursive = FALSE),
                 unlist(hmc, recursive = FALSE), unlist(hfc, recursive = FALSE),
                 unlist(wmc, recursive = FALSE), unlist(wfc, recursive = FALSE))

cosine_df <- data.frame(race_list, gender_list, format_list, cosine_list)
colnames(cosine_df) <- c('race', 'gender', 'format', 'cosine')

cosine_df <- cosine_df %>% 
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(format = as.factor(format)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man"))

# Standardize cosine similarity before fitting mixed-effects models
cosine_std <- cosine_df %>% 
  mutate(across(where(is.numeric), scale))

# Build mixed effects model including the main effects -------------------------

# Model examining the main effect of race/ethnicity
race.effect <- lmer(cosine ~ 1 + race + (1|format), 
                    data = cosine_std, 
                    control = lmerControl(optimizer = "nmkbw", 
                                          calc.derivs = FALSE))

# Report coefficients
summary(race.effect)

# Report degrees of freedom for t-statistics
summary(race.effect)$coefficients[2, "df"]
summary(race.effect)$coefficients[3, "df"]
summary(race.effect)$coefficients[4, "df"]

# Perform likelihood ratio test for all fixed effects
mixed(cosine ~ 1 + race + (1|format),
      data = cosine_std, 
      control = lmerControl(optimizer = "nmkbw", 
                            calc.derivs = FALSE),
      method = "LRT")

# Save as .RData ---------------------------------------------------------------

# save.image("suppression_study_1_cosines.RData")
load("suppression_study_1_cosines.RData")

# Plot the effect of race/ethnicity --------------------------------------------

ggplot(cosine_std, aes(x = race, y = cosine, color = race)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  geom_point(stat = "summary", fun = "mean", size = 2, 
             position = position_dodge(0.5)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        strip.text.x = element_blank()) + 
  labs(x = "Racial/Ethnic Groups", 
       y = "Standardized Cosine Similarity", 
       color = "Racial/Ethnic Groups") + 
  coord_cartesian(ylim = c(-0.40, 0.30)) +
  scale_color_aaas()

ggsave("Figures/suppression_study_1_race.pdf", width = 6, height = 2.5, dpi = "retina")

