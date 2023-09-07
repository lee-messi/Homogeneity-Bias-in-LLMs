
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 7 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Initilize the text Package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define Preprocessing Steps ---------------------------------------------------

simple_prep = function(x) {
  # x = str_to_lower(x) # we do not lower case as we use "roberta-base" model here
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load Stories -----------------------------------------------------------------

stories <- read.csv('../data/generated_text_final.csv') %>%
  mutate(text = simple_prep(text))

# Separate Stories by Gender, Racial/Ethnic Group, and Text Format -------------

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

# Calculate Cosine Similarity between Sentence Embeddings for each Text Format -

bmc <- bfc <- amc <- afc <- hmc <- hfc <- wmc <- wfc <- list()

for (i in black_males){
  bm.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  bm.embeds <- as.matrix(bm.embeddings[['texts']]$text)
  bm.cosines <- sim2(bm.embeds)
  bmc <- append(bmc, list(bm.cosines[upper.tri(bm.cosines)]))
}

gc()

for (i in black_females){
  bf.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  bf.embeds <- as.matrix(bf.embeddings[['texts']]$text)
  bf.cosines <- sim2(bf.embeds)
  bfc <- append(bfc, list(bf.cosines[upper.tri(bf.cosines)]))
}

gc()

for (i in asian_males){
  am.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  am.embeds <- as.matrix(am.embeddings[['texts']]$text)
  am.cosines <- sim2(am.embeds)
  amc <- append(amc, list(am.cosines[upper.tri(am.cosines)]))
}

gc()

for (i in asian_females){
  af.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  af.embeds <- as.matrix(af.embeddings[['texts']]$text)
  af.cosines <- sim2(af.embeds)
  afc <- append(afc, list(af.cosines[upper.tri(af.cosines)]))
}

gc()

for (i in hispanic_males){
  hm.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  hm.embeds <- as.matrix(hm.embeddings[['texts']]$text)
  hm.cosines <- sim2(hm.embeds)
  hmc <- append(hmc, list(hm.cosines[upper.tri(hm.cosines)]))
}

gc()

for (i in hispanic_females){
  hf.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  hf.embeds <- as.matrix(hf.embeddings[['texts']]$text)
  hf.cosines <- sim2(hf.embeds)
  hfc <- append(hfc, list(hf.cosines[upper.tri(hf.cosines)]))
}

gc()

for (i in white_males){
  wm.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
  wm.embeds <- as.matrix(wm.embeddings[['texts']]$text)
  wm.cosines <- sim2(wm.embeds)
  wmc <- append(wmc, list(wm.cosines[upper.tri(wm.cosines)]))
}

gc()

for (i in white_females){
  wf.embeddings <- textEmbed(i$text, model = "roberta-base", layers = -3, keep_token_embeddings = FALSE)
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

# Load .RData file -------------------------------------------------------------

# load("s3_layer_roberta_cosine.RData")

# Build mixed effects model including the main effects -------------------------

cosine_std <- cosine_df %>% 
  mutate(across(where(is.numeric), scale))

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

# Perform Likelihood ratio test for the main effect of race/ethnicity
mixed(cosine ~ 1 + race + (1|format),
      data = cosine_std, 
      control = lmerControl(optimizer = "nmkbw", 
                            calc.derivs = FALSE),
      method = "LRT")

# Model examining the main effect of gender
gender.effect <- lmer(cosine ~ 1 + gender + (1|format), 
                      data = cosine_std, 
                      control = lmerControl(optimizer = "nmkbw", 
                                            calc.derivs = FALSE))

# Report coefficients
summary(gender.effect)

# Report degrees of freedom for t-statistics
summary(gender.effect)$coefficients[2, "df"]

# Perform Likelihood ratio test for the main effect of gender
mixed(cosine ~ 1 + gender + (1|format),
      data = cosine_std, 
      control = lmerControl(optimizer = "nmkbw", 
                            calc.derivs = FALSE),
      method = "LRT")

# Build mixed effects model including interactions (Supplement) ----------------

cosine.model <- lmer(cosine ~ 1 + race * gender + (1|format), 
                     data = cosine_std, 
                     control = lmerControl(optimizer = "nmkbw", 
                                           calc.derivs = FALSE))

# Report coefficients
summary(cosine.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(cosine.model)$coefficients[2, "df"]
summary(cosine.model)$coefficients[3, "df"]
summary(cosine.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(cosine.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(cosine.model)$coefficients[6, "df"]
summary(cosine.model)$coefficients[7, "df"]
summary(cosine.model)$coefficients[8, "df"]

# Perform likelihood ratio test for all fixed effects
mixed(cosine ~ 1 + race * gender + (1|format),
      data = cosine_std, 
      control = lmerControl(optimizer = "nmkbw", 
                            calc.derivs = FALSE),
      method = "LRT")

# Conduct pairwise comparisons -------------------------------------------------

cosine.interactions <- emmeans(cosine.model, ~ race * gender)
pairs(cosine.interactions, simple = "gender")

# Save as .RData ---------------------------------------------------------------

rm(i, simple_prep)
save.image("s3_layer_roberta_cosine.RData")
