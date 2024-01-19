
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 18 Jan 2024

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

extra_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, " african ", " ") # remove mentioning of race/ethnicity
  x = str_replace_all(x, " asian ", " ") 
  x = str_replace_all(x, " hispanic ", " ") 
  x = str_replace_all(x, " white ", " ") 
  x = str_replace_all(x, "woman", " ") # remove mentioning of gender
  x = str_replace_all(x, "man", " ") 
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load Stories -----------------------------------------------------------------

stories <- read.csv('../data/generated_text_final.csv') %>%
  mutate(text = extra_prep(text))

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
  bm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bm.embeds <- as.matrix(bm.embeddings[['texts']]$text)
  bm.cosines <- sim2(bm.embeds)
  bmc <- append(bmc, list(bm.cosines[upper.tri(bm.cosines)]))
}

gc()

for (i in black_females){
  bf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bf.embeds <- as.matrix(bf.embeddings[['texts']]$text)
  bf.cosines <- sim2(bf.embeds)
  bfc <- append(bfc, list(bf.cosines[upper.tri(bf.cosines)]))
}

gc()

for (i in asian_males){
  am.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  am.embeds <- as.matrix(am.embeddings[['texts']]$text)
  am.cosines <- sim2(am.embeds)
  amc <- append(amc, list(am.cosines[upper.tri(am.cosines)]))
}

gc()

for (i in asian_females){
  af.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  af.embeds <- as.matrix(af.embeddings[['texts']]$text)
  af.cosines <- sim2(af.embeds)
  afc <- append(afc, list(af.cosines[upper.tri(af.cosines)]))
}

gc()

for (i in hispanic_males){
  hm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hm.embeds <- as.matrix(hm.embeddings[['texts']]$text)
  hm.cosines <- sim2(hm.embeds)
  hmc <- append(hmc, list(hm.cosines[upper.tri(hm.cosines)]))
}

gc()

for (i in hispanic_females){
  hf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hf.embeds <- as.matrix(hf.embeddings[['texts']]$text)
  hf.cosines <- sim2(hf.embeds)
  hfc <- append(hfc, list(hf.cosines[upper.tri(hf.cosines)]))
}

gc()

for (i in white_males){
  wm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  wm.embeds <- as.matrix(wm.embeddings[['texts']]$text)
  wm.cosines <- sim2(wm.embeds)
  wmc <- append(wmc, list(wm.cosines[upper.tri(wm.cosines)]))
}

gc()

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

# Load .RData file -------------------------------------------------------------

load("preprocessing.RData")

# Fit all mixed effects models -------------------------------------------------

# Model examining the main effect of race/ethnicity
race.model <- lmer(cosine ~ 1 + race + (1|format), 
                   data = cosine_std, 
                   control = lmerControl(optimizer = "nmkbw", 
                                         calc.derivs = FALSE))

# Model examining the main effect of gender
gender.model <- lmer(cosine ~ 1 + gender + (1|format), 
                     data = cosine_std, 
                     control = lmerControl(optimizer = "nmkbw", 
                                           calc.derivs = FALSE))

# Model examining both race/ethnicity and gender
race.gender.model <- lmer(cosine ~ 1 + race + gender + (1|format), 
                          data = cosine_std, 
                          control = lmerControl(optimizer = "nmkbw", 
                                                calc.derivs = FALSE))

# Model examining all terms including the interaction
interaction.model <- lmer(cosine ~ 1 + race * gender + (1|format), 
                          data = cosine_std, 
                          control = lmerControl(optimizer = "nmkbw", 
                                                calc.derivs = FALSE))

# Race model output ------------------------------------------------------------

# Summary output and log likelihood of the race model
summary(race.model)
logLik(race.model)

# Report degrees of freedom for t-statistics
summary(race.model)$coefficients[2, "df"]
summary(race.model)$coefficients[3, "df"]
summary(race.model)$coefficients[4, "df"]

# Gender model output ----------------------------------------------------------

# Summary output and log likelihood of the gender model
summary(gender.model)
logLik(gender.model)

# Report degrees of freedom for t-statistics
summary(gender.model)$coefficients[2, "df"]

# Race and gender model output -------------------------------------------------

# Summary output and log likelihood of the race and gender model
summary(race.gender)
logLik(race.gender)

# Report degrees of freedom for t-statistics
summary(gender.model)$coefficients[2, "df"]

# Interaction model output -----------------------------------------------------

# Report coefficients
summary(interaction.model)
logLik(interaction.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(interaction.model)$coefficients[2, "df"]
summary(interaction.model)$coefficients[3, "df"]
summary(interaction.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(interaction.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(interaction.model)$coefficients[6, "df"]
summary(interaction.model)$coefficients[7, "df"]
summary(interaction.model)$coefficients[8, "df"]

# Pairwise comparisons ---------------------------------------------------------

cosine.interactions <- emmeans(interaction.model, ~ race * gender)
pairs(cosine.interactions, simple = "gender", reverse = TRUE)

# Save as .RData ---------------------------------------------------------------

rm(i, extra_prep)
save.image("preprocessing.RData")
