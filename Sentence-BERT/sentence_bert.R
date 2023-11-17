
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 17 Nov 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("arrow")){install.packages("arrow", dependencies = TRUE); require("arrow")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Load Data Frames -------------------------------------------------------------

# Import data frame generated in the python programming environment
# Set format, gender, and race as factor variables 
# Re-level gender and race variables so that White Americans and man as reference groups

mpnetbase = read_feather("all-mpnet-base-v2.feather") %>%
  mutate(format = as.factor(format)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man")) %>%
  mutate(cosine = scale(cosine))

distilroberta = read_feather("all_distilroberta_v1.feather") %>%
  mutate(format = as.factor(format)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man")) %>%
  mutate(cosine = scale(cosine))

minilm = read_feather("all-MiniLM-L12-v2.feather") %>%
  mutate(format = as.factor(format)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man")) %>%
  mutate(cosine = scale(cosine)) 

# Fit all mixed effects models (all-mpnet-base-v2 model) -----------------------

# Model examining the main effect of race/ethnicity
mpnetbase.race <- lmer(cosine ~ 1 + race + (1|format), 
                       data = mpnetbase, 
                       control = lmerControl(optimizer = "nmkbw", 
                                             calc.derivs = FALSE))

# Model examining the main effect of gender
mpnetbase.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                         data = mpnetbase, 
                         control = lmerControl(optimizer = "nmkbw", 
                                               calc.derivs = FALSE))

# Model examining both race/ethnicity and gender
mpnetbase.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                              data = mpnetbase, 
                              control = lmerControl(optimizer = "nmkbw",
                                                    calc.derivs = FALSE))

# Model examining all terms including the interaction
mpnetbase.interaction <- lmer(cosine ~ 1 + race * gender + (1|format),
                              data = mpnetbase,
                              control = lmerControl(optimizer = "nmkbw",
                                                    calc.derivs = FALSE))

# Fit all mixed effects models (all_distilroberta_v1 model) --------------------

# Model examining the main effect of race/ethnicity
distilroberta.race <- lmer(cosine ~ 1 + race + (1|format), 
                           data = distilroberta, 
                           control = lmerControl(optimizer = "nmkbw", 
                                                 calc.derivs = FALSE))

# Model examining the main effect of gender
distilroberta.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                             data = distilroberta, 
                             control = lmerControl(optimizer = "nmkbw", 
                                                   calc.derivs = FALSE))

# Model examining both race/ethnicity and gender
distilroberta.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                                  data = distilroberta, 
                                  control = lmerControl(optimizer = "nmkbw",
                                                        calc.derivs = FALSE))

# Model examining all terms including the interaction
distilroberta.interaction <- lmer(cosine ~ 1 + race * gender + (1|format),
                                  data = distilroberta,
                                  control = lmerControl(optimizer = "nmkbw",
                                                        calc.derivs = FALSE))

# Fit all mixed effects models (all-MiniLM-L12-v2 model) -----------------------

# Model examining the main effect of race/ethnicity
minilm.race <- lmer(cosine ~ 1 + race + (1|format), 
                    data = minilm, 
                    control = lmerControl(optimizer = "nmkbw", 
                                          calc.derivs = FALSE))

# Model examining the main effect of gender
minilm.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                      data = minilm, 
                      control = lmerControl(optimizer = "nmkbw", 
                                            calc.derivs = FALSE))

# Model examining both race/ethnicity and gender
minilm.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                           data = minilm, 
                           control = lmerControl(optimizer = "nmkbw",
                                                 calc.derivs = FALSE))

# Model examining all terms including the interaction
minilm.interaction <- lmer(cosine ~ 1 + race * gender + (1|format),
                           data = minilm,
                           control = lmerControl(optimizer = "nmkbw",
                                                 calc.derivs = FALSE))

# Save as .RData ---------------------------------------------------------------

# Save all the models as an .RData file
save.image('sentence_bert.RData')
