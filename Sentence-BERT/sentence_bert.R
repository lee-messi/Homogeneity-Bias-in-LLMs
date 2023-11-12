
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 10 Nov 2023

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

allminilm = read_feather("all-MiniLM-L12-v2.feather") %>%
  mutate(format = as.factor(format)) %>%
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man")) %>%
  mutate(cosine = scale(cosine)) 

# Load .RData file -------------------------------------------------------------

# If you have run the entire code before, you can load the .RData file here
# If this is the first time running this code, ignore the load() function
# and the four summary() functions that follow. 
load('sentence_bert.RData')

# Summary output of the four models (M1 ~ M4) using all-mpnet-base-v2
summary(mpnetbase.race)
summary(mpnetbase.gender)
summary(mpnetbase.race.gender)
summary(mpnetbase.model)

# Log likelihood of the four models (M1 ~ M4) using all-mpnet-base-v2
logLik(mpnetbase.race)
logLik(mpnetbase.gender)
logLik(mpnetbase.race.gender)
logLik(mpnetbase.model)

# Summary output of the four models (M1 ~ M4) using all_distilroberta_v1 model
summary(distilroberta.race)
summary(distilroberta.gender)
summary(distilroberta.race.gender)
summary(distilroberta.model)

# Log likelihood of the four models (M1 ~ M4) using all_distilroberta_v1 model
logLik(distilroberta.race)
logLik(distilroberta.gender)
logLik(distilroberta.race.gender)
logLik(distilroberta.model)

# Summary output of the four models (M1 ~ M4) using all_distilroberta_v1 model
summary(allminilm.race)
summary(allminilm.gender)
summary(allminilm.race.gender)
summary(allminilm.model)

# Log likelihood of the four models (M1 ~ M4) using all_distilroberta_v1 model
logLik(allminilm.race)
logLik(allminilm.gender)
logLik(allminilm.race.gender)
logLik(allminilm.model)

# Mixed Effects Model for the all-mpnet-base-v2 model --------------------------

# Fit mixed effects model with three fixed effects and one random effect
# Three fixed effects being main effect of race, gender, and their interactions
mpnetbase.model <- lmer(cosine ~ 1 + race * gender + (1|format),
                        data = mpnetbase,
                        control = lmerControl(optimizer = "nmkbw",
                                              calc.derivs = FALSE))

# Perform Likelihood Ratio Tests 
mixed(cosine ~ 1 + race * gender + (1|format),
      data = mpnetbase,
      method = "LRT",
      control=lmerControl(optimizer = "nmkbw",
                          calc.derivs = FALSE))

# Summary output of the full model
summary(mpnetbase.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(mpnetbase.model)$coefficients[2, "df"]
summary(mpnetbase.model)$coefficients[3, "df"]
summary(mpnetbase.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(mpnetbase.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(mpnetbase.model)$coefficients[6, "df"]
summary(mpnetbase.model)$coefficients[7, "df"]
summary(mpnetbase.model)$coefficients[8, "df"]

# Conduct pairwise comparisons 
mpnetbase.model.emmeans <- emmeans(mpnetbase.model, ~ race * gender)
pairs(mpnetbase.model.emmeans, simple = "gender")

# Fit simplified model for race
mpnetbase.race <- lmer(cosine ~ 1 + race + (1|format), 
                       data = mpnetbase, 
                       control = lmerControl(optimizer = "nmkbw", 
                                             calc.derivs = FALSE))

# Report coefficients
summary(mpnetbase.race)

# Report degrees of freedom for t-statistics
summary(mpnetbase.race)$coefficients[2, "df"]
summary(mpnetbase.race)$coefficients[3, "df"]
summary(mpnetbase.race)$coefficients[4, "df"]

# Fit simplified model for gender
mpnetbase.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                         data = mpnetbase, 
                         control = lmerControl(optimizer = "nmkbw", 
                                               calc.derivs = FALSE))

# Report coefficients
summary(mpnetbase.gender)

# Report degrees of freedom for t-statistics
summary(mpnetbase.gender)$coefficients[2, "df"]

# Model examining both race/ethnicity and gender
mpnetbase.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                              data = mpnetbase, 
                              control = lmerControl(optimizer = "nmkbw",
                                                    calc.derivs = FALSE))

summary(mpnetbase.race.gender)

# Log likelihood of the four models (M1 ~ M4)
logLik(mpnetbase.race)
logLik(mpnetbase.gender)
logLik(mpnetbase.race.gender)
logLik(mpnetbase.model)

# Mixed Effects Model for the all_distilroberta_v1 model -----------------------

# Fit mixed effects model with three fixed effects and one random effect
# Three fixed effects being main effect of race, gender, and their interactions
distilroberta.model <- lmer(cosine ~ 1 + race * gender + (1|format),
                            data = distilroberta,
                            control = lmerControl(optimizer = "nmkbw",
                                                  calc.derivs = FALSE))

# Perform Likelihood Ratio Tests 
mixed(cosine ~ 1 + race * gender + (1|format),
      data = distilroberta,
      method = "LRT",
      control=lmerControl(optimizer = "nmkbw",
                          calc.derivs = FALSE))

# Summary output of the full model
summary(distilroberta.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(distilroberta.model)$coefficients[2, "df"]
summary(distilroberta.model)$coefficients[3, "df"]
summary(distilroberta.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(distilroberta.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(distilroberta.model)$coefficients[6, "df"]
summary(distilroberta.model)$coefficients[7, "df"]
summary(distilroberta.model)$coefficients[8, "df"]

# Conduct pairwise comparisons 
distilroberta.model.emmeans <- emmeans(distilroberta.model, ~ race * gender)
pairs(distilroberta.model.emmeans, simple = "gender")

# Fit simplified model for race
distilroberta.race <- lmer(cosine ~ 1 + race + (1|format), 
                           data = distilroberta, 
                           control = lmerControl(optimizer = "nmkbw", 
                                                 calc.derivs = FALSE))

# Report coefficients
summary(distilroberta.race)

# Report degrees of freedom for t-statistics
summary(distilroberta.race)$coefficients[2, "df"]
summary(distilroberta.race)$coefficients[3, "df"]
summary(distilroberta.race)$coefficients[4, "df"]

# Fit simplified model for gender
distilroberta.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                             data = distilroberta, 
                             control = lmerControl(optimizer = "nmkbw", 
                                                   calc.derivs = FALSE))

# Report coefficients
summary(distilroberta.gender)

# Report degrees of freedom for t-statistics
summary(distilroberta.gender)$coefficients[2, "df"]

# Model examining both race/ethnicity and gender
distilroberta.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                                  data = distilroberta, 
                                  control = lmerControl(optimizer = "nmkbw",
                                                        calc.derivs = FALSE))

summary(distilroberta.race.gender)

# Log likelihood of the four models (M1 ~ M4)
logLik(distilroberta.race)
logLik(distilroberta.gender)
logLik(distilroberta.race.gender)
logLik(distilroberta.model)

# Mixed Effects Model for the all-MiniLM-L12-v2 model --------------------------

# Fit mixed effects model with three fixed effects and one random effect
# Three fixed effects being main effect of race, gender, and their interactions
allminilm.model <- lmer(cosine ~ 1 + race * gender + (1|format),
                        data = allminilm,
                        control = lmerControl(optimizer = "nmkbw",
                                              calc.derivs = FALSE))

# Perform Likelihood Ratio Tests 
mixed(cosine ~ 1 + race * gender + (1|format),
      data = allminilm,
      method = "LRT",
      control=lmerControl(optimizer = "nmkbw",
                          calc.derivs = FALSE))

# Summary output of the full model
summary(allminilm.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(allminilm.model)$coefficients[2, "df"]
summary(allminilm.model)$coefficients[3, "df"]
summary(allminilm.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(allminilm.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(allminilm.model)$coefficients[6, "df"]
summary(allminilm.model)$coefficients[7, "df"]
summary(allminilm.model)$coefficients[8, "df"]

# Conduct pairwise comparisons 
allminilm.model.emmeans <- emmeans(allminilm.model, ~ race * gender)
pairs(allminilm.model.emmeans, simple = "gender")

# Fit simplified model for race
allminilm.race <- lmer(cosine ~ 1 + race + (1|format), 
                       data = allminilm, 
                       control = lmerControl(optimizer = "nmkbw", 
                                             calc.derivs = FALSE))

# Report coefficients
summary(allminilm.race)

# Report degrees of freedom for t-statistics
summary(allminilm.race)$coefficients[2, "df"]
summary(allminilm.race)$coefficients[3, "df"]
summary(allminilm.race)$coefficients[4, "df"]

# Fit simplified model for gender
allminilm.gender <- lmer(cosine ~ 1 + gender + (1|format), 
                         data = allminilm, 
                         control = lmerControl(optimizer = "nmkbw", 
                                               calc.derivs = FALSE))

# Report coefficients
summary(allminilm.gender)

# Report degrees of freedom for t-statistics
summary(allminilm.gender)$coefficients[2, "df"]

# Model examining both race/ethnicity and gender
allminilm.race.gender <- lmer(cosine ~ 1 + race + gender + (1|format), 
                              data = allminilm, 
                              control = lmerControl(optimizer = "nmkbw",
                                                    calc.derivs = FALSE))

summary(allminilm.race.gender)

# Log likelihood of the four models (M1 ~ M4)
logLik(allminilm.race)
logLik(allminilm.gender)
logLik(allminilm.race.gender)
logLik(allminilm.model)

# Save as .RData ---------------------------------------------------------------

# Save all the models as an .RData file
# rm(i, simple_prep)
# save.image('sentence_bert.RData')
