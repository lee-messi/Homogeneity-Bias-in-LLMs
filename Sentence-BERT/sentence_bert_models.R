
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

# Load .RData file -------------------------------------------------------------

# If you have run the entire code before, you can load the .RData file here
# If this is the first time running this code, ignore the load() function
# and the four summary() functions that follow. 
load('sentence_bert.RData')

# Race model output (all-mpnet-base-v2 model) ----------------------------------

# Summary output and log likelihood of the race model
summary(mpnetbase.race.model)
logLik(mpnetbase.race.model)

# Report degrees of freedom for t-statistics
summary(mpnetbase.race.model)$coefficients[2, "df"]
summary(mpnetbase.race.model)$coefficients[3, "df"]
summary(mpnetbase.race.model)$coefficients[4, "df"]

# Gender model output (all-mpnet-base-v2 model) --------------------------------

# Summary output and log likelihood of the gender model
summary(mpnetbase.gender.model)
logLik(mpnetbase.gender.model)

# Report degrees of freedom for t-statistics
summary(mpnetbase.gender.model)$coefficients[2, "df"]

# Race and gender model output (all-mpnet-base-v2 model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(mpnetbase.race.gender)
logLik(mpnetbase.race.gender)

# Report degrees of freedom for t-statistics
summary(mpnetbase.gender.model)$coefficients[2, "df"]

# Interaction model output (all-mpnet-base-v2 model) ---------------------------

# Report coefficients
summary(mpnetbase.interaction.model)
logLik(mpnetbase.interaction.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(mpnetbase.interaction.model)$coefficients[2, "df"]
summary(mpnetbase.interaction.model)$coefficients[3, "df"]
summary(mpnetbase.interaction.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(mpnetbase.interaction.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(mpnetbase.interaction.model)$coefficients[6, "df"]
summary(mpnetbase.interaction.model)$coefficients[7, "df"]
summary(mpnetbase.interaction.model)$coefficients[8, "df"]

# Pairwise comparisons (all-mpnet-base-v2 model) -------------------------------

mpnetbase.interactions <- emmeans(mpnetbase.interaction.model, ~ race * gender)

pairs(mpnetbase.interactions, simple = "gender")
pairs(mpnetbase.interactions, simple = "race")

# Race model output (all_distilroberta_v1model) ----------------------------------

# Summary output and log likelihood of the race model
summary(distilroberta.race.model)
logLik(distilroberta.race.model)

# Report degrees of freedom for t-statistics
summary(distilroberta.race.model)$coefficients[2, "df"]
summary(distilroberta.race.model)$coefficients[3, "df"]
summary(distilroberta.race.model)$coefficients[4, "df"]

# Gender model output (all_distilroberta_v1model) --------------------------------

# Summary output and log likelihood of the gender model
summary(distilroberta.gender.model)
logLik(distilroberta.gender.model)

# Report degrees of freedom for t-statistics
summary(distilroberta.gender.model)$coefficients[2, "df"]

# Race and gender model output (all_distilroberta_v1model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(distilroberta.race.gender)
logLik(distilroberta.race.gender)

# Report degrees of freedom for t-statistics
summary(distilroberta.gender.model)$coefficients[2, "df"]

# Interaction model output (all_distilroberta_v1model) ---------------------------

# Report coefficients
summary(distilroberta.interaction.model)
logLik(distilroberta.interaction.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(distilroberta.interaction.model)$coefficients[2, "df"]
summary(distilroberta.interaction.model)$coefficients[3, "df"]
summary(distilroberta.interaction.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(distilroberta.interaction.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(distilroberta.interaction.model)$coefficients[6, "df"]
summary(distilroberta.interaction.model)$coefficients[7, "df"]
summary(distilroberta.interaction.model)$coefficients[8, "df"]

# Pairwise comparisons (all_distilroberta_v1model) -------------------------------

distilroberta.interactions <- emmeans(distilroberta.interaction.model, ~ race * gender)

pairs(distilroberta.interactions, simple = "gender")
pairs(distilroberta.interactions, simple = "race")

# Race model output (all-MiniLM-L12-v2 model) ----------------------------------

# Summary output and log likelihood of the race model
summary(mpnetbase.race.model)
logLik(mpnetbase.race.model)

# Report degrees of freedom for t-statistics
summary(mpnetbase.race.model)$coefficients[2, "df"]
summary(mpnetbase.race.model)$coefficients[3, "df"]
summary(mpnetbase.race.model)$coefficients[4, "df"]

# Gender model output (all-MiniLM-L12-v2 model) --------------------------------

# Summary output and log likelihood of the gender model
summary(minilm.gender.model)
logLik(minilm.gender.model)

# Report degrees of freedom for t-statistics
summary(minilm.gender.model)$coefficients[2, "df"]

# Race and gender model output (all-MiniLM-L12-v2 model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(minilm.race.gender)
logLik(minilm.race.gender)

# Report degrees of freedom for t-statistics
summary(minilm.gender.model)$coefficients[2, "df"]

# Interaction model output (all-MiniLM-L12-v2 model) ---------------------------

# Report coefficients
summary(minilm.interaction.model)
logLik(minilm.interaction.model)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(minilm.interaction.model)$coefficients[2, "df"]
summary(minilm.interaction.model)$coefficients[3, "df"]
summary(minilm.interaction.model)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(minilm.interaction.model)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(minilm.interaction.model)$coefficients[6, "df"]
summary(minilm.interaction.model)$coefficients[7, "df"]
summary(minilm.interaction.model)$coefficients[8, "df"]

# Pairwise comparisons (all-MiniLM-L12-v2 model) -------------------------------

minilm.interactions <- emmeans(minilm.interaction.model, ~ race * gender)

pairs(minilm.interactions, simple = "gender")
pairs(minilm.interactions, simple = "race")
