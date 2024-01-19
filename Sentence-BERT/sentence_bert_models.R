
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 18 Jan 2024

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
summary(mpnetbase.race)
logLik(mpnetbase.race)

# Report degrees of freedom for t-statistics
summary(mpnetbase.race)$coefficients[2, "df"]
summary(mpnetbase.race)$coefficients[3, "df"]
summary(mpnetbase.race)$coefficients[4, "df"]

# Gender model output (all-mpnet-base-v2 model) --------------------------------

# Summary output and log likelihood of the gender model
summary(mpnetbase.gender)
logLik(mpnetbase.gender)

# Report degrees of freedom for t-statistics
summary(mpnetbase.gender)$coefficients[2, "df"]

# Race and gender model output (all-mpnet-base-v2 model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(mpnetbase.race.gender)
logLik(mpnetbase.race.gender)

# Report degrees of freedom for t-statistics
summary(mpnetbase.gender)$coefficients[2, "df"]

# Interaction model output (all-mpnet-base-v2 model) ---------------------------

# Report coefficients
summary(mpnetbase.interaction)
logLik(mpnetbase.interaction)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(mpnetbase.interaction)$coefficients[2, "df"]
summary(mpnetbase.interaction)$coefficients[3, "df"]
summary(mpnetbase.interaction)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(mpnetbase.interaction)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(mpnetbase.interaction)$coefficients[6, "df"]
summary(mpnetbase.interaction)$coefficients[7, "df"]
summary(mpnetbase.interaction)$coefficients[8, "df"]

# Pairwise comparisons (all-mpnet-base-v2 model) -------------------------------

mpnetbase.interactions <- emmeans(mpnetbase.interaction, ~ race * gender)
pairs(mpnetbase.interactions, simple = "gender", reverse = TRUE)

# Race model output (all_distilroberta_v1model) ----------------------------------

# Summary output and log likelihood of the race model
summary(distilroberta.race)
logLik(distilroberta.race)

# Report degrees of freedom for t-statistics
summary(distilroberta.race)$coefficients[2, "df"]
summary(distilroberta.race)$coefficients[3, "df"]
summary(distilroberta.race)$coefficients[4, "df"]

# Gender model output (all_distilroberta_v1model) --------------------------------

# Summary output and log likelihood of the gender model
summary(distilroberta.gender)
logLik(distilroberta.gender)

# Report degrees of freedom for t-statistics
summary(distilroberta.gender)$coefficients[2, "df"]

# Race and gender model output (all_distilroberta_v1model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(distilroberta.race.gender)
logLik(distilroberta.race.gender)

# Report degrees of freedom for t-statistics
summary(distilroberta.gender)$coefficients[2, "df"]

# Interaction model output (all_distilroberta_v1model) ---------------------------

# Report coefficients
summary(distilroberta.interaction)
logLik(distilroberta.interaction)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(distilroberta.interaction)$coefficients[2, "df"]
summary(distilroberta.interaction)$coefficients[3, "df"]
summary(distilroberta.interaction)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(distilroberta.interaction)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(distilroberta.interaction)$coefficients[6, "df"]
summary(distilroberta.interaction)$coefficients[7, "df"]
summary(distilroberta.interaction)$coefficients[8, "df"]

# Pairwise comparisons (all_distilroberta_v1model) -------------------------------

distilroberta.interactions <- emmeans(distilroberta.interaction, ~ race * gender)
pairs(distilroberta.interactions, simple = "gender", reverse = TRUE)

# Race model output (all-MiniLM-L12-v2 model) ----------------------------------

# Summary output and log likelihood of the race model
summary(minilm.race)
logLik(minilm.race)

# Report degrees of freedom for t-statistics
summary(minilm.race)$coefficients[2, "df"]
summary(minilm.race)$coefficients[3, "df"]
summary(minilm.race)$coefficients[4, "df"]

# Gender model output (all-MiniLM-L12-v2 model) --------------------------------

# Summary output and log likelihood of the gender model
summary(minilm.gender)
logLik(minilm.gender)

# Report degrees of freedom for t-statistics
summary(minilm.gender)$coefficients[2, "df"]

# Race and gender model output (all-MiniLM-L12-v2 model) -----------------------

# Summary output and log likelihood of the race and gender model
summary(minilm.race.gender)
logLik(minilm.race.gender)

# Report degrees of freedom for t-statistics
summary(minilm.gender)$coefficients[2, "df"]

# Interaction model output (all-MiniLM-L12-v2 model) ---------------------------

# Report coefficients
summary(minilm.interaction)
logLik(minilm.interaction)

# Report degrees of freedom for t-statistics (race/ethnicity)
summary(minilm.interaction)$coefficients[2, "df"]
summary(minilm.interaction)$coefficients[3, "df"]
summary(minilm.interaction)$coefficients[4, "df"]
# Report degrees of freedom for t-statistics (gender)
summary(minilm.interaction)$coefficients[5, "df"]
# Report degrees of freedom for t-statistics (interaction effect)
summary(minilm.interaction)$coefficients[6, "df"]
summary(minilm.interaction)$coefficients[7, "df"]
summary(minilm.interaction)$coefficients[8, "df"]

# Pairwise comparisons (all-MiniLM-L12-v2 model) -------------------------------

minilm.interactions <- emmeans(minilm.interaction, ~ race * gender)
pairs(minilm.interactions, simple = "gender", reverse = TRUE)
