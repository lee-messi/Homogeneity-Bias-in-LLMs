
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 10 Apr 2024

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}

# Load .RData file -------------------------------------------------------------

# If you have run the entire code before, you can load the .RData file here
# If this is the first time running this code, ignore the load() function
# and the four summary() function that follow. 
load("roberta2.RData")

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
summary(race.gender.model)
logLik(race.gender.model)

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
