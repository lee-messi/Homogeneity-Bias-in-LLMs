
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 10 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("effectsize")){install.packages("effectsize", dependencies = TRUE); require("effectsize")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}

# Initilize the text Package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define Function to Perform Preprocessing -------------------------------------

original_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load Stories -----------------------------------------------------------------

setwd('Pilot Data')

stories <- read.csv('pilot_data.csv') %>%
  mutate(clean_text = original_prep(text)) %>%
  mutate(clean_text = gsub('"', '', clean_text)) %>%
  mutate(clean_text = gsub(',', '', clean_text))

# Sample three stories that are coded as 1 and three coded as 0
sampled_stories <- stories %>% group_by(hardship) %>% sample_n(3)

# Separate Stories by Gender and Racial/Ethnic Group ---------------------------

black <- stories %>% filter(race == "African")
asian <- stories %>% filter(race == "Asian")
hispanic <- stories %>% filter(race == "Hispanic")
white <- stories %>% filter(race == "White")

# Create Sentence Embeddings for the Generated Stories -------------------------

black_embeddings = textEmbed(black$text, keep_token_embeddings = FALSE)
black_embeds <- as.matrix(black_embeddings[['texts']]$text)

asian_embeddings = textEmbed(asian$text, keep_token_embeddings = FALSE)
asian_embeds <- as.matrix(asian_embeddings[['texts']]$text)

hispanic_embeddings = textEmbed(hispanic$text, keep_token_embeddings = FALSE)
hispanic_embeds <- as.matrix(hispanic_embeddings[['texts']]$text)

white_embeddings = textEmbed(white$text, keep_token_embeddings = FALSE)
white_embeds <- as.matrix(white_embeddings[['texts']]$text)

# Calculate Similarity between Sentence Embeddings within Groups ---------------

black_cosines <- sim2(black_embeds)
bmc <- black_cosines[upper.tri(black_cosines)]

asian_cosines <- sim2(asian_embeds)
amc <- asian_cosines[upper.tri(asian_cosines)]

hispanic_cosines <- sim2(hispanic_embeds)
hmc <- hispanic_cosines[upper.tri(hispanic_cosines)]

white_cosines <- sim2(white_embeds)
wmc <- white_cosines[upper.tri(white_cosines)]

# Calculate the Mean -----------------------------------------------------------

black_mean <- mean(bmc)
asian_mean <- mean(amc)
hispanic_mean <- mean(hmc)
white_mean <- mean(wmc)

# Tally the number of stories coded as 1
stories %>% group_by(race) %>% summarise(count = sum(hardship))
