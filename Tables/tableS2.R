
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 19 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("effectsize")){install.packages("effectsize", dependencies = TRUE); require("effectsize")}
if(!require("effsize")){install.packages("effsize", dependencies = TRUE); require("effsize")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}

# Initialize the text package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define function to perform preprocessing -------------------------------------

original_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load generated text ----------------------------------------------------------

setwd('Pilot Data')

data <- read.csv('pilot_data.csv') %>%
  mutate(clean_text = original_prep(text)) %>%
  mutate(clean_text = gsub('"', '', clean_text)) %>%
  mutate(clean_text = gsub(',', '', clean_text))

# Sample three stories that are coded as 1 and three coded as 0
sampled_data <- data %>% group_by(hardship) %>% sample_n(3)

# Separate texts by gender and race/ethnicity ----------------------------------

black <- data %>% filter(race == "African")
asian <- data %>% filter(race == "Asian")
hispanic <- data %>% filter(race == "Hispanic")
white <- data %>% filter(race == "White")

# Create sentence embeddings for the generated texts ---------------------------

black_embeddings = textEmbed(black$text, keep_token_embeddings = FALSE)
black_embeds <- as.matrix(black_embeddings[['texts']]$text)

asian_embeddings = textEmbed(asian$text, keep_token_embeddings = FALSE)
asian_embeds <- as.matrix(asian_embeddings[['texts']]$text)

hispanic_embeddings = textEmbed(hispanic$text, keep_token_embeddings = FALSE)
hispanic_embeds <- as.matrix(hispanic_embeddings[['texts']]$text)

white_embeddings = textEmbed(white$text, keep_token_embeddings = FALSE)
white_embeds <- as.matrix(white_embeddings[['texts']]$text)

# Calculate similarity between sentence embeddings within groups ---------------

black_cosines <- sim2(black_embeds)
bmc <- black_cosines[upper.tri(black_cosines)]

asian_cosines <- sim2(asian_embeds)
amc <- asian_cosines[upper.tri(asian_cosines)]

hispanic_cosines <- sim2(hispanic_embeds)
hmc <- hispanic_cosines[upper.tri(hispanic_cosines)]

white_cosines <- sim2(white_embeds)
wmc <- white_cosines[upper.tri(white_cosines)]

# Calculate the mean -----------------------------------------------------------

black_mean <- mean(bmc)
asian_mean <- mean(amc)
hispanic_mean <- mean(hmc)
white_mean <- mean(wmc)

# Tally the number of pairs
stories_subset <- stories %>% select(c(race, hardship))

black_ss <- stories_subset %>% filter(race == "African") %>% select(hardship)
asian_ss <- stories_subset %>% filter(race == "Asian") %>% select(hardship)
hispanic_ss <- stories_subset %>% filter(race == "Hispanic") %>% select(hardship)
white_ss <- stories_subset %>% filter(race == "White") %>% select(hardship)

black_pairs <- combn(black_ss$hardship, 2)
black_df <- data.frame(Var1 = black_pairs[1, ], Var2 = black_pairs[2, ])
black_df %>% 
  mutate(class = case_when(
    Var1 == 0 & Var2 == 0 ~ 0,
    Var1 == 1 & Var2 == 1 ~ 1,
    Var1 == 0 & Var2 == 1 ~ 2,
    Var1 == 1 & Var2 == 0 ~ 2)) %>%
  group_by(class) %>%
  summarize(n = n())

asian_pairs <- combn(asian_ss$hardship, 2)
asian_df <- data.frame(Var1 = asian_pairs[1, ], Var2 = asian_pairs[2, ])
asian_df %>% 
  mutate(class = case_when(
    Var1 == 0 & Var2 == 0 ~ 0,
    Var1 == 1 & Var2 == 1 ~ 1,
    Var1 == 0 & Var2 == 1 ~ 2,
    Var1 == 1 & Var2 == 0 ~ 2)) %>%
  group_by(class) %>%
  summarize(n = n())

hispanic_pairs <- combn(hispanic_ss$hardship, 2)
hispanic_df <- data.frame(Var1 = hispanic_pairs[1, ], Var2 = hispanic_pairs[2, ])
hispanic_df %>% 
  mutate(class = case_when(
    Var1 == 0 & Var2 == 0 ~ 0,
    Var1 == 1 & Var2 == 1 ~ 1,
    Var1 == 0 & Var2 == 1 ~ 2,
    Var1 == 1 & Var2 == 0 ~ 2)) %>%
  group_by(class) %>%
  summarize(n = n())

white_pairs <- combn(white_ss$hardship, 2)
white_df <- data.frame(Var1 = white_pairs[1, ], Var2 = white_pairs[2, ])
white_df %>% 
  mutate(class = case_when(
    Var1 == 0 & Var2 == 0 ~ 0,
    Var1 == 1 & Var2 == 1 ~ 1,
    Var1 == 0 & Var2 == 1 ~ 2,
    Var1 == 1 & Var2 == 0 ~ 2)) %>%
  group_by(class) %>%
  summarize(n = n())
