
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 18 Nov 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("hash")){install.packages("hash", dependencies = TRUE); require("hash")}
if(!require("psych")){install.packages("psych", dependencies = TRUE); require("psych")}

# Initilize the text Package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define functions -------------------------------------------------------------

simple_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

# Load text --------------------------------------------------------------------

stories <- read.csv('../data/generated_text_final.csv') %>%
  mutate(text = simple_prep(text))

# Select stories about White American males ------------------------------------

white_males <- stories %>% 
  filter(gender == "man" & race == "White" & format == "story about") %>% 
  mutate(gender = "Man") %>% mutate(race = "White Americans") %>% pull(text)

# Generate all pairwise combinations of stories about White American men -------

white_male_pairs <- t(combn(white_males, 2))
wm_df <- as.data.frame(white_male_pairs)
unique_text <- unique(white_males)

# Generate sentence embeddings for each of the stories -------------------------

# We store the sentence embeddings inside hash tables
embeddings <- hash()

for (sentence in unique_text) {
  embeddings[[sentence]] <- as.matrix(textEmbed(sentence, 
                                                keep_token_embeddings = FALSE)[['texts']]$text)
}

# Calculate cosine similarity for each row -------------------------------------

wm_df$cosine <- apply(wm_df, 1, function(row) {
  emb1 <- embeddings[[as.character(row[1])]]
  emb2 <- embeddings[[as.character(row[2])]]
  cosine_similarity(emb1, emb2)
})

# Arrange data frame by cosine similarity --------------------------------------

set.seed(1048596)
sampled_wm_df <- wm_df %>% sample_n(10) %>% arrange(desc(cosine))
