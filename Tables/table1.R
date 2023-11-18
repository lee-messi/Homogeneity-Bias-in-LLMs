
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 17 Nov 2023

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

# Load .RData ------------------------------------------------------------------

load('../BERT-2/bert2.RData')

# Select stories about African American males ----------------------------------

black_males <- stories %>% 
  filter(gender == "man" & race == "African" & format == "story about") %>% 
  mutate(gender = "Man") %>% mutate(race = "African Americans") %>% pull(text)

# Generate all pairwise combinations of stories about African American men -----

black_male_pairs <- t(combn(black_males, 2))
bm_df <- as.data.frame(black_male_pairs)
unique_text <- unique(black_males)

# Generate sentence embeddings for each of the stories -------------------------

# We store the sentence embeddings inside hash tables
embeddings <- hash()
  
for (sentence in unique_text) {
  embeddings[[sentence]] <- as.matrix(textEmbed(sentence, 
                                                keep_token_embeddings = FALSE)[['texts']]$text)
}
  
# Calculate cosine similarity for each row -------------------------------------

bm_df$cosine <- apply(bm_df, 1, function(row) {
  emb1 <- embeddings[[as.character(row[1])]]
  emb2 <- embeddings[[as.character(row[2])]]
  cosine_similarity(emb1, emb2)
})

# Theoretically, the cosine values in the bm_df data frame should be identical
# to that used to fit mixed effects models. Let's confirm that this is true. 

bm_cosines <- cosine_df %>% 
  filter(race == "African Americans") %>% 
  filter(gender == "Man") %>% filter(format == "story about") %>%
  pull(cosine)

describe(bm_cosines)
describe(bm_df$cosine)

# Find the standardized cosine similarity values reported in Table 1 -----------

bm_standardized_cosines <- cosine_std %>% 
  filter(race == "African Americans") %>% 
  filter(gender == "Man") %>% filter(format == "story about")

# greatest standardized cosine similarity
bm_standardized_cosines %>% arrange(cosine) %>% top_n(1)
bm_df %>% top_n(1, cosine)

# smallest standardized cosine similarity
bm_standardized_cosines %>% arrange(cosine) %>% top_n(-1)
bm_df %>% top_n(-1, cosine)

