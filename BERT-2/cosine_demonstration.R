
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 7 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}

# Define Preprocessing Steps ---------------------------------------------------

simple_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load Stories -----------------------------------------------------------------

setwd('../Data')

df = read.csv('generated_text_final.csv') %>%
  mutate(clean_text = simple_prep(text)) %>%
  mutate(clean_text = gsub('"', '', clean_text)) %>%
  mutate(clean_text = gsub(',', '', clean_text))

df_subset = df %>% 
  filter(race == "White") %>%
  filter(gender == "man") %>%
  filter(format == "story about")

# Calculate Cosine Similarity between Sentence Embeddings ----------------------

set.seed(1048596)

# Randomly sample 20 stories
sample_row_1 <- sample(c(1:nrow(df_subset)), 20, replace = FALSE)
sampled_stories_1 <- df_subset[sample_row_1, ]

# Randomly sample 20 stories
sample_row_2 <- sample(c(1:nrow(df_subset)), 20, replace = FALSE)
sampled_stories_2 <- df_subset[sample_row_2, ]

sampled_df <- data.frame(sampled_stories_1 %>% select(c('text', 'clean_text')), 
                         sampled_stories_2 %>% select(c('text', 'clean_text')))
colnames(sampled_df) <- c('sent1.pre', 'sent1.post', 'sent2.pre', 'sent2.post')

# Generate sentence embeddings for the randomly selected stories
sent1_embeddings = textEmbed(sampled_df$sent1.post, keep_token_embeddings = FALSE)
sent1_embeds <- as.matrix(sent1_embeddings[['texts']]$text)

# Generate sentence embeddings for the randomly selected stories
sent2_embeddings = textEmbed(sampled_df$sent2.post, keep_token_embeddings = FALSE)
sent2_embeds <- as.matrix(sent2_embeddings[['texts']]$text)

cosines <- psim2(sent1_embeds, sent2_embeds)

sampled_df <- sampled_df %>%
  mutate(cosine = cosines) %>%
  select(c("sent1.pre", "sent2.pre", "cosine")) %>%
  arrange(desc(cosine))

sampled_df
