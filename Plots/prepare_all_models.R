
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 19 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Load all data frames ---------------------------------------------------------

# Note that all models are saved as 'cosine_std' in the BERT and RoBERTa models
# We rename them to clarify which model the data frame is from

load('../BERT-2/bert2.RData')
bert2 <- cosine_std

load('../BERT-3/bert3.RData')
bert3 <- cosine_std

load('../RoBERTa-2/roberta2.RData')
roberta2 <- cosine_std

load('../RoBERTa-3/roberta3.RData')
roberta3 <- cosine_std

load('../Sentence-BERT/sentence_bert.RData')

# Remove all objects other than the data frames
rm(list = setdiff(ls(), c("bert2", "bert3", "roberta2", "roberta3", 
                          "mpnetbase", "distilroberta", "minilm")))

# Mutate gender column ---------------------------------------------------------

mutate_gender <- function(x){
  x <- x %>% 
    mutate(gender = case_when(gender == "Man" ~ "Men", gender == "Woman" ~ "Women")) %>%
    mutate(gender = as.factor(gender))
  return(x)
}

bert2 <- mutate_gender(bert2)
bert3 <- mutate_gender(bert3)
roberta2 <- mutate_gender(roberta2)
roberta3 <- mutate_gender(roberta3)
mpnetbase <- mutate_gender(mpnetbase)
distilroberta <- mutate_gender(distilroberta)
minilm <- mutate_gender(minilm)

# Save all models as an .RData file --------------------------------------------

save.image('all_models.RData')

