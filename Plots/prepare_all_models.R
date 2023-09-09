## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 9 Sept 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}

# Load All Models --------------------------------------------------------------

# Note that all models are saved as 'cosine_std' in all .RData files
# We rename them to clarify which model the data set is from

setwd('../Main')
load('main_bert_cosine.RData')
main_cosine_std <- cosine_std

setwd('../S1.Layer')
load('s1_layer_cosine.RData')
s1_layer_cosine_std <- cosine_std

setwd('../S2.RoBERTa')
load('s2_roberta_cosine.RData')
s2_roberta_cosine_std <- cosine_std

setwd('../S3.Layer.RoBERTa')
load('s3_layer_roberta_cosine.RData')
s3_roberta_layer_cosine_std <- cosine_std

setwd('../S4.Sentence-BERT')
load('sentence_bert_models.RData')
s4_mpnetbase_cosine_std <- mpnetbase
s4_distilroberta_cosine_std <- distilroberta
s4_allminilm_cosine_std <- allminilm

# Mutate Gender Column ---------------------------------------------------------

mutate_gender <- function(x){
  x <- x %>% 
    mutate(gender = case_when(gender == "Man" ~ "Men", gender == "Woman" ~ "Women")) %>%
    mutate(gender = as.factor(gender))
  return(x)
}

main_cosine_std <- mutate_gender(main_cosine_std)
s1_layer_cosine_std <- mutate_gender(s1_layer_cosine_std)
s2_roberta_cosine_std <- mutate_gender(s2_roberta_cosine_std)
s3_roberta_layer_cosine_std <- mutate_gender(s3_roberta_layer_cosine_std)
s4_mpnetbase_cosine_std <- mutate_gender(s4_mpnetbase_cosine_std)
s4_distilroberta_cosine_std <- mutate_gender(s4_distilroberta_cosine_std)
s4_allminilm_cosine_std <- mutate_gender(s4_allminilm_cosine_std)

unique(s4_allminilm_cosine_std$gender)
unique(main_cosine_std$gender)

setwd('../Plots')
save.image('all_models.RData')
