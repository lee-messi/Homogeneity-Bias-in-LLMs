
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 4 Aug 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("psych")){install.packages("psych", dependencies = TRUE); require("psych")}

# Import ChatGPT-generated Text ------------------------------------------------

texts.1 <- read.csv('Original/generated_text_1.csv')
texts.2 <- read.csv('Original/generated_text_2.csv')
texts.3 <- read.csv('Original/generated_text_3.csv')
texts.4 <- read.csv('Original/generated_text_4.csv')

# Merge the Four Data frames ---------------------------------------------------

merged.text <- bind_rows(texts.1, texts.2, texts.3, texts.4)

# Inspect Merged Data frame to Identify Problematic Text Generations -----------

# Those texts containing the expressions "chatbot", "language model", 
# "As a computer program", "I cannot", "I can't generate", "won't be able to generate"

ordinary.text <- merged.text %>% 
  filter(!grepl("chatbot", text)) %>% 
  filter(!grepl("language model", text)) %>% 
  filter(!grepl("As a computer program", text)) %>% 
  filter(!grepl("I cannot", text)) %>%
  filter(!grepl("I can't generate", text)) %>%
  filter(!grepl("won't be able to generate", text))

problematic.text <- merged.text %>% 
  filter(grepl("chatbot", text)|
           grepl("language model", text)|
           grepl("As a computer program", text)|
           grepl("I cannot", text)|
           grepl("I can't generate", text)|
           grepl("won't be able to generate", text))

# Identify and tabulate the vignettes that need filling-in
table(problematic.text %>% select(c('format', 'race', 'gender')))

# Vignettes that need filling-in are the following -----------------------------

#   - 4 funny story about, African, man
#   - 29 horror story about, African, man
#   - 2 tragic story about, African, man
#   - 1 funny story about, Asian, man
#   - 1 horror story about, Asian, man
#   - 1 horror story about, White, man

#   - 3 funny story about, African, woman
#   - 1 horror story about, African, woman
#   - 4 funny story about, Asian, woman
#   - 1 funny story about, Hispanic, woman
#   - 1 tragic story about, Hispanic, woman
#   - 1 character description of, White, woman
#   - 1 horror story about, White, woman

# Inspect supplemented texts to identify problematic text generations ----------

extra.text.1 <- read.csv('Extra/extra_text_1.csv')

extra.text.1.ordinary <- extra.text.1 %>% 
  filter(!grepl("chatbot", text)) %>% 
  filter(!grepl("language model", text)) %>% 
  filter(!grepl("As a computer program", text)) %>% 
  filter(!grepl("I cannot", text)) %>%
  filter(!grepl("I can't generate", text)) %>%
  filter(!grepl("won't be able to generate", text))

extra.text.1.problematic <- extra.text.1 %>% 
  filter(grepl("chatbot", text)|
           grepl("language model", text)|
           grepl("As a computer program", text)|
           grepl("I cannot", text)|
           grepl("I can't generate", text)|
           grepl("won't be able to generate", text))

# Tabulate the vignettes that need filling-in
table(extra.text.1.problematic %>% select(c('format', 'race', 'gender')))

# Vignettes that need filling-in are the following: 
#   - 3 horror story about, African, man

extra.text.2 <- read.csv('Extra/extra_text_2.csv')

extra.text.2.ordinary <- extra.text.2 %>% 
  filter(!grepl("chatbot", text)) %>% 
  filter(!grepl("language model", text)) %>% 
  filter(!grepl("As a computer program", text)) %>% 
  filter(!grepl("I cannot", text)) %>%
  filter(!grepl("I can't generate", text)) %>%
  filter(!grepl("won't be able to generate", text))

extra.text.2.problematic <- extra.text.2 %>% 
  filter(grepl("chatbot", text)|
           grepl("language model", text)|
           grepl("As a computer program", text)|
           grepl("I cannot", text)|
           grepl("I can't generate", text)|
           grepl("won't be able to generate", text))

# Tabulate the vignettes that need filling-in
table(extra.text.2.problematic %>% select(c('format', 'race', 'gender')))

# Write the Data frame as a .csv File ------------------------------------------

final.text <- bind_rows(ordinary.text, 
                        extra.text.1.ordinary, 
                        extra.text.2.ordinary)

write.csv(final.text, 
          "generated_text_final.csv", 
          row.names = FALSE)

# Inspect length of ChatGPT-generated text -------------------------------------

length.df <- final.text %>% 
  rowwise() %>%
  mutate(textlen = length(unlist(strsplit(text, " "))))

describe(length.df$textlen)
