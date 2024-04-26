
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 18 Jan 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("ggplot2")){install.packages("ggplot2", dependencies = TRUE); require("ggplot2")}
if(!require("Cairo")){install.packages("Cairo", dependencies = TRUE); require("Cairo")} # Install XQuartz for this

# Initilize the text package ---------------------------------------------------

# textrpp_install()
# textrpp_initialize(save_profile = TRUE)
# ?textrpp_initialize

# Define preprocessing steps ---------------------------------------------------

simple_prep = function(x) {
  x = str_to_lower(x) # make text lower case
  x = str_replace_all(x, "[^[:alnum:]]", " ") # remove non-alphanumeric symbols
  x = str_replace_all(x, "\\s+", " ") # collapse multiple spaces
}

# Load text --------------------------------------------------------------------

stories <- read.csv('Original/suppression_study_2.csv') %>%
  mutate(text = simple_prep(text))

# Count the number of texts containing adversity-related keywords
keywords <- c("adversity", "barrier")

stories %>% filter(race %in% c("African", "Asian", "Hispanic")) %>% 
  mutate(contains = str_detect(text, paste(keywords, collapse = "|"))) %>% 
  pull(contains) %>% sum()

# Total number of texts about African, Asian, and Hispanic Americans
stories %>% filter(race %in% c("African", "Asian", "Hispanic")) %>% nrow()

# Separate stories by gender and racial/ethnic group  --------------------------

black_males <- stories %>% 
  filter(gender == "male" & race == "African") %>% 
  mutate(gender = "Man") %>% mutate(race = "African Americans")

black_females <- stories %>% 
  filter(gender == "female" & race == "African") %>% 
  mutate(gender = "Woman") %>% mutate(race = "African Americans")

asian_males <- stories %>% 
  filter(gender == "male" & race == "Asian") %>% 
  mutate(gender = "Man") %>% mutate(race = "Asian Americans")

asian_females <- stories %>% 
  filter(gender == "female" & race == "Asian") %>% 
  mutate(gender = "Woman") %>% mutate(race = "Asian Americans")

hispanic_males <- stories %>% 
  filter(gender == "male" & race == "Hispanic") %>% 
  mutate(gender = "Man") %>% mutate(race = "Hispanic Americans")

hispanic_females <- stories %>% 
  filter(gender == "female" & race == "Hispanic") %>% 
  mutate(gender = "Woman") %>% mutate(race = "Hispanic Americans")

white_males <- stories %>% 
  filter(gender == "male" & race == "White") %>% 
  mutate(gender = "Man") %>% mutate(race = "White Americans")

white_females <- stories %>% 
  filter(gender == "female" & race == "White") %>% 
  mutate(gender = "Woman") %>% mutate(race = "White Americans")

# Calculate Cosine Similarity between sentence embeddings ----------------------

bm.embeddings <- textEmbed(black_males$text, keep_token_embeddings = FALSE)
bm.embeds <- as.matrix(bm.embeddings[['texts']]$text)
bm.cosines <- sim2(bm.embeds)
bmc <- bm.cosines[upper.tri(bm.cosines)]

bf.embeddings <- textEmbed(black_females$text, keep_token_embeddings = FALSE)
bf.embeds <- as.matrix(bf.embeddings[['texts']]$text)
bf.cosines <- sim2(bf.embeds)
bfc <- bf.cosines[upper.tri(bf.cosines)]

am.embeddings <- textEmbed(asian_males$text, keep_token_embeddings = FALSE)
am.embeds <- as.matrix(am.embeddings[['texts']]$text)
am.cosines <- sim2(am.embeds)
amc <- am.cosines[upper.tri(am.cosines)]

af.embeddings <- textEmbed(asian_females$text, keep_token_embeddings = FALSE)
af.embeds <- as.matrix(af.embeddings[['texts']]$text)
af.cosines <- sim2(af.embeds)
afc <- af.cosines[upper.tri(af.cosines)]

hm.embeddings <- textEmbed(hispanic_males$text, keep_token_embeddings = FALSE)
hm.embeds <- as.matrix(hm.embeddings[['texts']]$text)
hm.cosines <- sim2(hm.embeds)
hmc <- hm.cosines[upper.tri(hm.cosines)]

hf.embeddings <- textEmbed(hispanic_females$text, keep_token_embeddings = FALSE)
hf.embeds <- as.matrix(hf.embeddings[['texts']]$text)
hf.cosines <- sim2(hf.embeds)
hfc <- hf.cosines[upper.tri(hf.cosines)]

wm.embeddings <- textEmbed(white_males$text, keep_token_embeddings = FALSE)
wm.embeds <- as.matrix(wm.embeddings[['texts']]$text)
wm.cosines <- sim2(wm.embeds)
wmc <- wm.cosines[upper.tri(wm.cosines)]

wf.embeddings <- textEmbed(white_females$text, keep_token_embeddings = FALSE)
wf.embeds <- as.matrix(wf.embeddings[['texts']]$text)
wf.cosines <- sim2(wf.embeds)
wfc <- wf.cosines[upper.tri(wf.cosines)]

# Create a data frame with all cosine similarity measurements ------------------

group.length <- length(unlist(bmc, recursive = FALSE))

race_list <- rep(c('African Americans', 'Asian Americans', 
                   'Hispanic Americans', 'White Americans'), each = group.length * 2)

gender_list <- rep(rep(c("Man", "Woman"), each = group.length), 4)

cosine_list <- c(unlist(bmc, recursive = FALSE), unlist(bfc, recursive = FALSE),
                 unlist(amc, recursive = FALSE), unlist(afc, recursive = FALSE),
                 unlist(hmc, recursive = FALSE), unlist(hfc, recursive = FALSE),
                 unlist(wmc, recursive = FALSE), unlist(wfc, recursive = FALSE))

cosine_df <- data.frame(race_list, gender_list, cosine_list)
colnames(cosine_df) <- c('race', 'gender', 'cosine')

cosine_df <- cosine_df %>% 
  mutate(gender = as.factor(gender)) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans")) %>%
  mutate(gender = relevel(gender, ref = "Man"))

# Save as .RData ---------------------------------------------------------------

# save.image("suppression_study_2_cosines.RData")
load("suppression_study_2_cosines.RData")

# Compare cosine similarity between racial/ethnic groups -----------------------

t.test(cosine_df %>% filter(race == "African Americans") %>% pull(cosine), 
       cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
       alternative = "greater")

t.test(cosine_df %>% filter(race == "Asian Americans") %>% pull(cosine), 
       cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
       alternative = "greater")

t.test(cosine_df %>% filter(race == "Hispanic Americans") %>% pull(cosine), 
       cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
       alternative = "greater")

# Plot the effect of race/ethnicity --------------------------------------------

# Standardize cosine similarity before fitting mixed-effects models
cosine_std <- cosine_df %>% 
  mutate(across(where(is.numeric), scale))

ggplot(cosine_std, aes(x = race, y = cosine, color = race)) + 
  geom_hline(yintercept = 0.0, linetype = "dashed") + 
  stat_summary(fun = mean, geom = "point", size = 2, 
               position = position_dodge(0.5)) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2, 
               position = position_dodge(0.5)) +
  theme_bw() + 
  theme(legend.position = "none",
        axis.title.x = element_blank(), 
        strip.text.x = element_blank(),
        axis.title.y = element_text(size = 10)) + 
  labs(x = "Racial/Ethnic Groups", 
       y = "Standardized Cosine Similarity", 
       color = "Racial/Ethnic Groups") + 
  coord_cartesian(ylim = c(-0.60, 0.60)) +
  scale_color_aaas()

ggsave("Figures/suppression_study_2_race.pdf", width = 6, height = 2.5, 
       dpi = "retina", device = cairo_pdf)
