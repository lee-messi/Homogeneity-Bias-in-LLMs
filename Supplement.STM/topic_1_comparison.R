
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 25 Apr 2024

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("text")){install.packages("text", dependencies = TRUE); require("text")}
if(!require("text2vec")){install.packages("text2vec", dependencies = TRUE); require("text2vec")}
if(!require("lme4")){install.packages("lme4", dependencies = TRUE); require("lme4")}
if(!require("lmerTest")){install.packages("lmerTest", dependencies = TRUE); require("lmerTest")}
if(!require("afex")){install.packages("afex", dependencies = TRUE); require("afex")}
if(!require("emmeans")){install.packages("emmeans", dependencies = TRUE); require("emmeans")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}
if(!require("Hmisc")){install.packages("Hmisc", dependencies = TRUE); require("Hmisc")}
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

# Load generated text ----------------------------------------------------------

topic.1 <- read.csv('topic_1.csv') %>%
  mutate(text = simple_prep(text))

# Separate stories by gender, race/ethnicity, and text format ------------------

black_males <- topic.1 %>% 
  filter(gender == "Men" & race == "African Americans") %>% 
  group_split(format)

black_females <- topic.1 %>% 
  filter(gender == "Women" & race == "African Americans") %>% 
  group_split(format)

asian_males <- topic.1 %>% 
  filter(gender == "Men" & race == "Asian Americans") %>% 
  group_split(format)

asian_females <- topic.1 %>% 
  filter(gender == "Women" & race == "Asian Americans") %>% 
  group_split(format)

hispanic_males <- topic.1 %>% 
  filter(gender == "Men" & race == "Hispanic Americans") %>% 
  group_split(format)

hispanic_females <- topic.1 %>% 
  filter(gender == "Women" & race == "Hispanic Americans") %>% 
  group_split(format)

white_males <- topic.1 %>% 
  filter(gender == "Men" & race == "White Americans") %>% 
  group_split(format)

white_females <- topic.1 %>% 
  filter(gender == "Women" & race == "White Americans") %>% 
  group_split(format)

# Calculate cosine similarity between sentence embeddings for each text format -

bmc <- bfc <- amc <- afc <- hmc <- hfc <- wmc <- wfc <- list()

for (i in black_males){
  bm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bm.embeds <- as.matrix(bm.embeddings[['texts']]$text)
  bm.cosines <- sim2(bm.embeds)
  bmc <- append(bmc, list(bm.cosines[upper.tri(bm.cosines)]))
}

for (i in black_females){
  bf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  bf.embeds <- as.matrix(bf.embeddings[['texts']]$text)
  bf.cosines <- sim2(bf.embeds)
  bfc <- append(bfc, list(bf.cosines[upper.tri(bf.cosines)]))
}

for (i in asian_males){
  am.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  am.embeds <- as.matrix(am.embeddings[['texts']]$text)
  am.cosines <- sim2(am.embeds)
  amc <- append(amc, list(am.cosines[upper.tri(am.cosines)]))
}

for (i in asian_females){
  af.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  af.embeds <- as.matrix(af.embeddings[['texts']]$text)
  af.cosines <- sim2(af.embeds)
  afc <- append(afc, list(af.cosines[upper.tri(af.cosines)]))
}

for (i in hispanic_males){
  hm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hm.embeds <- as.matrix(hm.embeddings[['texts']]$text)
  hm.cosines <- sim2(hm.embeds)
  hmc <- append(hmc, list(hm.cosines[upper.tri(hm.cosines)]))
}

for (i in hispanic_females){
  hf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  hf.embeds <- as.matrix(hf.embeddings[['texts']]$text)
  hf.cosines <- sim2(hf.embeds)
  hfc <- append(hfc, list(hf.cosines[upper.tri(hf.cosines)]))
}

for (i in white_males){
  wm.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  wm.embeds <- as.matrix(wm.embeddings[['texts']]$text)
  wm.cosines <- sim2(wm.embeds)
  wmc <- append(wmc, list(wm.cosines[upper.tri(wm.cosines)]))
}

for (i in white_females){
  wf.embeddings <- textEmbed(i$text, keep_token_embeddings = FALSE)
  wf.embeds <- as.matrix(wf.embeddings[['texts']]$text)
  wf.cosines <- sim2(wf.embeds)
  wfc <- append(wfc, list(wf.cosines[upper.tri(wf.cosines)]))
}

gc()

# Create a data frame with all cosine similarity measurements ------------------

race_list <- c(rep("African Americans", 
                   length(c(unlist(bmc, recursive = FALSE), unlist(bfc, recursive = FALSE)))), 
               rep("Asian Americans", 
                   length(c(unlist(amc, recursive = FALSE), unlist(afc, recursive = FALSE)))), 
               rep("Hispanic Americans", 
                   length(c(unlist(hmc, recursive = FALSE), unlist(hfc, recursive = FALSE)))), 
               rep("White Americans", 
                   length(c(unlist(wmc, recursive = FALSE), unlist(wfc, recursive = FALSE)))))

gender_list <- c(rep("Men", length(unlist(bmc, recursive = FALSE))), 
                 rep("Women", length(unlist(bfc, recursive = FALSE))),
                 rep("Men", length(unlist(amc, recursive = FALSE))), 
                 rep("Women", length(unlist(afc, recursive = FALSE))),
                 rep("Men", length(unlist(hmc, recursive = FALSE))), 
                 rep("Women", length(unlist(hfc, recursive = FALSE))),
                 rep("Men", length(unlist(wmc, recursive = FALSE))), 
                 rep("Women", length(unlist(wfc, recursive = FALSE))))

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
  mutate(gender = relevel(gender, ref = "Men"))

cosine_std <- cosine_df %>% 
  mutate(across(where(is.numeric), scale))

# Compare cosine similarity between racial/ethnic groups -----------------------

bw.t <- t.test(cosine_df %>% filter(race == "African Americans") %>% pull(cosine), 
               cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
               alternative = "greater")
c(bw.t$parameter, bw.t$statistic)

aw.t <- t.test(cosine_df %>% filter(race == "Asian Americans") %>% pull(cosine), 
               cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
               alternative = "greater")
c(aw.t$parameter, aw.t$statistic)

hw.t <- t.test(cosine_df %>% filter(race == "Hispanic Americans") %>% pull(cosine), 
               cosine_df %>% filter(race == "White Americans") %>% pull(cosine), 
               alternative = "greater")
c(hw.t$parameter, hw.t$statistic)

# Save as .RData ---------------------------------------------------------------

# save.image("topic_1_cosines.RData")
load("topic_1_cosines.RData")

# Plot the effect of race/ethnicity --------------------------------------------

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
  coord_cartesian(ylim = c(-0.50, 0.30)) +
  scale_color_aaas()

ggsave("Figures/topic_1_race.pdf", width = 6, height = 2.5, 
       dpi = "retina", device = cairo_pdf)

