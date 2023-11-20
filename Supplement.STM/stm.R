
## Anonymous
# Large Language Models Portray Socially Subordinate Groups as More Homogeneous, 
# Consistent with a Bias Observed in Humans

## Script date: 19 Nov 2023

# Install and/or load packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("stm")){install.packages("stm", dependencies = TRUE); require("stm")}
if(!require("reshape2")){install.packages("reshape2", dependencies = TRUE); require("reshape2")}
if(!require("ggsci")){install.packages("ggsci", dependencies = TRUE); require("ggsci")}

# Load generated text ----------------------------------------------------------

data <- read.csv('../Data/generated_text_final.csv')

# Prepare text for the STM -----------------------------------------------------

processed <- textProcessor(data$text, 
                           metadata = data, 
                           customstopwords = c("african", "asian", "hispanic", 
                                               "white", "american", "man", "woman"))

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta

out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta, 
                     lower.thresh = 10)

# Determine optimal number of topics -------------------------------------------

K <- c(5, 10, 15, 20)

searchK(docs, 
        vocab,
        K, 
        prevalence = ~format + race + gender, 
        data = meta, 
        cores = 4)

# Fit the STM model using K = 15 -----------------------------------------------

fit <- stm(documents = out$documents, 
           vocab = out$vocab, 
           K = 15, 
           prevalence = ~format + race + gender, 
           data = out$meta, 
           init.type = "Spectral", 
           seed = 1048596)

# Save as .RData file ----------------------------------------------------------

# save.image('stm.RData')
load('stm.RData')
labelTopics(fit)

pdf(file = "Figures/frex_words.pdf", width = 10, height = 6) 
plot.STM(fit, n = 5)
dev.off()

# Create a dataframe of thetas -------------------------------------------------

theta.values <- fit$theta
theta.with.race <- data.frame(theta.values, race = meta$race)

theta.with.race.long <- theta.with.race %>%
  pivot_longer(cols = starts_with("X"), 
               names_to = "topic", 
               values_to = "theta") %>% 
  mutate(topic = factor(topic, 
                        levels = unique(topic[order(as.numeric(str_extract(topic, "\\d+")))])))

theta.long.plot.df <- theta.with.race.long %>% 
  mutate(race = recode(race,
                       "Asian" = "Asian\nAmericans",
                       "African" = "African\nAmericans",
                       "Hispanic" = "Hispanic\nAmericans",
                       "White" = "White\nAmericans"))

# Visualize the theta values by race/ethnicity ---------------------------------

ggplot(theta.long.plot.df, aes(x = topic, y = theta, fill = topic)) +
  geom_violin() +
  facet_grid(race ~ topic, scales="free", space="free") +
  theme_minimal() +
  labs(title="Distribution of Topics by Racial/Ethnic Group",
       y="Theta Values",
       x="Topics") +
  theme(axis.text.x=element_text(angle=45),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

# ggsave("Figures/stm_violin_plot.pdf", width = 6, height = 6, dpi = "retina")

# Identify topic of the text and make it a new column --------------------------

max_index_list <- apply(theta.with.race[,1:15], 1, function(row) which.max(row))
data.with.topic <- data %>% mutate(topic = max_index_list)

# Compare proportions of texts about topic 1 or 10 -----------------------------

data.with.topic %>% group_by(race) %>% summarize(n = n())
data.with.topic %>% filter(topic == 1 | topic == 10) %>% group_by(race) %>% summarize(n = n())

prop.test(x = c(5442, 464), n = c(13000, 13000), alternative = "greater")$statistic
prop.test(x = c(3400, 464), n = c(13000, 13000), alternative = "greater")$statistic
prop.test(x = c(2425, 464), n = c(13000, 13000), alternative = "greater")$statistic

# Calculate the proportion of texts in the top 1 to 4 topics by group ----------

topic_frequencies <- data.with.topic %>% group_by(race, topic) %>%
  summarise(count = n(), .groups = 'drop') %>%
  group_by(race) %>%
  mutate(probability = count / sum(count))

n = 5  # Replace 5 with the number of top topics you want

top_topics <- topic_frequencies %>%
  group_by(race) %>%
  arrange(desc(probability)) %>%
  slice_head(n = n)

top_topics %>%
  group_by(race) %>%
  summarise(sum_probability = sum(probability))

# Visualize the theta values by race/ethnicity ---------------------------------

data.with.topic <- data.with.topic %>% 
  mutate(race = case_when(race == "African" ~ "African Americans", 
                          race == "Asian" ~ "Asian Americans", 
                          race == "Hispanic" ~ "Hispanic Americans", 
                          race == "White" ~ "White Americans")) %>%
  mutate(race = as.factor(race)) %>%
  mutate(race = relevel(race, ref = "White Americans"))
 
ggplot(data.with.topic, aes(x = topic, fill = race)) +
  geom_density(alpha = 0.6) + 
  theme_minimal() +
  scale_x_discrete(limits = levels(factor(data.with.topic$topic))) + 
  labs(title="Distribution of Topics by Racial/Ethnic Group",
       y="Proportion of Texts",
       x="Topics", 
       fill = "Race/Ethnicity") +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank(), 
        title = element_text(size = 15), 
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.text = element_text(size = 15)) + 
  scale_fill_aaas()

ggsave("Figures/stm_topic_distributions.pdf", width = 10, height = 6, dpi = "retina")

# Perform F tests to compare variances -----------------------------------------

african.topics <- data.with.topic %>% filter(race == "African Americans") %>% pull(topic)
asian.topics <- data.with.topic %>% filter(race == "Asian Americans") %>% pull(topic)
hispanic.topics <- data.with.topic %>% filter(race == "Hispanic Americans") %>% pull(topic)
white.topics <- data.with.topic %>% filter(race == "White Americans") %>% pull(topic)

var.test(african.topics, white.topics, alternative = "greater")
var.test(asian.topics, white.topics, alternative = "greater")
var.test(hispanic.topics, white.topics, alternative = "greater")

# Compare homogeneity within Topic 1 and 10 texts ------------------------------

topic.1 <- data.with.topic %>% filter(topic == 1)
topic.1 %>% group_by(race) %>% summarize(n = n())
write.csv(topic.1, "topic_1.csv", row.names = FALSE)
