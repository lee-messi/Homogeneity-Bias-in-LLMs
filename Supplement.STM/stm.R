
## Anonymous
# The Effect of Group Status on the Variability of Group Representations in LLM-generated Text

## Script date: 29 Oct 2023

# Install and/or Load Packages -------------------------------------------------

if(!require("tidyverse")){install.packages("tidyverse", dependencies = TRUE); require("tidyverse")}
if(!require("stm")){install.packages("stm", dependencies = TRUE); require("stm")}
if(!require("reshape2")){install.packages("reshape2", dependencies = TRUE); require("reshape2")}

# Load Generated Text ----------------------------------------------------------

data <- read.csv('../Data/generated_text_final.csv')

# Prepare text for STM ---------------------------------------------------------

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

save.image('stm.RData')
labelTopics(fit)
plot.STM(fit, n = 5)

# Create a Dataframe of Thetas -------------------------------------------------

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

# Visualize the Theta Values by Racial/Ethnic Group ----------------------------

ggplot(theta.long.plot.df, aes(x = topic, y = theta, fill = topic)) +
  geom_violin() +
  facet_grid(race ~ topic, scales="free", space="free") +
  theme_minimal() +
  labs(title="Distribution of Topics by Racial/Ethnic Groups",
       y="Theta Values",
       x="Topics") +
  theme(axis.text.x=element_text(angle=45),
        plot.title = element_text(hjust = 0.5),
        legend.position="none")

ggsave("stm_violin_plot.png", width = 6, height = 6, dpi = "retina")

# Store the theta values as individual lists -----------------------------------

african.thetas <- theta.with.race.long %>% 
  filter(race == "African") %>%
  pull(theta)

asian.thetas <- theta.with.race.long %>% 
  filter(race == "Asian") %>%
  pull(theta)

hispanic.thetas <- theta.with.race.long %>%
  filter(race == "Hispanic") %>%
  pull(theta)

white.thetas <- theta.with.race.long %>%
  filter(race == "White") %>%
  pull(theta)

# Perform F tests to compare variances -----------------------------------------

var.test(white.thetas, african.thetas, alternative = "greater")
var.test(white.thetas, asian.thetas, alternative = "greater")
var.test(white.thetas, hispanic.thetas, alternative = "greater")

