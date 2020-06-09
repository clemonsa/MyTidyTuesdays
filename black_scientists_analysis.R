library(tidyverse)
library(ggplot2)

scientists <- read_csv("./science_comp.csv")

occupations <- 
  # replaces super-long unseparated occupation title
  str_replace(scientists$occupation_s,"ZoologistexplorerAnthropologist", "Zoologist; Explorer; Anthroplogist") %>%
  # Separate occupations
  str_split(";") %>% 
  unlist() %>%
  # make occupations Title Case
  str_to_title() %>%
  # remove spaces near beginning
  str_remove("^\\s") %>%
  # categorize all "____ Researcher" in same category
  str_replace(".* Researcher", "Researcher") %>% 
  # removes additional text
  str_remove(fixed("[Citation Needed]")) %>%
  str_remove("Woods Hole Marine Biology Institute ") %>%
  # removes "And "
  str_remove("\\bAnd\\b\\s+")
  
# Tibble of most common occupations in descending order
most_common <- tibble(
  occupation = occupations) %>%
  group_by(occupation) %>% 
  summarise(n = n()) %>% 
  distinct() %>% 
  arrange(desc(n))

# ggplot
p <- ggplot(tibble(
  occupation = occupations))

# bar graph
p + 
  geom_bar(aes(x=occupation, fill = occupation), show.legend = F) +
  coord_flip() +
  ggtitle("Most Common Occupations of African-American Scientists") +
  xlab("Occupation") + ylab("Number of African-American Scientists")