library(ggplot2)
library(lubridate)
library(readr)
library(scales)
library(stringr)
library(tidyr)
library(tidytext)
library(tidyverse)

# 0. Raw data
data0 <- read_csv("all_CM_restaurants_Columbus.csv")

# 1. Remove columns of NA's, clean up date format
data1 <- data0 %>% select(1:7)
ymd <- ymd(data1$date_published)
mdy <- mdy(data1$date_published)
ymd[is.na(ymd)] <- mdy[is.na(ymd)]
data1$date_published <- ymd

# 2. Normalize table, split text strings
data_restaurant <- data1 %>%
  select(1:3) %>%
  unique()
data2 <- data1 %>%
  select(1, 4:7) %>%
  unnest_tokens(review_word, review_text)

# 3. Remove common words and rare words
data("stop_words")
colnames(stop_words) <- c("review_word", "lexicon")
common_words <- stop_words %>%
  filter(lexicon %in% c("snowball", "onix")) %>%
  select(review_word) %>%
  unique()
data3 <- data2 %>%
  anti_join(common_words, by = "review_word") %>%
  filter(is.na(as.numeric(review_word))) %>%
  select(review_word, rating_value)
word_freq <- data3 %>%
  count(review_word, sort = TRUE) %>%
  filter(n >= 50)
data3 <- data3 %>%
  right_join(word_freq, by = "review_word")

# 3.1 Visualize frequency based on data3
word_freq %>%
  head(10) %>%
  ggplot(aes(x = reorder(review_word, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = NULL)

# 4. Categorize words by ratings and prepare
word_by_rating <- c()
for (i in 1:5) {
  word_by_rating[[i]] <- data3 %>%
    filter(rating_value == i) %>%
    select(review_word)
}
data4 <- data3 %>%
  mutate(rating_factor = as.factor(ifelse(
    rating_value == 5, "high",
    ifelse(rating_value >= 3, "med", "low")
  ))) %>%
  count(review_word, rating_factor) %>%
  group_by(rating_factor) %>%
  mutate(prop = n / sum(n)) %>%
  select(-n)

# 4.1 Visualize relationship between words and ratings
data4 %>%
  spread(rating_factor, prop) %>%
  gather(rating_factor, prop, med, low) %>%
  ggplot(aes(x = prop, y = high, color = log(high / prop))) +
    geom_abline(color = "gray40", lty = 2) +
    geom_jitter(alpha = 0.1, size = 2.5, width = 0.3, height = 0.3) +
    geom_text(aes(label = review_word), check_overlap = TRUE, vjust = 1.5) +
    scale_x_log10(labels = percent_format()) +
    scale_y_log10(labels = percent_format()) +
    scale_color_gradient2(limits = c(-3, 3)) +
    facet_wrap(~rating_factor, ncol = 2) +
    theme(legend.position = "bottom") +
    labs(y = "High", x = NULL)

# 5. Models and hypotheses on previous relationship
# e.g. "die" and "amazing" for high, "average" and "disappointment" for low and med
--TODO

