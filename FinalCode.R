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
data1 <- data0 %>%
  select(1:7) %>%
  mutate(id = row_number())
ymd <- ymd(data1$date_published)
mdy <- mdy(data1$date_published)
ymd[is.na(ymd)] <- mdy[is.na(ymd)]
data1$date_published <- ymd

# 2. Normalize table, split text strings
data_restaurant <- data1 %>%
  select(1:3) %>%
  unique()
data2 <- data1 %>%
  select(id, rating_value, review_text) %>%
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
    geom_col(fill = "lightblue") +
    coord_flip() +
    geom_text(aes(label = n, hjust = 1)) +
    labs(x = "Words", y = "Count")

# 4. Categorize words by ratings and do pre-model analysis
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

# 4.1 Pre-model visualization
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

# 4.2 Prepare sentiment data according to both the analysis above and built-in
#     sentiment package
data4a <- data4 %>%
  spread(rating_factor, prop) %>%
  mutate(eta = log(high / low))
pos_words <- data4a %>%
  filter(eta > log(5)) %>%
  select(review_word)
neg_words <- data4a %>%
  filter(eta < -log(5)) %>%
  select(review_word)
specific_sentm <- rbind(pos_words, neg_words) %>%
  cbind(data.frame("sentiment" = c(
    rep("positive", nrow(pos_words)),
    rep("negative", nrow(neg_words))
  )))
bing_sentm <- get_sentiments("bing")
colnames(bing_sentm) <- c("review_word", "sentiment")
sentm <- bing_sentm %>%
  filter(!review_word %in% specific_sentm$review_word) %>%
  rbind(specific_sentm)

# 5. Model the relationship between rating values and sentiment
data5 <- data2 %>%
  left_join(sentm, by = "review_word") %>%
  mutate(value = ifelse(
    is.na(sentiment), 0,
    ifelse(sentiment == "positive", 1, -1)
  )) %>%
  group_by(id) %>%
  summarise(sent_val = sum(value))
data5$rating_value <- data1$rating_value
data5 <- data5 %>%
  mutate(rating_factor = factor(ifelse(
    rating_value == 5, "high",
    ifelse(rating_value >= 3, "med", "low")
  ), levels = c("low", "med", "high")))

# 5.1 Rough model with linear regression
lr_try <- glm(rating_value ~ sent_val, data = data5)
summary(lr_try)

# 5.2 More reasonable model with ordinal logistic regression
olr_try <- MASS::polr(rating_factor ~ sent_val, data = data5)
# MASS library is not imported because it will mask dplyr::select
summary(olr_try)

# 6. Take 2-word phrases into consideration
data6 <- data1 %>%
  unnest_tokens(bigram, review_text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  select(id, rating_value, word1, word2)
phrase_freq <- data6 %>%
  filter(!word1 %in% common_words$review_word) %>%
  filter(!word2 %in% common_words$review_word) %>%
  count(word1, word2, sort = TRUE) %>%
  filter(n >= 30)

# 6.1 Take negation of sentiments into consideration, adjust the previous data
#     to the final dataset that we model
neg_indicator <- c("not", "no", "none", "nothing", "never", "without", "hardly")
sent_adjust <- data6 %>%
  filter(word1 %in% neg_indicator | str_detect(
    word1, regex(".n't", dotall = TRUE)
  )) %>%
  select(id, review_word = word2) %>%
  inner_join(sentm, by = "review_word") %>%
  mutate(value = ifelse(sentiment == "positive", -2, 2)) %>%
  group_by(id) %>%
  summarise(sent_val = sum(value))
data_final <- rbind(data5[, 1:2], sent_adjust) %>%
  group_by(id) %>%
  summarise(adjusted_val = sum(sent_val)) %>%
  cbind(data5[, 3:4])

# Repeat the rough linear model and ordinal logistic regression model
lr <- glm(rating_value ~ adjusted_val, data = data_final)
summary(lr)
olr <- MASS::polr(rating_factor ~ adjusted_val, data = data_final)
summary(olr)

