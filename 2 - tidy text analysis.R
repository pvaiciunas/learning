# https://www.tidytextmining.com/sentiment.html


text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")


text

library(dplyr)

textdf <- tibble(line = 1:4, text = text)

library(tidytext)
library(janeaustenr)
library(stringr)

# WE can examine JAne AUsten's works using the janeaustenr package
original_books <- austen_books() %>% 
  group_by(book) %>% 
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>% 
  ungroup()


# Tidy data analysis concentrates on creating a dataset of one token per row.
# A token can be a word or ngram, and is a meaningful unit of text

tidy_books <- original_books %>% 
  unnest_tokens(word, text)

# This puts the books into a tidy table format, which allows for tidy tools
# To be used. 
# 
# One simpel step at thtis point is to remove the stop words in the book. These
# Are the common words like "the", "to", etc. We will use anti_join
# 
# The stop_Words data set has data from three lexicons. These can all be used
# Or filtered to whats appropriate.

data(stop_words)

tidy_books <- tidy_books %>% 
  anti_join(stop_words)

# Now let's see what's common

tidy_books %>% 
  count(word, sort = TRUE)

# Since it's in tidy format, we can pipe right into other tidy tools

library(ggplot2)

tidy_books %>% 
  ungroup() %>% 
  count(word, sort = TRUE) %>% 
  filter(n > 600) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()




# Word Frequency ----------------------------------------------------------

# Inverse documet frequency and term frequency are two measures, that when
# combined, give an indidcation of how important a word is relative to the
# body of works it is foundin. It is a great approach to compare unique and
# important words across different texts, not within 

library(dplyr)
library(janeaustenr)
library(tidytext)

book_words <- austen_books() %>% 
  unnest_tokens(word, text) %>% 
  count(book, word, sort = TRUE)

total_words <- book_words %>% 
  group_by(book) %>% 
  summarize(total = sum(n))

book_words <- left_join(book_words, total_words)

# The number of times a word appears divided by the total number of words in the
# Book is the term frequency. Usually, the results are not surprising, and 
# contain the words that don't matter tha most (e.g. the, to, his, etc)

library(ggplot2)

ggplot(book_words, aes(n / total, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~book, ncol = 2, scales = "free_y")

# Generally, all written or spoken body of words will follow the distribution
# seen in the graphs above. This was informally described in Zipf's law, saying
# that the frequency that a word appears is inversly proportional to its rank
# 
# We can calculate it here:

freq_by_rank <- book_words %>% 
  group_by(book) %>% 
  mutate(rank = row_number(), 
         'term frequency' = n/total)


# This graph shows what the relationship looks like in JAne Austen books.
# A Power law model was fitted to the data (see site for specs)
# 
# Most bodies of work will have fewer rare words than is predicted by the model,
# but JAne Austen is unusual in that she also uses much fewer common words

freq_by_rank %>% 
  ggplot(aes(rank, `term frequency`, color = book)) + 
  geom_abline(intercept = -0.62, slope = -1.1, color = "gray50", linetype = 2) +
  geom_line(size = 1.1, alpha = 0.8, show.legend = FALSE) + 
  scale_x_log10() +
  scale_y_log10()


# now moving on to comparing what words are important in each text.
# The bind_tf_idf function creates the object we're looking for

book_words <- book_words %>% 
  bind_tf_idf(word, book, n)

# Very common words found in all books will have a score of 0. These are the 
# wrods we are not interested in. 

book_words %>% 
  select(-total) %>% 
  arrange(desc(tf_idf))

# This will show us the more important words. You'll notice that it is names 
# at the very top. These are unique to each book, and this increass their 
# importance score.
# 
# WE can vizualize this:

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(book) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()



# Ngrams and Relationships between words ----------------------------------

# Going beyond just looking at individual words, there is a lot of value in 
# factoring in the relationships that exist between them.
# 
# The token = "ngrams" in the unnest_Tokens functions performs the functionality
# we need

austen_bigrams <- austen_books() %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)

austen_bigrams

# most of the same tools used on individual words can be used here too

austen_bigrams %>% 
  count(bigram, sort = TRUE)

# Given that the most common bigrams are composed of stop words, it's a good time
# to use the stop_Words lexicon to get rid of them

library(tidyr)

bigrams_separated <- austen_bigrams %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
