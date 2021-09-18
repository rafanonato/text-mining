### TRABALHO PROFESSOR GUSTAVO ####

install.packages("devtools")
devtools::install_github("bradleyboehmke/harrypotter")


library(tidyverse)    
library(stringr)    
library(tidytext) 
library(harrypotter)

philosophers_stone[1:2]


titles <- c("Philosopher's Stone", "Chamber of Secrets", "Prisoner of Azkaban",
            "Goblet of Fire", "Order of the Phoenix", "Half-Blood Prince",
            "Deathly Hallows")

books <- list(philosophers_stone, chamber_of_secrets, prisoner_of_azkaban,
              goblet_of_fire, order_of_the_phoenix, half_blood_prince,
              deathly_hallows)

series <- tibble()

for(i in seq_along(titles)) {
  
  clean <- tibble(chapter = seq_along(books[[i]]),
                  text = books[[i]]) %>%
    unnest_tokens(word, text) %>%
    mutate(book = titles[i]) %>%
    select(book, everything())
  
  series <- rbind(series, clean)
}

# set factor to keep books in order of publication
series$book <- factor(series$book, levels = rev(titles))

series

book_words <- series %>%
  count(book, word, sort = TRUE) %>%
  ungroup()

series_words <- book_words %>%
  group_by(book) %>%
  summarise(total = sum(n))

book_words <- left_join(book_words, series_words)

book_words

book_words %>%
  mutate(ratio = n / total) %>%
  ggplot(aes(ratio, fill = book)) +
  geom_histogram(show.legend = FALSE) +
  scale_x_log10() +
  facet_wrap(~ book, ncol = 2)


##### Lei de Zipf

freq_by_rank <- book_words %>%
  group_by(book) %>%
  mutate(rank = row_number(),
         `term freq` = n / total)

ggplot(freq_by_rank, aes(rank, `term freq`, color = book)) +
  geom_line() +
  scale_x_log10() +
  scale_y_log10()


lower_rank <- freq_by_rank %>%
  filter(rank < 500)

lm(log10(`term freq`) ~ log10(rank), data = lower_rank)
## 
## Call:
## lm(formula = log10(`term freq`) ~ log10(rank), data = lower_rank)
## 
## Coefficients:
## (Intercept)  log10(rank)  
##     -0.9414      -0.9694

freq_by_rank %>% 
  ggplot(aes(rank, `term freq`, color = book)) +
  geom_abline(intercept = -0.9414, slope = -0.9694, color = "gray50", linetype = 2) +
  geom_line(size = 1.2, alpha = 0.8) +
  scale_x_log10() +
  scale_y_log10()


####### Freqüência inversa

book_words <- book_words %>%
  bind_tf_idf(word, book, n)

book_words

book_words %>%
  arrange(desc(tf_idf))

book_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))),
         book = factor(book, levels = titles)) %>% 
  group_by(book) %>%
  top_n(15, wt = tf_idf) %>%
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = book)) +
  geom_bar(stat = "identity", alpha = .8, show.legend = FALSE) +
  labs(title = "Qtda alta de palavras tf-idf na serie do Harry Potter",
       x = NULL, y = "tf-idf") +
  facet_wrap(~book, ncol = 2, scales = "free") +
  coord_flip()
