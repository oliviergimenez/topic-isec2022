#' ---
#' title: "Topic modelling of ISEC2022 abstracts"
#' author: "Olivier Gimenez"
#' date: '2022-07-04'
#' output: html_document
#' ---
#' 
## ----setup, include=FALSE----------------------------------------
knitr::opts_chunk$set(echo = TRUE)

#' 
#' # Motivation
#' 
#' This is a quick and dirty text mining analysis of the corpus of [ISEC 2022](https://www.isec2022.org/) abstracts (talks and posters) using topic modelling. 
#' 
#' # Clean up data
#' 
## ----------------------------------------------------------------
library(tidyverse)
theme_set(theme_light(base_size = 14))

#' 
#' Read in text file of all abstracts, and [split on empty line](https://stackoverflow.com/questions/38958597/r-split-text-on-empty-line):
## ----------------------------------------------------------------
allabstracts <- readLines("txt/ISEC-2022-Abstract-Book.txt")
nvec <- length(allabstracts)
breaks <- which(grepl("^[[:space:]]*$", allabstracts))
nbreaks <- length(breaks)
if (breaks[nbreaks] < nvec) {
  breaks <- c(breaks, nvec + 1L)
  nbreaks <- nbreaks + 1L
}
if (nbreaks > 0L) {
  allabstracts <- mapply(function(a,b) paste(allabstracts[a:b], collapse = " "),
                   c(1L, 1L + breaks[-nbreaks]),
                   breaks - 1L)
}

#' 
#' Fix encoding:
## ----------------------------------------------------------------
Encoding(allabstracts) <- "latin1"
allabstracts <- iconv(allabstracts, "latin1", "UTF-8",sub='')
abstracts_tbl <- as_tibble(allabstracts)

#' 
#' Get rid of empty lines:
## ----------------------------------------------------------------
dat <- abstracts_tbl %>%
  rowwise() %>%
  mutate(val = str_length(value)) %>%
  filter(val > 1)

#' 
#' Add a column of id for each abstract:
## ----------------------------------------------------------------
dat$id <- seq(1, nrow(dat))

#' 
#' Split columns into tokens:
## ----------------------------------------------------------------
library(tidytext)
tidy_abstracts <- dat %>%
  unnest_tokens(word, value)

#' 
#' Get rid of common stop words:
## ----------------------------------------------------------------
data(stop_words)
tidy_abstracts <- tidy_abstracts %>%
  anti_join(stop_words)

#' 
#' Create DocumentTermMatrix object:
## ----------------------------------------------------------------
abstracts_dtm <- tidy_abstracts %>%
  group_by(id, word) %>%
  summarize(count = n()) %>%
  mutate(document = id, term = word) %>%
  cast_dtm(document, term, count)

#' 
#' # Counting
#' 
#' Count word occurrence:
## ----------------------------------------------------------------
tidy_abstracts %>%
  count(word, sort = TRUE)

#' 
#' Visualize:
## ----------------------------------------------------------------
counts <- tidy_abstracts %>%
  count(word, sort = TRUE) %>%
  filter(n > 150) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL)
counts

#' 
#' Save plot:
## ----------------------------------------------------------------
ggsave(filename = "fig/words-isec2022.png",
       plot = counts,
       dpi = 600)

#' 
#' # Topic modeling
#' 
#' Estimate LDA model:
## ----------------------------------------------------------------
library(topicmodels)
isec_lda <- LDA(x = abstracts_dtm, 
                k = 10, 
                control = list(seed = 1234))

#' 
#' Tidy up coefficients:
## ----------------------------------------------------------------
library(tidytext)
topics <- tidy(isec_lda, matrix = "beta")

#' 
#' Get top terms:
## ----------------------------------------------------------------
topterms <- topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

#' 
#' Visualize:
## ----------------------------------------------------------------
topicplot <- topterms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
topicplot

#' 
#' Save plot:
## ----------------------------------------------------------------
ggsave(filename = "fig/topic-isec2022.png",
       plot = topicplot,
       dpi = 600,
       width = 14)

#' 
#' 
