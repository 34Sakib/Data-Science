install.packages(c("rvest", "textclean", "stringr", "tidyverse", "textstem", "tm"))
library(rvest)
library(textclean)
library(stringr)
library(tidyverse)
library(textstem)
library(tm)


url <- "https://en.prothomalo.com/sports/football/2ytb85s1ek"
html_content <- read_html(url)
article_text <- html_content %>%
  html_nodes("p") %>%
  html_text()


text <- paste(article_text, collapse = " ")


clean_text <- str_replace_all(text, "[\r\n]", " ")
clean_text <- str_replace_all(clean_text, "\\s+", " ")  
clean_text <- str_replace_all(clean_text, "[[:punct:]]", " ")  


tokenized_text <- str_split(clean_text, "\\s+")[[1]]
tokenized_text <- tolower(tokenized_text)
tokenized_text <- tokenized_text[nchar(tokenized_text) > 2]


stop_words <- stopwords(c("en","take","will","one","say"))
tokenized_text <- tokenized_text[!tokenized_text %in% stop_words]


tokenized_text <- tokenized_text[!grepl("\\$|\\d+", tokenized_text)]
tokenized_text <- replace_contraction(tokenized_text)
tokenized_text <- lemmatize_words(tokenized_text)
tokenized_text <- str_replace_all(tokenized_text, "[\U00010000-\U0010FFFF]", "")


word_freq <- table(tokenized_text)
tokenized_text <- tokenized_text[tokenized_text %in% names(word_freq[word_freq > 1])]

tokenized_text <- unique(tokenized_text)

print(tokenized_text)

writeLines(tokenized_text, "tokenized_single_line.txt")
