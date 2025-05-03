install.packages(c("rvest", "textclean", "stringr", "tidyverse", "textstem", "tm"))
install.packages("topicmodels")
library(topicmodels)
library(rvest)
library(textclean)
library(stringr)
library(tidyverse)
library(textstem)
library(tm)


url1 <- "https://www.thedailystar.net/tech-startup/news/why-are-users-shifting-tiktok-rednote-3800636"
url2 <- "https://www.thedailystar.net/business/news/future-jobs-are-we-ready-3801216"
url3 <- "https://www.thedailystar.net/business/news/inflation-biggest-concern-bangladesh-wef-3801261"
url4 <- "https://www.thedailystar.net/opinion/perspective/news/campus-without-student-politics-1818682"


urls <- c(url1, url2, url3, url4)
articles <- list()

for (i in 1:length(urls)) {
  html_content <- read_html(urls[i])
  
  article_text <- html_content %>%
    html_nodes("div.pb-20.clearfix p") %>%
    html_text()

  articles[[i]] <- paste(article_text, collapse = " ")
}

clean_text <- function(text) {

  text <- tolower(text)
  text <- str_remove_all(text, "[[:punct:]]")
  text <- str_remove_all(text, "\\d+")
  text <- removeWords(text, stopwords("en"))
  text <- str_squish(text)
  text <- lemmatize_strings(text)
  
  return(text)
}

tokenize_text <- function(text) {
  tokens <- str_split(text, "\\s+")[[1]]
  return(tokens)
}
cleaned_articles <- lapply(articles, clean_text)
tokenized_articles <- lapply(cleaned_articles, tokenize_text)
print(cleaned_articles[[2]])
print(tokenized_articles[[2]])




reconstructed_articles <- sapply(tokenized_articles, paste, collapse = " ")
print(reconstructed_articles)

corpus <- VCorpus(VectorSource(reconstructed_articles))
dtm <- DocumentTermMatrix(corpus)


print(dtm) 
inspect(dtm[1:4, 1:10])  
dtm_filtered <- removeSparseTerms(dtm, 0.98)
dtm_matrix <- as.matrix(dtm_filtered)

print(dim(dtm_matrix))  
print(dtm_matrix[1:4, 1:10])  
print(dtm_matrix)



dtm_tfidf <- weightTfIdf(dtm)
inspect(dtm_tfidf[1:4, 1:10])
terms <- findFreqTerms(dtm_tfidf, lowfreq = 0.02)
print(terms)
tfidf_df <- as.data.frame(as.matrix(dtm_tfidf))
print(head(tfidf_df))

dtm_lda <- removeSparseTerms(dtm,0.99)
k <- 4
lda_model <- LDA(dtm_lda, k=k, control = list(seed = 1234))
top_terms <- terms(lda_model, 5)
print(top_terms)

doc_topics <- topics(lda_model)
print(doc_topics)


top_words <- terms(lda_model, 10)
print("Top words for each topic:")
print(top_words)


topic_probabilities <- posterior(lda_model)$topics
print("Topic probabilities for each document:")
print(topic_probabilities)



