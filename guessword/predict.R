library(data.table)
library(tidytext)
library(tidyverse)

clean_text <- function(text) {
  ## Convert encoding to ascii and normallize apostrophies 
  Encoding(text) <- "latin1"
  text <- enc2utf8(text)
  text <- gsub("â€™|â€˜", "'", text)
  text <- gsub("â€¦", "", text)
  text <- gsub("â€", "-", text)
  text <- iconv(text, "UTF-8", "ASCII", sub = "")
  
  ## Remove URLs & Hashtags & retweets
  text <- gsub("(ftp|http|www\\.|#)\\S*","", tolower(text))
  text <- gsub("(\\b)rt(\\b)", "", text)
  ## Replace _/- with space
  text <- gsub("[_-]", " ", text)
  ## Remove numbers
  text <- gsub("(((\\d{1,3})(,\\d{3})*)|(\\d+))(.\\d+)?", "", text)
  
  ## Replace contractions
  text <- gsub("(\\b)(are|could|did|does|do|had|has|have|might|must|should|was|were|would)n'?t(\\b)", "\\1\\2 not\\3", text)
  text <- gsub("(\\b)can'?t|cannot(\\b)", "\\1can not\\2", text)
  text <- gsub("(\\b)isn'?t|ain'?t(\\b)", "\\1is not\\2", text)
  text <- gsub("(\\b)won't", "\\1will not", text)
  text <- gsub("(\\b)(how|i|should|they|we|must|what|who|would|you)'?ve(\\b)", "\\1\\2 have\\3", text)
  text <- gsub("(\\b)(it|they|what|who|you)'?ll(\\b)", "\\1\\2 will\\3", text)
  text <- gsub("(\\b)(he|i|she|we)'ll(\\b)", "\\1\\2 will\\3", text)
  text <- gsub("(\\b)(they|what|who|why|you)'?re(\\b)", "\\1\\2 are\\3", text)
  text <- gsub("(\\b)we're", "\\1we are", text)
  text <- gsub("(\\b)i'?m(\\b)", "\\1i am\\3", text)
  text <- gsub("(\\b)i'?d(\\b)", "\\1i would\\3", text)
  text <- gsub("(\\b)let'?s", "\\1let us", text)
  text <- gsub("(\\b)y'?all|ya'll", "\\1you all", text)
  text <- gsub("(\\b)(he|she|how|that|there|what|when|where|who|why)'?s(\\b)", "\\1\\2 is\\3", text)
  text <- gsub("(\\b)it's|'tis", "\\1it is", text)
  text <- gsub("(\\b)(how|what|where|who|why)'?d(\\b)", "\\1\\2 did\\3", text)
  ## Replace abreveations
  text <- gsub("(\\b)r(\\b)", "\\1are\\2", text)
  text <- gsub("(\\b)u(\\b)", "\\1you\\2", text)
  text <- gsub("(\\b)b4(\\b)", "\\1before\\2", text)
  text <- gsub("(\\b)b/?c(\\b)", "\\1because\\2", text)
  text <- gsub("(\\b)b(\\b)", "\\1be\\2", text)
  text <- gsub("(\\b)1st(\\b)", "\\1first\\2", text)
  text <- gsub("(\\b)2nd(\\b)", "\\1second\\2", text)
  text <- gsub("(\\b)3rd(\\b)", "\\1third\\2", text)
  text <- gsub("(\\w)'n(\\b)", "\\1ing\\2", text)
  text <- gsub("(\\b)n(\\b)", "\\1and\\2", text)
  text <- gsub("(\\b)w/o(\\b)", "\\1without\\2", text)
  text <- gsub("(\\b)w(\\b)", "\\1with\\2", text)
  text <- gsub("(\\b)w/(.)", "\\1with \\2", text)
  text <- gsub("(\\s+)@(\\s+)", "\\1at\\2", text)
  text <- gsub("(\\s+)&(\\s+)", "\\1and\\2", text)
  
  ##replace repeating characters
  text <- gsub("([a-z])\\1{2,}", "\\1", text)
  text
}

ugrams <- data.table(readRDS("data/ugram_mod_f.rds"))[order(-pkn)]
bgrams <- data.table(readRDS("data/bgram_mod_f.rds"))[order(-pkn)]
tgrams <- data.table(readRDS("data/tgram_mod_f.rds"))[order(-pkn)]

predict_word <- function (w1 = NA, w2 = NA){
  
  ## Use trigrams
  pred <- tgrams[(ugram1==w1) & (ugram2==w2), .(word=ugram3, p_word=pkn)]
  
  if (nrow(pred) < 5) {
    ## If not enough results use bigrams
    w <- ifelse(is.na(w2), w1, w2)
    pred <- merge(pred, 
                  bgrams[(ugram1==w), .(word=ugram2, p_word=pkn)]
                  [!(word %in% pred$word)], all=TRUE)
    
    if (nrow(pred) < 5) {
      ## If not enough results use unigrams
      pred <- merge(pred,
                    if (is.na(w1))
                      ugrams[1:25, .(word=ugram, p_word=p)]
                    else
                      ugrams[1:25, .(word=ugram, p_word=pkn)]
                    [!(word %in% pred$word)], all=TRUE)         
    }
  }
  
  pred[order(-p_word)][1:5]
}

process_phrase <- function (phrase = "") {
  phrase <- tibble(text = clean_text(phrase))
  words <- tail(unnest_tokens(phrase, word, text), 2)
  predict_word(words$word[1], words$word[2])
}


  