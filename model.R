library(tidyverse)
library(tidytext)

setwd("~/coursera/DataScience-CS")

## Load file of 10,000 most common english words with bad words removed
englishwords <- read_lines("data/google-10000-english-no-swears.txt")

## Generate unigram counts
if (!file.exists("data/ugramfreqf.rds")) {
  ugrams <- ungroup(readRDS("data/ugrams.rds")) %>% 
    group_by(ugram) %>% 
    summarise(count = sum(n)) %>%
    
    ## filter out unigrams not in common word list
    filter(ugram %in% englishwords)
  
  saveRDS(ugrams, "data/ugramfreqf.rds")
} else {
  ugrams <- readRDS("data/ugramfreqf.rds")
}

## Generate bigram counts
if (!file.exists("data/bgramfreqf.rds")) {
  bgrams <- readRDS("data/bgrams.rds") %>% 
    separate(bgram, c("ugram1", "ugram2"), sep=" ") %>%
    
    ## keep only tgrams that have words in filtered ugram list 
    filter(ugram1 %in% ugrams$ugram) %>% 
    filter(ugram2 %in% ugrams$ugram) %>%
    
    group_by(ugram1, ugram2) %>% 
    summarise(count = sum(n))
  
  saveRDS(bgrams, "data/bgramfreqf.rds")
} else {
  bgrams <- readRDS("data/bgramfreqf.rds")
}

## Generate trigram counts
if (!file.exists("data/tgramfreqf.rds")) {
  tgrams <- ungroup(readRDS("data/tgrams.rds")) %>% 
    separate(tgram, c("ugram1", "ugram2", "ugram3"), sep=" ") %>%
    
    ## keep only tgrams that have words in filtered ugram list 
    filter(ugram1 %in% ugrams$ugram) %>%
    filter(ugram2 %in% ugrams$ugram) %>%
    filter(ugram3 %in% ugrams$ugram) %>%
    group_by(ugram1, ugram2, ugram3) %>% 
    summarise(count = sum(n))
  saveRDS(tgrams, "data/tgramfreqf.rds")
} else {
  tgrams <- readRDS("data/tgramfreqf.rds")
}

## Calculate variables for lowest level Kneser-Ney
ugrams <- 
  ## Calculate continuation counts in bigrams
  left_join(ugrams, 
            group_by(bgrams, ugram2) %>% summarize(ccount = n()),
            by = c("ugram" = "ugram2")) %>%

  ## Calculate preceeding counts in bigrams
  left_join(group_by(bgrams, ugram1) %>% summarize(pcount = n()),
            by = c("ugram" = "ugram1")) %>%

  ## Calculate middle counts in trigrams
  left_join(group_by(tgrams, ugram2) %>% summarize(mcount = n()),
            by = c("ugram" = "ugram2")) %>%
  replace_na(list(ccount=0, pcount=0, mcount=0))

## Calculate number of unique bgrams
n_bgrams <- nrow(bgrams)
n_tokens <- sum(ugrams$count)

## Calculate ML & Kneser-Ney probabilities for all unigrams
ugrams <- mutate(ugrams, p = count/n_tokens, pkn = ccount/n_bgrams)

## Calculate Kneser-Ney variables for bigrams
bgrams <- 
  ## Calculate bigram continuation counts in trigrams
  left_join(bgrams, 
            group_by(tgrams, ugram2, ugram3) %>% summarize(ccount = n()),
            by = c("ugram1" = "ugram2", "ugram2" = "ugram3")) %>%
  
  ## Calculate bigram preceeding counts in trigrams
  left_join(group_by(tgrams, ugram1, ugram2) %>% summarize(pcount = n()),
            by = c("ugram1" = "ugram1", "ugram2" = "ugram2")) %>%
  
  ## Add counts, preceeding counts and middle counts for first unigram of bigram
  left_join(select(ugrams, ugram, w1_count = count, w1_pcount = pcount, w1_mcount = mcount),
            by = c("ugram1" = "ugram")) %>%
  
  ## Add Kneser-Ney probabity for second unigram of bigram
  left_join(select(ugrams, ugram, w2_pkn = pkn),
            by = c("ugram2"= "ugram")) %>%
  replace_na(list(ccount=0, pcount=0, w1_count=0, w1_pcount=0, w1_mcount=0, w2_pkn=0))

## Calculate number of bigrams appearing once and twice
bgram_1 <- sum(bgrams$count==1)
bgram_2 <- sum(bgrams$count==2)
## Calculate discount for bigrams
d2 <- bgram_1/(bgram_1 + 2 * bgram_2)

## Calculate Kneser-Ney probability for all bigrams
bgrams <- rowwise(bgrams) %>%
  mutate(
         ## KN probability for middle step of 3-gram KN
         w2w3_pkn = max(ccount - d2, 0) / w1_mcount + d2 * (w1_pcount / w1_mcount) * w2_pkn,
         ## KN probability for bigram, no need for max since min count is 1
         pkn = (count - d2) / w1_count + d2 * (w1_pcount / w1_count) * w2_pkn) %>%
  ungroup()

## Calculate variables for top level Kneser-Ney
tgrams <- 
  ## Add counts and preceeding counts for w1w2
  left_join(tgrams,
            select(bgrams, ugram1, ugram2, w1w2_count = count, w1w2_pcount = pcount),
            by = c("ugram1"= "ugram1", "ugram2" = "ugram2")) %>%
  ## Add Kneser-Ney probabity for w2w3
  left_join(select(bgrams, ugram1, ugram2, w2w3_pkn),
            by = c("ugram2"= "ugram1", "ugram3" = "ugram2")) %>%
  replace_na(list(w1w2_count=0, w1w2_pcount=0, w2w3_pkn=0))

## Calculate number of trigrams appearing once and twice
tgram_1 <- sum(tgrams$count==1)
tgram_2 <- sum(tgrams$count==2)
## Calculate discount for bigrams
d3 <- tgram_1/(tgram_1 + 2 * tgram_2)

## Calculate Kneser-Ney probability for all bigrams, no need for max since min count is 1
tgrams <- 
  mutate(tgrams, 
         pkn = (count - d3) / w1w2_count + d3 * (w1w2_pcount / w1w2_count) * w2w3_pkn)


saveRDS(select(ugrams, ugram, count, p, pkn), 
        "data/ugram_mod_f.rds")
saveRDS(top_n(group_by(select(bgrams, ugram1, ugram2, pkn), ugram1), 5), 
        "data/bgram_mod_f.rds")
saveRDS(top_n(group_by(select(tgrams, ugram1, ugram2, ugram3, pkn), ugram1, ugram2), 5), 
        "data/tgram_mod_f.rds")
