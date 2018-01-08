Wizard of Oz
================
Taylor Bluth
November 4, 2017

Background
==========

This analysis will look into the Wizard of Oz. The book will be analyzed as a whole (most frequent terms) and the 6 chapters in the book will be compared. First is to load relevant libraries and to load the data itself.

Load Data
=========

``` r
library(tm)           # VectorSource, VCorpus, TextDocumentMatrix, Zipf_plot
library(wordcloud)    # wordcloud
library(textstem)     # lemmatize_strings
library(tidyverse)    # %>% 
library(gutenbergr)   # gutenberg_download
library(data.table)   # as.data.table
library(lsa)          # cosine

url <- 'http://www.gutenberg.org/cache/epub/25519/pg25519.txt'
book <- read_file(url)  
book <- gutenberg_download('25519') %>% as.data.table() 
head(book) 
```

    ##    gutenberg_id                                 text
    ## 1:        25519 [Illustration: THIS BOOK BELONGS TO]
    ## 2:        25519                                     
    ## 3:        25519                       [Illustration]
    ## 4:        25519                                     
    ## 5:        25519          LITTLE WIZARD STORIES OF OZ
    ## 6:        25519

The text column will be what is of interest. In order to compare different documents the decision has to be made as to what a document is considered. In this case, each chapter will be a document so a way to split the books into its chapters will need to be executed. Below, the chapter titles are found in the document and their line indices are found so that there is a location to split the book into chapters.

``` r
# break up the story into short stories (chapters)
titles <- toupper(c("The Cowardly Lion and the Hungry Tiger",
                    "Little Dorothy and Toto",
                    "Tiktok and the Nome King",
                    "Ozma and the Little Wizard",
                    "Jack Pumpkinhead and the Sawhorse",
                    "The Scarecrow and the Tin Woodman"))

# gets the row numbers where the value of the text column is in the titles
start_idxs <- c(book[text %in% titles, , which=T], nrow(book) + 1)

get_chap <- function(dt, st_idxs, chap_num) {
  return(dt[(st_idxs[chap_num] + 1):(st_idxs[chap_num + 1] - 1), 'text'])
}

stories <- lapply(seq(length(titles)), get_chap, dt = book, st_idxs = start_idxs)

# collapse the text into a single character string -- this was tricky to figure out
collapse_txt <- function(story) {
  return(do.call(paste, c(story, collapse = ' ')))
}

book_ready <- lapply(stories, collapse_txt) %>% as.vector()
```

Now that the chapters have been split on I can begin processing them to be standardized for analysis.

Standardization
===============

``` r
WoOz <- VCorpus(VectorSource(book_ready))

WoOz_clean <- WoOz %>% 
              tm_map(content_transformer(tolower)) %>% 
              tm_map(removeNumbers) %>% 
              tm_map(removePunctuation) %>% 
              tm_map(stripWhitespace) %>% 
              tm_map(removeWords, c(tm::stopwords('SMART'),"illustration")) %>% 
              tm_map(content_transformer(lemmatize_strings))
              
WoOz_tdm <- TermDocumentMatrix(WoOz_clean)
```

Now that there is a clean corpus and a TDM, the real analyses and information retrieval can begin.

``` r
term_freq <- rowSums(as.matrix(WoOz_tdm)) %>% 
             sort(decreasing = T)
term_freq[term_freq >= 20]
```

    ##    wizard       man      head scarecrow      king      ozma      make 
    ##        48        45        41        38        36        36        34 
    ##       tin      jack      lion     tiger      find       imp   dorothy 
    ##        34        31        29        28        27        27        26 
    ##       eye      girl      good    tiktok      tree      baby    friend 
    ##        25        25        25        25        25        22        21 
    ##       ask 
    ##        20

``` r
findFreqTerms(WoOz_tdm, lowfreq = 20)
```

    ##  [1] "ask"       "baby"      "dorothy"   "eye"       "find"     
    ##  [6] "friend"    "girl"      "good"      "head"      "imp"      
    ## [11] "jack"      "king"      "lion"      "make"      "man"      
    ## [16] "ozma"      "scarecrow" "tiger"     "tiktok"    "tin"      
    ## [21] "tree"      "wizard"

When words are summed up across all 6 chapters we can get each word's count in the book. Words that show up at least 20 times can be listed with their respective counts as seen above. If you don't care about the sorted counts and just want to see the words that have 20 or more appearances (alphabetically), a much easier function is `findFreqTerms`.

Information Retrieval
=====================

At it's core information retrieval determines similarity among documents, which in this case are chapters in a book. Chapters most similar to each other could be found. We could also see which of the documents are most similar to a query string that we give. This similarity will have to be found using some sort of distance metric. Euclidean distance would be good, except document vectors would have to be normalized due to differing lengths of chapters. To get around this, cosine similarity will be used which is discussed later.

Before calculating similarities it is good practice to weight the term frequencies within documents. This can be done in a process called Term Frequency-Inverse Document Frequency weighting. In essence words that show up few times across many documents are given the least weight and words that show up in few documents with more frequency are most heavily weighted. These weighted document vectors will then be used to compare documents and other queries to each other.

``` r
tf_idf_m <- TermDocumentMatrix(WoOz_clean, control = list(weightTfIdf)) %>%
            as.matrix()

dist(scale(t(tf_idf_m))) # Document Term Matrix looks at document distances
```

    ##          1        2        3        4        5
    ## 2 50.89089                                    
    ## 3 53.22721 53.42430                           
    ## 4 51.91919 52.20765 53.41704                  
    ## 5 50.25971 50.90200 51.91864 50.43649         
    ## 6 52.74038 52.73567 53.67742 52.22916 51.28373

Chapters with the smallest scaled euclidean distance are also the least distant in similarity. In this case chapters 1 and 5. Now a custom function must be made to create a mini search engine.

``` r
Search_Engine <- function(query, TDM = tf_idf_m){ 
  q_words <- match(query, rownames(TDM))
  q_vec <- rep.int(0,nrow(TDM))
  q_vec[q_words] <- 1
  rank <- list()
  for(i in 1:ncol(TDM)){
    rank[i] <- cosine(TDM[,i], q_vec)
  }
  print(unlist(rank))
}

Search_Engine("scarecrow")
```

    ## [1] 0.01636488 0.00000000 0.00000000 0.00000000 0.00000000 0.50114527

``` r
Search_Engine(c("wizard","oz"))
```

    ## [1] 0.01636488 0.06986433 0.04853251 0.44709095 0.11915865 0.08126680

The output is a vector of cosine similarities to the search query ordered by chapter (1-6). If each document is a vector of terms then each term is a dimension and the frequency of terms the magnitude of each dimension. This will give us 6 vectors (chapters) placed in high dimensional space (\# of terms in corpus). To compare these vectors one may use the angle between the vectors which is commonly done by obtaining the cosine between vectors. When the cosine between vectors is equal to 0 the vectors have a 90 degree difference from each other meaning they do not have any similarities. When the cosine is 1 they have no difference in direction, meaning they are identical in nature. It looks like chapter 6 is most related to "scarecrow" and chapter 4 is most related to the terms "wizard" and "oz".

This is an **extremely** simplified version of what professional search engines do to rank documents based off a query. Professional information retrieval tools are much more capable of deciphering word meaning by using natural language processing. They also incorporate clustering which can give importance to documents related to your query, even if the documents contain little to no query terms.
