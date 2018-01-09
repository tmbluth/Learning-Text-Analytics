News Groups
================
Taylor Bluth
November 19, 2017

Intro
=====

This data is a collection of 11314 documents / articles in the news that span 20 topics, called targets. The targets will be predicted by learning the relationships between words and topics. Since a Document Term Matrix will be used in this context, it will be different than a typical supervised learning problem. Let's get into it.

``` r
library(tidyverse)
library(tm)
library(caret)
library(klaR)
library(wordcloud)
library(RTextTools)

news <- read_csv("data/20_newsgroups.csv") 
news$target <- as.factor(news$target)
news <- news[sample(nrow(news)*0.6, replace = FALSE), ] 
```

The data has been loaded and halved due to memory issues in R. Document term matrices are very sparse and large and once turned into a data frame things get a little packed. Also the reason a DTM is being used instead of a TDM because we can add the target column to it that correspond to the documents. Let's prepare that DTM.

Pre-process
===========

``` r
clean_corpus <- function(x){
  x %>% 
    tm_map(content_transformer(tolower)) %>% 
    tm_map(removeNumbers) %>% 
    tm_map(removePunctuation) %>% 
    tm_map(stripWhitespace) %>% 
    tm_map(removeWords, c(stopwords(), 'subject', 'nntppostinghost')) %>% 
    tm_map(stemDocument)
}

news.clean <- VCorpus(VectorSource(news$text)) %>% clean_corpus()

DTM <- DocumentTermMatrix(news.clean, control = list(weighting = weightTfIdf)) %>%  
                          removeSparseTerms(sparse = 0.99)
DTM.df <- as.data.frame(as.matrix(DTM))
```

The DTM has been cleaned and turned into a data frame. Before moving forward with the classifiers, a descriptive frequency plot can help see what some of the most used words.

``` r
word.freq <- colSums(DTM.df, na.rm = T) %>% sort(decreasing = T)
barplot(word.freq[1:10])
```

![Top 10](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/News_Groups/VIZ-1.png)


Classify with RTextTools
========================

RTextTools is a package that simplifies classification. It does have a slower run time and will have memory issues if too big of a dataset is fed into it, so I would recommend using normal supervised learning and packages like `tm` or `qdap` to preprocess the text. For now I will simply sample a subset of the data.

``` r
news2 <- read_csv("data/20_newsgroups.csv") %>% sample_frac(size = 0.5) 
dtm <- create_matrix(news2, toLower = TRUE, removeNumbers = TRUE, removePunctuation = TRUE, 
                     stripWhitespace = TRUE, removeStopwords = TRUE, stemWords = TRUE,
                     removeSparseTerms = 0.99, weighting = weightTfIdf)
split.index <- round(nrow(news2)*0.8)
dtm.container <- create_container(dtm, news2$target, trainSize = 1:split.index, 
                                  testSize = (split.index+1):nrow(news2), virgin = FALSE)
```

With RTextTools `create_matrix` and `create_container` do the preprocessing and pull from the popular `tm` functions to do so.

The following functions let us model the data and pick from different algorithms to do so. The 1create\_container\` function allows us to split the data into training and test sets, which in this case is 80-20, like what was attempted earlier. In this case SVM will be the only algorithm used.

``` r
svm <- cross_validate(dtm.container, 3, algorithm = 'SVM', kernel = 'linear')
```

    ## Fold 1 Out of Sample Accuracy = 0.7632674
    ## Fold 2 Out of Sample Accuracy = 0.7559524
    ## Fold 3 Out of Sample Accuracy = 0.7678855

``` r
svm.model <- train_model(dtm.container, algorithms = "SVM")

classes <- classify_model(dtm.container, svm.model)
```

The data is then cross validated and modeled. Once modeling is complete we can use it to classify the data from the previous split in dtm.container. Then a data frame output can be created and viewed below.

``` r
svm.analytic <- create_analytics(dtm.container, classes)
head(svm.analytic@document_summary)
```

    ##   SVM_LABEL  SVM_PROB MANUAL_CODE CONSENSUS_CODE CONSENSUS_AGREE
    ## 1         2 0.2111442           1              2               1
    ## 2        14 0.1153992          14             14               1
    ## 3         5 0.1735488           1              5               1
    ## 4        18 0.2334741          12             18               1
    ## 5        14 0.1554732          14             14               1
    ## 6        14 0.1186427          12             14               1
    ##   CONSENSUS_INCORRECT PROBABILITY_CODE PROBABILITY_INCORRECT
    ## 1                   1                2                     1
    ## 2                   0               14                     0
    ## 3                   1                5                     1
    ## 4                   1               18                     1
    ## 5                   0               14                     0
    ## 6                   1               14                     1

The SVM\_LABEL is the real value and the PROBABILITY\_CODE is the predicted label. If we put in new text documents we can expect accuracy near 78%, but most likely it will be worse due to variability in real world data.
