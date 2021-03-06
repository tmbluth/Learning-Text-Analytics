Email Clustering
================
Taylor Bluth
November 24, 2017

This data set is a large set of emails (1.3 GB of text). The full amount of information isn't necessary to practice the concepts taught below, so feel free to use less. In light of this only 1% of the data is included below.

In order to preprocess emails one must use different rules to remove commonly used, yet irrelevant text such as hyperlinks and email addresses. Also most of the normal preprocessing must take place. Below I have incorporated these preprocessing steps and have created a Document-Term Matrix (DTM). Its important to know this is not a Term-Document Matrix (TDM), because the rows/documents are being clustered here. If we wanted to cluster by words then a TDM should be made here, but this is not the goal.

``` r
emails.corpus <- VCorpus(VectorSource(emails$message))

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
removeEmail <- function(x) gsub("\\S*@\\S*\\s?", "", x)
removeReturn <- function(x) gsub("\\n", " ", x)

emails.clean <- tm_map(emails.corpus, content_transformer(tolower)) %>%
                tm_map(content_transformer(removeURL) ) %>% 
                tm_map(content_transformer(removeReturn) ) %>% 
                tm_map(content_transformer(removeEmail) ) %>% 
                tm_map(removeNumbers) %>%  
                tm_map(removePunctuation) %>%  
                tm_map(removeWords, c(stopwords(), 'enron')) %>%
                tm_map(stemDocument) 

DTM <- DocumentTermMatrix(emails.clean, control = list(weighting = weightTfIdf)) %>%
       removeSparseTerms(sparse = 0.9)

DTM
```

    ## <<DocumentTermMatrix (documents: 5174, terms: 121)>>
    ## Non-/sparse entries: 111767/514287
    ## Sparsity           : 82%
    ## Maximal term length: 16
    ## Weighting          : term frequency - inverse document frequency (normalized) (tf-idf)

``` r
DTM.m <- as.matrix(DTM)
```

The DTM has been reduced and only 126 terms remain of our 5174 documents. Also the DTM is weighted by Term Frequency - Inverse Document Frequency (TF-IDF) to reduce noise of common words in the document. A Latent Semantic Analysis (LSA) can also be used to reduce dimensionality into much fewer "topics" but reduces the granularity of having lots of variation in documents. This really shines when working with huge amounts of data. I will look at both the LSA and the TF-IDF-only routes. With these come the need to calculate sample distances.

There are a few options to calculate document vector distance between the documents in the DTM, two being cosine similarity and Euclidean distance. I have chosen to use the scaled Euclidean distances between objects since it gives more info (in my opinion) than the angle between documents. It is scaled because some documents are longer than others, meaning their term frequencies will naturally be bigger than other documents term frequencies even though the might have similar proportions of word distribution. Once scaled the distance between document vectors determines similarity. This can be especially helpful in clustering.

``` r
dist.m <- dist(scale(DTM.m)) 
data.frame(Distance = dist.m[1:5], Document = attr(dist.m, "Labels")[1:5])
```

    ##   Distance Document
    ## 1 17.76506        1
    ## 2 18.39673        2
    ## 3 13.25899        3
    ## 4 17.25399        4
    ## 5 19.23565        5

``` r
cos.m <- cosine(t(DTM.m)) # Transpose DTM to a TDM to show document similarity
cos.m[1:5, 1:5]
```

    ##            1          2          3          4          5
    ## 1 1.00000000 0.09632895 0.09562423 0.32999772 0.16607048
    ## 2 0.09632895 1.00000000 0.02499072 0.07440152 0.17680362
    ## 3 0.09562423 0.02499072 1.00000000 0.13928715 0.23207822
    ## 4 0.32999772 0.07440152 0.13928715 1.00000000 0.07691352
    ## 5 0.16607048 0.17680362 0.23207822 0.07691352 1.00000000

The first output shows the scaled document distances of document-terms to each other and will be used in hierarchical clustering analysis (HCA). The second is the average cosine distance between all documents and is shown just for demonstration.

There still remains a problem though. I do not have subject matter expertise and do not know a good amount of clusters to make. Dendrograms from hierarchical clustering are good tools to estimate which number of clusters is the best, so that route can be taken. Once you have a ballpark number, an even better way to find the optimal number of clusters would be to test a lot of different clusters and compare them.

``` r
HCA <- hclust(dist.m, method = "ward.D2")
plot(HCA, xlab = "Terms")
```

![Dendrogram](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/Email_Clustering_files/HCA-1.png)

NOTE: Dendrogram is messy due to large number of smaples. Around 2 to 5 clusters might be optimal according to this dendrogram.

Several metrics have been used to assess cluster fit, and among the best are connectivity, silhouette width, and the Dunn Index. Connectivity is a measure of how connected clusters are. In clustering, you do NOT want your clusters to be well connected since it means they are difficult to distinguish, so this is a metric you want to minimize. It can span from 0 to infinity. Due to its difficulty to explain I do not prefer this metric.

Silhouette width is easier to understand in that it is a measure of the average distance of each point in comparison to all other points in the nearest cluster minus the average distance of each point to all the other points within the cluster all over the max of each. Its a mouthful, but its a way to measure separation of clusters and similarity within a cluster from -1 (bad) to 1 (good). This -1 to 1 scale makes it comparable to other clusters, even with completely different data sets!

Then there is the Dunn Index. This is the easiest to understand, and my favorite. In the `clValid` vignette it is defined as follows: "The Dunn Index is the ratio of the smallest distance between observations not in the same cluster to the largest intra-cluster distance." This is measured from 0 to infinity where closer to infinity is preferred.

All three can be assessed across many different numbers of clusters and the best of these 3 metrics will be picked. Also multiple clustering methods can be used within the same call. In this case I will use HCA and K-means.

``` r
validate <- clValid(DTM.m, 2:5, maxitems = length(DTM$dimnames$Docs),
                    clMethods=c("hierarchical","kmeans"), validation="internal")
summary(validate)
```

    ## 
    ## Clustering Methods:
    ##  hierarchical kmeans 
    ## 
    ## Cluster sizes:
    ##  2 3 4 5 
    ## 
    ## Validation Measures:
    ##                                   2        3        4        5
    ##                                                               
    ## hierarchical Connectivity    2.9290   5.9579  12.0071  14.9361
    ##              Dunn            0.6262   0.2784   0.2138   0.2193
    ##              Silhouette      0.6861   0.5985   0.5314   0.5200
    ## kmeans       Connectivity    9.7060  12.6349 107.7135 258.1266
    ##              Dunn            0.1128   0.1416   0.0621   0.0621
    ##              Silhouette      0.5038   0.5040   0.1257   0.1300
    ## 
    ## Optimal Scores:
    ## 
    ##              Score  Method       Clusters
    ## Connectivity 2.9290 hierarchical 2       
    ## Dunn         0.6262 hierarchical 2       
    ## Silhouette   0.6861 hierarchical 2

Its unanimous. HCA with 2 clusters is the best way to go. 

One can also go with the K-means algorithm using the LSA output by doing the following:

``` r
LSA <- lsa(t(DTM)) # lsa takes TermDocumentMatrix

Kmeans.docs <- kmeans(LSA$dk, 2)
table(Kmeans.docs$cluster)
```

    ## 
    ##    1    2   
    ## 4336  838  

``` r
Kmeans.terms <- kmeans(LSA$tk, 2)
table(Kmeans.terms$cluster)
```

    ## 
    ##   1    2 
    ## 107   14   

``` r
Kmeans.terms$cluster[Kmeans.terms$cluster == 1][1:10]
```

    ##   also    ask attach  avail   back    bcc   bill    bit   busy   call 
    ##      1      1      1      1      1      1      1      1      1      1

``` r
Kmeans.terms$cluster[Kmeans.terms$cluster == 2][1:10]
```

    ## agreement    change   discuss    thread     power     price    report    trade     deal     email 
    ##         2         2         2         2         2         2         2        2        2         2
       
First cluster emails into 2 classes and their distribution can be seen. Then view some terms from the classes with their class labels to see if common themes appear.
