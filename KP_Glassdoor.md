Kaiser Permanente Glassdoor Reviews
================
Taylor Bluth
December 13, 2017

As an intern of Kaiser Permanenete (KP) I have my own views of the company in my own sphere of work however it's beneficial to get a comprehensive view of the good and the bad. To have a more well rounded view of the company I can analyze scraped comments of KP employees from Glassdoor.com.

``` r
Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jdk-9.0.1')
library(tidyverse)
library(tm)
library(qdap)
library(wordcloud)
library(tidytext)

kp <- read_csv('data/kaiser_permanente_glassdoor_reviews.csv') %>% 
      rename(text = `summaries|pros|cons|adviceMgmt|ratings`) %>% 
      mutate(id = row_number())
```

Before doing this, I want to address the influence of post size on polarity to make sure a correct polarity analysis is being performed. For now, I will be focusing on the relationship between net polarity (positive word count - negative word count per post) and post length. This relationship may be an intuitive one, but its something that should be validated for consideration of future analyses. If not, we may overlook shorter posts.

This relationship can be plotted by first counting up the number of words per post and comparing them to the magnitude of sentiment polarity.

``` r
# Make comments into tidy form, count post word count, and remove stopwords
tidy.kp <- unnest_tokens(kp, word, text) %>% group_by(id) %>% 
           mutate(word.number = seq_along(id))
           anti_join(tidy.kp, data.frame(word = stopwords('en'), stringsAsFactors = F))
```

    ## Joining, by = "word"

    ## # A tibble: 1,156 x 3
    ## # Groups:   id [?]
    ##       id    word word.number
    ##    <int>   <chr>       <int>
    ##  1     1    good           2
    ##  2     1  caring           4
    ##  3     1 company           5
    ##  4     1 working           8
    ##  5     1   group          10
    ##  6     1  health          11
    ##  7     1   sales          12
    ##  8     1  couple          15
    ##  9     1   years          16
    ## 10     1     say          19
    ## # ... with 1,146 more rows

``` r
# Inner join our list of words to the Bing lexicon           
tidy.kp.pol <- inner_join(tidy.kp, get_sentiments('bing'), by = 'word') %>% 
               count(sentiment) %>% # Count total positive and negative words per post
               spread(sentiment, n, fill = 0) %>% # Split sentiment column into 2 columns
               mutate(polarity = positive - negative) # Find net polarity
# Count number of words per post
post.length <- count(tidy.kp, id) 
# Join post number of words and netpolarity by the id of the post
(final.kp <- inner_join(tidy.kp.pol, post.length, by = 'id'))
```

    ## # A tibble: 86 x 5
    ## # Groups:   id [?]
    ##       id negative positive polarity     n
    ##    <int>    <dbl>    <dbl>    <dbl> <int>
    ##  1     1        0        4        4    48
    ##  2     2        0        7        7    52
    ##  3     3        0        6        6    28
    ##  4     4        0        2        2    16
    ##  5     5        0        2        2     4
    ##  6     6        2        4        2    88
    ##  7     7        0        6        6    20
    ##  8     8        0        3        3    13
    ##  9     9        0        3        3    16
    ## 10    10        1        4        3    18
    ## # ... with 76 more rows

The data frame has been prepared and the relationship between post length and net polarity is seen below.

``` r
cor(final.kp$polarity, final.kp$n)
```

    ## [1] 0.4782147

``` r
ggplot(final.kp, aes(x = polarity, y = n)) +
  geom_point() +   
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = 'Polarity', 
       y = "Post's Number of Words", 
       title = "Post Length vs. Net Polarity")
```

![Post Length vs Polarity](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/KP_Glassdoor/length_v_polarity.png)

Such a correlation should concern us, since longer posts will come up as more significant and will drown out the say of shorter posts.

To adjust for this relationship I will use a `qdap` function called `polarity`. This function will sum up all the positive and negative words in each post and find the difference (net polarity). This difference will then be divided by the square root of total words. This last part will normalize long posts that have lots of positive and negative words to make them more comparable to the shorter posts that will likely have a lesser magnitude. The distribution of this scoring is seen below.

``` r
kp.pol <- polarity(kp$text) 
```

    ## Warning in polarity(kp$text): 
    ##   Some rows contain double punctuation.  Suggested use of `sentSplit` function.

``` r
ggplot(kp.pol$all, aes(x = polarity)) +
  geom_histogram(binwidth = 0.2, fill = "indianred3", color = "black")
```

![Polarity Distribution](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/KP_Glassdoor/polarity_dist.png)

The posts are overwhelmingly positive with many neutral posts and only 2 (weakly) negative ones. Since dividing up the positive and negative terms would leave us with way too many positive terms and next to no negative terms I will scale these posts to adjust for the inflated level of positive posts. By doing this the mean polarity values will be the dividing line. Posts under this mean value will be considered "Less Positive" and of course the ones above the mean polarity will be considered "More Positive". Then words that appear more frequently in each will be shown in a comparison word cloud. This cloud will only show non-overlapping terms between classes. Let's check it out.

``` r
# Way too positive, scaled to reveal 'least positive' comments
kp.pol2 <- mutate(kp, polarity = scale(kp.pol$all$polarity))

# Take comments that are greater than or equal to the mean polarity
more.positive <- filter(kp.pol2, polarity >= 0) %>% 
            select(text) %>% 
            paste(collapse = " ") # Collapse all positive comments into one string
# Take comments that are less than the mean polarity
less.positive <- filter(kp.pol2, polarity < 0) %>% 
            select(text) %>% 
            paste(collapse = " ") # Collapse all negative comments into one string
# Make a two column data frame of positive and negative comments
all <- c(more.positive, less.positive)

pos.neg.corpus <- VCorpus(VectorSource(all)) 
  
tdm <- TermDocumentMatrix(
       pos.neg.corpus, 
       control = list(weighting = weightTfIdf, #Common words in posts are underweighted
                      removePunctuation = TRUE, 
                      stopwords = c(stopwords('en'), 'didnt'))
       )
```

As seen in the code the `TermDocumentMatrix` object has TF-IDF weighting so that words that appear across many documents are under-weighted and words with higher frequency that appear in fewer documents are given more weight/relevance. With this TDM a comparison cloud can now be made as seen below.

``` r
tdm.m <- as.matrix(tdm)
colnames(tdm.m) <- c("Most Positive", "Least Positive")

# Comparison cloud
set.seed(1)
comparison.cloud(tdm.m, max.words = 50, colors = c("darkgreen","darkred"))
```

![Comparison Cloud](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/KP_Glassdoor/comp_cloud.png)

Things like work schedule and the integrated nature of Kaiser Permanente were unique common qualities of the more positive posts. The least positive posts contain the word "managers" indicating some dissatisfaction with management. The more negative posts also included hours and time. Perhaps certain jobs within KP have hard hours while other jobs have great schedules. For those without this insider knowledge, this is a great topic for further analysis.
