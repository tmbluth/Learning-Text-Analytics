North Korea in Social Media
================
Taylor Bluth
Dec 11th, 2017

Social media is ubiquitous and has far reaching effects, even in third world countries. It has inherent value due to its prevalence, which is why learning how to analyze it can be beneficial to understanding populations of interest. To practice text analysis in the wild I will be looking at tweets on Twitter. Though micro-blogging has quite small documents, the sentiment of each tweet is capturable. First let's make the link between my computer and Twitter through what is known as the Twitter API.

``` r
library(twitteR)
library(tm)
library(tidyverse)
library(tidytext)
library(radarchart)

setup_twitter_oauth(api_key, api_secret, token_key, token_secret)
nk <- searchTwitter('#northkorea', n = 2000, since = '2017-12-07 00:00:00 ', resultType = 'recent')
dprk.news <- searchTwitter('@DPRK_News', n = 2000, since = '2017-12-07 00:00:00', resultType = 'recent')
rm(api_key, api_secret, token_key, token_secret)
```

Of interest, as of late, is the news of North Korea. Much of the world has dubbed North Korea as a dangerous hermit state that bullies other countries and threatens its own citizens. We are obviously being fed some negative news about North Korea and are likely taking a similar sentiment ourselves. We can get a pulse on what sentiment is surrounding the topic by analyzing the posts with `#northkorea` as well as those from a satirical depiction of North Korea from `@DPRK_News`. This will allow us to see if this satirical depiction is actually similar to conversations that are likely more genuine. These were all posted on December 10th.

First these tweets must be pulled and put into data frames. Each of the data frames will have retweets filtered out and will have their associated subject linked to each tweet. Then the two subjects - `#northkorea` and `@DPRK_News` - will be combined

``` r
tweets.nk <- data.frame(text = sapply(nk, function(x) x$getText()),
                        retweet = sapply(nk, function(x) x$isRetweet),
                        stringsAsFactors = FALSE) %>% 
             filter(retweet == FALSE) %>% 
             select(-retweet) %>% 
             mutate(line = row_number(),
                    subject = '#northkorea') 
             

tweets.dprk <- data.frame(text = sapply(dprk.news, function(x) x$getText()),
                          retweet = sapply(dprk.news, function(x) x$isRetweet),
                          stringsAsFactors = FALSE) %>% 
               filter(retweet == FALSE) %>% 
               select(-retweet) %>% 
               mutate(line = row_number(),
                      subject = '@DPRK_News') 

tweets.all <- rbind(tweets.nk, tweets.dprk)
```

In order to better manipulate and graph the data, it will be transformed into tidy format. This format allows each word to have its own line with the associated subjects in one column. Counts of word frequency will also be added so that they can be linked to lexicons and then graphed to show sentiment polarity.

``` r
tidy.tweets <- unnest_tokens(tweets.all, output = word, input = text)
tidy.tweets$subject <- as.factor(tidy.tweets$subject)

tidy.tweets.count <- anti_join(tidy.tweets, 
                               data.frame(word = c(stopwords('en'),'dprk','dprk_news',
                                                   'north', 'korea', 'northkorea',
                                                   'rt', 'https', 't.co')), by = 'word') %>%
                     count(subject, word, sort = T)
```

In this instance the AFINN lexicon will be added to our tweets. This lexicon has thousands of words with values from -5 to 5. Only the words that appear in both AFINN and our data frame will be used going forward.

We can also plot the distribution of scores in a density plot which will make a continuous probability curve of potential values that collections of documents are likely to be based off their distribution. Below are 2 density plots, one for each subject at hand. The sum of area under each curve will total to 1.

``` r
tweets.afinn <- inner_join(tidy.tweets.count, get_sentiments('afinn'), by = 'word') 
               
ggplot(tweets.afinn, aes(x = score, fill = subject)) + 
  geom_density(alpha = 0.3) + 
  ggtitle("AFINN Score Densities")
```

![](North_Korea_Senitment_Analysis_files/figure-markdown_github-ascii_identifiers/Density%20Plots-1.png)

![AFINN Density Plot](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/North_Korea_Senitment_Analysis/AFINN_density.png)

On average `#northkorea` posts were quite a bit more negative while `@DPRK_News` posts were evenly negative and positive on average. While this does give a good range of feeling, it is only on one dimension. Emotion is much more than just positive or negative words. For this reason I will also use the `nrc` lexicon, which gives 8 emotions along with positive and negative. Since we already have a gist for positive and negative posts by subject they will be removed from consideration.

``` r
tweets.nrc <- inner_join(tidy.tweets.count, get_sentiments('nrc'), by = 'word')

# Drop positive or negative
emotions <- tweets.nrc %>%
  filter(!grepl("positive|negative", sentiment)) %>% 
  group_by(subject, sentiment) %>%
  tally() %>% 
  spread(subject, nn) 
```

    ## Using `n` as weighting variable

``` r
# Review scores
emotions
```

    ## # A tibble: 8 x 3
    ##      sentiment `#northkorea` `@DPRK_News`
    ## *        <chr>         <int>        <int>
    ## 1        anger           232           75
    ## 2 anticipation           264          118
    ## 3      disgust            86           57
    ## 4         fear           420           87
    ## 5          joy           137           92
    ## 6      sadness           163           76
    ## 7     surprise           173           70
    ## 8        trust           264          145

``` r
# JavaScript radar chart
chartJSRadar(emotions)
```

![Radar Plot](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/North_Korea_Senitment_Analysis/small_radar.png)

Above we can see the more specific emotional trends of each topic. The `@DPRK_News` is much smaller due to the fewer tweets collected on it, making the two subjects unscaled. This can be fixed.

``` r
emotions.scaled <- data.frame(Sentiment = emotions$sentiment, North_Korea = scale(emotions$`#northkorea`, center = F), DPRK_News = scale(emotions$`@DPRK_News`, center = F))

chartJSRadar(emotions.scaled)
```

![Scaled Radar Plot](https://github.com/tmbluth/Learning-Text-Analytics/blob/master/figures/North_Korea_Senitment_Analysis/scaled_radar.png)

Now we can see which emotions each tweet source gravitates toward more evenly. I'm surprised to see posts of trust from `@DPRK_News`. As expected those talking about North Korea have high amounts of fear. Also both seem to share a heightened sense of anticipation, which seems like a very understandable emotion to be feeling as the US and the DPRK exchange threats of war.

It seems as though `DPRK_News` varies quite a bit from the average `#northkorea` conversation. This seems intuitive since it is meant to impersonate the DPRK in a more extreme fashion. Similar yet more detailed analyses could be conducted to determine typical sentiment of authors and compare them to other texts claiming to be said author as a form of verification alongside natural language processing.
