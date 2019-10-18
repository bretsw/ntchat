## --------------------------------------------------------------
## load libraries
## --------------------------------------------------------------

library(tidyverse)  # for data manipulation; includes library(dplyr); library(ggplot2)
library(stringr)  # for ease of working with string and character varbiables
library(rtweet)
library(anytime); library(lubridate)  # for working with dates
library(igraph)  # for processing social network
library(ggraph)  # for visualizing social network
library(irr)

## --------------------------------------------------------------
## load, clean, view data
## --------------------------------------------------------------

start_date <- "05-01-2018 00:00:00 EST" %>% mdy_hms %>% as_datetime
end_date <- "10-01-2019 00:00:00 EST" %>% mdy_hms %>% as_datetime  ## was 02-01-2019


file_list <- stringr::str_extract_all(dir("data"), "^ntchat_raw\\S+", simplify=TRUE)
file_list <- file_list[file_list[,1] != "", ]
file_list <- paste0("data/", filelist)


ntchat_all <- lapply(file_list, read.csv, header=TRUE, colClasses='character')
ntchat_all %>% sapply(dim)


## --------------------------------------------------------------
## pull full tweet content using library(rtweet)
## --------------------------------------------------------------

## See https://rtweet.info/ for details on library(rtweet)
## See https://rtweet.info/reference/index.html for all rtweet functions
## See https://apps.twitter.com/ for details on Twitter developer application

statuses <- ntchat_all[[1]] %>% as.data.frame %>% pull(id_str)

ntchat_rtweet <- statuses %>% lookup_tweets %>% flatten  # Returns data on up to 90,000 Twitter statuses. 
#ntchat_rtweet %>% dim
#ntchat_rtweet %>% write.csv("data/ntchat_rtweet.csv", row.names=FALSE)


## --------------------------------------------------------------
## 
## --------------------------------------------------------------

ntchat_rtweet <- read.csv("data/ntchat_rtweet.csv", header=TRUE, colClasses='character')
#ntchat_rtweet %>% names

ntchat <- ntchat_rtweet %>% as.data.frame %>%
        distinct(status_id, .keep_all = TRUE) %>%
        filter(status_id != "",
               !is.na(status_id),
               user_id != "",
               user_id != "[deleted]",
               text != "[deleted]",
               text != "[removed]",
               text != "",
               lang=="en"
               ) %>%
        mutate(date_time = {created_at %>% ymd_hms %>% as_datetime}) %>%
        select(-created_at) %>% 
        filter((date_time > start_date) & (date_time < end_date))


## --------------------------------------------------------------
## Raw Numbers
## --------------------------------------------------------------

n_tweets <- ntchat %>% pull(status_id) %>% unique %>% length
n_tweeters <- ntchat %>% pull(user_id) %>% unique %>% length
paste("Number of tweets", n_tweets); paste("Number of tweeters:", n_tweeters)

## --------------------------------------------------------------
## Frequencies
## --------------------------------------------------------------

tweets_table <- ntchat %>% pull(user_id) %>%
        table %>% as.data.frame %>%
        rename(user = ".", all_tweets = "Freq") %>%
        arrange(desc(all_tweets))
head(tweets_table, 10)

tweets_table$all_tweets %>% mean %>% round(2)
tweets_table$all_tweets %>% sd %>% round(2)
tweets_table$all_tweets %>% median
tweets_table$all_tweets %>% min
tweets_table$all_tweets %>% max

tweeter_freq_table <- tweets_table$all_tweets %>% table %>% as.data.frame %>%
        rename(number_of_tweets = ".", number_of_tweeters = "Freq") %>%
        mutate(percentage_of_tweeters = ((number_of_tweeters / n_tweeters) * 100) %>% round(2))
head(tweeter_freq_table, 10)

paste0(sum(tweeter_freq_table$percentage_of_tweeters[1:17]), 
       "% of #ntchat tweeters tweeted 17 times or less (i.e., monthly).")


## --------------------------------------------------------------
## Dates
## --------------------------------------------------------------

## 9 months: May 1, 2018 - Jan 31, 2019
max(ntchat$date_time) %>% difftime(min(ntchat$date_time), units="days")
max(ntchat$date_time) %>% difftime(min(ntchat$date_time), units="weeks")


## --------------------------------------------------------------
## Raw Numbers - No Retweets
## --------------------------------------------------------------

## first, filter out all RTs
ntchat_no_rt <- ntchat %>% filter(is_retweet==FALSE)
ntchat_no_rt %>% dim

## percentage of tweets that are not retweets
n_no_rt <- ntchat_no_rt %>% pull(status_id) %>% unique %>% length
paste0(n_no_rt, ", or ", ((n_no_rt / n_tweets) * 100) %>% round(2),
       "% of #ntchat tweets, were not retweets.")

## percentage of tweeters that contributed something other than retweets
n_tweeters_no_rt <- ntchat_no_rt %>% pull(user_id) %>% unique %>% length
paste0(n_tweeters_no_rt, ", or ", ((n_tweeters_no_rt / n_tweeters) * 100) %>% round(2),
       "% of #ntchat tweeters, contributed something other than retweets.")


## --------------------------------------------------------------
## Frequencies - No Retweets
## --------------------------------------------------------------

tweets_no_rt_table <- ntchat_no_rt %>% pull(user_id) %>%
        table %>% as.data.frame %>%
        rename(user = ".", all_tweets = "Freq") %>%
        arrange(desc(all_tweets))
head(tweets_no_rt_table, 10)

## average tweets per tweeter
tweets_no_rt_table$all_tweets %>% mean %>% round(2)
tweets_no_rt_table$all_tweets %>% sd %>% round(2)
tweets_no_rt_table$all_tweets %>% median
tweets_no_rt_table$all_tweets %>% min
tweets_no_rt_table$all_tweets %>% max

tweeter_no_rt_freq_table <- tweets_no_rt_table$all_tweets %>% table %>% as.data.frame %>%
        rename(number_of_tweets = ".", number_of_tweeters = "Freq") %>%
        mutate(percentage_of_tweeters = ((number_of_tweeters / n_tweeters_no_rt) * 100) %>% round(2))
head(tweeter_no_rt_freq_table, 10)

## tweeters (more than retweets) who only contributed once
paste0(tweeter_no_rt_freq_table[1, 'number_of_tweeters'], 
       " #ntchat tweeters only contributed once")

## tweeters (more than retweets) who contributed, an avearage, once per month or less
paste0(sum(tweeter_no_rt_freq_table$percentage_of_tweeters[1:17]), 
       "% of #ntchat tweeters tweeted 17 times or less (i.e., monthly).")

## average number of retweets per tweet
ntchat_no_rt$retweet_count %>% as.numeric %>% mean %>% round(2)
ntchat_no_rt$retweet_count %>% as.numeric %>% sd %>% round(2)
ntchat_no_rt$retweet_count %>% as.numeric %>% median %>% round(2)
ntchat_no_rt$retweet_count %>% as.numeric %>% range %>% round(2)

number_of_retweets_table <- ntchat_no_rt %>% pull(retweet_count) %>%
        table %>% as.data.frame %>%
        rename(retweets = ".", count = "Freq") %>%
        arrange(desc(count))
n_zero_retweets <- number_of_retweets_table$count[1]
paste0((((nrow(ntchat_no_rt) - n_zero_retweets) / n_no_rt) * 100) %>% round(2),
       "% of #ntchat tweets were retweeted")

## average number of likes per tweet
ntchat_no_rt$favorite_count %>% as.numeric %>% mean %>% round(2)
ntchat_no_rt$favorite_count %>% as.numeric %>% sd %>% round(2)
ntchat_no_rt$favorite_count %>% as.numeric %>% median %>% round(2)
ntchat_no_rt$favorite_count %>% as.numeric %>% range %>% round(2)

number_of_favorites_table <- ntchat_no_rt %>% pull(favorite_count) %>%
        table %>% as.data.frame %>%
        rename(retweets = ".", count = "Freq") %>%
        arrange(desc(count))
n_zero_favorites <- number_of_favorites_table$count[1]
paste0((((nrow(ntchat_no_rt) - n_zero_favorites) / n_no_rt) * 100) %>% round(2),
       "% of #ntchat tweets were not liked")

## average time on Twitter per tweeter (in years)
ntchat_no_rt %>% distinct(user_id, .keep_all = TRUE) %>%
        mutate(twitter_start = (account_created_at %>% ymd_hms %>% as_datetime),
               duration = (max(ntchat_no_rt$date_time) %>% 
                               difftime(twitter_start, units="days") %>% 
                               as.numeric / 365
                           ) %>% round(2)
               ) %>%
        summarize(mean_duration = duration %>% mean, 
                  sd_duration = duration %>% sd,
                  median_duration = duration %>% median, 
                  min_duration = duration %>% min,
                  max_duration = duration %>% max
                  )

## average number of followers per tweeter
ntchat_no_rt %>% distinct(user_id, .keep_all = TRUE) %>%
        mutate(followers_count = followers_count %>% as.numeric) %>% 
        summarize(mean_followers = followers_count %>% mean, 
                  sd_followers = followers_count %>% sd,
                  median_followers = followers_count %>% median, 
                  min_followers = followers_count %>% min,
                  max_followers = followers_count %>% max
                  )

## average number of friends per tweeter
ntchat_no_rt %>% distinct(user_id, .keep_all = TRUE) %>%
        mutate(friends_count = friends_count %>% as.numeric) %>% 
        summarize(mean_friends = friends_count %>% mean, 
                  sd_friends = friends_count %>% sd,
                  median_friends = friends_count %>% median, 
                  min_friends = friends_count %>% min,
                  max_friends = friends_count %>% max
                  )

## average number of tweets per tweeter (for the duration of their time on Twitter)
ntchat_no_rt %>% distinct(user_id, .keep_all = TRUE) %>%
        mutate(statuses_count = statuses_count %>% as.numeric) %>% 
        summarize(mean_statuses = statuses_count %>% mean, 
                  sd_statuses = statuses_count %>% sd,
                  median_statuses = statuses_count %>% median, 
                  min_statuses = statuses_count %>% min,
                  max_statuses = statuses_count %>% max
                  )

## average number of likes per tweeter
ntchat_no_rt %>% distinct(user_id, .keep_all = TRUE) %>%
        mutate(favourites_count = favourites_count %>% as.numeric) %>% 
        summarize(mean_favourites = favourites_count %>% mean, 
                  sd_favourites = favourites_count %>% sd,
                  median_favourites = favourites_count %>% median, 
                  min_favourites = favourites_count %>% min,
                  max_favourites = favourites_count %>% max
                  )


## --------------------------------------------------------------
## Sample for human coding
## --------------------------------------------------------------

set.seed(02182019)
sample_ids <- ntchat_no_rt %>% pull(status_id) %>% sample(size=200)
sample_ids %>% head

ntchat_tweet_sample <- ntchat_no_rt %>% filter(status_id %in% sample_ids) %>%
        mutate(self=0, others=0, mutual=0, misc=0) %>%
        select(status_id, user_id, date_time, screen_name, text, status_url,
               reply_to_screen_name, self, others, mutual, misc
               )
ntchat_tweet_sample %>% dim
#ntchat_tweet_sample %>% write.csv("data/ntchat_tweet_sample.csv", row.names=FALSE)

ntchat_sample %>% pull(user_id) %>% unique %>% length

ntchat_tweeter_sample <- ntchat_no_rt %>% filter(status_id %in% sample_ids) %>%
        distinct(user_id, .keep_all = TRUE) %>%
        mutate(pst=0, teacher=0, te=0, consult=0, admin=0, non=0, other=0) %>%
        select(user_id, screen_name, name, location, description, 
               url, account_created_at, verified,
               followers_count, friends_count, statuses_count, favourites_count,
               pst, teacher, te, consult, admin, non, other
               )
ntchat_tweeter_sample %>% dim
#ntchat_tweeter_sample %>% write.csv("data/ntchat_tweeter_sample.csv", row.names=FALSE)


## --------------------------------------------------------------
## Analyzing coded data
## --------------------------------------------------------------

## input coded data
coded_tweets <- read.csv(file="data/ntchat_tweet_sample_coder1.csv", header=TRUE, colClasses='character')
coded_tweeters <- read.csv(file="data/ntchat_tweeter_sample_coder1.csv", header=TRUE, colClasses='character')


# margin of error for 95% confidence level
moe <- function(x, n, z=1.96) {
        x = x / 100
        y = {(100 * z * sqrt(x * (1 - x) / n))} %>% round(2)
        return(y)
}

## purpose of tweets
tweet_raw <- coded_tweets %>% summarize(self = self %>% as.numeric %>% sum,
                           others = others %>% as.numeric %>% sum,
                           mutual = mutual %>% as.numeric %>% sum,
                           misc = misc %>% as.numeric %>% sum
                           )
tweet_proportion <- (100 * tweet_raw / nrow(coded_tweets)) %>% round(2)
tweet_moe <- tweet_proportion %>% moe(., nrow(coded_tweets))
rbind(tweet_raw, tweet_proportion, tweet_moe)

## characteristics of tweeters
tweeter_raw <- coded_tweeters %>% summarize(pst = pst %>% as.numeric %>% sum,
                                            teacher = teacher %>% as.numeric %>% sum,
                             te = te %>% as.numeric %>% sum,
                             consult = consult %>% as.numeric %>% sum,
                             admin = admin %>% as.numeric %>% sum,
                             grad = grad %>% as.numeric %>% sum,
                             non.ed = non.ed %>% as.numeric %>% sum,
                             corp = corp %>% as.numeric %>% sum,
                             other = other %>% as.numeric %>% sum
                           )
tweeter_proportion <- {100 * tweeter_raw / nrow(coded_tweeters)} %>% round(2)
tweeter_moe <- tweeter_proportion %>% moe(., nrow(coded_tweeters))
rbind(tweeter_raw, tweeter_proportion, tweeter_moe)
                        
## country of tweeters
country_table <- coded_tweeters$location_manual %>% table %>% as.data.frame %>% 
        mutate(Freq = {Freq * 100 / nrow(coded_tweeters)} %>% round(2)) %>%
        arrange(desc(Freq))


## --------------------------------------------------------------
## Visualization: Tweet Purposes
## --------------------------------------------------------------

tweet_data <- rbind(tweet_proportion, tweet_moe) %>% 
    t() %>%
    as.data.frame() %>%
    rownames_to_column(var = 'purpose')
colnames(tweet_data) <- c("purpose", "proportion", "moe")
tweet_data$purpose <- factor(tweet_data$purpose, levels = tweet_data$purpose)

# include MOE bars: https://heuristically.wordpress.com/2013/10/20/bar-plot-with-error-bars-r/
ggplot(data = tweet_data, aes(x=purpose, y=proportion)) +
  geom_col(fill="#52854C", color="black", width = 0.8, size=3) +
  geom_errorbar(aes(ymin=proportion-moe, ymax=proportion+moe), 
                width=.1,
                size=2) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = .5, colour = "black"),
        axis.title = element_text(size=48, family="serif"),
        axis.text = element_text(size=44, family="serif")
        ) +
  xlab("Tweet Purpose") + ylab("Proportion") + 
  geom_hline(yintercept=0, color="gray20", size = .75)

ggsave("img/tweet-proportion-by-purpose.png", width = 1 * 16, height = 1 * 9)


## --------------------------------------------------------------
## Visualization: Tweeters
## --------------------------------------------------------------

tweeter_data <- rbind(tweeter_proportion, tweeter_moe) %>% 
  rename(PST = pst,
         Teacher = teacher,
         TE = te,
         Consult = consult,
         Admin = admin,
         Grad = grad,
         NonEd = non.ed,
         Corp = corp,
         Other = other
  ) %>%
  t() %>%
  as.data.frame() %>%
  rownames_to_column(var = 'vocation')
colnames(tweeter_data) <- c("vocation", "proportion", "moe")
tweeter_data$vocation <- factor(tweeter_data$vocation, levels = tweeter_data$vocation)

# include MOE bars: https://heuristically.wordpress.com/2013/10/20/bar-plot-with-error-bars-r/
ggplot(data = tweeter_data, aes(x=vocation, y=proportion)) +
  geom_col(fill="#52854C", color="black", width = 0.8, size=2.5) +
  geom_errorbar(aes(ymin=proportion-moe, ymax=proportion+moe), 
                width=.2,
                size=1.5) +
  theme(panel.background = element_rect(fill = "white", colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = .5, colour = "black"),
        axis.title = element_text(size=48, family="serif"),
        axis.text = element_text(size=32, family="serif")
  ) +
  xlab("Tweeter Vocation") + ylab("Proportion") + 
  geom_hline(yintercept=0, color="gray20", size = .75)

ggsave("img/tweeter-proportion-by-vocation.png", width = 1 * 16, height = 1 * 9)


## --------------------------------------------------------------
## Calculating inter-rater reliability (IRR)
## --------------------------------------------------------------

## input coded data
coded_tweets_2 <- read.csv(file="data/ntchat_tweet_sample_coder2.csv", header=TRUE, colClasses='character')
coded_tweeters_2 <- read.csv(file="data/ntchat_tweeter_sample_coder2.csv", header=TRUE, colClasses='character')

purpose1 <- coded_tweets %>% mutate(purpose = ifelse(self=="1", "self",
                                                     ifelse(others=="1","others", 
                                                            ifelse(mutual=="1", "mutual", "misc")
                                                            )
                                                     )
                                    ) %>% select(purpose)

purpose2 <- coded_tweets_2 %>% mutate(purpose = ifelse(self=="1", "self",
                                                       ifelse(others=="1","others", 
                                                              ifelse(mutual=="1", "mutual", "misc")
                                                              )
                                                       )
                                      ) %>% select(purpose)

purpose1 %>% cbind(purpose2) %>% agree %>% `[[`(5) %>% as.numeric %>% round(2)
purpose1 %>% cbind(purpose2) %>% kappa2 %>% `[[`(5) %>% as.numeric %>% round(2)

profile1 <- coded_tweeters %>% select(pst, teacher, te, consult, admin, grad, non.ed, corp, other)
profile2 <- coded_tweeters_2 %>% select(pst, teacher, te, consult, admin, grad, non.ed, corp, other)

profile_agree <- vector(); profile_kappa <- vector()

for(i in 1:9) {
        profile_agree[i] <- profile1[,i] %>% cbind(profile2[,i]) %>% agree %>% `[[`(5) %>% as.numeric %>% round(2)
        profile_kappa[i] <- profile1[,i] %>% cbind(profile2[,i]) %>% kappa2 %>% `[[`(5) %>% as.numeric %>% round(2)
}

irr_scores <- profile_agree %>% cbind(profile_kappa)
rownames(irr_scores) <- c("pst", "teacher", "te", "consult", "admin", "grad", "non.ed", "corp", "other")
colnames(irr_scores) <- c("agree", "kappa")
irr_scores