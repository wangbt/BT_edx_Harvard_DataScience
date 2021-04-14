# Sec4 date times and text mining#####
# Sec4.1
# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

# ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster == "Ipsos" & state =="U.S.") %>%
  ggplot(aes(startdate, rawpoll_trump)) +
  geom_line()

# lubridate: the tidyverse date package
library(lubridate)

# select some random dates from polls
set.seed(2)
dates <- sample(polls_us_election_2016$startdate, 10) %>% sort
dates

# extract month, day, year from date strings
data.frame(date = dates, 
           month = month(dates),
           day = day(dates),
           year = year(dates))

month(dates, label = TRUE)    # extract month label

# ymd works on mixed date styles
x <- c(20090101, "2009-01-02", "2009 01 03", "2009-1-4",
       "2009-1, 5", "Created on 2009 1 6", "200901 !!! 07")
ymd(x)

# different parsers extract year, month and day in different orders
x <- "09/01/02"
ymd(x)
mdy(x)
ydm(x)
myd(x)
dmy(x)
dym(x)

now()    # current time in your time zone
now("GMT")    # current time in GMT
now() %>% hour()    # current hour
now() %>% minute()    # current minute
now() %>% second()    # current second

# parse time
x <- c("12:34:56")
hms(x)

#parse datetime
x <- "Nov/2/2012 12:34:56"
mdy_hms(x)

# Case study: Trump Tweets####
library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)


url <- 'https://drive.google.com/file/d/16wm-2NTKohhcA26w-kaWfhLIGwl_oX95/view'
trump_tweets <- map(2009:2017, ~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame = TRUE) %>%
  filter(!is_retweet & !str_detect(text, '^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")

# This is data frame with information about the tweet:
head(trump_tweets)

names(trump_tweets)

# The help file ?trump_tweets provides details on what each variable represents. 
# The tweets are represented by the text variable:
trump_tweets %>% select(text) %>% head

# and the source variable tells us the device that was used to compose and
# upload each tweet:
trump_tweets %>% count(source) %>% arrange(desc(n))

# We can use extract to remove the Twitter for part of the source and filter 
# out retweets
trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  count(source) 

# the campaign dates 
campaign_tweets <- trump_tweets %>% 
  extract(source, "source", "Twitter for (.*)") %>%
  filter(source %in% c("Android", "iPhone") &
           created_at >= ymd("2015-06-17") & 
           created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)
head(campaign_tweets)

# visualizaiton 
ds_theme_set()
campaign_tweets %>%
  mutate(hour = hour(with_tz(created_at, "EST"))) %>%
  count(source, hour) %>%
  group_by(source) %>%
  mutate(percent = n / sum(n)) %>%
  ungroup %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Hour of day (EST)",
       y = "% of tweets",
       color = "")

# Text as data 
library(tidytext)

#The functions will take a vector of strings and extract the tokens
# so that each one gets a row in the new table.
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example
example %>% unnest_tokens(word, text)

# Now let's look at a quick example with a tweet number 3008:
i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# We can now use the unnest_tokens() function with the regex option 
# and appropriately extract the hashtags and mentions:
campaign_tweets[i,] %>% 
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

# Another minor adjustment we want to make is remove the links to pictures:
campaign_tweets[i,] %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  select(word)

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) 

tweet_words %>% 
  count(word) %>%
  arrange(desc(n))

stop_words

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word ) 

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words <- campaign_tweets %>% 
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, text, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))

android_iphone_or <- tweet_words %>%
  count(word, source) %>%
  spread(source, n, fill = 0) %>%
  mutate(or = (Android + 0.5) / (sum(Android) - Android + 0.5) / 
           ( (iPhone + 0.5) / (sum(iPhone) - iPhone + 0.5)))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)


android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(desc(or))

android_iphone_or %>% filter(Android+iPhone > 100) %>%
  arrange(or)

# Sentiment Analysis
sentiments 
get_sentiments("bing")
get_sentiments("afinn")
get_sentiments("loughran") %>% count(sentiment)
get_sentiments("nrc") %>% count(sentiment)

nrc <- get_sentiments("nrc") %>%
  select(word, sentiment)

tweet_words %>% inner_join(nrc, by = "word") %>% 
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts <- tweet_words %>%
  left_join(nrc, by = "word") %>%
  count(source, sentiment) %>%
  spread(source, n) %>%
  mutate(sentiment = replace_na(sentiment, replace = "none"))
sentiment_counts

tweet_words %>% group_by(source) %>% summarize(n = n())

sentiment_counts %>%
  mutate(Android = Android / (sum(Android) - Android) , 
         iPhone = iPhone / (sum(iPhone) - iPhone), 
         or = Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or <- sentiment_counts %>%
  mutate( log_or = log( (Android / (sum(Android) - Android)) / (iPhone / (sum(iPhone) - iPhone))),
          se = sqrt( 1/Android + 1/(sum(Android) - Android) + 1/iPhone + 1/(sum(iPhone) - iPhone)),
          conf.low = log_or - qnorm(0.975)*se,
          conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))

log_or


log_or %>%
  mutate(sentiment = reorder(sentiment, log_or),) %>%
  ggplot(aes(x = sentiment, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment, log_or)) +
  ylab("Log odds ratio for association between Android and sentiment") +
  coord_flip() 


android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment == "disgust" & Android + iPhone > 10) %>%
  arrange(desc(or))


android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Assesment part 1####
library(dslabs)
library(lubridate)
options(digits = 3)    # 3 significant digits

# Q3
data(brexit_polls)
head(brexit_polls)
class(brexit_polls$startdate)

# how many had a startdate in April
brexit_polls %>% 
  filter(month(startdate) == 4) %>%
  nrow()
##key
sum(month(brexit_polls$startdate) == 4)

# how many polls ended the week of 2016-06-12?
brexit_polls %>% 
  filter(round_date(enddate,unit="week") == "2016-06-12" ) %>%
  nrow()
##key
sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

# q4
# class(brexit_polls$enddate)
df <- brexit_polls %>% 
  mutate(weekdays = weekdays(enddate)) %>%
  group_by(weekdays)%>%
  summarize( n = n())%>%
  arrange(desc(n))

# key
table(weekdays(brexit_polls$enddate))

# q5
data(movielens)

dat <- movielens %>% 
  mutate(year  = year(as_datetime(timestamp)), hours = hour(as_datetime(timestamp))) %>%
  
  #group_by(year) %>%
  table(year)
  # summarize(n = n()) %>%
  names(which.max(year))#summarize(max = max(n()))

dat <- movielens %>% 
  mutate(year  = year(as_datetime(timestamp)), hour = hour(as_datetime(timestamp))) %>%
  group_by(hour) %>%
   #%>%

# key
dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews

reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews


# Assesment part2####
library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)

data(gutenberg_metadata)


str_detect( title, "Pride and Prejudice" )

# q6 Use str_detect() to find the ID of the novel Pride and Prejudice.
# How many different ID numbers are returned?
# Key 
gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

# q7
gutenberg_works(languages = "en",only_languages = TRUE,all_languages = FALSE,distinct = TRUE) %>%
  filter(str_detect(title, "Pride and Prejudice"))

# key
gutenberg_works(title == "Pride and Prejudice")$gutenberg_id

# q8
books <- gutenberg_download(1342)
words <- books %>%
  unnest_tokens(word, text)

# key
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)
  
# Q9
book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)
nrow(words)

# key
words <- words %>% anti_join(stop_words)
nrow(words)


# q9
words <- book %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^.*\\d.*$"))
nrow(words)

# key
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

# q11
words %>% group_by(word) %>%
  mutate( n = n()) %>%
  filter( n >100) %>%
  #count(word)
  arrange(desc(n))%>%
  nrow()
  
# key
# How many words appear more than 100 times in the book?
words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()
# What is the most common word in the book?
words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)
# How many times does that most common word appear?
words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(n)

# q12
# How many elements of words have sentiments in the afinn lexicon?
afinn <- get_sentiments("afinn")
library(tidytext)

afinn_sentiments <- words %>%
  inner_join(afinn, by = "word") %>%
  select(word, value)
nrow(afinn_sentiments)
count(afinn_sentiments)
# key
afinn_sentiments <- inner_join(afinn, words)
nrow(afinn_sentiments)

# What proportion of words in afinn_sentiments have a positive value?
count(filter(afinn_sentiments, value>0))/count(afinn_sentiments)
# key
mean(afinn_sentiments$value > 0)
 
 
# How many elements of afinn_sentiments have a value of 4?
count(filter(afinn_sentiments, value==4 ))
# key
sum(afinn_sentiments$value == 4)

# Comprehensive Assessment: Puerto Rico Hurricane Mortality####

library(tidyverse)
library(pdftools)
options(digits = 3)  

# q1
fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")

system("cmd.exe", input = paste("start", fn))

# q2
txt <- pdf_text(fn)
class(txt)

# q3
x <- str_split(txt[9] ,"\n")
# class(x)
# length(x)

# q4 
s <- x[[1]]
# class(s)
# length(s)

# q5
s <- str_trim(s,side = "both")

# s[[1]]

#q6
header_index <- str_which(s,pattern = "2015")[1]

# q7
header <- str_split(s[[header_index]],"\\s+",simplify = TRUE)[-1]

month <- str_split(s[[header_index]],"\\s+",simplify = TRUE)[1]

# q8
tail_index <- str_which(s,pattern = "Total")


# q9
n <- str_count(s, pattern = "\\d+")
# singlenumber_index <- which(n==1)
# key
# sum(n==1)
# q10
# row_number <- header_index:tail_index 
# row_number <- setdiff(row_number,c(singlenumber_index,tail_index,header_index))

# row_number <- row_number[-which(n = singlenumber_index)]


# temp <- s[row_number]
# temp
# length(temp)

# key1
# n <- str_count(string = s, pattern = "\\d+")
# out <- 1:header_index # remove above the header
# out <- append(out, which(n == 1)) # only has one number in these rows
# out <- append(out, tail_index:length(s)) # remove summary table
# # remove the rows
# s <- s[-out]
# length(s)

# key2 book
out <- c(1:header_index, which(n==1), tail_index:length(s))
s <- s[-out]
length(s)

# q11
s <- str_remove_all(s, "[^\\d\\s]")

# q12
s <- str_split_fixed(s, "\\s+", n = 6)[,1:5]

# class(s)
# colnames(s) <- c("day","2015","2016","2017","2018") 
# 
# tab <- as.data.frame(s)
# 
# 
# tab<- as.numeric(tab)
# 
# tab<- as.numeric(s)%>% head
# tab<- as.data.frame(tab)
# 
# # tab <- as.data.frame(tab)
# 
# # as.numeric(as.character(tab))
# 
# colnames(tab) <- c("day","2015","2016","2017","2018") 
# colnames(tab) <- c("day",header,"month") 
# 
# tab$month = month
# 
# class(tab)
# view(tab)

# tab1 <- as.data.frame(tab) %>% lapply(as.numeric)# %>% 
# 
# as.numeric(tab)
# 
# mean2015 <- mean(tab$"2015")
# mean2015 <- mean(as.numeric(tab$"2015"))
# 
# mean2016 <- mean(tab1$2016)

# key1
m <- mapply(s, FUN=as.numeric)
# convert into a matrix of doubles, ss
ss <- matrix(data=m, ncol=5, nrow=30)
# change column names of double matrix ss
colnames(ss) <- c("day", "s2015", "s2016", "s2017", "s2018")
# finall convert 's' into a useable data frame, 'sdf'
sdf <- as.data.frame(ss)

# calculate the mean of deaths in 2015 column
mean(sdf$s2015)
# calculate the mean of deaths in 2016 column
mean(sdf$s2016)
# calculate the mean of deaths in 2017 column before hurricane (sept 1-19)
mean(sdf[1:19, 4])
# calculate the mean of deaths in 2017 column after hurricane (sept 20-30)
mean(sdf[20:30, 4])

# Keybook
tab <- s %>% 
  as_data_frame() %>%
  setNames(c("day",header)) %>%
  mutate_all(as.numeric)

mean(tab$"2015")

mean(tab$"2016")

mean(tab$"2017"[1:19])

mean(tab$"2017"[20:30])

# q13
tab <- tab %>% gather(year, deaths, -day) %>%
  mutate(deaths = as.numeric(deaths))
tab


# q14
tab %>% 
ggplot(aes(x = day, y = deaths, color = year)) +
  geom_line()+
  geom_vline(xintercept = 20)

# kebook
tab %>% filter(year < 2018) %>% 
  ggplot(aes(day, deaths, color = year)) +
  geom_line() +
  geom_vline(xintercept = 20) +
  geom_point()