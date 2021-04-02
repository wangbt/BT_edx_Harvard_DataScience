# Sec3 String Parsing
library(tidyverse)

s <- '10"'
cat(s)
s<- "5'10\""

cat(s)

# gun murdercase 

url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[2]] %>%
  setNames(c("state", "population", "total", "murders","gun_murders","gun_ownership","total_rate","murder_rate","gun_murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

murders_raw$population[1:3]

as.numeric(murders_raw$population[1:3])

commas <- function(x) any(str_detect(x,","))
murders_raw %>% summarize_all(funs(commas))

test_1 <- str_replace_all(murders_raw$population,",","")
test_1 <- as.numeric(test_1,)

test_2 <- parse_number(murders_raw$population)
head(test_2)
test_2

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new %>% head


########sec3.1####
# read in raw murders data from Wikipedia
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>% 
  html_nodes("table") %>% 
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))

# inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)

# murders_raw defined in web scraping video

# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

library(tidyverse)    # includes stringr


commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

test_2 <- parse_number(murders_raw$population)
identical(test_1, test_2)

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)


murders_new <- murders_raw %>% mutate_at(2:3, as.numeric)

murders_new <- murders_raw %>% mutate_all(parse_number)


murders_new %>% head

#####sec 3.2 #####
library(dslabs)
data("reported_heights")
class(reported_heights$height)
x <- as.numeric(reported_heights$height)

head(x)
sum(is.na(x))

reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(n= 12)

alpha <- 1/10^6 
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>% 
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat

# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern <- ","
str_detect(murders_raw$total, pattern) 

# show the subset of strings including "cm"
str_subset(reported_heights$height, "cm")

# use the "or" symbol inside a regex (|)
yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)
str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")
# str_detect(s, ' " ') | str_detect(s, "'")

# highlight the first occurrence of a pattern
str_view(s, pattern)

# highlight all instances of a pattern
str_view_all(s, pattern)

yes <- c("5","6","5'10","5 feet","4'11")
no <- c("", ".", " Five", "six")
s <- c(yes, no)
pattern <- "\\d"
str_detect(s,pattern)
str_view(s, pattern)
str_view_all(s, pattern)
str_view(s,"[56]")











