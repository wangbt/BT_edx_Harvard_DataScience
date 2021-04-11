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
# [56] means 5 or 6
str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")
str_view(s,"[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits 
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
s <- c(yes, no)

str_detect(yes, pattern)
str_detect(no, pattern)
str_view(s,pattern)

pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems,pattern))

problems[c(2,10,11,12,15)] %>% str_view(pattern)

# inspect examples of entries with problems
str_subset(problems,"inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,4}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% #replace feet, ft,foot with '
  str_replace("inches|in|''|\"","") %>% # remove all inches symbols
  str_detect(pattern)%>%
  sum()

# R does not ignore whitespaces
identical("Hi", "Hi ")

#\\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B","A12B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
data.frame(string = yes,
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))


# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>% 
  sum()

#####groups with regex#######
pattern_without_groups <- "^[4-7],\\d*$"

pattern_with_groups <-  "^([4-7]),(\\d*)$"

yes <- c("5,9","5,11","6,","6,1")
no <- c("5'9",",","2,8","6.1.1")
s <- c(yes,no)

# demonstrate the effect of groups
str_detect(s,pattern_without_groups)
str_detect(s,pattern_with_groups)


# demonstrate difference between str_match and str_extract
str_match(s,pattern_with_groups)
str_match(s,pattern_without_groups)

str_extract(s,pattern_with_groups)
# str_extract(s,pattern_without_groups)



# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>% 
  str_replace(pattern_with_groups, "\\1'\\2") %>% head


#######Testing and Improving#######
# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}# identify entries with problems


problems <- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
length(problems)

converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]    # show problems



# Assesment####
# q2
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}

not_inches(c(175))
not_inches(c("5'8\""))
not_inches(c(70))
sum(not_inches(c(85)))

# q4
s <-   c("70", "5 ft" , "4'11" , "" ,".","Six feet")
pattern <- "\\d|ft"

str_view_all(s, pattern)

# q5
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

# q6
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

# q7
animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

# q8
animals <- c("moose", "monkey", "meerkat", "mountain lion")
pattern <- "mo*"
pattern <- "mo?"
pattern <- "mo+"
pattern <- "moo*"


str_detect(animals, pattern)

# q9
schools <-c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts", "University Georgia" ,"U California","California State University")
schools_corr <- schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
final <- c("University of Kentucky","University of New Hampshire","University of Massachusetts","University of Georgia","University of California","California State University")
# final
identical (schools_corr , final)

# q10
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# q11
problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

# q12
converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]

# q13
yes <- c("5 feet 7inches", "5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>%
# str_replace(     "feet|foot|ft"     , "'") %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  
# str_replace(     "inches|in|''|\""     , "") %>%
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)


# Sec3.3 Separate with regex####
# first example - normally formatted heights
s <- c("5'10", "6'1")
tab <- data.frame(x = s)

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"","5'8inches")
tab <- data.frame(x = s)

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet","inches"), sep = "'", fill = "right")
tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

# case1 Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6'
# - our pattern requires that inches be included.
yes <- c("5", "6", "5")
no <- c("5'", "5''", "5'4")
s <- c(yes, no)
str_replace(s, "^([4-7])$", "\\1'0")

# case2 and 4
str_replace(s, "^([56])'?$", "\\1'0")

# str_detect(s, "^([56])'?$")

# case 3
# permit zero or one period . followed by zero or more digits. So we will use both ? and *.
pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

# case 5 Some entries are in meters and some of these use European decimals: 1.6, 1,7.
yes <- c("1,7", "1, 8", "2, " )
no <- c("5,8", "5,3,2", "1.7")
s <- c(yes, no)
str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")


# Trimming
s <- "Hi "
cat(s)
identical(s, "Hi")
str_trim("5 ' 9 ")

identical(str_trim(s), "Hi")

# to upper and to lowecase.names
s <- c("Five feet eight inches")
str_to_lower(s)

# Putting it into a function
convert_format <- function(s){
  s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}

# We can also write a function that converts words to numbers:
words_to_numbers <- function(s){
  str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# Now we can see which problematic entries remain:
converted <- problems %>% words_to_numbers %>% convert_format
remaining_problems <- converted[not_inches_or_cm(converted)]
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


# Putting it all together
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest <- 50
tallest <- 84
new_heights <- reported_heights %>% 
  mutate(original = height, 
         height = words_to_numbers(height) %>% convert_format()) %>%
  extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>% 
  mutate_at(c("height", "feet", "inches"), as.numeric) %>%
  mutate(guess = 12*feet + inches) %>%
  mutate(height = case_when(
    !is.na(height) & between(height, smallest, tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

# We can check all the entries we converted using the following code:
new_heights %>%
  filter(not_inches(original)) %>%
  select(original, height) %>% 
  arrange(height) %>%
  View()

# Let's take a look at the shortest students in our dataset using the following code:
new_heights %>% arrange(height) %>% head(n=7)



# Sec 3.3 String processing Part 3####
# read raw murders data line by line
filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head()


# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",") 
x %>% head()
col_names <- x[[1]]
x <- x[-1]

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>% 
  as.data.frame() 

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE) 
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# case study: Extracting a Table from a PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates 

library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)

raw_data_research_funding_rates <- txt[2]

data("raw_data_research_funding_rates")

raw_data_research_funding_rates %>% head

tab <- str_split(raw_data_research_funding_rates, "\n")

tab <- tab[[1]]

tab %>% head

the_names_1 <- tab[3]
the_names_2 <- tab[4]

the_names_1
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2
the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()


# We can see that the objects are identical:
identical(research_funding_rates, new_research_funding_rates)

#Recoding####
# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>% 
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>% 
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country) 

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country, 
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# Assesment 3####
schedule <- data.frame(day = c ('Monday','Tuesday'),
                       staff = c("Mandy, Chris and Laura","Steve, Ruth and Frank") )

schedule

str_split(schedule$staff, ", | and ")
str_split(schedule$staff, ",\\s|\\sand\\s")




tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ")) %>% 
  unnest(cols = c('day','staff'))

tidy <- schedule %>% 
  mutate(staff = str_split(staff, ", | and ", simplify = TRUE)) %>% 
  unnest(cols = c())

tidy <- separate(schedule, staff, into = c("s1","s2","s3"), sep =" ,") %>% 
  gather(key = s, value = staff, s1:s3)

# Assesment part 2 ####
# Import raw Brexit referendum polling data from Wikipedia:
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)

head(polls)

colnames(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")

nrow(polls %>% filter (str_detect(remain, "%")))

#key#
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls[str_detect(polls$remain, "%"), -9]
nrow(polls)

as.numeric(str_remove(polls$remain, "%"))
as.numeric(polls$remain)/100
parse_number(polls$remain)
str_remove(polls$remain, "%")/100
as.numeric(str_replace(polls$remain, "%", ""))/100
parse_number(polls$remain)/100

# function_name(polls$undecided, "arg1", "arg2").
str_replace_all(polls$undecided,"N/A", "0")

# The end date of the poll will always be one or two digits, 
# followed by a space, followed by the month as one or more 
# letters (either capital or lowercase). In these data, 
# all month abbreviations or names have 3, 4 or 5 letters.

a <- "\\d?\\s[a-zA-Z]?" # F
b <- "\\d+\\s[a-zA-Z]+" # T
c <- "\\d+\\s[A-Z]+"  # F
d <- "[0-9]+\\s[a-zA-Z]+" #T
e <- "\\d{1,2}\\s[a-zA-Z]+" #T
f <- "\\d{1,2}[a-zA-Z]+"  # F
g <- "\\d+\\s[a-zA-Z]{3,5}"  # T


temp <- str_extract_all(polls$dates, a)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, b)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, c)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, d)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, e)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, f)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)


temp <- str_extract_all(polls$dates, g)
end_date <- sapply(temp, function(x) x[length(x)]) # take last element (handles polls that cross month boundaries)
