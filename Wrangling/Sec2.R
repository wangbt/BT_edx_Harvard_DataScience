###########Sec2(in book chapter 4)#########
library(tidyverse)
library(dslabs)

path <- system.file("extdata",package = "dslabs")
filename <- file.path(path,"fertility-two-countries-example.csv")
wide_data<-read_csv(filename)


data("gapminder")
tidy_data <- gapminder %>%
  filter(country %in% c("South Korea","Gernamy"))%>%
  select(country,year,fertility)
# head(tidy_data)


new_tidy_data <- wide_data %>%
  gather(year,fertility,`1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)

class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()


# spread tidy data to generate wide data
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)

path <- system.file("extdata",package = "dslabs")
filename <- file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)
select(raw_dat, 1:5)

#gather all columns except country
dat <- raw_dat %>%
  gather(key,value,-country)
head(dat)
dat$key[1:5]

#separate on underscores 
dat %>% separate(key, c("year","variablename"),"_")
dat %>% separate(key, c("year", "variable_name"))

# split on all underscores, pad empty cells with NA
dat %>% separate(key,c("year","fist_variable","second_varuable"),
                 fill = "right")

# split on first underscore but keep life_expectancy merged
dat %>% separate(key,c("year","variable_name"),sep = "_",
                 extra = "merge")

# separate then spread
dat %>% separate(key,c("year","variable_name"),sep = "_",
                  extra = "merge")%>%
  spread(variable_name,value)

# separate then unite
dat %>%
  separate(key,c("year","first_variable_name","second_variable_name"),fill = "right") %>%
  unite(variable_name, first_variable_name,second_variable_name, sep = "_")

# full code for tidying data
dat %>%
  separate(key,c("year","first_variable_name","second_variable_name"),fill = "right")%>%
  unite(variable_name,first_variable_name,second_variable_name,sep = "_")%>%
  spread(variable_name,value)%>%
  rename(fertility = fertility_NA)

####Assessment#####
# Q10
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))


co2_tidy <- gather(co2_wide,month,co2,-year)

# co2_wide %>% gather(month,co2,-year)

# Q11
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# Q12
data(admissions)
dat <- admissions %>% select(-applicants)

dat_tidy <- spread(dat,gender,admitted)

# Q13
tmp <- gather(admissions, key, value, admitted:applicants)
tmp2 <- unite(tmp,col = "column_name", c(key,gender),sep = "_")

# Q14

# major, admitted_men, admitted_women, applicants_men and applicants_women
spread(tmp2,column_name,value)


###########Sec2.2##########
data(murders)
head(murders)
data(polls_us_election_2016)
head(results_us_election_2016)

identical(results_us_election_2016$state,murders$state)

tab <- left_join(murders,results_us_election_2016,by= "state")
head(tab)

tab1 <- slice(murders, 1:6) %>% select(state,population)
tab1

tab2 <- slice(results_us_election_2016, c(1:3,5,7:8))%>% select(state,electoral_votes)
tab2

tab1%>%left_join(tab2)
tab1%>%right_join(tab2)

inner_join(tab1,tab2)
full_join(tab1,tab2)
semi_join(tab1,tab2)
semi_join(tab2,tab1)
anti_join(tab1,tab2)

# binding
bind_cols(a = 1:3, b = 4:6)
tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
new_tab <- bind_cols(tab1,tab2,tab3)
new_tab


#Assessment#
# Q4
df1 <- bind_cols(x = c("a","b"),y = c("a","a"))
df2 <- bind_cols(x = c("a","a"),y = c("a","b"))
setdiff(df1,df2)

# Q5
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()

Master %>% as_tibble()

top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names)%>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

awd_2016 <- AwardsPlayers %>% filter(yearID == 2016) 

length(intersect(top$playerID,awd_2016$playerID))

length(setdiff(awd_2016$playerID,top$playerID))



####sec2.3#####
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
# h
# html_text(h)

tab <-  h %>% html_nodes("table")
class(tab)
tab1 <- tab[[1]] %>%html_table
class(tab1)
tab <- tab1 %>% setNames(c("state", "population", "total", "murder_rate")) 
head(tab)
# tab[[2]]
tab2 <- tab[[2]] %>% html_table
class(tab2)
tab <- tab2 %>% setNames(c("state", "population", "total", "murder_rate")) 
head(tab)


###guacamole recipe ####
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient--CheckboxLabel") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
# guacamole
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient--CheckboxLabel") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
  }
get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")

# sec2.3 Assessment####
# Q1
library(rvest)
library(tidyverse)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

nodes <- html_nodes(h, "table")

html_text(nodes[[8]])

html_table(nodes[[8]])

# lapply(nodes[3:6],html_table)
sapply(nodes[1:4], html_table)

# q2
length_n <- length(nodes)
html_table(nodes[length_n])
html_table(nodes[length_n-1])
html_table(nodes[length_n-2])
# lapply(nodes[length_n - 1], html_table)
# sapply(nodes[length_n - 1], html_table)

#q3

tab_1 <- html_table(nodes[[10]],head = TRUE)  %>%select (!No.)
# tab_1 <- html_table(nodes[10],head = TRUE)
class(tab_1)

tab_2 <- html_table(nodes[[19]],head = TRUE) 

class(tab_2)

full_join(tab_1,tab_2,  by = "Team")

# key
tab_1 <- html_table(nodes[[10]])
tab_2 <- html_table(nodes[[19]])
col_names <- c("Team", "Payroll", "Average")
tab_1 <- tab_1[-1, -1]
tab_2 <- tab_2[-1,]
names(tab_2) <- col_names
names(tab_1) <- col_names
full_join(tab_1,tab_2, by = "Team")


# q4
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
length(tab)

# q5
for(n in 1:length(tab)) {
  tab_n <- html_table(tab[[n]], fill = TRUE)
  if (ncol(tab_n) == 9 & names(tab_n)[1] == "Date(s) conducted")
  {print (n) break}
  }

tab[[5]] %>% html_table(fill = FALSE) %>% names()    # inspect column names

