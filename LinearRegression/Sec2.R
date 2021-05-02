# sec2.1 introduction to linear models####

# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

get_slope <- function(x, y) 
  cor(x, y) * sd(y) / sd(x)

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


# sec2.2 Least Squares Estimates####
# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# the lm function
fit <- lm(son ~ father , data = galton_heights)
fit

summary(fit)

#LSE are Randome Variables

# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

head(lse)

# Plot the distriibution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>%
  ggplot(aes(beta_0))+
  geom_histogram(binwidth = 5, color = "black")
p2 <- lse %>% 
  ggplot(aes(beta_1))+
  geom_histogram(binwidth = 0.1, color = "black")
grid.arrange(p1,p2,ncol = 2)

# summary statistics
sample_n(galton_heights,N,replace= TRUE) %>%
  lm(son~ father, data = .) %>% summary

lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))


# Although interpretation is not straight-forward, it is also useful to know that
# the LSE can be strongly correlated, which can be seen using this code:
lse %>% summarize(cor(beta_0, beta_1))

#Here we standardize the father height, which changes xi to xi - x_bar
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})

cor(lse[1,], lse[2,]) 


# Predicted Variables are Random Variables
# plot predictions and confidence intervals
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# predict Y directly
fit <- galton_heights %>% lm(son ~ father, data = .) 
Y_hat <- predict(fit, se.fit = TRUE)
names(Y_hat)

# plot best fit line
galton_heights %>%
  mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
  ggplot(aes(father, Y_hat))+
  geom_line()

# Assessment  
# q1
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + 
  geom_line() + 
  geom_line(aes(beta1, rss), col=2)

# q3
q3 <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G,
         HR_per_game = HR/G,
         R_per_game = R / G
         ) %>%
  lm(R_per_game ~ BB_per_game + HR_per_game, data = .) %>%
  .$coef

# key
library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))

# q4
# Monte Carlo simulation, N =100
B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

#q5
# a is FAUSE
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth()
# b is TRUE 
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")

# c is TRUE
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


# d is FAUSE
model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model)
data <- as.tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# q7
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

fit <- lm(mother ~ daughter, data = female_heights)
fit$coef[2]
fit$coef[1]
summary (fit)
fit %>% .$coef

# q8
Y_hat <- predict(fit, se.fit = TRUE)

names(Y_hat)
Y_hat$fit[1]
female_heights$mother[1]

# key 
predict(fit)[1]
female_heights$mother[1]

# q9
# ctreate 2002 table 
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, 
         singles = (H - X2B - X3B - HR)/pa, 
         bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)

# create 1999 to 2001
bat_99to01 <- Batting %>% filter ( yearID %in% 1999:2001) %>%
  mutate ( pa = AB + BB,
           singles = (H - X2B - X3B - HR)/pa,
           bb = BB/pa) %>%  
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), 
         mean_bb = mean(bb))

#How many players had a single rate mean_singles of greater
# than 0.2 per plate appearance over 1999-2001?
bat_99to01 %>%
  filter(mean_singles > 0.2) %>%
  nrow()
# key
sum(bat_99_01$mean_singles > 0.2)

#How many players had a BB rate mean_bb of greater 
#than 0.2 per plate appearance over 1999-2001?
bat_99to01 %>%
  filter(mean_bb > 0.2) %>%
  nrow()
#key
sum(bat_99_01$mean_bb > 0.2)

# q10
dat <- inner_join(bat_02,bat_99to01, by = "playerID") 
dat %>%
  summarize(single_cor = cor(singles,mean_singles ),
           BB_cor = cor(bb,mean_bb ))
# q11

library(gridExtra)
p1 <- dat %>%
  ggplot(aes(singles,mean_singles ))+
  geom_point() #+
  #geom_abline(slope = 0.551)

p2 <- dat %>% 
  ggplot(aes(bb,mean_bb ))+
  geom_point()#+
  #geom_abline(slope = 0.717)
grid.arrange(p1,p2,ncol = 2)

# q12
# Fit a linera modle to predict 2002 single given 1999-2001 mean_singles
dat %>%
  lm(singles~mean_singles,data = . ) %>%
  .$coef

# Fit a linear model to predict 2002 bb given 1999-2001 mean_bb.
# What is the coefficient of mean_bb, the slope of the fit?
dat %>%
  lm(bb ~ mean_bb,data = . ) %>%
  .$coef

# sec2.3 Tibbles, do, and broom####
#Advanced dplyr: Tibbles
# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()


#Tibbles: Differences from Data Frames#
# inspect data frame and tibble
Teams
as_tibble(Teams)
# Note that the function was formerly called as.tibble()

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr

as_tibble(Teams)$HR
class(as_tibble(Teams)$HR)
as_tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))


# do#
# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))

# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2], 
             se = summary(fit)$coefficient[2,2])
}

# return the desired data frame
dat %>%  
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%  
  group_by(HR) %>%
  do(slope = get_slope(.))

# dat %>%  
#   group_by(HR) %>%
#   mutate(new = get_slope(.))


# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}

dat %>%  
  group_by(HR) %>%
  do(get_lse(.))


# broom#
# use tidy to return lm estimates and related information as a data frame
library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int = TRUE)

# pipeline with lm, do, tidy
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high)

# dat %>%  
#   group_by(HR) %>%
#   do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
#   filter(term == "(Intercept)") %>%
#   select(HR, estimate, conf.low, conf.high)


# make ggplots
dat %>%  
  group_by(HR) %>%
  do(tidy(lm(R ~ BB, data = .), conf.int = TRUE)) %>%
  filter(term == "BB") %>%
  select(HR, estimate, conf.low, conf.high) %>%
  ggplot(aes(HR, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)

names(glance(fit))

# assesment
# q5 
get_slope <- function(data) {
  fit <- lm(R ~ BB, data = data)
  sum.fit <- summary(fit)
  
  data.frame(slope = sum.fit$coefficients[2, "Estimate"], 
             se = sum.fit$coefficients[2, "Std. Error"],
             pvalue = sum.fit$coefficients[2, "Pr(>|t|)"])
}

# a
dat %>% 
  group_by(HR) %>% 
  do(get_slope)

# b TRUE  This will create a tibble with four columns: 
# HR, slope, se, and pvalue for each level of HR.
dat %>% 
  group_by(HR) %>% # If you forget group_by(), then the results will be a model on the data as a whole, rather than on the data stratified by home runs.
  do(get_slope(.))# The data frame must be passed to get_slope() using (.)
# If you name the results of the do() command such as in the code do(slope = get_slope(.)), that will save all results in a single column called slope


# c
dat %>% 
  group_by(HR) %>% 
  do(slope = get_slope(.))

# d
dat %>% 
  do(get_slope(.))


# q7
# You want to know whether the relationship between home runs and runs per
# game varies by baseball league. You create the following dataset:
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

# a TRUE 
dat %>%
  group_by(lgID) %>%
  do(tidy(lm(R ~ HR, data= .),conf.int = T)) %>%
  filter( term == "HR")
  
# b
dat %>% 
  group_by(lgID) %>% 
  do(glance(lm(R ~ HR, data = .)))

# c
dat %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR")

# d
dat %>% 
  group_by(lgID) %>% 
  do(mod = lm(R ~ HR, data = .))


# q8
library(tidyverse)
library(HistData)
data("GaltonFamilies")
# set.seed(1) # if you are using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if you are using R 3.6 or later
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

head(galton, 10)

galton %>%
  group_by(pair)  %>%
  summarize( n= n())

# q9 find the stongest corelation
galton %>%
  group_by(pair)  %>%
  summarize(cor = cor(parentHeight,childHeight)) %>%
  filter(cor == max(cor))
  
galton %>%
  group_by(pair)  %>%
  summarize(cor = cor(parentHeight,childHeight)) %>%
  filter(cor == min(cor))
  # do(tidy(lm(childHeight ~ parentHeight, data= .),conf.int = T)) %>%
  # filter( term == "parentHeight")

# q10
library(broom)

galton %>%
group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight")
  
galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
   filter(term == "parentHeight") %>%
  # filter(term == "parentHeight" & p.value < .05)
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()

galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight") %>%
  # filter(term == "parentHeight" & p.value < .05)
  select(pair, estimate, conf.low, conf.high) %>%
  ggplot(aes(pair, y = estimate, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar() +
  geom_point()





