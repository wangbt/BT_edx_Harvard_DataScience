#Data Science Linear Regression
# sec1.1 ####
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

# Code: Scatterplot of the relationship between HRs and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)


#Code: Scatterplot of the relationship between stolen bases and wins
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Code: Scatterplot of the relationship between bases on balls and runs
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# Assessment 1.1 ####
# You want to know whether teams with more at-bats per game have 
# more runs per game.
# A
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  ggplot(aes(AB, R)) + 
  geom_point(alpha = 0.5)

# B TRUE
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

# C
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) + 
  geom_line()

# D
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point()

# q6
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(AB_per_game = AB/G, R_per_game = R/G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

# q7
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(W_per_game = W/G, E_per_game = E/G) %>%
  ggplot(aes(E_per_game, W_per_game)) +
  geom_point(alpha = 0.5)

# q8
Teams %>% filter(yearID %in% 1961:2001 ) %>%
  mutate(X2B_per_game = X2B/G, X3B_per_game = X3B/G) %>%
  ggplot(aes(X2B_per_game, X3B_per_game)) +
  geom_point(alpha = 0.5)

# sec1.2####
# create the dataset
library(tidyverse)
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

# means and standard deviations
galton_heights %>%
  summarize(mean(father), sd(father), mean(son), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5)

galton_heights <- GaltonFamilies %>% 
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight) 

galton_heights %>%
  summarize((cor(father,son)))

rho <- mean(scale(x)*scale(y))
galton_heights %>% summarize(r = cor(father, son)) %>% pull(r)

# scale(c(1,2,2.5))

# Sample Correlation is a Random Variable
# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarize(r = cor(father, son))
R

# Monte Carlo simulation to show distribution of sample correlation
B <- 1000
N <- 25
R <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    summarize(r = cor(father, son)) %>%
    pull(r)
})
length(R)

qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))


# assessment 1.2
# q7 
Teams %>% filter(yearID %in%  1961:2001) %>%
  mutate(AB_per_game = AB/G, 
         R_per_game = R/G,
         W_per_game = W/G, 
         E_per_game = E/G,
         X2B_per_game = X2B/G, 
         X3B_per_game = X3B/G) %>%
  #summarize(r = cor(R_per_game,AB_per_game))
  # summarize(r = cor(W_per_game,E_per_game))
  summarize(r = cor(X2B_per_game, X3B_per_game))


# sec 1.3####
# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarize(avg = mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata = factor(round(father))) %>%
  ggplot(aes(father_strata, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg = mean(son)) %>%
  ggplot(aes(father, son_conditional_avg)) +
  geom_point()

# plot the standardized heights against each other, 
# son versus father, with a line that# has a slope 
# equal to the correlation.

r_1 <- galton_heights %>% summarize(r = cor(father,son)) %>% .$r

galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father)%>%
  summarize(son = mean(son)) %>%
  mutate(z_father = scale(father),z_son = scale(son)) %>%
  ggplot(aes(z_father,z_son)) +
  geom_point()+
  geom_abline(intercept = 0, slope = r)



# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x
# identical(r,r_1)

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# plot in standard units
galton_heights %>%
  ggplot (aes(scale(father),scale(son))) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = r)
  

# facet plot with z
galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)


# compute a regression line to predict the son's height from the father's height
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m_1 <-  r * s_y / s_x
b_1 <- mu_y - m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2 <-  r * s_x / s_y
b_2 <- mu_x - m_2*mu_y

data.frame(m = c(m_1 , m_2),
          b = c(b_1,b_2))

# cor(galton_heights$son, galton_heights$father)

# Assessment 1.3####
# q8 mean sd of mother and daughter, and correlation
set.seed(1989) #if you are using R 3.5 or earlier
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_mother <- mean(female_heights$mother)
mu_daughter <- mean(female_heights$daughter)
s_mother <- sd(female_heights$mother)
s_daughter <- sd(female_heights$daughter)
r_f <- cor(female_heights$mother,female_heights$daughter)

# q9 slope and intercept, mother hight increas 1 inch,daughter increas how much.
m_f <- r_f*s_daughter/s_mother
b_f <- mu_daughter - m_f*mu_mother
r_f*s_daughter/s_mother

# q10 What percent of the variability in daughter heights is explained by the 
# mother's height?
r_f^2*100

# q11 mother is 60 inch high, what is the conditional expected value of 
# daughter's height given mother's height
x <- 60
m_f * x + b_f




