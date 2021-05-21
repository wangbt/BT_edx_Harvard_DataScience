# Final assessment #####
library(dslabs)
library(tidyr)
library(tidyverse)
data("research_funding_rates")
research_funding_rates

# Q1
tab <- research_funding_rates %>% 
  mutate(not_awards_mem= applications_men - awards_men, 
         not_awards_women = applications_women - awards_women) %>%
  select(awards_men,awards_women,not_awards_mem,not_awards_women) %>%
  
  summarize(yes_men = sum(awards_men),
            yes_women = sum(awards_women),
            no_men = sum(not_awards_mem),
            no_women = sum(not_awards_women)) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)

# key
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women#,
            #yesrate_men = awards_men/applications_men,
            #yesrate_women = awards_women / applications_women
            
            ) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

# q2 What is the percentage of men awarded?
# key
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)
#What is the percentage of women awarded?
# key
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

# q3
chisq_test <- two_by_two %>% 
  select(-awarded) %>% 
  chisq.test() %>%
  tidy()

# key
two_by_two %>% 
  select(-awarded) %>% 
  chisq.test() %>% 
  tidy() %>% 
  pull(p.value)


# Q4
dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat


dat %>% 
  ggplot(aes(discipline, success, col = gender, size = applications)) + 
  geom_point()

# key
dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()
