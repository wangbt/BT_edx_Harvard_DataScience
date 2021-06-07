# Data Science: Machine Learning
# Prerequisite ####
library(tidyverse)
library(caret)
library(dslabs)
data(heights)
set.seed(2, sample.kind = "Rounding")
library(ggrepel)



data(heights)
heights

class(heights)
class(heights$sex)
class(heights$height)
class("Male")
class(75.00000)

nrow(heights)

heights[777,]

heights$sex[777]

max(heights$height)

which.min(heights$height)

heights %>%
  summarize(mean = mean(height),
            median = median(height),
            male = mean(sex == "Male")
            )
sum(heights$ height > 78)

heights %>%
  filter(sex == "Female") %>%
  summarize(sum = sum(height > 78))

sum(heights$sex == "Female" & heights$height > 78)

# Sec1 ####
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# define the outcome and predictors
y <- heights$sex
x <- heights$height





