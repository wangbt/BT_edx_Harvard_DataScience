# Sec2#####
# generate training and test sets
set.seed(2, sample.kind = "Rounding") # if using R 3.5 or earlier, remove the sample.kind argument
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]


# guess the outcome####
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
heights %>% group_by(sex) %>% summarize(mean(height), sd(height))
y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex))
mean(y == y_hat)

# examine the accuracy of 10 cutoffs
cutoff <- seq(61, 70)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  mean(y_hat == train_set$sex)
})

data.frame(cutoff, accuracy) %>% 
  ggplot(aes(cutoff, accuracy)) + 
  geom_point() + 
  geom_line() 
max(accuracy)

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


# Assessment####
# library(devtools)
# devtools::install_github("rafalab/dslabs")
# library(dslabs)
# mnist <- read_mnist()
# library(dslabs)
# mnist <- read_mnist()
y <- mnist$train$labels
ncol (mnist$train$images)


##### Confusion Matrix#####
# tabulate each combination of prediction and actual value
#table function create the confusion matrix
table(predicted = y_hat, actual = test_set$sex)
test_set %>% 
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>% 
  summarize(accuracy = mean(y_hat == sex))
prev <- mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

cm <- confusionMatrix(data = y_hat, reference = test_set$sex)
cm$overall["Accuracy"]

#> Accuracy 
#>    0.804
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]
#> Sensitivity Specificity  Prevalence 
#>       0.403       0.921       0.227



# Balanced accuracy and F1 score
# maximize F-score#####
cutoff <- seq(61, 70)
F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(cutoff, F_1) %>% 
  ggplot(aes(cutoff, F_1)) + 
  geom_point() + 
  geom_line()

max(F_1)

best_cutoff <- cutoff[which.max(F_1)]
best_cutoff

y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
sensitivity(data = y_hat, reference = test_set$sex)
#> [1] 0.63
specificity(data = y_hat, reference = test_set$sex)
#> [1] 0.833


#ROC and precision-recall curves
p <- 0.9
n <- length(test_index)
y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)
#>[1] 0.72

# ROC curve
probs <- seq(0, 1, length.out = 10)
guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
guessing %>% qplot(FPR, TPR, data =., xlab = "1 - Specificity", ylab = "Sensitivity")

cutoffs <- c(50, seq(60, 75), 80)
height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("1 - Specificity") +
  ylab("Sensitivity")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point() 
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()


# Assessment####
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

# Q1
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

dat %>% filter(type == "inclass") %>%
  summarize(mean = mean(sex == "Female"))

# key
dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))


# Q2

y_hat <- dat %>% 
  ifelse(type == "inclass","Female","Male") %>%
  factor(levels = levels(y))

y_hat <- ifelse(dat$type == "inclass", "Female" , "Male") %>%
  factor(levels = levels(y))

mean(y==y_hat)

y_hat <- ifelse(x == "inclass", "Female", "Male")
y_hat 
mean(y == y_hat)

# q3

table(y_hat, y)
#table(predicted = y_hat, actual = y)

# q4
sensitivity(data = y_hat, reference = y)
class(y_hat)
class(y)
#q5
specificity(data = y_hat, reference = y)

# q6
confusionMatrix(data = y_hat, reference = y) 
mean(y == "Female")

# q7
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

test <- iris[test_index,]
train <- iris[-test_index,]

# Q8
# x <- 6
# Sepal.Length
cutoff <- seq(min(train$Sepal.Length),max(train$Sepal.Length),0.1)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Sepal.Length > x,"virginica" ,"versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

# Sepal.Width
cutoff <- seq(min(train$Sepal.Width),max(train$Sepal.Width),0.1)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Sepal.Width > x,"virginica" ,"versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

# Petal.Length
cutoff <- seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Petal.Length > x,"virginica" ,"versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

# Petal.Width
cutoff <- seq(min(train$Petal.Width),max(train$Petal.Width),0.1)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Petal.Width > x,"virginica" ,"versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})
max(accuracy)

# key
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}
predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	


# q9
cutoff <- seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
smart <-cutoff [which.max(predictions$Petal.Length)]

y_hat <- 
  ifelse(test$Petal.Length >smart,"virginica" ,"versicolor" ) %>%
  factor(levels = levels(train$Species))
mean(y_hat == test$Species)

# key
predictions <- foo(train[,3])
rangedValues <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs <-rangedValues[which(predictions==max(predictions))]
y_hat <- ifelse(test[,3]>cutoffs[1],'virginica','versicolor')
mean(y_hat==test$Species)


# q10
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo)
max(sapply(predictions,max)	)


# q11
plot(iris,pch=21,bg=iris$Species)


cutoff <- seq(min(train$Petal.Length),max(train$Petal.Length),0.1)
accuracy <- map_dbl(cutoff,function(x){
  y_hat <- ifelse(train$Petal.Length > x,"virginica" ,"versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
  
})


foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1)
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
}


predictions <- apply(train[,-5],2,foo)
sapply(predictions,max)	

#predictions <- foo(train[,3])

rangedValues_length <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
cutoffs_Petal_length <-rangedValues_length[which(predictions$Petal.Length == max(predictions$Petal.Length))]

rangedValues_width <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs_Petal_Width <- rangedValues_width[which(predictions$Petal.Width==max(predictions$Petal.Width))]

y_hat <- ifelse(test[,3]>cutoffs_Petal_length[1]  | test[,4] > cutoffs_Petal_Width[1] ,'virginica','versicolor')
mean(y_hat==test$Species)


data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
plot(iris,pch=21,bg=iris$Species)
# set.seed(2) # if using R 3.5 or earlier
set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]

petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
length_predictions <- sapply(petalLengthRange,function(i){
  y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
width_predictions <- sapply(petalWidthRange,function(i){
  y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
  mean(y_hat==train$Species)
})
width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
mean(y_hat==test$Species)

# Sec 2.2 conditional probabilities####
# comprehension check
# set.seed(1) # if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))


test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

mean(disease)

mean(test[disease==0])
mean(test[disease==1])
