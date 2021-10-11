# Comprehension Check: Trees and Random Forests
# 
# q1
library(rpart)
n <- 1000
sigma <- 0.25
# set.seed(1) # if using R 3.5 or ealier
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat)

# q2
# 
# 
# 
# q3
# dat %>% 
#   mutate(y_hat = predict(fit)) %>% 
#   ggplot() +
#   geom_point(aes(x, y)) #+
  #BLANK
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)
# 
# 
# 
# q4
  # library(randomForest)
  # fit <- #BLANK 
  #   dat %>% 
  #   mutate(y_hat = predict(fit)) %>% 
  #   ggplot() +
  #   geom_point(aes(x, y)) +
  #   geom_step(aes(x, y_hat), col = "red")
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat) 
  dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")
# 
# 
# 
# q5
  plot(fit)
# 
# 
# q6
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
  dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = "red")
 

  
  
## Comprehension Check - Caret Package
  

# q1
  set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
  data("tissue_gene_expression")
  
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rpart",
                    tuneGrid = data.frame(cp = seq(0, 0.1, 0.01))))
  
  ggplot(fit)
# 
# 
# 
# q2
  # set.seed(1991) # if using R 3.5 or earlier
  set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
  fit_rpart <- with(tissue_gene_expression, 
                    train(x, y, method = "rpart",
                          tuneGrid = data.frame(cp = seq(0, 0.10, 0.01)),
                          control = rpart.control(minsplit = 0)))
  ggplot(fit_rpart)
  confusionMatrix(fit_rpart)
# 
# 
# 
# q3
# 
# 
# 
# q4
  # set.seed(1991) # if using R 3.5 or earlier
  set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
  library(randomForest)
  fit <- with(tissue_gene_expression, 
              train(x, y, method = "rf", 
                    nodesize = 1,
                    tuneGrid = data.frame(mtry = seq(50, 200, 25))))
  confusionMatrix(fit)
  ggplot(fit)
# 
# 
# 
# q5
  imp <- varImp(fit)
  imp
# 
# 
# q6
  tree_terms <- as.character(unique(fit_rpart$finalModel$frame$var[!(fit_rpart$finalModel$frame$var == "<leaf>")]))
  tree_terms
  
  data_frame(term = rownames(imp$importance), 
             importance = imp$importance$Overall) %>%
    mutate(rank = rank(-importance)) %>% arrange(desc(importance)) %>%
    filter(term %in% tree_terms)
  
  
# Titanic Exercises Part 1
  if(!require(titanic)) install.packages("titanic")
  library(titanic)    # loads titanic_train data frame
  # 3 significant digits
  options(digits = 3)
  # clean the data - `titanic_train` is loaded with the titanic package
  titanic_clean <- titanic_train %>%
    mutate(Survived = factor(Survived),
           Embarked = factor(Embarked),
           Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
           FamilySize = SibSp + Parch + 1) %>%    # count family members
    dplyr::select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
  # q1
  #set.seed(42) # if using R 3.5 or earlier
  set.seed(42, sample.kind = "Rounding") # if using R 3.6 or later
  test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE) # create a 20% test set
  test_set <- titanic_clean[test_index,]
  train_set <- titanic_clean[-test_index,]
  nrow(train_set)
  nrow(test_set)
  mean(train_set$Survived == 1)
  
  # 
  # 
  # q2
  #set.seed(3)
  set.seed(3, sample.kind = "Rounding")
  # guess with equal probability of survival
  guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
  mean(guess == test_set$Survived)
  # 
  # 
  # 
  # q3
  # a
  train_set %>%
    group_by(Sex) %>%
    summarize(Survived = mean(Survived == 1)) %>%
    filter(Sex == "female") %>%
    pull(Survived)
  train_set %>%
    group_by(Sex) %>%
    summarize(Survived = mean(Survived == 1)) %>%
    filter(Sex == "male") %>%
    pull(Survived)
  
  # b
  sex_model <- ifelse(test_set$Sex == "female", 1, 0)    # predict Survived=1 if female, 0 if male
  mean(sex_model == test_set$Survived)    # calculate accuracy
  
  # 
  # 
  # 
  # q4
  # a
  train_set %>%
    group_by(Pclass) %>%
    summarize(Survived = mean(Survived == 1))
  # b
  class_model <- ifelse(test_set$Pclass == 1, 1, 0)    # predict survival only if first class
  mean(class_model == test_set$Survived)    # calculate accuracy
  # c
  train_set %>%
    group_by(Sex, Pclass) %>%
    summarize(Survived = mean(Survived == 1)) %>%
    filter(Survived > 0.5)
  # d
  sex_class_model <- ifelse(test_set$Sex == "female" & test_set$Pclass != 3, 1, 0)
  mean(sex_class_model == test_set$Survived)
  # q5
  # a
  confusionMatrix(data = factor(sex_model), reference = factor(test_set$Survived))
  confusionMatrix(data = factor(class_model), reference = factor(test_set$Survived))
  confusionMatrix(data = factor(sex_class_model), reference = factor(test_set$Survived))
  # 
  # q6
  F_meas(data = factor(sex_model), reference = test_set$Survived)
  F_meas(data = factor(class_model), reference = test_set$Survived)
  F_meas(data = factor(sex_class_model), reference = test_set$Survived)
  # 
  # 
  # q7
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
  lda_preds <- predict(train_lda, test_set)
  mean(lda_preds == test_set$Survived)
  
  #set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
  qda_preds <- predict(train_qda, test_set)
  mean(qda_preds == test_set$Survived)
# 
# q8
  #set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  train_glm_age <- train(Survived ~ Age, method = "glm", data = train_set)
  glm_preds_age <- predict(train_glm_age, test_set)
  mean(glm_preds_age == test_set$Survived)
# 
  #set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  train_glm <- train(Survived ~ Sex + Pclass + Fare + Age, method = "glm", data = train_set)
  glm_preds <- predict(train_glm, test_set)
  mean(glm_preds == test_set$Survived)
# 
    #set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  train_glm_all <- train(Survived ~ ., method = "glm", data = train_set)
  glm_all_preds <- predict(train_glm_all, test_set)
  mean(glm_all_preds == test_set$Survived)
# 
# q9
  # a
  #set.seed(6)
  set.seed(6, sample.kind = "Rounding") # if using R 3.6 or later
  train_knn <- train(Survived ~ .,
                     method = "knn",
                     data = train_set,
                     tuneGrid = data.frame(k = seq(3, 51, 2)))
  train_knn$bestTune
  
  # b
  ggplot(train_knn)
  # c
  knn_preds <- predict(train_knn, test_set)
  mean(knn_preds == test_set$Survived)
# 
# 
# q10
  #set.seed(8)
  set.seed(8, sample.kind = "Rounding")    # simulate R 3.5
  train_knn_cv <- train(Survived ~ .,
                        method = "knn",
                        data = train_set,
                        tuneGrid = data.frame(k = seq(3, 51, 2)),
                        trControl = trainControl(method = "cv", number = 10, p = 0.9))
  train_knn_cv$bestTune
  
  knn_cv_preds <- predict(train_knn_cv, test_set)
  mean(knn_cv_preds == test_set$Survived)
  
# 
# 
# q11
  # a
  #set.seed(10)
  set.seed(10, sample.kind = "Rounding")    # simulate R 3.5
  train_rpart <- train(Survived ~ ., 
                       method = "rpart",
                       tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)),
                       data = train_set)
  train_rpart$bestTune
  
  rpart_preds <- predict(train_rpart, test_set)
  mean(rpart_preds == test_set$Survived)
  # b
  train_rpart$finalModel # inspect final model
  # make plot of decision tree
  plot(train_rpart$finalModel, margin = 0.1)
  text(train_rpart$finalModel)
  
  
  
# 
# 
# q12
  #set.seed(14)
  set.seed(14, sample.kind = "Rounding")    # simulate R 3.5
  train_rf <- train(Survived ~ .,
                    data = train_set,
                    method = "rf",
                    ntree = 100,
                    tuneGrid = data.frame(mtry = seq(1:7)))
  train_rf$bestTune
  
  rf_preds <- predict(train_rf, test_set)
  mean(rf_preds == test_set$Survived)
  
  varImp(train_rf)    # first row
# 
# 

# Comprehension Check: Ensembles
  
  
  # q1
  models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")
  library(caret)
  library(dslabs)
  library(tidyverse)
  
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  data("mnist_27")
  fits <- lapply(models, function(model){ 
    print(model)
    train(y ~ ., method = model, data = mnist_27$train)
  }) 
  
  names(fits) <- models
  # 
  # 
  # 
  # q2
  pred <- sapply(fits, function(object) 
    predict(object, newdata = mnist_27$test))
  dim(pred)
  # 
  # 
  # 
  # q3
  acc <- colMeans(pred == mnist_27$test$y)
  acc
  mean(acc)
  # 
  # 
  # 
  # q4
  votes <- rowMeans(pred == "7")
  y_hat <- ifelse(votes > 0.5, "7", "2")
  mean(y_hat == mnist_27$test$y)
  # 
  # 
  # 
  # q5
  ind <- acc > mean(y_hat == mnist_27$test$y)
  sum(ind)
  models[ind]
  # 
  # 
  # q6
  acc_hat <- sapply(fits, function(fit) min(fit$results$Accuracy))
  mean(acc_hat)
  # 
  # 
  # q7
  ind <- acc_hat >= 0.8
  votes <- rowMeans(pred[,ind] == "7")
  y_hat <- ifelse(votes>=0.5, 7, 2)
  mean(y_hat == mnist_27$test$y)
  
  ## Comprehension Check - Recommendation Systems
  
  # q1
  library(tidyverse)
  library(lubridate)
  library(dslabs)
  data("movielens")
  movielens %>% group_by(movieId) %>%
    summarize(n = n(), year = as.character(first(year))) %>%
    qplot(year, n, data = ., geom = "boxplot") +
    coord_trans(y = "sqrt") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  # 1995
  # 
  # 
  # q2
  movielens %>% 
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2018 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    top_n(25, rate) %>%
    arrange(desc(rate))
  # 
  # 
  # 
  # q3
  movielens %>% 
    filter(year >= 1993) %>%
    group_by(movieId) %>%
    summarize(n = n(), years = 2018 - first(year),
              title = title[1],
              rating = mean(rating)) %>%
    mutate(rate = n/years) %>%
    ggplot(aes(rate, rating)) +
    geom_point() +
    geom_smooth()
  # 
  # 
  # 
  # q4
  # D
  # 
  # 
  # q5
  movielens <- mutate(movielens, date = as_datetime(timestamp))
  # 
  # 
  # q6
  movielens %>% mutate(date = round_date(date, unit = "week")) %>%
    group_by(date) %>%
    summarize(rating = mean(rating)) %>%
    ggplot(aes(date, rating)) +
    geom_point() +
    geom_smooth()
  # 
  # 
  # q7
  # D
  # 
  # q8
  movielens %>% group_by(genres) %>%
    summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
    filter(n >= 1000) %>% 
    mutate(genres = reorder(genres, avg)) %>%
    ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
    geom_point() +
    geom_errorbar() + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  
  # q9
  # c
  
  
  
  ## Comprehension Check - Regularization
  
  # set.seed(1986) # if using R 3.5 or earlier
  set.seed(1986, sample.kind="Rounding") # if using R 3.6 or later
  n <- round(2^rnorm(1000, 8, 1))
 
   # set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  mu <- round(80 + 2*rt(1000, 5))
  range(mu)
  schools <- data.frame(id = paste("PS",1:1000),
                        size = n,
                        quality = mu,
                        rank = rank(-mu))
  
  schools %>% top_n(10, quality) %>% arrange(desc(quality))
  
  # set.seed(1) # if using R 3.5 or earlier
  set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  mu <- round(80 + 2*rt(1000, 5))
 # set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
  
  scores <- sapply(1:nrow(schools), function(i){
    scores <- rnorm(schools$size[i], schools$quality[i], 30)
    scores
  })
  schools <- schools %>% mutate(score = sapply(scores, mean))
  # q1
  schools %>% top_n(10, score) %>% arrange(desc(score)) %>% dplyr::select(id, size, score)
  
  # 
  # 
  # 
  # q2
  median(schools$size)
  schools %>% top_n(10, score) %>% .$size %>% median()
  
  # 
  # 
  # 
  # q3
  median(schools$size)
  schools %>% top_n(-10, score) %>% .$size %>% median()
  
  # 
  # 
  # 
  # q4
  schools %>% ggplot(aes(size, score)) +
    geom_point(alpha = 0.5) +
    geom_point(data = filter(schools, rank<=10), col = 2)
  
  # 
  # 
  # 
  # q5
  overall <- mean(sapply(scores, mean))
  alpha <- 25
  score_reg <- sapply(scores, function(x)  overall + sum(x-overall)/(length(x)+alpha))
  schools %>% mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))
  
  # 
  # 
  # q6
  alphas <- seq(10,250)
  rmse <- sapply(alphas, function(alpha){
    score_reg <- sapply(scores, function(x) overall+sum(x-overall)/(length(x)+alpha))
    sqrt(mean((score_reg - schools$quality)^2))
  })
  plot(alphas, rmse)
  alphas[which.min(rmse)]
  
  # 
  # 
  # q7
  
  alpha <- alphas[which.min(rmse)]  
  score_reg <- sapply(scores, function(x)
    overall+sum(x-overall)/(length(x)+alpha))
  schools %>% mutate(score_reg = score_reg) %>%
    top_n(10, score_reg) %>% arrange(desc(score_reg))
  
  # 
  # q8
  alphas <- seq(10,250)
  rmse <- sapply(alphas, function(alpha){
    score_reg <- sapply(scores, function(x) sum(x)/(length(x)+alpha))
    sqrt(mean((score_reg - schools$quality)^2))
  })
  plot(alphas, rmse)
  alphas[which.min(rmse)]
  
  
  ## Comprehension Check - Matrix Factorization
  set.seed(1987)
  #if using R 3.6 or later, use `set.seed(1987, sample.kind="Rounding")` instead
  n <- 100
  k <- 8
  Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
  m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
  m <- m[order(rowMeans(m), decreasing = TRUE),]
  y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
  colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                   paste(rep("Science",k), 1:k, sep="_"),
                   paste(rep("Arts",k), 1:k, sep="_"))
  
  # q1
  my_image <- function(x, zlim = range(x), ...){
    colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
    cols <- 1:ncol(x)
    rows <- 1:nrow(x)
    image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
          xlab="", ylab="",  col = colors, zlim = zlim, ...)
    abline(h=rows + 0.5, v = cols + 0.5)
    axis(side = 1, cols, colnames(x), las = 2)
  }
  my_image(y)
  # 
  # 
  # 
  # q2
  my_image(cor(y), zlim = c(-1,1))
  range(cor(y))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
  # 
  # 
  # 
  # q3
  s <- svd(y)
  names(s)
  y_svd <- s$u %*% diag(s$d) %*% t(s$v)
  max(abs(y - y_svd))
  # 
  ss_y <- apply(y^2, 2, sum)
  ss_yv <- apply((y%*%s$v)^2, 2, sum)
  sum(ss_y)
  sum(ss_yv)
  # 
  # 
  # q4
  plot(ss_y)
  plot(ss_yv)
  # 
  # 
  # 
  # q5
  data.frame(x = sqrt(ss_yv), y = s$d) %>%
    ggplot(aes(x,y)) +
    geom_point()
  # 
  # 
  # q6
  sum(s$d[1:3]^2) / sum(s$d^2)
  # 
  # 
  # q7
  # 
  # 
  # 
  # q8
  plot(s$u[,1]*s$d[1], rowMeans(y))
  
  
  
  # q9
  my_image(s$v)
  
  
  # q10
  plot(s$u[,1], ylim = c(-0.25, 0.25))
  plot(s$v[,1], ylim = c(-0.25, 0.25))
  with(s, my_image((u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE])))
  my_image(y)
  # Q11
  resid <- y - with(s,(u[, 1, drop=FALSE]*d[1]) %*% t(v[, 1, drop=FALSE]))
  my_image(cor(resid), zlim = c(-1,1))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
  
  plot(s$u[,2], ylim = c(-0.5, 0.5))
  plot(s$v[,2], ylim = c(-0.5, 0.5))
  with(s, my_image((u[, 2, drop=FALSE]*d[2]) %*% t(v[, 2, drop=FALSE])))
  my_image(resid)
  
  # q12
  resid <- y - with(s,sweep(u[, 1:2], 2, d[1:2], FUN="*") %*% t(v[, 1:2]))
  my_image(cor(resid), zlim = c(-1,1))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
  plot(s$u[,3], ylim = c(-0.5, 0.5))
  plot(s$v[,3], ylim = c(-0.5, 0.5))
  with(s, my_image((u[, 3, drop=FALSE]*d[3]) %*% t(v[, 3, drop=FALSE])))
  my_image(resid)
  
  # q13
  
  resid <- y - with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
  my_image(cor(resid), zlim = c(-1,1))
  axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)
  
  y_hat <- with(s,sweep(u[, 1:3], 2, d[1:3], FUN="*") %*% t(v[, 1:3]))
  my_image(y, zlim = range(y))
  my_image(y_hat, zlim = range(y))
  my_image(y - y_hat, zlim = range(y))
  
  
  ## Comprehension Check - Dimension Reduction
  
  # q1
  data("tissue_gene_expression")
  dim(tissue_gene_expression$x)
  
  pc <- prcomp(tissue_gene_expression$x)
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
  # 
  # 
  # q2
  avgs <- rowMeans(tissue_gene_expression$x)
  data.frame(pc_1 = pc$x[,1], avg = avgs, 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(avgs, pc_1, color = tissue)) +
    geom_point()
  cor(avgs, pc$x[,1])
  # 
  # 
  # 
  # q3
  #BLANK
  x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
  
  pc <- prcomp(x)
  data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
             tissue = tissue_gene_expression$y) %>%
    ggplot(aes(pc_1, pc_2, color = tissue)) +
    geom_point()
  # 
  # 
  # 
  # q4
  for(i in 1:10){
    boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
  }
  # 
  # 
  # 
  # q5
  plot(summary(pc)$importance[3,])
  
  # 
  ## Comprehension Check - Clustering
  # q1
  d <- dist(tissue_gene_expression$x - rowMeans(tissue_gene_expression$x))
  
  # 
  # 
  # 
  # q2
  h <- hclust(d)
  plot(h)
  
  # 
  # 
  # 
  # q3
  if(!require(RColorBrewer)) install.packages("RColorBrewer")
  library(RColorBrewer)
  sds <- matrixStats::colSds(tissue_gene_expression$x)
  ind <- order(sds, decreasing = TRUE)[1:50]
  colors <- brewer.pal(7, "Dark2")[as.numeric(tissue_gene_expression$y)]
  heatmap(t(tissue_gene_expression$x[,ind]), col = brewer.pal(11, "RdBu"), scale = "row", ColSideColors = colors)
  # 
  # 
  
  # q1
  # 
  # 
  # 
  # q2
  # 
  # 
  # 
  # q3
  # 
  # 
  # 
  # q4
  # 
  # 
  # 
  # q5
  # 
  # 
  # q6
  # 
  # 
  # q7
  # 
  # 
  # 
  # q8
  
  
  # q9
  
  
  # q10
  
  
  # q1
  # 
  # 
  # 
  # q2
  # 
  # 
  # 
  # q3
  # 
  # 
  # 
  # q4
  # 
  # 
  # 
  # q5
  # 
  # 
  # q6
  # 
  # 
  # q7
  # 
  # 
  # 
  # q8
  
  
  # q9
  
  
  # q10
  
  
  
# q1
# 
# 
# 
# q2
# 
# 
# 
# q3
# 
# 
# 
# q4
# 
# 
# 
# q5
# 
# 
# q6
# 
# 
# q7
# 
# q8
# q9
# q10











