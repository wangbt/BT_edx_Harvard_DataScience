# sec 7 Comprehensive Check
options(digits = 3)
library(matrixStats)
library(tidyverse)
library(caret)
#install.packages("dslabs")
library(dslabs)
data(brca)

# q1
dim(brca$x)[1]
dim(brca$x)[2]
mean(brca$y == "M")
which.max(colMeans(brca$x))
which.min(colSds(brca$x))
# 
# 
# 
# q2
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")
sd(x_scaled[,1])

# 
median(x_scaled[,1])

# 
# 
# q3
d_samples <- dist(x_scaled)
dist_BtoB <- as.matrix(d_samples)[1, brca$y == "B"]
mean(dist_BtoB[2:length(dist_BtoB)])
# 
dist_BtoM <- as.matrix(d_samples)[1, brca$y == "M"]
mean(dist_BtoM)
# 
# 
# q4
d_features <- dist(t(x_scaled))
heatmap(as.matrix(d_features), labRow = NA, labCol = NA)

# 
# 
# q5
h <- hclust(d_features)
groups <- cutree(h, k = 5)
split(names(groups), groups)
# 
# 
# q6
pca <- prcomp(x_scaled)
summary(pca)
# 
# 
# q7
# 
data.frame(pca$x[,1:2], type = brca$y) %>%
  ggplot(aes(PC1, PC2, color = type)) +
  geom_point()
# 
# 
# q8
data.frame(type = brca$y, pca$x[,1:10]) %>%
  gather(key = "PC", value = "value", -type) %>%
  ggplot(aes(PC, value, fill = type)) +
  geom_boxplot()

# q9
# set.seed(1) if using R 3.5 or earlier
set.seed(1, sample.kind = "Rounding")    # if using R 3.6 or later
test_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
test_x <- x_scaled[test_index,]
test_y <- brca$y[test_index]
train_x <- x_scaled[-test_index,]
train_y <- brca$y[-test_index]


mean(train_y == "B")


mean(test_y == "B")


# q10
predict_kmeans <- function(x, k) {
  centers <- k$centers    # extract cluster centers
  # calculate distance to cluster centers
  distances <- sapply(1:nrow(x), function(i){
    apply(centers, 1, function(y) dist(rbind(x[i,], y)))
  })
  max.col(-t(distances))  # select cluster with min distance to center
}


  # A
set.seed(3, sample.kind = "Rounding")    # if using R 3.6 or later
k <- kmeans(train_x, centers = 2)
kmeans_preds <- ifelse(predict_kmeans(test_x, k) == 1, "B", "M")
mean(kmeans_preds == test_y)


  # B
sensitivity(factor(kmeans_preds), test_y, positive = "B")

sensitivity(factor(kmeans_preds), test_y, positive = "M")



# q11 
train_glm <- train(train_x, train_y, method = "glm")
glm_preds <- predict(train_glm, test_x)
mean(glm_preds == test_y)
# 


# q12 
train_lda <- train(train_x, train_y, method = "lda")
lda_preds <- predict(train_lda, test_x)
mean(lda_preds == test_y)
# 
train_qda <- train(train_x, train_y, method = "qda")
qda_preds <- predict(train_qda, test_x)
mean(qda_preds == test_y)
# 


# q13
# set.seed(5)
set.seed(5, sample.kind = "Rounding") # simulate R 3.5
train_loess <- train(train_x, train_y, method = "gamLoess")
loess_preds <- predict(train_loess, test_x)
mean(loess_preds == test_y)
# 
# 
# q14
# set.seed(7)
set.seed(7, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(k = seq(3, 21, 2))
train_knn <- train(train_x, train_y,
                   method = "knn", 
                   tuneGrid = tuning)
train_knn$bestTune

# What is the accuracy of the kNN model on the test set?
  
knn_preds <- predict(train_knn, test_x)
mean(knn_preds == test_y)
# 
# 
# q15
# a
set.seed(9, sample.kind = "Rounding") # simulate R 3.5
tuning <- data.frame(mtry = c(3, 5, 7, 9))
train_rf <- train(train_x, train_y,
                  method = "rf",
                  tuneGrid = tuning,
                  importance = TRUE)
train_rf$bestTune
# 
rf_preds <- predict(train_rf, test_x)
mean(rf_preds == test_y)
# 
varImp(train_rf)

# b

# q16
# a
ensemble <- cbind(glm = glm_preds == "B", lda = lda_preds == "B", qda = qda_preds == "B", loess = loess_preds == "B", rf = rf_preds == "B", knn = knn_preds == "B", kmeans = kmeans_preds == "B")
ensemble_preds <- ifelse(rowMeans(ensemble) > 0.5, "B", "M")
mean(ensemble_preds == test_y)
# b
models <- c("K means", "Logistic regression", "LDA", "QDA", "Loess", "K nearest neighbors", "Random forest", "Ensemble")
accuracy <- c(mean(kmeans_preds == test_y),
              mean(glm_preds == test_y),
              mean(lda_preds == test_y),
              mean(qda_preds == test_y),
              mean(loess_preds == test_y),
              mean(knn_preds == test_y),
              mean(rf_preds == test_y),
              mean(ensemble_preds == test_y))
data.frame(Model = models, Accuracy = accuracy)
# 
# q17
# 
# 
# q18 
