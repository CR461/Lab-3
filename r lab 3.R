# Load required libraries
library(ggplot2)
library(dplyr)
library(cluster)

# Function to simulate student features
simulate_student_features <- function(n = 100) {
  # Set the random seed
  set.seed(260923)
  
  # Generate unique student IDs
  student_ids <- seq(1, n)
  
  # Simulate student engagement
  student_engagement <- rnorm(n, mean = 50, sd = 10)
  
  # Simulate student performance
  student_performance <- rnorm(n, mean = 60, sd = 15)
  
  # Combine the data into a data frame
  student_features <- data.frame(
    student_id = student_ids,
    student_engagement = student_engagement,
    student_performance = student_performance
  )
  
  # Return the data frame
  return(student_features)
}

# Simulate the data
student_data <- simulate_student_features()

# Perform PCA
pca_result <- prcomp(student_data[, -1], scale. = TRUE)

# Plot variance explained by each principal component
plot(pca_result$sdev^2 / sum(pca_result$sdev^2), xlab = "Principal Component", ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b", pch = 19, col = "blue", main = "Scree Plot")

# Plot PCA biplot
biplot(pca_result)

# Cluster the data using KMeans
set.seed(123)
kmeans_clusters <- kmeans(student_data[, -1], centers = 3, nstart = 25)
student_data$cluster_kmeans <- as.factor(kmeans_clusters$cluster)

# Plot clusters
plot(student_data$student_engagement, student_data$student_performance, col = kmeans_clusters$cluster, pch = 19, 
     xlab = "Student Engagement", ylab = "Student Performance", main = "KMeans Clustering", 
     xlim = range(student_data$student_engagement), ylim = range(student_data$student_performance))
points(kmeans_clusters$centers[, 1], kmeans_clusters$centers[, 2], col = 1:3, pch = 8, cex = 2)

hc <- hclust(dist(student_data[, -1]), method = "complete")
plot(hc)

