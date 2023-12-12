# Load necessary libraries
library(cluster)
library(fpc)
library(ggplot2)

# Remove rows with missing values
df <- na.omit(df)

# Function to calculate total within-cluster sum of squares (WCSS)
calculate_wcss <- function(data, k) {
  kmeans_model <- kmeans(data, centers = k)
  return(kmeans_model$tot.withinss)
}

# Range of k values to explore
k_values <- 2:10  # You can adjust the range as needed

# Calculate WCSS for each k
wcss_values <- sapply(k_values, function(k) calculate_wcss(df, k))

# Plot the elbow curve
elbow_plot <- ggplot() +
  geom_line(aes(x = k_values, y = wcss_values), color = "blue") +
  geom_point(aes(x = k_values, y = wcss_values), color = "red", size = 2) +
  labs(title = "Elbow Method for Optimal k",
       x = "Number of Clusters (k)",
       y = "Within-Cluster Sum of Squares (WCSS)") +
  theme_minimal()

print(elbow_plot)

# Identify the optimal k (elbow point)
optimal_k <- k_values[which.min(diff(wcss_values))]
cat("Optimal k:", optimal_k, "\n")

# K-means clustering
k_means <- kmeans(df, centers = optimal_k)
silhouette_kmeans <- silhouette(k_means$cluster, dist(df))

# PAM (Partitioning Around Medoids)
pam_result <- pam(df, k = optimal_k)
silhouette_pam <- silhouette(pam_result$cluster, dist(df))

# DBSCAN
dbscan_result <- dbscan(df, eps = 0.5, MinPts = 5)
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist(df))

# Hierarchical clustering (single link, complete link, average link)
single_link <- hclust(dist(df), method = "single")
complete_link <- hclust(dist(df), method = "complete")
average_link <- hclust(dist(df), method = "average")

cutree_single <- cutree(single_link, k = optimal_k)
cutree_complete <- cutree(complete_link, k = optimal_k)
cutree_average <- cutree(average_link, k = optimal_k)

silhouette_single <- silhouette(cutree_single, dist(df))
silhouette_complete <- silhouette(cutree_complete, dist(df))
silhouette_average <- silhouette(cutree_average, dist(df))

# Print silhouette scores
cat("Silhouette Score - K-means:", mean(silhouette_kmeans[, optimal_k]), "\n")
cat("Silhouette Score - PAM:", mean(silhouette_pam[, ]), "\n")
# Not applicable
#cat("Silhouette Score - DBSCAN:", mean_silhouette_dbscan, "\n")
cat("Silhouette Score - Hierarchical (Single Link):", mean(silhouette_single[, optimal_k]), "\n")
cat("Silhouette Score - Hierarchical (Complete Link):", mean(silhouette_complete[, optimal_k]), "\n")
cat("Silhouette Score - Hierarchical (Average Link):", mean(silhouette_average[, optimal_k]), "\n")

# Visualize the silhouette plots 
plot(silhouette_kmeans, main = "Silhouette Plot - K-means")
plot(silhouette_pam, main = "Silhouette Plot - PAM")
# Not applicable
#plot(silhouette_dbscan, main = "Silhouette Plot - DBSCAN")
plot(silhouette_single, main = "Silhouette Plot - Hierarchical (Single Link)")
plot(silhouette_complete, main = "Silhouette Plot - Hierarchical (Complete Link)")
plot(silhouette_average, main = "Silhouette Plot - Hierarchical (Average Link)")
