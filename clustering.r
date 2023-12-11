# Load necessary libraries
library(cluster)
library(fpc)

# Remove rows with missing values
df <- na.omit(df)

# K-means clustering
k_means <- kmeans(df, centers = 3)
silhouette_kmeans <- silhouette(k_means$cluster, dist(df))

# PAM (Partitioning Around Medoids)
pam_result <- pam(df, k = 3)
silhouette_pam <- silhouette(pam_result$cluster, dist(df))

# DBSCAN
dbscan_result <- dbscan(df, eps = 0.5, MinPts = 5)
# Check if clusters were found
if (any(dbscan_result$cluster == 0)) {
  # If there are noise points (cluster 0), consider them as a separate cluster
  dbscan_result$cluster[dbscan_result$cluster == 0] <- max(dbscan_result$cluster) + 1
}
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist(df))

# Hierarchical clustering (single link, complete link, average link)
single_link <- hclust(dist(df), method = "single")
complete_link <- hclust(dist(df), method = "complete")
average_link <- hclust(dist(df), method = "average")

cutree_single <- cutree(single_link, k = 3)
cutree_complete <- cutree(complete_link, k = 3)
cutree_average <- cutree(average_link, k = 3)

silhouette_single <- silhouette(cutree_single, dist(df))
silhouette_complete <- silhouette(cutree_complete, dist(df))
silhouette_average <- silhouette(cutree_average, dist(df))

# Print silhouette scores
cat("Silhouette Score - K-means:", mean(silhouette_kmeans[, 3]), "\n")
cat("Silhouette Score - PAM:", mean(silhouette_pam[, 3]), "\n")
cat("Silhouette Score - DBSCAN:", mean(silhouette_dbscan[, 1]), "\n")
cat("Silhouette Score - Hierarchical (Single Link):", mean(silhouette_single[, 3]), "\n")
cat("Silhouette Score - Hierarchical (Complete Link):", mean(silhouette_complete[, 3]), "\n")
cat("Silhouette Score - Hierarchical (Average Link):", mean(silhouette_average[, 3]), "\n")

# Visualize the silhouette plots 
plot(silhouette_kmeans, main = "Silhouette Plot - K-means")
plot(silhouette_pam, main = "Silhouette Plot - PAM")
plot(silhouette_dbscan, main = "Silhouette Plot - DBSCAN")
plot(silhouette_single, main = "Silhouette Plot - Hierarchical (Single Link)")
plot(silhouette_complete, main = "Silhouette Plot - Hierarchical (Complete Link)")
plot(silhouette_average, main = "Silhouette Plot - Hierarchical (Average Link)")
