# Load necessary libraries
library(cluster)
library(fpc)

# Set the path for datasets
datasets_path = "datasets/"

# Load the dataset
dados_finais <- read.xlsx("datasets/dados_finais.xlsx")
features <- dados_finais[, -1]
features <- na.omit(features)

# Remove rows with missing values
features <- na.omit(features)

# Perform PCA
pca_result <- prcomp(features, scale. = TRUE)

# Summary of PCA
summary(pca_result)

# Scree plot to visualize variance explained by each principal component
plot(pca_result, type = "l", main = "Scree Plot")

# Biplot to visualize the first two principal components
biplot(pca_result, scale = 0)

# Access principal components and standard deviations
pca_components <- pca_result$rotation
pca_std_dev <- pca_result$sdev

# Access the transformed data (scores)
pca_scores <- as.data.frame(pca_result$x)

# Print the first few rows of the transformed data
head(pca_scores)

# K-means clustering
k_means <- kmeans(features, centers = 3)
silhouette_kmeans <- silhouette(k_means$cluster, dist(features))

# PAM (Partitioning Around Medoids)
pam_result <- pam(features, k = 3)
silhouette_pam <- silhouette(pam_result$cluster, dist(features))

# DBSCAN
dbscan_result <- dbscan(features, eps = 0.5, MinPts = 5)
silhouette_dbscan <- silhouette(dbscan_result$cluster, dist(features))

# Hierarchical clustering (single link, complete link, average link)
single_link <- hclust(dist(features), method = "single")
complete_link <- hclust(dist(features), method = "complete")
average_link <- hclust(dist(features), method = "average")

cutree_single <- cutree(single_link, k = 3)
cutree_complete <- cutree(complete_link, k = 3)
cutree_average <- cutree(average_link, k = 3)

silhouette_single <- silhouette(cutree_single, dist(features))
silhouette_complete <- silhouette(cutree_complete, dist(features))
silhouette_average <- silhouette(cutree_average, dist(features))

# Print silhouette scores
cat("Silhouette Score - K-means:", mean(silhouette_kmeans[, 3]), "\n")
cat("Silhouette Score - PAM:", mean(silhouette_pam[, 3]), "\n")
cat("Silhouette Score - DBSCAN:", mean(silhouette_dbscan[, 3]), "\n")
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
