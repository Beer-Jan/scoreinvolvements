# data set used for pam data
playerYearStats <- read.csv("pamdata.csv")

clusterData <- playerYearStats %>%
  select(!c(season, firstName, lastName, teamName))

k <- 7

set.seed(1)
pamResult <- pam(clusterData, k, diss = FALSE)

pca <- prcomp(clusterData)
pca_df <- data.frame(pca$x, cluster=as.factor(pamResult$cluster))

# plot graph to visualise clusters with 2 principal components
# positions correspond to: 1 - outside midfielder, 2 - small forward, 3 - ruck,
# 4 - key defender, 5 - small defender, 6 - clearance midfielder, 7 - key
# forward (determined by inspection)
ggplot(pca_df, aes(x=PC1, y=PC2, color=cluster)) +
  geom_point(size=2) +
  labs(title="K-means Clustering Results",
       x="Principal Component 1",
       y="Principal Component 2") +
  theme_minimal()

