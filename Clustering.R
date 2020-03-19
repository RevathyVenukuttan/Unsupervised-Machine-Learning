#############################################################
################### HIERARCHICAL CLUSTERING #################
#############################################################


# DELTA (0-4Hz)

output.delta.long.kmeans <- read.csv('output_delta_long_kmeans.csv')
output.delta.wide <- output.delta.long.kmeans %>% spread(Frequency, Amplitude)
write.csv(output.delta.wide, "output_delta_wide.csv")
ari_delta <- output.delta.wide[,1] # for testing the ARI index

#Label the rows in order to do external validation later
rownames(output.delta.wide) <- output.delta.wide[,1]
output.delta.wide <- output.delta.wide[-c(1)]

# Compute hierarchical clustering with AGNES
hc.Ward.delta <- agnes(output.delta.wide, method = "ward")
pltree(hc.Ward.delta, cex = 0.6, hang = -1, main = "Dendrogram of agnes", xlab = "Delta [0-4 Hz]")

# Cut agnes() tree into 2 groups
sub_grp.delta.2 <- cutree(as.hclust(hc.Ward.delta), k = 2)

# Add rectangle markings on the graph
delta.plot <- rect.hclust(hc.Ward.delta, k = 2, border = 2:5)

# Number of members in each cluster
table(sub_grp.delta.2)

# Agglomerative coefficient
hc.Ward.delta$ac

# Linkage methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute coefficients
ac <- function(x) {
  agnes(output.delta.wide, method = x)$ac
}

# Table of coefficients for each linkage method
map_dbl(m, ac)


# Optimal number of clusters using average silhouette width
delta.cluster <- fviz_nbclust(output.delta.wide, FUN = hcut, method = "silhouette") + labs(title = "Optimal number of clusters for Delta")

# Extract avg. silhouette for 1-10 clusters
delta.cluster$data

# Test Adjusted Random Index (ARI)
cluster_similarity(ari_delta, sub_grp.delta.2, similarity="rand")

# THETA (4-8Hz)

output.theta.long.kmeans <- read.csv('output_theta_long_kmeans.csv')
output.theta.wide <- output.theta.long.kmeans %>% spread(Frequency, Amplitude)
write.csv(output.theta.wide, "output_theta_wide.csv")
ari_theta <- output.theta.wide[,1] # for testing the ARI index

#Label the rows in order to do external validation later
rownames(output.theta.wide) <- output.theta.wide[,1]
output.theta.wide <- output.theta.wide[-c(1)]

# Compute hierarchical clustering with AGNES
hc.Ward.theta <- agnes(output.theta.wide, method = "ward")
pltree(hc.Ward.theta, cex = 0.6, hang = -1, main = "Dendrogram of agnes", xlab = "Theta [4-8 Hz]")

# Cut agnes() tree into ten groups
sub_grp.theta.10 <- cutree(as.hclust(hc.Ward.theta), k = 10)

# Add rectangle markings on the graph
theta.plot <- rect.hclust(hc.Ward.theta, k = 10, border = 2:5)

# Number of members in each cluster
table(sub_grp.theta.10)

# Agglomerative coefficient
hc.Ward.theta$ac

# Linkage methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute coefficients
ac <- function(x) {
  agnes(output.theta.wide, method = x)$ac
}

# Table of coefficients for each linkage method
map_dbl(m, ac)


# Optimal number of clusters using average silhouette width
theta.cluster <- fviz_nbclust(output.theta.wide, FUN = hcut, method = "silhouette") + labs(title = "Optimal number of clusters for Theta")

# Extract avg. silhouette for 1-10 clusters
theta.cluster$data

# Test Adjusted Random Index (ARI)
cluster_similarity(ari_theta, sub_grp.theta.10, similarity="rand")

# ALPHA (8-16Hz)

output.alpha.long.kmeans <- read.csv('output_alpha_long_kmeans.csv')
output.alpha.wide <- output.alpha.long.kmeans %>% spread(Frequency, Amplitude)
write.csv(output.alpha.wide, "output_alpha_wide.csv")
ari_alpha <- output.alpha.wide[,1] # for testing the ARI index

#Label the rows in order to do external validation later
rownames(output.alpha.wide) <- output.alpha.wide[,1]
output.alpha.wide <- output.alpha.wide[-c(1)]

# Compute hierarchical clustering with AGNES
hc.Ward.alpha <- agnes(output.alpha.wide, method = "ward")
pltree(hc.Ward.alpha, cex = 0.6, hang = -1, main = "Dendrogram of agnes", xlab = "Alpha [8-16 Hz]")

# Cut agnes() tree into 2 groups
sub_grp.alpha.2 <- cutree(as.hclust(hc.Ward.alpha), k = 2)

# Add rectangle markings on the graph
alpha.plot <- rect.hclust(hc.Ward.alpha, k = 2, border = 2:5)

# Number of members in each cluster
table(sub_grp.alpha.2)

# Agglomerative coefficient
hc.Ward.alpha$ac

# Linkage methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute coefficients
ac <- function(x) {
  agnes(output.alpha.wide, method = x)$ac
}

# Table of coefficients for each linkage method
map_dbl(m, ac)


# Optimal number of clusters using average silhouette width
alpha.cluster <- fviz_nbclust(output.alpha.wide, FUN = hcut, method = "silhouette") + labs(title = "Optimal number of clusters for Alpha")

# Extract avg. silhouette for 1-10 clusters
alpha.cluster$data

# Test Adjusted Random Index (ARI)
cluster_similarity(ari_alpha, sub_grp.alpha.2, similarity="rand")

# BETA (16-32Hz)

output.beta.long.kmeans <- read.csv('output_beta_long_kmeans.csv')
output.beta.wide <- output.beta.long.kmeans %>% spread(Frequency, Amplitude)
write.csv(output.beta.wide, "output_beta_wide.csv")
ari_beta <- output.beta.wide[,1] # for testing the ARI index

#Label the rows in order to do external validation later
rownames(output.beta.wide) <- output.beta.wide[,1]
output.beta.wide <- output.beta.wide[-c(1)]

# Compute hierarchical clustering with AGNES
hc.Ward.beta <- agnes(output.beta.wide, method = "ward")
pltree(hc.Ward.beta, cex = 0.6, hang = -1, main = "Dendrogram of agnes", xlab = "Beta [16-32 Hz]")

# Cut agnes() tree into 2 groups
sub_grp.beta.2 <- cutree(as.hclust(hc.Ward.beta), k = 2)

# Add rectangle markings on the graph
beta.plot <- rect.hclust(hc.Ward.beta, k = 2, border = 2:5)

# Number of members in each cluster
table(sub_grp.beta.2)

# Agglomerative coefficient
hc.Ward.beta$ac

# Linkage methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute coefficients
ac <- function(x) {
  agnes(output.beta.wide, method = x)$ac
}

# Table of coefficients for each linkage method
map_dbl(m, ac)


# Optimal number of clusters using average silhouette width
beta.cluster <- fviz_nbclust(output.beta.wide, FUN = hcut, method = "silhouette") + labs(title = "Optimal number of clusters for Beta")

# Extract avg. silhouette for 1-10 clusters
beta.cluster$data

# Test Adjusted Random Index (ARI)
cluster_similarity(ari_beta, sub_grp.beta.2, similarity="rand")

# GAMMA (32-64Hz)

output.gamma.long.kmeans <- read.csv('output_gamma_long_kmeans.csv')
output.gamma.wide <- output.gamma.long.kmeans %>% spread(Frequency, Amplitude)
write.csv(output.gamma.wide, "output_gamma_wide.csv")
ari_gamma <- output.gamma.wide[,1] # for testing the ARI index

#Label the rows in order to do external validation later
rownames(output.gamma.wide) <- output.gamma.wide[,1]
output.gamma.wide <- output.gamma.wide[-c(1)]

# Compute hierarchical clustering with AGNES
hc.Ward.gamma <- agnes(output.gamma.wide, method = "ward")
pltree(hc.Ward.gamma, cex = 0.6, hang = -1, main = "Dendrogram of agnes", xlab = "Gamma [32-64 Hz]")

# Cut agnes() tree into 2 groups
sub_grp.gamma.2 <- cutree(as.hclust(hc.Ward.gamma), k = 2)

# Add rectangle markings on the graph
gamma.plot <- rect.hclust(hc.Ward.gamma, k = 2, border = 2:5)

# Number of members in each cluster
table(sub_grp.gamma.2)

# Agglomerative coefficient
hc.Ward.gamma$ac

# Linkage methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# Function to compute coefficients
ac <- function(x) {
  agnes(output.gamma.wide, method = x)$ac
}

# Table of coefficients for each linkage method
map_dbl(m, ac)


# Optimal number of clusters using average silhouette width
gamma.cluster <- fviz_nbclust(output.gamma.wide, FUN = hcut, method = "silhouette") + labs(title = "Optimal number of clusters for Gamma")

# Extract avg. silhouette for 1-10 clusters
gamma.cluster$data

# Test Adjusted Random Index (ARI)
cluster_similarity(ari_gamma, sub_grp.gamma.2, similarity="rand")

# PLOTS FOR THE PAPER 

grid.arrange(delta.cluster, theta.cluster, alpha.cluster, beta.cluster, gamma.cluster, nrow = 2)


#############################################################
################## K-means Clustering #######################
#############################################################


# DELTA (0-4Hz)

#Determining the kmeans for different values of k
delta.k2 <- kmeans(output.delta.wide, centers = 2, nstart = 25)
delta.k3 <- kmeans(output.delta.wide, centers = 3, nstart = 25)
delta.k4 <- kmeans(output.delta.wide, centers = 4, nstart = 25)
delta.k5 <- kmeans(output.delta.wide, centers = 5, nstart = 25)

# Plots to compare
delta.p1 <- fviz_cluster(delta.k2, geom = "point",  data = output.delta.wide) + ggtitle("k = 2")
delta.p2 <- fviz_cluster(delta.k3, geom = "point",  data = output.delta.wide) + ggtitle("k = 3")
delta.p3 <- fviz_cluster(delta.k4, geom = "point",  data = output.delta.wide) + ggtitle("k = 4")
delta.p4 <- fviz_cluster(delta.k5, geom = "point",  data = output.delta.wide) + ggtitle("k = 5")

grid.arrange(delta.p1, delta.p2, delta.p3, delta.p4, nrow = 2)

# Function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(output.delta.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(output.delta.wide))
  mean(ss[, 3])
}

# Compute and plot avg_sil for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
avg_sil_values.delta <- map_dbl(k.values, avg_sil)

# Plotting for optimal number of clusters using average silhouette method
fviz_nbclust(output.delta.wide, kmeans, method = "silhouette")

# Compute k-means clustering with optimal number of clusters; k=2
set.seed(123)
final.delta <- kmeans(output.delta.wide, 2, nstart = 25)
print(final.delta)

fviz_cluster(final.delta, data = output.delta.wide)

output.delta.wide %>%
  mutate(Cluster = final.delta$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Computing ARI
cluster_similarity(ari_delta, final.delta$cluster, similarity="rand")


# THETA (4-8Hz) 

# Determining kmeans for different values of k
theta.k2 <- kmeans(output.theta.wide, centers = 2, nstart = 25)
theta.k3 <- kmeans(output.theta.wide, centers = 3, nstart = 25)
theta.k4 <- kmeans(output.theta.wide, centers = 4, nstart = 25)
theta.k5 <- kmeans(output.theta.wide, centers = 5, nstart = 25)

# Plots to compare
theta.p1 <- fviz_cluster(theta.k2, geom = "point",  data = output.theta.wide) + ggtitle("k = 2")
theta.p2 <- fviz_cluster(theta.k3, geom = "point",  data = output.theta.wide) + ggtitle("k = 3")
theta.p3 <- fviz_cluster(theta.k4, geom = "point",  data = output.theta.wide) + ggtitle("k = 4")
theta.p4 <- fviz_cluster(theta.k5, geom = "point",  data = output.theta.wide) + ggtitle("k = 5")

grid.arrange(theta.p1, theta.p2, theta.p3, theta.p4, nrow = 2)

# Function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(output.theta.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(output.theta.wide))
  mean(ss[, 3])
}

# Compute and plot avg_sil for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
avg_sil_values.theta <- map_dbl(k.values, avg_sil)

# Plot for optimal number of clusters using average silhouette method
fviz_nbclust(output.theta.wide, kmeans, method = "silhouette")

# Compute k-means clustering with optimal value of k=2
set.seed(123)
final.theta <- kmeans(output.theta.wide, 2, nstart = 25)
print(final.theta)

fviz_cluster(final.theta, data = output.theta.wide)

output.theta.wide %>%
  mutate(Cluster = final.theta$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Computing ARI
cluster_similarity(ari_theta, final.theta$cluster, similarity="rand")


# ALPHA (8-16Hz) 

# Determining kmeans with different values of k
alpha.k2 <- kmeans(output.alpha.wide, centers = 2, nstart = 25)
alpha.k3 <- kmeans(output.alpha.wide, centers = 3, nstart = 25)
alpha.k4 <- kmeans(output.alpha.wide, centers = 4, nstart = 25)
alpha.k5 <- kmeans(output.alpha.wide, centers = 5, nstart = 25)

# Plots to compare
alpha.p1 <- fviz_cluster(alpha.k2, geom = "point",  data = output.alpha.wide) + ggtitle("k = 2")
alpha.p2 <- fviz_cluster(alpha.k3, geom = "point",  data = output.alpha.wide) + ggtitle("k = 3")
alpha.p3 <- fviz_cluster(alpha.k4, geom = "point",  data = output.alpha.wide) + ggtitle("k = 4")
alpha.p4 <- fviz_cluster(alpha.k5, geom = "point",  data = output.alpha.wide) + ggtitle("k = 5")

grid.arrange(alpha.p1, alpha.p2, alpha.p3, alpha.p4, nrow = 2)

set.seed(123)

# Function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(output.alpha.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(output.alpha.wide))
  mean(ss[, 3])
}

# Compute and plot avg sil for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
avg_sil_values.alpha <- map_dbl(k.values, avg_sil)

# Plot for optimal number of clusters using average silhouette method
fviz_nbclust(output.alpha.wide, kmeans, method = "silhouette")

# Compute k-means clustering with optimal value for clusters; k=2
set.seed(123)
final.alpha <- kmeans(output.alpha.wide, 2, nstart = 25)
print(final.alpha)

fviz_cluster(final.alpha, data = output.alpha.wide)

output.alpha.wide %>%
  mutate(Cluster = final.alpha$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Computing ARI
cluster_similarity(ari_alpha, final.alpha$cluster, similarity="rand")


# BETA (16-32Hz) 

# Determining the kmeans for different values of k
beta.k2 <- kmeans(output.beta.wide, centers = 2, nstart = 25)
beta.k3 <- kmeans(output.beta.wide, centers = 3, nstart = 25)
beta.k4 <- kmeans(output.beta.wide, centers = 4, nstart = 25)
beta.k5 <- kmeans(output.beta.wide, centers = 5, nstart = 25)

# Plots to compare
beta.p1 <- fviz_cluster(beta.k2, geom = "point",  data = output.beta.wide) + ggtitle("k = 2")
beta.p2 <- fviz_cluster(beta.k3, geom = "point",  data = output.beta.wide) + ggtitle("k = 3")
beta.p3 <- fviz_cluster(beta.k4, geom = "point",  data = output.beta.wide) + ggtitle("k = 4")
beta.p4 <- fviz_cluster(beta.k5, geom = "point",  data = output.beta.wide) + ggtitle("k = 5")

grid.arrange(beta.p1, beta.p2, beta.p3, beta.p4, nrow = 2)

# Function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(output.beta.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(output.beta.wide))
  mean(ss[, 3])
}

# Compute and plot avg sil for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
avg_sil_values.beta <- map_dbl(k.values, avg_sil)

# Plot for optimal number of clusters using average silhouette method
fviz_nbclust(output.beta.wide, kmeans, method = "silhouette")

# Compute k-means clustering with optimal number of clusters; k=2
set.seed(123)
final.beta <- kmeans(output.beta.wide, 2, nstart = 25)
print(final.beta)

fviz_cluster(final.beta, data = output.beta.wide)

output.beta.wide %>%
  mutate(Cluster = final.beta$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Computing ARI
cluster_similarity(ari_beta, final.beta$cluster, similarity="rand")



# GAMMA (32-64Hz) 


## Determining the kmeans for different values of k
gamma.k2 <- kmeans(output.gamma.wide, centers = 2, nstart = 25)
gamma.k3 <- kmeans(output.gamma.wide, centers = 3, nstart = 25)
gamma.k4 <- kmeans(output.gamma.wide, centers = 4, nstart = 25)
gamma.k5 <- kmeans(output.gamma.wide, centers = 5, nstart = 25)

# Plots to compare
gamma.p1 <- fviz_cluster(gamma.k2, geom = "point",  data = output.gamma.wide) + ggtitle("k = 2")
gamma.p2 <- fviz_cluster(gamma.k3, geom = "point",  data = output.gamma.wide) + ggtitle("k = 3")
gamma.p3 <- fviz_cluster(gamma.k4, geom = "point",  data = output.gamma.wide) + ggtitle("k = 4")
gamma.p4 <- fviz_cluster(gamma.k5, geom = "point",  data = output.gamma.wide) + ggtitle("k = 5")

grid.arrange(gamma.p1, gamma.p2, gamma.p3, gamma.p4, nrow = 2)

# Function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(output.gamma.wide, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(output.gamma.wide))
  mean(ss[, 3])
}

# Compute and plot avg sil for k = 2 to k = 15
k.values <- 2:15

# Extract avg silhouette for 2-15 clusters
avg_sil_values.gamma <- map_dbl(k.values, avg_sil)

# Plot for optimal number of clusters using average silhouette method
fviz_nbclust(output.gamma.wide, kmeans, method = "silhouette")

# Compute k-means clustering with optimal number of clusters; k=2
set.seed(123)
final.gamma <- kmeans(output.gamma.wide, 2, nstart = 25)
print(final.gamma)

fviz_cluster(final.gamma, data = output.gamma.wide)

output.gamma.wide %>%
  mutate(Cluster = final.gamma$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

# Computing ARI
cluster_similarity(ari_gamma, final.gamma$cluster, similarity="rand")
