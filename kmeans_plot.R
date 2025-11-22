setwd('~/Desktop/SP3275')
library(readxl)

library(ggplot2)

library(class)
library(rpart)
library(rpart.plot)
library(e1071)
library(ROCR)
library('arules') 
library('arulesViz')

# --- 1. Run K-means on shoot density only ---
set.seed(123)
km <- kmeans(scale(df$biomass), centers = 4, nstart = 25)
df$cluster4 <- km$cluster   # new column for 4 clusters

# Colour palette for your 11 groups (unchanged)
group_colors <- c(
  "#FF0000",  # group 1 - red
  "#FF4500",  # group 2
  "#FF7F00",  # group 3
  "#FFB300",  # group 4
  "#FFD700",  # group 5
  "#FFE680",  # group 6
  
  "#0000FF",  # group 7 - blue
  "#4169E1",  # group 8
  "#1E90FF",  # group 9
  "#00CED1",  # group 10 - turquoise
  "#00BFFF"   # group 11
)

# Plot: 11 group colours + 4 K-means shapes
ggplot(df, aes(x = shoot_density, 
               y = group,
               color = factor(group), 
               shape = factor(cluster4))) +
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = group_colors) +
  scale_shape_manual(values = c(16, 17, 15, 18)) +  # 4 shapes
  theme_minimal(base_size = 14) +
  labs(
    title = "Shoot Density by Group (Colour) and K-means Clusters (4 Shapes)",
    x = "Shoot Density",
    y = "Group",
    color = "Group",
    shape = "K-means Cluster (4)"
  )

library(ggplot2)

wss <- numeric()

for (k in 1:10) {
  km <- kmeans(scale(df$shoot_density), centers = k, nstart = 20)
  wss[k] <- km$tot.withinss
}

plot(1:10, wss, type="b", pch=19,
     xlab="Number of clusters (k)",
     ylab="Total within-cluster sum of squares",
     main="Elbow Plot")
# Read your data






library(readxl)
library(ggplot2)

# --- 1. Read data ---
df <- read_excel("clusterbybiomass.xlsx")

# --- 2. K-means with 6 clusters ---
set.seed(123)
km <- kmeans(scale(df$biomass), centers = 6, nstart = 25)
df$cluster6 <- km$cluster


# --- 3. COLOURS for k-means clusters (6 groups) ---
cluster6_colors <- c(
  "#FF0000",  # Cluster 1 - red
  "#FF7F00",  # Cluster 2 - orange
  "#FFD700",  # Cluster 3 - yellow
  "#1E90FF",  # Cluster 4 - blue
  "#00CED1",  # Cluster 5 - turquoise
  "#8A2BE2"   # Cluster 6 - purple
)


# --- 4. SHAPES for 11 original groups ---
# R has 25 usable point shapes, so 11 is fine.
group_shapes <- c(16, 17, 15, 18, 8, 4, 3, 7, 9, 10, 12)
# Explanation:
# 16 = circle
# 17 = triangle
# 15 = square
# 18 = diamond
# 8  = star
# 4  = cross
# 3  = plus
# 7  = upside-down triangle
# 9  = circle with border
# 10 = triangle left
# 12 = square hollow


# --- 5. FINAL PLOT ---
ggplot(df, aes(x = shoot_density, 
               y = group,
               color = factor(cluster6),     # colour = K-means clusters
               shape = factor(group))) +     # shape = original groups
  geom_point(size = 4, alpha = 0.9) +
  scale_color_manual(values = cluster6_colors) +
  scale_shape_manual(values = group_shapes) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Shoot Density: Shapes = Original Groups, Colours = 6 K-means Clusters",
    x = "Shoot Density",
    y = "Group",
    color = "K-means Cluster (6)",
    shape = "Original Group (1–11)"
  )





library(readxl)
library(ggplot2)

# 1. Read data
df <- read_excel("clusterbybiomass.xlsx")

# 2. K-means with 4 clusters on shoot_density
set.seed(123)
km <- kmeans(scale(df$biomass), centers = 2, nstart = 25)
df$cluster4 <- km$cluster   # new column for 4 clusters

# 3. Colours for the 4 K-means clusters
cluster4_colors <- c(
  "#FF0000",  # Cluster 1 - red
  "#1E90FF",  # Cluster 2 - blue
)

# 4. Plot:
#    - x = shoot_density
#    - y = group
#    - colour = 4 k-means clusters
#    - shape = SAME for all original groups
ggplot(df, aes(x = biomass,
               y = group,
               color = factor(cluster4))) +  # colour by cluster
  geom_point(size = 4, alpha = 0.9, shape = 16) +  # same shape for everyone
  scale_color_manual(values = cluster4_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Total Biomass by Group and 4 K-means Clusters",
    x = "Total biomass",
    y = "Original Group",
    color = "K-means Cluster (4)"
  )


library(readxl)
library(ggplot2)

# 1. Read data
df <- read_excel("clusterbybiomass.xlsx")

# 2. K-means with 2 clusters on shoot_density
set.seed(123)
km <- kmeans(scale(df$biomass), centers = 2, nstart = 25)
df$cluster2 <- km$cluster   # new column for 2 clusters

# 3. Colours for the 2 clusters
cluster2_colors <- c(
  "#FF0000",  # Cluster 1 - red
  "#1E90FF"   # Cluster 2 - blue
)

# 4. Plot:
#    - x = shoot_density
#    - y = group
#    - colour = 2 clusters
#    - shape = same shape for all original groups
ggplot(df, aes(x = biomass,
               y = group,
               color = factor(cluster2))) +  # colour by cluster
  geom_point(size = 4, alpha = 0.9, shape = 16) +  # same shape (circle)
  scale_color_manual(values = cluster2_colors) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Total biomass by right Group and 2 K-means Clusters",
    x = "Total biomass",
    y = "Original Group",
    color = "K-means Cluster (2)"
  )
