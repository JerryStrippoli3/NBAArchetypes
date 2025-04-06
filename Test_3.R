rm(list = ls())

library(tidyverse)
library(caret)
library(cluster)
library(ggplot2)
library(readr)
library(dplyr)
library(factoextra)

# Read in the data
bucks_data <- read.csv("bucksFinalTable.csv")
cavs_data <- read.csv("cavsFinalTable.csv")
celtics_data <- read.csv("celticsFinalTable.csv")
grizz_data <- read.csv("grizzFinalTable.csv")
kings_data <- read.csv("kingsFinalTable.csv")
nuggets_data <- read.csv("nuggetsFinalTable.csv")
philly_data <- read.csv("phillyFinalTable.csv")
suns_data <- read.csv("sunsFinalTable.csv")
master_table <- read.csv("masterTable.csv")

# Select and preprocess the data
master_table <- master_table %>%
  select(Player, Pos, Team, GP, Ht, Wt, AGE, PPG, FG., FT., X3P., eFG., APG, TPG, RPG, OREB, PER, TS., USG., ORtg, OBPM, VI, VORP) %>%
  mutate(ht_inches = ifelse(grepl("^\\d+-\\d+$", Ht),
                            sapply(strsplit(Ht[grepl("^\\d+-\\d+$", Ht)], "-"), function(x) (as.numeric(x[1]) * 12 + as.numeric(x[2])) / 2),
                            Ht)) %>%
  select(-Ht) %>%
  rename(Ht = ht_inches)

# Standardize the data
master_table_std <- master_table %>% 
  select_if(is.numeric) %>% # Select only numeric columns
  scale() %>% 
  as.data.frame() %>% 
  cbind(master_table %>% select(Player, Team))

# Compute PCA
master_pca <- prcomp(master_table_std %>% select(-Player, -Team), scale = TRUE)

# Plot the proportion of variance explained by each principal component
summary(master_pca)

ggplot(data = data.frame(PC = 1:length(master_pca$sdev), Prop_Var = master_pca$sdev^2/sum(master_pca$sdev^2)), aes(x = PC, y = Prop_Var)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Principal Component", y = "Proportion of Variance Explained") +
  ggtitle("Scree Plot") +
  theme_minimal()

# Find the optimal number of clusters using the elbow method
set.seed(123)
fviz_nbclust(master_table_std %>% select(-Player, -Team), hcut, method = "wss", k.max = 15)

# Cluster the data using hierarchical clustering
set.seed(123)
hclust_results <- hcut(master_table_std %>% select(-Player, -Team), k = 4, hc_method = "ward.D2")

# Visualize the clusters
fviz_dend(hclust_results, k = 4, cex = 0.6, k_colors = c("#E69F00", "#56B4E9", "#009E73", "red"))

# Add cluster assignment to the master_table dataframe
master_table_clustered <- cbind(master_table, Cluster = cutree(hclust_results, k = 4))

master_table_clustered <- master_table_clustered %>%
  mutate(Archetype = case_when(
    Cluster == 1 ~ "Inside Scorer",
    Cluster == 2 ~ "Outside Scorer",
    Cluster == 3 ~ "All-Around",
    Cluster == 4 ~ "Defensive Specialist"
  ))

cluster_names <- c("Inside Scorer", "Outside Scorer", "All-Around", "Defensive Specialist")

# Define colors and cluster names
cluster_colors <- c("#E69F00", "#56B4E9", "#009E73", "red")

# Visualize the clusters in the first two principal components
ggplot(data = as.data.frame(master_pca$x) %>% cbind(Cluster = master_table_clustered$Cluster, Archetype = master_table_clustered$Archetype, Player = master_table_clustered$Player),
       aes(x = PC1, y = PC2, color = factor(Archetype))) +
  geom_point(size = 3) +
  scale_color_manual(values = cluster_colors, labels = cluster_names) + # Add cluster names to the legend
  labs(x = "PC1", y = "PC2", color = "Archetype") +
  ggtitle("K-means Clustering Results") +
  theme_minimal() +
  geom_text_repel(data = as.data.frame(master_pca$x) %>% cbind(Cluster = master_table_clustered$Cluster, Archetype = master_table_clustered$Archetype, Player = master_table_clustered$Player),
                  aes(label = Player), size = 3)

# Cluster the data using k-means with different number of clusters
set.seed(123)
k2 <- hcut(master_table_std %>% select(-Player, -Team), centers = 2, nstart = 25)
k3 <- hcut(master_table_std %>% select(-Player, -Team), centers = 3, nstart = 25)
k4 <- hcut(master_table_std %>% select(-Player, -Team), centers = 4, nstart = 25)
k5 <- hcut(master_table_std %>% select(-Player, -Team), centers = 5, nstart = 25)

# Visualize clusters
library(gridExtra)
library(ggplot2)
library(factoextra)

p1 <- fviz_cluster(k2, geom = "point", data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
