rm(list = ls())

library(tidyverse)
library(caret)
library(cluster)
library(ggplot2)
library(readr)
library(dplyr)
library(factoextra)
library(gridExtra)



bucks_data <- read.csv("bucksFinalTable.csv")
cavs_data <- read.csv("cavsFinalTable.csv")
celtics_data <- read.csv("celticsFinalTable.csv")
grizz_data <- read.csv("grizzFinalTable.csv")

kings_data <- read.csv("kingsFinalTable.csv")
nuggets_data <- read.csv("nuggetsFinalTable.csv")
philly_data <- read.csv("phillyFinalTable.csv")
suns_data <- read.csv("sunsFinalTable.csv")
master_table <- read.csv("masterTable.csv")


master_table <- master_table %>%
  select(Player, Pos, Team, MPG, GP, Ht, Wt, AGE, PPG, FG., FT., X3P., eFG., APG, TPG, RPG, OREB, PER, TS., USG., ORtg, OBPM, VI, VORP) %>%
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

# Cluster the data using k-means with different number of clusters
set.seed(123)
k2 <- kmeans(master_table_std %>% select(-Player, -Team), centers = 2, nstart = 25)
k3 <- kmeans(master_table_std %>% select(-Player, -Team), centers = 3, nstart = 25)
k4 <- kmeans(master_table_std %>% select(-Player, -Team), centers = 4, nstart = 25)
k5 <- kmeans(master_table_std %>% select(-Player, -Team), centers = 5, nstart = 25)

p1 <- fviz_cluster(k2, geom = "point", data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = master_table_std %>% select(-Player, -Team)) + ggtitle("k = 5")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# Find the optimal number of clusters using the elbow method
set.seed(123)
fviz_nbclust(master_table_std %>% select(-Player, -Team), kmeans, method = "wss", k.max = 15, nstart = 25)

# Cluster the data using k-means
set.seed(123)
kmeans_results <- kmeans(master_table_std %>% select(-Player, -Team), centers = 4, nstart = 25)

# Visualize the clusters
fviz_cluster(list(data = master_table_std %>% select(-Player, -Team), cluster = kmeans_results$cluster))

library(ggrepel)

# Add cluster assignment to the master_table dataframe
master_table_clustered <- cbind(master_table, Cluster = kmeans_results$cluster)

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


team_totals <- master_table %>%
  group_by(Team) %>%
  summarise(Team_Mins = sum(MPG * GP)) %>%
  ungroup() %>%
  mutate(Total_Mins = sum(Team_Mins),
         Team_Pct = Team_Mins / Total_Mins)

# Calculate the percentage of players in each archetype for each team
team_archetypes <- master_table %>%
  group_by(Team, Archetype) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  mutate(Pct = count / sum(count)) %>%
  pivot_wider(names_from = Archetype, values_from = Pct, values_fill = 0)

# Join the two data frames and calculate the position of each archetype within the stacked bar
team_bar_data <- left_join(team_totals, team_archetypes, by = "TEAM") %>%
  mutate(across(c(Scorer:Versatile), ~if_else(. == 0, NA_real_, .))) %>%
  mutate(across(c(Scorer:Versatile), ~cumsum(.), .names = "pos_{.col}")) %>%
  select(TEAM, Team_Pct, Scorer:Versatile, starts_with("pos_"))

# Convert the data to long format for plotting
team_bar_data_long <- team_bar_data %>%
  pivot_longer(cols = Scorer:Versatile, names_to = "Archetype", values_to = "Pct") %>%
  filter(!is.na(Pct))


# Create the stacked bar chart
ggplot(team_bar_data_long, aes(x = Pct, y = TEAM, fill = Archetype)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("orange", "lightblue", "darkgreen", "red")) +
  geom_text(aes(x = pos_Scorer + (Pct / 2), y = TEAM, label = scales::percent(Pct)), size = 3) +
  geom_text(aes(x = Team_Pct + 0.02, y = TEAM, label = scales::percent(Team_Pct)), hjust = 0) +
  scale_x_continuous(labels = scales::percent_format()) +
  labs(title = "Relative frequency of minutes played by archetype and team",
       subtitle = "Stacked bar chart",
       x = "Relative frequency of minutes played",
       y = "Team") +
  theme_minimal()
