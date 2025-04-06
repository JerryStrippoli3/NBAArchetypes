rm(list = ls())

library(tidyverse)
library(caret)
library(cluster)
library(ggplot2)
library(readr)
library(dplyr)
library(factoextra)




bucks_data <- read.csv("bucksFinalTable.csv")
cavs_data <- read.csv("cavsFinalTable.csv")
celtics_data <- read.csv("celticsFinalTable.csv")
grizz_data <- read.csv("grizzFinalTable.csv")

kings_data <- read.csv("kingsFinalTable.csv")
nuggets_data <- read.csv("nuggetsFinalTable.csv")
philly_data <- read.csv("phillyFinalTable.csv")
suns_data <- read.csv("sunsFinalTable.csv")
master_table <- read.csv("masterTable.csv")


# Clean and preprocess data
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
wss <- function(k) {
  kmeans(master_table_std %>% select(-Player, -Team), centers = k, nstart = 25)$tot.withinss
}
k.values <- 1:15
wss_values <- map_dbl(k.values, wss)
elbow <- data.frame(k = k.values, wss = wss_values)
ggplot(elbow, aes(x = k, y = wss)) +
  geom_line() +
  geom_vline(xintercept = 3, linetype = "dashed", color = "red") +
  labs(x = "Number of Clusters (k)", y = "Within-Cluster Sum of Squares (WSS)") +
  ggtitle("Elbow Plot") +
  theme_minimal()

# Cluster the data using k-means
set.seed(123)
kmeans_results <- kmeans(master_table_std %>% select(-Player, -Team), centers = 3, nstart = 25)

# Visualize the clusters
fviz_cluster(list(data = master_table_std %>% select(-Player, -Team), cluster = kmeans_results$cluster))

# Add cluster assignment to the master_table dataframe
master_table_clustered <- cbind(master_table, Cluster = kmeans_results$cluster)

# Define archetypes based on cluster characteristics
master_table_clustered <- master_table_clustered %>%
  mutate(Archetype = case_when(
    Cluster == 1 & PPG > 20 & eFG. > .55 ~ "Scoring Machine",
    Cluster == 1 & PPG > 20 & eFG. <= .55 ~ "Efficient Scorer",
    Cluster == 1 & PPG <= 20 ~ "Role Player",
    Cluster == 2 & FG. > .45 & TS. > .55 ~ "Efficient Inside Scorer",
    Cluster == 2 & FG. > .45 & TS. <= .55 ~ "Inside Scorer",
    Cluster == 2 & FG. <= .45 & TS. > .55 ~ "Efficient Outside Scorer",
    Cluster == 2 & FG. <= .45 & TS. <= .55 ~ "Outside Scorer",
    Cluster == 3 & RPG > 5 & TPG < 2 & APG < 2 & USG. < 20 ~ "Efficient Rebounder",
    Cluster == 3 & RPG > 5 & (TPG >= 2 | APG >= 2) & USG. < 20 ~ "Rebounder/Playmaker",
    Cluster == 3 & RPG > 5 & USG. >= 20 ~ "Rebounding Star",
    Cluster == 3 & RPG <= 5 & TPG < 2 & APG < 2 & USG. < 20 ~ "Efficient Role Player",
    Cluster == 3 & RPG <= 5 & (TPG >= 2 | APG >= 2) & USG. < 20 ~ "Playmaking Role Player",
    Cluster == 3 & RPG <= 5 & USG. >= 20 ~ "Role Player with Scoring Flair",
    TRUE ~ "Other"
  ))

#Print the number of players in each archetype
table(master_table_clustered$Archetype)

#Visualize the archetypes in a radar chart
master_table_clustered %>%
  group_by(Archetype) %>%
  summarize(across(matches("PPG|eFG\\.|FG\\.|FT\\.|X3P\\.|TS\\.|APG|RPG|OREB|TPG|PER|USG\\."), mean)) %>%
  ggplot(aes(x = PPG, y = eFG., group = Archetype)) +
  coord_polar() +
  geom_polygon(aes(fill = Archetype), alpha = .5) +
  geom_text(aes(label = Archetype), angle = 45, hjust = 1.2, vjust = 1.2) +
  scale_fill_brewer(palette = "Dark2") +
  labs(title = "Player Archetypes", x = "Points per Game", y = "Effective Field Goal Percentage") +
  theme_minimal()


master_table_clustered %>%
  count(Archetype) %>%
  ggplot(aes(x = reorder(Archetype, n), y = n, fill = Archetype)) +
  geom_bar(stat = "identity") +
  labs(x = "Archetype", y = "Number of Players", fill = "Archetype") +
  ggtitle("Number of Players per Archetype") +
  theme_minimal()


