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


# Load required packages
library(dplyr)
library(tidyr)
library(cluster)

# Select relevant features
features <- c("PPG", "FG.", "FT.", "X3P.", "eFG.", "APG", "TPG", "RPG", "USG.", "PER",
              "TS.", "SPG", "BPG", "DRtg", "DWS", "DBPM", "ORtg", "OBPM", "VI", "VORP")

# Pre-process data
master_table_processed <- master_table %>%
  select(features, Pos, Ht, Wt, AGE) %>%
  mutate(ht_inches = ifelse(grepl("^\\d+-\\d+$", Ht),
                            sapply(strsplit(Ht[grepl("^\\d+-\\d+$", Ht)], "-"), function(x) (as.numeric(x[1]) * 12 + as.numeric(x[2])) / 2),
                            Ht)) %>%
  select(-Ht) %>%
  rename(Ht = ht_inches)

# Scale features
master_table_scaled <- scale(master_table_processed[features])

# Cluster players using k-means clustering
set.seed(123)
k <- 4  # number of clusters
km <- kmeans(master_table_scaled, centers = k, nstart = 25)

# Assign archetypes based on clusters
master_table_clustered <- master_table %>%
  select(Player, Pos, Team, GP, Ht, Wt, AGE, PPG, FG., FT., X3P., eFG., APG, TPG, RPG, OREB, PER, TS., USG., SPG, BPG, DRtg, DWS, DBPM, ORtg, OBPM, VI, VORP) %>%
  mutate(ht_inches = ifelse(grepl("^\\d+-\\d+$", Ht),
                            sapply(strsplit(Ht[grepl("^\\d+-\\d+$", Ht)], "-"), function(x) (as.numeric(x[1]) * 12 + as.numeric(x[2])) / 2),
                            Ht)) %>%
  select(-Ht) %>%
  rename(Ht = ht_inches) %>%
  mutate(Cluster = km$cluster) %>%
  mutate(Archetype = case_when(
    Cluster == 1 ~ "Inside Scorer",
    Cluster == 2 ~ "Outside Scorer",
    Cluster == 3 ~ "All-Around",
    Cluster == 4 ~ "Defensive Specialist"
  ))


# Load required packages
library(ggplot2)
library(ggrepel)
library(prcomp)

# Compute principal components
pca <- prcomp(master_table_scaled, center = TRUE, scale. = TRUE)

# Extract the first two principal components
PC1 <- pca$x[, 1]
PC2 <- pca$x[, 2]

# Add archetype names to data frame
data <- data.frame(PC1, PC2, Archetype = case_when(
  km$cluster == 1 ~ "Inside Scorer",
  km$cluster == 2 ~ "Outside Scorer",
  km$cluster == 3 ~ "All-Around",
  km$cluster == 4 ~ "Defensive Specialist"
), Player = master_table$Player)

# Create scatter plot with labels
ggplot(data = data, aes(x = PC1, y = PC2, color = Archetype)) +
  geom_point(size = 3) +
  geom_text_repel(aes(label = Player), size = 2.5) +
  xlab("PC1") +
  ylab("PC2") +
  ggtitle("Cluster Analysis of NBA Players") +
  theme_bw()

