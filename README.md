# DS320_Final_Proj: Clustering NBA Player Archetypes

This project applies unsupervised machine learning techniques to NBA player statistics to identify player archetypes. The main objective is to group players into performance-based categories such as Inside Scorer, Outside Scorer, All-Around, and Defensive Specialist using K-Means clustering and Principal Component Analysis (PCA).

The analysis is conducted in R and includes custom data cleaning, standardization, clustering, and visualization pipelines. The dataset comprises combined performance stats from multiple NBA teams.

## Project Structure

- `DS_320_Clustering.R`: Main script that processes, clusters, and visualizes NBA player data.
- `Test_1.R` to `Test_5.R`: Supplementary scripts used for experimentation, comparison of clustering methods, and different visualizations.
- `masterTable.csv`: Combined dataset of player stats across multiple NBA teams.
- `*_FinalTable.csv`: Individual team datasets.
- `master_table_clustered`: Output of the main clustering with labeled archetypes.
- `.Rproj` file: Project configuration for RStudio.

## Methods Used

- Feature Engineering: Converting height from feet-inches to inches, selecting numeric columns, scaling features.
- Dimensionality Reduction: PCA to visualize variance and reduce feature space.
- Clustering: K-Means clustering with evaluation via the elbow method.
- Visualization: Scree plots, PCA scatter plots with labeled clusters, and team-wise archetype distributions.

## Player Archetypes

Cluster labels were manually interpreted and mapped to one of the following archetypes:
- Inside Scorer
- Outside Scorer
- All-Around
- Defensive Specialist

## Test Script Breakdown

### `Test_1.R`
Generates visualizations that analyze the distribution and count of each archetype.

### `Test_2.R`
Performs a comparison of K-Means clustering with varying values of k and visualizes each outcome using PCA.

### `Test_3.R`
Uses hierarchical clustering (Ward’s method) as an alternative to K-Means and visualizes results.

### `Test_4.R`
Simplified version of the clustering pipeline with a clean PCA plot showing player clusters.

### `Test_5.R`
Tests K-Means clustering with larger k values (2–7) and compares clustering outcomes visually.

## How to Run

To run this project:
1. Open the `.Rproj` file in RStudio.
2. Execute `DS_320_Clustering.R` to reproduce the main clustering results.
3. Optionally run any of the `Test_*.R` files to experiment with different clustering strategies.

Ensure all `.csv` data files are in the same working directory as your scripts.

## Dependencies

This project uses the following R packages:

- tidyverse
- dplyr
- ggplot2
- cluster
- factoextra
- ggrepel
- caret
- gridExtra

Install any missing packages via:

```R
install.packages(c("tidyverse", "cluster", "factoextra", "ggrepel", "caret", "gridExtra"))
```
