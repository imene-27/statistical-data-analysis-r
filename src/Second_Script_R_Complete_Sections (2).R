# Second Written Report --------------------------------------------------------

library(tidymodels)
library(tidyverse)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(knitr)
library(ggplot2)
library(ggrepel)
library(dplyr)

# Data preparation
tracks_df <- read.csv("tracks_df.csv")

# We remove the missing values to ensure compatibility with PCA
# and correlation functions
tracks_df <- na.omit(tracks_df)


## Principal Component Analysis ------------------------------------------------

# We have selected 7 variables to make our PCA and then we standardised each variable in order to make them more directly comparable
PCA_df <- tracks_df |>
  dplyr::select(energy, tempo, danceability, loudness, duration_ms, valence, liveness) |>
  scale()

# We compute the sample correlation matrix of the standardised matrix
S <- cor(PCA_df)

# We compute the eigenvalues and eigenvectors of the correlation matrix S
E <- eigen(S)

# PC loadings are the entries of the eigenvectors of S
PCloadings <- E$vectors

  # Note: While reviewing the data, I noticed prcomp() flips the sign of PC4 
  # compared to eigen(). I am manually flipping them here so our tables match the plots.
    # We also understood that this happens because one uses the Singular Value Decomposition
    # and the other uses a Spectral Decomposition.
    PCloadings[, 4] <- -PCloadings[, 4]

# We rename the rows appropriately
rownames(PCloadings) <- colnames(PCA_df)

# We derive the PCs by multiplying the standardised matrix with the PC loadings
PCs <- PCA_df %*% PCloadings

# We compute the correlations between the original variables and the eigenvectors of S
scaledPcLoadings <- PCloadings
for(j in 1:ncol(PCloadings)){
  scaledPcLoadings[, j] <- sqrt(E$values[j]) * PCloadings[, j]
}

# We use the square of the scaled PC loadings as indicator of variable representation quality
cos2 <- scaledPcLoadings^2

# We compute the variance explained by each component
var_explained  <- 100 * E$values / sum(E$values)
cumulative_var <- cumsum(var_explained)

# We summarise eigenvalues and variance in a single formatted table
eig_table <- data.frame(
  Component      = paste0("PC", seq_along(E$values)),
  Eigenvalue     = round(E$values, 4),
  Variance_Pct   = round(var_explained, 2),
  Cumulative_Pct = round(cumulative_var, 2))

kable(eig_table,
      caption     = "Eigenvalues and Variance Explained by each Principal Component",
      col.names   = c("Component", "Eigenvalue", "% Variance", "Cumulative %"))

# --- Component selection: two paths under the Kaiser criterion ---
#
# The strict Kaiser rule retains only components with eigenvalue > 1,
# since those explain more variance than a single original (standardised) variable.
# However, components with eigenvalues
# only slightly below 1 (PC3 and PC4 reach ~0.94) still carry meaningful
# insights in practice, especially if the scree plot, interpretability support it.
#
# We, therefore, derive both a strict and a relaxed selection so that the
# analysis can be conducted under each assumption, and confronted.

# PATH A — Strict Kaiser (eigenvalue > 1)
n_strict <- sum(E$values > 1)
cat("PATH A — Components retained (strict Kaiser, eigenvalue > 1):", n_strict, "\n")

# PATH B — Relaxed Kaiser (eigenvalue > 0.9)
flexible_threshold <- 0.9
n_flexible <- sum(E$values > flexible_threshold)
cat("PATH B — Components retained (relaxed Kaiser, eigenvalue >", flexible_threshold, "):", n_flexible, "\n")

# PATH A: strict selection table

kable(round(scaledPcLoadings[, 1:n_strict], 3),
      caption   = paste0("PATH A — Correlations between original variables and retained PCs",
                         " (strict Kaiser, n = ", n_strict, ")"),
      col.names = paste0("PC", 1:n_strict))

kable(round(cos2[, 1:n_strict], 3),
      caption   = paste0("PATH A — cos\u00b2 Quality of variable representation",
                         " (strict Kaiser, n = ", n_strict, ")"),
      col.names = paste0("PC", 1:n_strict))

# PATH B: relaxed selection table

kable(round(scaledPcLoadings[, 1:n_flexible], 3),
      caption   = paste0("PATH B — Correlations between original variables and retained PCs",
                         " (relaxed Kaiser, eigenvalue > ", flexible_threshold,
                         ", n = ", n_flexible, ")"),
      col.names = paste0("PC", 1:n_flexible))

kable(round(cos2[, 1:n_flexible], 3),
      caption   = paste0("PATH B — cos\u00b2 Quality of variable representation",
                         " (relaxed Kaiser, eigenvalue > ", flexible_threshold,
                         ", n = ", n_flexible, ")"),
      col.names = paste0("PC", 1:n_flexible))

# For the remaining part of the script we will work with the relaxed selection (PATH B)
# because it provides richer interpretability and is supported by the scree plot, as we will see in a moment.
n_components <- n_flexible

# We use the factor map to visualize the individuals
res.pca <- prcomp(PCA_df)

# We create a Scree plot to represent the variance explained by each component
fviz_eig(res.pca,
         addlabels = TRUE,
         ylim      = c(0, 50),
         main      = "Scree Plot: Variance Explained by Principal Components")


# PC1 & PC2 — the correlation table above shows these are driven by energy/loudness and danceability/valence
fviz_pca_var(res.pca,
             col.var      = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel        = TRUE,
             title        = "Variables Factor Map (PC1 & PC2)")

# PC3 & PC4 — the correlation table shows liveness and tempo are the dominant contributors here
fviz_pca_var(res.pca,
             axes         = c(3, 4),
             col.var      = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel        = TRUE,
             title        = "Variables Factor Map (PC3 & PC4)")


# We selected the 7 numerical variables for our PCA,
# let's also prepare the Categorical Variable that will be projected on our results:

PCA_df_2 <- as.data.frame(PCA_df) |>
  dplyr::mutate(playlist_genre = tracks_df$playlist_genre,
                macro_genre = case_when(
                  playlist_genre %in% c("electronic", "lofi", "gaming", "disco")                                            ~ "Electronic & Lofi",
                  playlist_genre %in% c("pop", "r&b", "k-pop", "j-pop", "cantopop", "mandopop")                            ~ "Pop",
                  playlist_genre %in% c("hip-hop", "afrobeats", "soul", "funk")                                             ~ "Hip-Hop & Afro",
                  playlist_genre %in% c("rock", "punk", "metal", "indie", "blues")                                          ~ "Rock & Alternative",
                  playlist_genre %in% c("latin", "world", "arabic", "brazilian", "turkish", "indian", "korean", "soca", "reggae") ~ "Latin & Global",
                  playlist_genre %in% c("ambient", "classical", "jazz", "wellness", "folk", "country", "gospel")            ~ "Acoustic & Classical",
                  TRUE ~ "Unknown"))

table(PCA_df_2$macro_genre) |> kable(caption = "Frequency Table on Macro Genre", col.names = c("Genre", "Freq"))

# With the Category "Unknown" we made sure that everything was captured.
# Our check with the frequency table confirmed the intended result.


# We extract individual PC scores and attach the genre labels directly,
# which lets us use ggplot2 for faceting without relying on factoextra internals
pca_scores <- as.data.frame(res.pca$x)
pca_scores$macro_genre <- PCA_df_2$macro_genre

# Individuals projection on PC1 & PC2, faceted by genre
ggplot(pca_scores, aes(x = PC1, y = PC2, color = macro_genre)) +
  geom_point(alpha = 0.35, size = 0.8) +        # alpha reduced to handle overplotting at this dataset size
  facet_wrap(~ macro_genre) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "PCA: Projection of Macro Genres (PC1 & PC2)", x = "PC1", y = "PC2")

# Individuals projection on PC3 & PC4, faceted by genre
# to match the variable factor maps produced on those same axes above
ggplot(pca_scores, aes(x = PC3, y = PC4, color = macro_genre)) +
  geom_point(alpha = 0.35, size = 0.8) +
  facet_wrap(~ macro_genre) +
  coord_cartesian(xlim = c(-5, 5), ylim = c(-5, 5)) +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(title = "PCA: Projection of Macro Genres (PC3 & PC4)", x = "PC3", y = "PC4")


## Correspondence Analysis -----------------------------------------------------

# 1. Prepare the Data

  # Converting the variable "tempo" measured in BPM (Beats Per Minute) into a categorical variable
  # to use it in the CA in combination with the variable "macro_genre".
      # Note: tempo was also used in the PCA above; here we recover the original (non-standardised)
      # values from tracks_df so that the BPM cut points are interpretable.

CA_df <- PCA_df_2 |>
  dplyr::select(macro_genre) |>
  mutate(tempo = tracks_df$tempo,
         tempo_category = cut(
           tempo,
           breaks = c(-Inf, 100, 130, Inf),       # The BPM boundaries
           labels = c("Slow", "Medium", "Fast"),   # The names of our new categories
           right  = FALSE))                        # Left-closed intervals: so that an 100 BPM belongs to Medium and a 130 BPM belongs to Fast.

table(CA_df$tempo_category) |> kable(caption = "Frequency Table of the new Tempo Categories", col.names = c("Tempo", "Freq"))


# 2. Constructing the Contingency Table
  # to verify that no sparse cells exist (below 5)

contingency_table <- table(CA_df$macro_genre, CA_df$tempo_category)
kable(contingency_table, caption = "Observed Contingency Table: Macro Genres vs. Tempo Categories")


# 3. Formal Statistical Justification (Chi-Square Test)
  # We test the hypothesis of independence before proceeding with CA

chi_test <- chisq.test(contingency_table)
print(chi_test)

  # We also display the expected frequencies under independence,
  # so we can directly compare them with the observed counts above
kable(round(chi_test$expected, 1),
      caption = "Expected Frequencies under the Hypothesis of Independence")


# 4. Row Profiles
  # Before running the CA we define the row profiles (conditional distributions within each genre),
  # which decompose the observed table into the relative frequency of each tempo category per genre

row_profiles <- prop.table(contingency_table, margin = 1)
kable(round(row_profiles, 3),
      caption = "Row Profiles: Conditional Distribution of Tempo Categories within each Macro Genre")


# 5. Execute the Correspondence Analysis
  # We use the CA() function from FactoMineR. graph = FALSE suppresses default base R plots to then do our own graph.

res.ca <- CA(contingency_table, graph = FALSE)

  # Extract and display the eigenvalues to understand the variance explained by the new dimensions
kable(round(res.ca$eig, 4),
      caption   = "CA Eigenvalues and Variance Explained",
      col.names = c("Eigenvalue", "% Variance", "Cumulative %"))


# 6. Visualization: The Symmetric Biplot
  # We plot both row profiles (Genres) and column profiles (Tempos) on the same 2D space.

fviz_ca_biplot(res.ca,
               repel     = TRUE,              
               col.row   = "#2C3E50",         
               col.col   = "#D35400",         
               shape.row = 16,                
               shape.col = 17,                
               title     = "Correspondence Analysis: Macro Genres vs Tempo Profiles\n") +
  theme_minimal(base_size = 12) +             
  theme(
    plot.title       = element_text(face = "bold", hjust = 0.5, size = 14), 
    axis.title       = element_text(face = "bold", color = "#333333"),      
    panel.grid.major = element_line(color = "grey85", linewidth = 0.5),     
    panel.grid.minor = element_blank(),                                     
    legend.position  = "bottom")


## Clustering analysis -----------------------------------------------------
# 1. Data preparation
# For the following cluster analysis we will use the coordinates of the tracks in the first 4 PCs
# which, as explained in the path B of the PCA part, explains 82.57% of the total variance. 
clustering_data <- res.pca$x[ ,1:4]

# 2. Choose the optimal number of clusters (K)
# We first need to set s seed since the algorithm's first step is a random assignation
# from 1 to K for each observation.
set.seed(123)

# We evaluate the Total WSS from K = 1 to K = 10
# We use the elbow method to find the most suitable K for our case 
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  #We draw a vertical line to indicate the chosen point in the graph
  geom_vline(xintercept = 3, linetype = 2, color = "red") +
  labs(title = "Elbow Method: Finding Optimal K",
       subtitle = "Total Within-cluster Sum of Squares (WSS)")

# 3. K-means execution
# We chose K = 3 based on the results of the elbow plot 
# We use nstart = 25 so the k-means algorithm is repeated 25 times with different initialisations,
# which substantially reduces the probability of converging to a poor local minimum.

kmeans_result <- kmeans(clustering_data, centers = 3, nstart = 25)

# 4. Cluster visualization
# We project the clusters we got after running the kmeans algorithm
# onto the first two Principal Components, representing Intensity and Duration
fviz_cluster(kmeans_result, data = clustering_data,
             geom = "point",
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal(),
             main = "K-means Clustering Results on PCA Dimensions")

# 5. Profile Analysis
# We put the cluster labels back into our data to understand what each group represents.
tracks_df$cluster <- as.factor(kmeans_result$cluster)

# We calculate the average of each variables to see what makes each group different
cluster_profile <- tracks_df %>%
  group_by(cluster) %>%
  summarise(across(c(energy, loudness, danceability, valence, duration_ms, tempo, liveness), mean))

kable(cluster_profile, digits = 3, caption = "Acoustic Profile per Cluster (Means)")
