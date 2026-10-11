# statistical-data-analysis-r
# Spotify Audio Features: Multivariate Statistical Analysis & Unsupervised Learning
This repository contains an end-to-end multivariate statistical analysis and machine learning project conducted in **R**, utilizing real audio metadata retrieved via the official **Spotify API** (1,686 tracks and 29 variables).
The project evaluates acoustic properties to discover latent musical topologies via **Dimensionality Reduction (PCA & CA)** and **Unsupervised Clustering (K-means)**, while modeling commercial success through **Supervised Learning (Linear & Logistic Regression)**.

# Tech Stack & R Libraries
- **Language:** R
- **Dimensionality Reduction & Clustering:** FactoMineR, factoextra
- **Statistical Modeling:** MASS (stepAIC), car (VIF Diagnostics), stats
- **Data Visualization & Manipulation:** ggplot2, dplyr

# Key Methodologies & Statistical Results
### 1. Exploratory Data Analysis
- **Feature Correlations:** Identified strong positive correlations between loudness and energy
  (r = 0.69) and valence and danceability (r = 0.51). Strong negative correlations were observed between acousticness and energy (r = -0.56).
- **Success Criteria:** Discovered non-linear distributions in track_popularity, establishing a threshold at 68/100 to classify "Hit" vs. "Flop" tracks.

### 2. Dimensionality Reduction (PCA & Correspondence Analysis)
- **Principal Component Analysis (PCA):** Standardized 7 continuous features. Using Kaiser's rule and Scree Plot elbows, retained **4 Principal Components explaining 82.5% of total acoustic variance**:
  - **PC1 (38.4%):** Acoustic Intensity & Power (energy and loudness).
  - **PC2 (16.6%):** Track Duration vs. Danceability.
  - **PC3 (14.1%):** Recording Context (liveness vs. studio production).
  - **PC4 (13.4%):** Tempo & Pacing.
- **Correspondence Analysis (CA):** Constructed a contingency table (macro_genre, tempo_category). Rejected independence via Chi-Square test (chi^2 = 154.32, p < 2.2). Dim 1 (83%) mapped structural rigidities: Classical/Acoustic over-indexed in slow tempos (<100 BPM), while Pop/Electronic aligned with medium tempos (100–130 BPM).

### 3. Unsupervised Machine Learning (K-means Clustering)
- **Input Space:** Cluster analysis performed on orthogonal 4D PCA score coordinates to prevent multicollinearity and scale bias.
- **Optimal K Selection:** Elbow method on Total Within-cluster Sum of Squares (WSS) identified
  K = 3.
- **Discovered Topologies:**
  1. **Cluster 1 ("Danceable Mainstream"):** Highest danceability (0.683) and valence (0.543).
  3. **Cluster 2 ("High-Intensive"):** Highest energy (0.731), loudness(-6.33), and liveness (0.391).
  4. **Cluster 3 ("Atmospheric & Quiet"):** Low energy (0.174), loudness (-21.64 dB), and slow average tempo (104.5 BPM).

### 4. Supervised Predictive Modeling
- **Multiple Linear Regression:** Built a backward stepwise model (AIC) with 7 predictors. Identified severe target variable ceiling effects at 100 causing heteroscedasticity. Out-of-sample test R^2 = 0.0709, {RMSE} = 19.3.
- **Logistic Regression ("Hit" Prediction):** Reformulated task into binary classification (>68 popularity). Backward AIC selected 5 key features:
  - loudness increased odds of a hit by **+78.5%** (OR = 1.785).
  - instrumentalness decreased odds of a hit by **-57.5%** (OR = 0.425).
  - Achieved **{AUC} = 0.711** and 67.22% overall accuracy.

---

## Full PDF Reports
Access the full research documents directly:
- 📥 [Read Full Report - Part 1: Exploratory Analysis & Regression Modeling](docs/Spotify_Music_Dataset.pdf)
- 📥 [Read Full Report - Part 2: PCA, CA & K-Means Clustering](docs/Spotify_Music_Dataset_Part2.pdf)
