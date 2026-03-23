# =============================================================================
# INTRO TO MATRICES & COMPOSITIONAL DATA ANALYSIS IN R
# =============================================================================

library(tidyverse)   
library(vegan)       
library(compositions) 

# =============================================================================
# PART 1: MATRIX BASICS IN BASE R
# =============================================================================

# A matrix is a 2D structure: rows x columns, all same data type

# Creating a matrix
m <- matrix(1:12, nrow = 3, ncol = 4)
m

# Fill row by row instead:
m_byrow <- matrix(1:12, nrow = 3, ncol = 4, byrow = TRUE)
m_byrow

# Naming rows and columns
rownames(m) <- c("Sample_A", "Sample_B", "Sample_C")
colnames(m) <- c("Tax1", "Tax2", "Tax3", "Tax4")
m

# Indexing
m[1, ]        # first row (all columns)
m[, 2]        # second column (all rows)
m[2, 3]       # single cell: row 2, col 3
m["Sample_A", "Tax3"]  # by name

# Basic matrix math
t(m)          # transpose: flip rows and columns
m * 2         # element-wise multiplication
m + 1         # addition
m + m         # matrix addition

# Row and column sums/means (base R)
rowSums(m)
colSums(m)
rowMeans(m)
colMeans(m)


# =============================================================================
# PART 2: MICROBIAL ABUNDANCE DATA
# =============================================================================

setwd("/home/gjordaopie/R_course/")

OTU <- read_csv("infant_OTU.csv") 

# Peek at the data
OTU
dim(OTU) 
class(OTU)        # "data.frame"

colSums(OTU[, -1])
rowSums(OTU[, -1])


OTU <- OTU %>% column_to_rownames("genome") %>% as.matrix()

OTU
dim(OTU)    
class(OTU)        # "matrix"

# Total reads per sample 
rowSums(OTU)
colSums(OTU)



# =============================================================================
# PART 3: COMPOSITIONAL DATA — WHY AND HOW
# =============================================================================
# KEY CONCEPT:
#   Sequencing gives us *relative* information, not absolute counts.
#   Solution: convert to relative abundances (proportions / percentages).
#   This is what makes microbiome data "compositional".

# Convert to relative abundances (proportions) 
rel_abund <- sweep(OTU, 2, colSums(OTU), "/")

# Verify: each column (sample) sums to 1
colSums(rel_abund)

# Convert to percentages
percent <- rel_abund * 100
round(percent[1:5, 1:5], 2)   

# Transpose so samples are rows (needed for dplyr & PCA) ---
percent_t <- t(percent)

percent_df <- as.data.frame(percent_t) %>%
  rownames_to_column("sampleID") %>%
  as_tibble()

percent_df

# metadata and taxonomy information
metadata <- read_csv("infant_metadata.csv")
metadata

taxonomy <- read_csv("infant_taxa.csv")
taxonomy

percent_df <- percent_df %>%
  left_join(metadata, by = c("sampleID" = "sampleID")) %>%  # <- adjust if needed
  relocate(where(is.character), .before = everything()) # move metadata cols to front

percent_df

# Store taxa names for use in summaries below
taxa_names <- colnames(percent_t)


#  Mean abundance per taxon, grouped by country 
taxon_means <- percent_df %>%
  group_by(country) %>%                                 # <- adjust if needed
  summarise(across(all_of(taxa_names), mean)) %>%
  pivot_longer(-country, names_to = "genome", values_to = "Mean_Pct") %>%
  arrange(country, desc(Mean_Pct)) %>%
  left_join(taxonomy, by = "genome")

taxon_means %>% arrange(-Mean_Pct) %>% print(n = 30)


# =============================================================================
# CLR TRANSFORMATION
# =============================================================================
# WHY LOG-RATIOS?
#   Compositional data lives in a simplex (values constrained to sum to 100).
#   Regular statistics assume unconstrained Euclidean space → violates assumptions.
#
# CLR (Centered Log-Ratio):
#   clr(x_i) = log(x_i / geometric_mean(x))
#   Positive = taxon is above the geometric mean; negative = below

# CLR with the compositions package
clr_matrix <- as.matrix(clr(OTU))   

# Each row sums to ~0 — this is a property of the CLR transform
round(rowSums(clr_matrix), 10)

# Peek
round(clr_matrix[1:4, 1:4], 3)


# =============================================================================
# AITCHISON PCA — coloured by country
# =============================================================================

#  Run PCA on CLR data 
pca_result <- prcomp(t(clr_matrix), center = TRUE, scale. = FALSE)

# Variance explained 
var_explained <- summary(pca_result)$importance
print(var_explained[, 1:5])

# Scree plot
screeplot(pca_result, type = "lines", main = "Scree Plot — Aitchison PCA")

# Extract PC scores and attach metadata 
pca_scores <- as.data.frame(pca_result$x) %>%
  rownames_to_column("sampleID") %>%
  left_join(metadata, by = c("sampleID" = "sampleID"))  

pca_scores

# PCA plot coloured by country
ggplot(pca_scores, aes(x = PC1, y = PC2, colour = country, label = sampleID)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = country), level = 0.80, linetype = "dashed") +
  labs(
    title    = "Aitchison PCA of Infant Microbial Abundances",
    subtitle = "CLR-transformed relative abundances, coloured by country",
    x        = paste0("PC1 (", pct_var["PC1"], "%)"),
    y        = paste0("PC2 (", pct_var["PC2"], "%)")
  ) +
  theme_bw() +
  theme(legend.position = "right")

ggplot(pca_scores, aes(x = PC1, y = PC2, colour = age_group, label = sampleID)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = age_group), level = 0.90, linetype = "dashed") +
  labs(
    title    = "Aitchison PCA of Infant Microbial Abundances",
    subtitle = "CLR-transformed relative abundances, coloured by country",
    x        = paste0("PC1 (", pct_var["PC1"], "%)"),
    y        = paste0("PC2 (", pct_var["PC2"], "%)")
  ) +
  theme_bw() +
  theme(legend.position = "right")

ggplot(pca_scores, aes(x = PC1, y = PC2, colour = income_group, label = sampleID)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(aes(group = income_group), level = 0.80, linetype = "dashed") +
  labs(
    title    = "Aitchison PCA of Infant Microbial Abundances",
    subtitle = "CLR-transformed relative abundances, coloured by country",
    x        = paste0("PC1 (", pct_var["PC1"], "%)"),
    y        = paste0("PC2 (", pct_var["PC2"], "%)")
  ) +
  theme_bw() +
  theme(legend.position = "right")

# Which taxa drive the separation? (top loadings on PC1)
loadings <- as.data.frame(pca_result$rotation[, 1:2]) %>%
  rownames_to_column("genome") %>%
  arrange(desc(abs(PC1))) %>%
  left_join(taxonomy, by = "genome")

head(loadings, n = 15)


# =============================================================================
# AITCHISON distances and PERMANOMA
# =============================================================================

# Aitchison Distance (Euclidean distance in CLR space)

aitchison_dist <- dist(t(clr_matrix), method = "euclidean")

round(as.matrix(aitchison_dist)[1:5, 1:5], 2)

row.names(metadata) <- metadata$sampleID


# PERMANOVA — use the distance matrix
adonis_r <- vegan::adonis2(aitchison_dist ~ age_group+`income_group`+country, data = metadata, by = "margin")
adonis_r

adonis_r <- vegan::adonis2(aitchison_dist ~ age_group+income_group+country, data = metadata, by = "terms")
adonis_r


# Test within-group dispersion between groups 
betadisper_res <- betadisper(aitchison_dist, metadata$country)
permutest(betadisper_res)

betadisper_res <- betadisper(aitchison_dist, metadata$age_group)
permutest(betadisper_res)

betadisper_res <- betadisper(aitchison_dist, metadata$income_group)
permutest(betadisper_res)


# Hierarchical clustering — use the distance matrix
hc <- hclust(aitchison_dist, method = "ward.D2")

hc <- as.dendrogram(hc)

plot(hc)

