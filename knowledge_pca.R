## Smallholder households' farming knowledge affects soil ability in marginal areas  
## Occelli, Mantino, Ragaglini, Dell'Acqua, Pè and Nuvolari

# Script for the computation of PCA on 9 core questions about knowledge

# Load libraries 
library(factoextra)
library(readxl)
library(FactoMineR)
library(ggforce)

#Set your working directory
# Load dataset
data <- read_excel("Knowldge_PCA_1.xlsx", 
                     col_types = c("numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                                    "numeric", "numeric", "numeric", "numeric"))
# Clean the dataset
data <- subset(data[-c(1)])
data <- na.omit(data)
View(data)

# Perform PCA for the both study areas
pca_all<- PCA(data, scale.unit = T,  graph = T)
# Plot PCA
a <- fviz_pca_var(pca_all)+
  geom_ellipse(aes(x0=0.35, y0=0.35, a=0.1, b=0.5, angle=-0.8, color="#2166ac")) +
  geom_ellipse(aes(x0=0.45, y0=-0.35, a=0.1, b=0.5, angle=1, color="#b2182b")) + 
  labs(title = "") + 
  theme_minimal() +
  theme(legend.position = 'none')
a
ggsave("PCA_knowledge.pdf", a, width = 11.69, height = 8.27, dpi = 300, units = "in")

# Highlight the relevant dimensions
dimdesc(pca_all)

# End file