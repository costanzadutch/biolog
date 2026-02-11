#script per fare PCA dynamica, n=3 per condizione (i questo caso)

#setwd ("C:\\Users\\costa\\OneDrive\\Desktop\\OneDrive - CNR\\Lavoro CNR\\lavoro CNR\\Progetti\\2025 Biolog\\2024_ Biolog Eleonora") #da lavoro
#setwd("C:\\Users\\costa\\OneDrive - CNR\\Lavoro CNR\\lavoro CNR\\Progetti\\2025 Biolog\\2024_ Biolog Eleonora") #da casa
setwd("C:\\Users\\costa\\OneDrive - CNR\\Lavoro CNR\\Lavoro da casa\\biolog\\2024 Biolog\\F7\\F7_PCA dynamic TUTTI")
setwd("C:\\Users\\costa\\OneDrive - CNR\\Lavoro CNR\\lavoro CNR\\Progetti\\2025 Biolog\\2024_ Biolog Eleonora\\Aree cumulative\\PCA_F5 e F7")



library(readxl)
library(ggplot2)
library(dplyr)

# --- Parametri ---
#file_input <- "1_F5area_xpiastra_TUTTI_medie.xlsx"
file_input<- "Dati PCA- togliendo poco espressi.xlsx"
#sheet_name <- "F5_medie"
sheet_name <- "F7_tutti_NORM_puliti"

# --- Leggi Excel ---
df <- read_excel(file_input, sheet = sheet_name)

# --- Colonna colore manuale ---
gruppi <- c(rep("CNTL", 34), rep("CARV", 34), rep("PHECARV", 33))
df$Gruppo <- gruppi

# --- Seleziona solo colonne numeriche per PCA ---
df_numerical <- df %>% select(where(is.numeric))

# --- Standardizza i dati ---
df_scaled <- scale(df_numerical)

# --- PCA ---
pca_result <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# --- Estrai le prime due componenti principali ---
pca_df <- as.data.frame(pca_result$x[,1:2])
colnames(pca_df) <- c("PC1", "PC2")

# --- Aggiungi la variabile colore ---
pca_df$Gruppo <- df$Gruppo

# --- Grafico PCA colorato ---
ggplot(pca_df, aes(x = PC1, y = PC2, color = Gruppo)) +
  geom_point(size = 3, alpha = 0.8) +
  ggtitle("F7_PCA - norm_ high expression") +
  xlab(paste0("PC1 (", round(summary(pca_result)$importance[2,1]*100,1), "%)")) +
  ylab(paste0("PC2 (", round(summary(pca_result)$importance[2,2]*100,1), "%)")) +
  theme_minimal() +
  scale_color_manual(values = c("CNTL" = "blue", "CARV" = "green", "PHECARV" = "red"))
