library(FactoMineR)
library(factoextra)

# charger les donnees depuis le fichier data.txt
data <- read.table("data.txt", header = TRUE, sep = "\t", dec = ".", stringsAsFactors = FALSE)

# affichage des premieres lignes
head(data)

vars <- c("popu", "entr", "rece", "sean", "comm", "etab", "salle", "faut", "artes", "multi")
cor_mat <- cor(data[, vars])
round(cor_mat, 2)

unique(data$depart)


data[data$depart == "Paris", ]

offre <- data[, c("sean", "comm", "etab", "salle", "faut", "artes", "multi")]

# ACP sur donnees centrees reduites
acp1 <- PCA(offre, scale.unit = TRUE, ncp = 5, graph = FALSE)

# cercle des correelations
fviz_pca_var(acp1, col.var = "steelblue")

# plan des individus
fviz_pca_ind(acp1, label = "all", repel = TRUE)

acp1$ind$coord["Paris", ]  # coordonnees de Paris

# creation de nouvelles variables par habitant
data$entr_hab <- data$entr / data$popu
data$rece_hab <- data$rece / data$popu
data$sean_hab <- data$sean / data$popu * 1000
data$comm_hab <- data$comm / data$popu
data$etab_hab <- data$etab / data$popu
data$salle_hab <- data$salle / data$popu
data$faut_hab <- data$faut / data$popu
data$artes_hab <- data$artes / data$popu
data$multi_hab <- data$multi / data$popu

# variables normalisees par habitant
offre2 <- data[, c("sean_hab", "comm_hab", "etab_hab", "salle_hab", "faut_hab", "artes_hab", "multi_hab")]

# ACP ponderee
poids <- data$popu / sum(data$popu)  # poids relatifs
acp2 <- PCA(offre2, scale.unit = TRUE, ncp = 5, row.w = poids, graph = FALSE)

# variables normalisees par habitant
offre2 <- data[, c("sean_hab", "comm_hab", "etab_hab", "salle_hab", "faut_hab", "artes_hab", "multi_hab")]

# ACP ponderee
poids <- data$popu / sum(data$popu)  # poids relatifs
acp2 <- PCA(offre2, scale.unit = TRUE, ncp = 5, row.w = poids, graph = FALSE)

acp2$eig  # valeurs propres

fviz_pca_var(acp2, col.var = "cos2")

fviz_pca_ind(acp2, col.ind = "cos2", repel = TRUE)

fviz_cos2(acp2, choice = "ind", axes = 1:2)

