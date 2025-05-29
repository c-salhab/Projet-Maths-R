# Exercice 2 : Etude de cas (1)

# insertion des données
ventes <- c(3, 4, 6, 21,
          2, 4, 6, 24,
          3, 4, 7, 26)

serie <- ts(ventes, start= c(2020,1), frequency = 4)

# print(serie)

# 1.
plot(serie, xaxt = "n", main = "Ventes trimestrielles de dinde (2020-2022)", ylab = "Ventes", xlab = "Année")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2020", "2021", "2022"))

# 2. 
temps <- 1:length(serie)
modele <- lm(serie ~ temps) # equation : y = a * x + b

plot(temps, serie, type = "o", main = "Ajustement affine", ylab = "Ventes", xlab = "Temps")
abline(modele, col = "blue", lwd = 2)


# 3.
library(TTR)  # pour les moyennes mobiles
moyenne_mobile <- SMA(serie, n = 4)

plot(serie, type = "l", col = "black", xaxt = "n", main = "Moyenne mobile", ylab = "Ventes")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2020", "2021", "2022"))
lines(moyenne_mobile, col = "red")
legend("topleft", legend = c("Données", "Moyenne mobile"), col = c("black", "red"), lty = 1)

# 4.
# on commence par la decomposition
# ventes = tendance + saisonnalité + aléa
decomp <- decompose(serie, type = "additive")
plot(decomp)

# on extrait les composantes
saisonnalite <- decomp$seasonal
tendance <- decomp$trend
residu <- decomp$random

# série désaisonnalisée
desaisonnalisee <- serie - saisonnalite
plot(desaisonnalisee, xaxt = "n", main = "Série désaisonnalisée", ylab = "Ventes")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2020", "2021", "2022"))

# 5.
# prevoir tendance au trimestre 4 de 2025
nouveau_temps <- length(serie) + (4 * (2025 -2022)) # 12 + 12 = 24
prevision_tendance <- predict(modele, newdata = data.frame(temps = nouveau_temps))
coeff_saisonnier_q4 <- saisonnalite[4]
prevision_finale <- prevision_tendance + coeff_saisonnier_q4

cat("Prévision pour le T4 2025 :", prevision_finale, "\n")
