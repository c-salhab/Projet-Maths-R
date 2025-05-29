# Exercice 2 : Etude de cas (1)

# insertion des données
annees <- c(2019, 2020, 2021, 2022)
trimestres <- 1:4
ventes <- c(4, 3, 5, 22,
            3, 4, 6, 21,
            2, 4, 6, 24,
            3, 4, 7, 26)

temps <- 1:length(ventes)  # 16 periodes

# 1.
plot(temps, ventes, type = "o", main = "Série chronologique des ventes", xlab = "Periodes", ylab = "Ventes")


# 2. 
n <- length(temps)
Sx <- sum(temps)
Sy <- sum(ventes)
Sxx <- sum(temps^2)
Sxy <- sum(temps * ventes)

# coefficients a (pente) et b (ordonnée) pour y = ax + b
a <- (n * Sxy - Sx * Sy) / (n * Sxx - Sx^2)
b <- (Sy - a * Sx) / n
cat("Droite d'ajustement : Y =", round(a, 4), "* t +", round(b, 4), "\n")
abline(a = b, b = a, col = "blue")


# 3.
moy_mobiles <- rep(NA, n - 3)
for (i in 1:(n - 3)) {
  moy_mobiles[i] <- mean(ventes[i:(i+3)])
}
print(moy_mobiles)

# extraction de la tendance estimee
tendance <- c(NA, moy_mobiles, NA)  # pour aligner sur les donnees
print(tendance)

# differences saisonnieres
residus <- ventes - tendance
print(residus)

# 4.
# calcul des moyennes des residus par trimestre
saisons <- matrix(residus, nrow = 4)
coeffs_saisonniers <- colMeans(saisons, na.rm = TRUE)

# centrage des coefficients saisonniers
coeffs_saisonniers <- coeffs_saisonniers - mean(coeffs_saisonniers)

# serie desaisonnalisee
desaisonnalisee <- ventes - rep(coeffs_saisonniers, times = 4)

print(coeffs_saisonniers)
print(desaisonnalisee)

# 5.
# prevoir tendance au trimestre 4 de 2025
# t = 28 pour T4 2025
t_2025_T4 <- 28
tendance_2025_T4 <- a * t_2025_T4 + b

# ajout du coefficient saisonnier T4
saisonnalite_T4 <- coeffs_saisonniers[4]
prevision <- tendance_2025_T4 + saisonnalite_T4

cat("Prévision pour T4 2025 :", round(prevision, 2), "\n")
