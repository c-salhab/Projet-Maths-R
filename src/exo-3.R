# Exercice 3

# insertion des données
annees <- 2018:2022
trimestres <- 1:4
ventes <- c(254, 414, 795, 198,   
            300, 472, 903, 265,  
            466, 568,1115, 290,   
            352, 624,1274, 303)

temps <- 1:length(ventes)  # 16 periodes

# 1.
plot(temps, ventes, type = "o", main = "Série chronologique des ventes", xlab = "Periodes", ylab = "Ventes")

# 2.
n <- length(ventes)
t <- 1:n  # temps
Sx <- sum(t)
Sy <- sum(ventes)
Sxx <- sum(t^2)
Sxy <- sum(t * ventes)

a <- (n * Sxy - Sx * Sy) / (n * Sxx - Sx^2)
b <- (Sy - a * Sx) / n

droite <- a * t + b
cat("Droite : Y =", round(a, 2), "* t +", round(b, 2), "\n")

# tracer la droite y = ax + b
plot(t, ventes, type = "o", main = "Tendance linéaire", xlab = "Temps", ylab = "Ventes")
lines(t, droite, col = "blue")


# 3.
moy_mobiles <- rep(NA, n - 3)
for (i in 1:(n - 3)) {
  moy_mobiles[i] <- mean(ventes[i:(i+3)])
}
print(moy_mobiles)

# pour aligner : NA en debut et fin
tendance <- c(NA, moy_mobiles, NA)
residus <- ventes - tendance  # differences saisonnieres
print(tendance)
print(residus)

# 4. 
# organisation en matrice 5x4 (5 années, 4 trimestres)
mat_residus <- matrix(residus, ncol = 4, byrow = TRUE)

# moyenne des residus par trimestre
coeff_saisons <- colMeans(mat_residus, na.rm = TRUE)

# centrage
coeff_saisons <- coeff_saisons - mean(coeff_saisons)

# Desaisonnalisation
desaisonnalisee <- ventes - rep(coeff_saisons, times = 5)
print(coeffs_saisonniers)
print(desaisonnalisee)

# 5.
# prevoir tendance au trimestre 3 de 2025
t_2025_T3 <- n + 7  # car T3 2025 est le 27e trimestre depuis debut 2018
tendance_prevue <- a * t_2025_T3 + b
saisonnalite_T3 <- coeff_saisons[3]
prevision <- tendance_prevue + saisonnalite_T3
cat("Prévision pour T3 2025 :", round(prevision, 2), "\n")

plot(t, ventes, type = "o", col = "blue", xlim = c(1, t_2025_T3),
     ylim = c(min(ventes), max(ventes)+200), xlab = "Temps", ylab = "Ventes", main = "Prévision T3 2025")
lines(t, droite, col = "red")
points(t_2025_T3, prevision, col = "green", pch = 19)
text(t_2025_T3, prevision + 20, labels = round(prevision, 1), col = "green")

