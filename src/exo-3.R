# Exercice 3

# insertion des données
ventes <- c(254, 414, 795, 198,   
            300, 472, 903, 265,  
            466, 568,1115, 290,   
            352, 624,1274, 303)

# serie a partir de 2019, trimestre 1 
serie <- ts(ventes, start = c(2019, 1), frequency = 4)  

# print(serie)

# 1.
plot(serie, xaxt = "n", main = "Ventes trimestrielles (2019-2022)", ylab = "Ventes", xlab = "Année")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2019", "2020", "2021"))

# 2.
temps <- 1:length(serie)
modele <- lm(serie ~ temps) # equation y = a * x + b

plot(temps, serie, type = "o", main = "Ajustement affine", xlab = "Temps", ylab = "Ventes")
abline(modele, col = "blue", lwd = 2)

# 3.
library(TTR) # pour les moyennes mobiles
moy_mob <- SMA(serie, n = 4)

plot(serie, type = "l",  col = "black", xaxt = "n",  main = "Moyenne mobile", ylab = "Ventes")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2019", "2020", "2021"))
lines(moy_mob, col = "red", lwd = 2)
legend("topleft", legend = c("Données", "Moyenne mobile"), col = c("black", "red"), lty = 1)

# 4. 
tendance <- modele$fitted.values
saisonnalite <- serie - tendance
coeff_sais <- tapply(saisonnalite, cycle(serie), mean)

# print(coeff_sais)

serie_deseasonal <- serie - rep(coeff_sais, length.out = length(serie))
plot(serie_deseasonal, xaxt = "n", main = "Série désaisonnalisée", ylab = "Ventes corrigées", xlab="Année")
axis(1, at = seq(2020, 2022, by = 1), labels = c("2019", "2020", "2021"))


# 5.
#  prevoir tendance au trimestre 3
temps_future <- 27
val_tendance <- predict(modele, newdata = data.frame(temps = temps_future))
val_prevue <- val_tendance + coeff_sais[3]

cat("Prévision pour le T3:", val_prevue, "\n")

