# 1. 

ventes <- c(4, 3, 5, 22,
            3, 4, 6, 21,
            2, 4, 6, 24,
            3, 4, 7, 26)

plot(ventes, type = "o", main = "Ventes trimestrielles de dindes", xlab = "Trimestre", ylab = "Ventes")

# 2. 

t <- 1:16
n <- length(t)
somme_xt <- sum(t * ventes)
somme_t <- sum(t)
somme_x <- sum(ventes)
somme_t2 <- sum(t^2)

a <- (n * somme_xt - somme_t * somme_x) / (n * somme_t2 - somme_t^2)
b <- (somme_x - a * somme_t) / n
Tt <- a * t + b

print(a)
print(b)

plot(t, ventes, type = "o",
     main = "Ventes trimestrielles de dindes",
     xlab = "Trimestre", ylab = "Ventes")
lines(t, Tt, col = "red", lwd = 2)
legend("topleft", legend = c("Ventes", "Droite d'ajustement affine"),
       col = c("black", "red"), lty = c(1, 1), lwd = 2)


# 3. 

# moyenne mobile pondérée centrée 4 (formule sujet)
MM <- rep(NA, 16)
for (i in 3:14) {
  MM[i] <- (ventes[i - 2] + 2 * ventes[i - 1] + 2 * ventes[i + 1] + ventes[i + 2]) / 6
}

# diff saisonieres 
St_hat <- ventes - Tt

resultats_q3 <- data.frame(
  Trimestre = t,
  Ventes = ventes,
  Tendance = round(Tt, 2),
  MoyMob = round(MM, 2),
  DiffSaison = round(St_hat, 2)
)

print(resultats_q3)

# 4. 

St_mat <- matrix(St_hat, ncol = 4, byrow = TRUE)
St_moy <- colMeans(St_mat)

St_corr <- St_moy - mean(St_moy)

Xt_deseason <- ventes - rep(St_corr, 4)

resultats_q4 <- data.frame(
  Trimestre = t,
  Ventes = ventes,
  Saisonnalite = rep(round(St_corr, 2), 4),
  Deseasonnalisee = round(Xt_deseason, 2)
)

print(resultats_q4)


plot(t, ventes, type = "o",
     main = "Ventes trimestrielles de dindes",
     xlab = "Trimestre", ylab = "Ventes")
lines(t, Xt_deseason, col = "blue", lwd = 2, lty = 2)
legend("topleft", legend = c("Ventes", "Désaisonnalisée"),
       col = c("black", "blue"), lty = c(1, 2), lwd = 2)

t_future <- 28
T_future <- a * t_future + b
S4 <- St_corr[4]
X_pred <- T_future + S4

plot(t, ventes, type = "o",
     ylim = c(min(ventes), X_pred + 5),
     xlim = c(1, 30),
     main = "Ventes trimestrielles de dindes",
     xlab = "Trimestre", ylab = "Ventes")

lines(t, Tt, col = "red", lwd = 2)
# La ligne désaisonnalisée est supprimée ici
# lines(t, Xt_deseason, col = "blue", lwd = 2, lty = 2)

points(t_future, X_pred, col = "green", pch = 19, cex = 1.5)
text(t_future, X_pred + 1, labels = paste("Prévision :", round(X_pred, 2)), col = "green")

legend("topleft", legend = c("Ventes", "Tendance", "Prévision"),
       col = c("black", "red", "green"),
       lty = c(1, 1, NA), pch = c(1, NA, 19), lwd = 2)

