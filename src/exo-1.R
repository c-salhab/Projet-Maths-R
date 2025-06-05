#Exercice 1 : Cas d'une matrice non diagonalisable-----------------------------------------------------------------------------

#Question 1 : Fonction pour générer une matrice aléatoire et calculer les valeurs propres-------------------------------
generate_matrix_and_eigenvalues <- function() {
  # Demander à l'utilisateur de saisir un entier p
  p <- as.integer(readline(prompt = "Entrez un entier naturel p > 1: "))
  
  # Vérifier que p est valide
  if (is.na(p) || p <= 1) {
    stop("L'entier doit être supérieur à 1")
  }
  
  # Générer une matrice aléatoire d'ordre p avec des entiers entre 1 et 10
  mat <- matrix(sample(1:10, p*p, replace = TRUE), nrow = p, ncol = p)
  
  # Calculer les valeurs propres
  eigen_values <- eigen(mat)$values
  
  # Arrondir les valeurs propres à 10^-2 près
  eigen_values_rounded <- round(eigen_values, 2)
  
  # Calculer les multiplicités
  multiplicities <- table(eigen_values_rounded)
  
  # Afficher les résultats
  cat("\nMatrice générée:\n")
  print(mat)
  cat("\nValeurs propres (arrondies à 10^-2):\n")
  print(eigen_values_rounded)
  cat("\nMultiplicités:\n")
  print(multiplicities)
  
  # Retourner la matrice
  return(mat)
}

#teste de la fct:
generate_matrix_and_eigenvalues()


# Appliquer sur la matrice A
A <- matrix(c(0, -1, -1, 2, 2, 1, 2, 2, 3), nrow = 3, byrow = FALSE)
cat("Matrice A:\n")
print(A)

# Calculer les valeurs propres de A
eigen_A <- eigen(A)
eigen_values_A <- round(eigen_A$values, 2)
multiplicities_A <- table(eigen_values_A)

cat("\nValeurs propres de A (arrondies à 10^-2):\n")
print(eigen_values_A)
cat("\nMultiplicités pour A:\n")
print(multiplicities_A)



#Question 2 : Vecteurs propres et diagonalisabilité--------------------------------------------------------------------
vp <- eigen(A)
valeurs <- round(vp$values, 2)
vecteurs <- round(vp$vectors, 2)

cat("Valeurs propres :\n")
print(valeurs)
cat("Vecteurs propres (colonnes) :\n")
print(vecteurs)

cat("Remarque : La valeur propre 2 a une multiplicité algébrique de 2,\n")
cat("mais on ne trouve que 2 vecteurs   propre linéairement indépendant.\n")
cat("Cela signifie que la valeur propre λ = 2 n’a qu’un seul vecteur propre (même si sa multiplicité est 2)-> A est non diagonalisable ")


#question 03 : matrice de jordan --------------------------------------------------------------------------------------------------
v_propre_2 <- Re(vecteurs[,1])  # vecteur propre pour λ = 2
v_propre_1 <- Re(vecteurs[,3])  # vecteur propre pour λ = 1


# 3. Construire (A - 2I)
A2 <- A - 2 * diag(3) 

# 4. Résoudre (A - 2I) * v_gen = v_propre_2 pour trouver le vecteur généralisé
library(MASS)
v_gen <- ginv(A2) %*% v_propre_2
print(v_gen)

P <- cbind(v_propre_2, v_gen, v_propre_1)


J <- matrix(c(2,1,0,
              0,2,0,
              0,0,1), nrow=3, byrow=TRUE)

# Vérifier que A = P J P⁻¹
P_inv <- solve(P)
A_reconstruit <- P %*% J %*% P_inv

# Comparaison
round(A_reconstruit, 0) # on retrouve bien A 




#Question 04:
# Fonction pour calculer (a_n, b_n, c_n) pour un n donné ------------------------------------------
calculate_abc_n <- function(n, P, J, X0) {
  # Vérifier que n est un entier naturel non nul
  if (!is.numeric(n) || n <= 0 || floor(n) != n) {
    stop("n doit être un entier naturel non nul")
  }
  
  # Calculer J^n (puissance de la matrice de Jordan)
  J_n <- diag(nrow(J))
  for (i in 1:n) {
    J_n <- J_n %*% J
  }
  
  # Calculer X_n = P J^n P^{-1} X0
  P_inv <- solve(P)
  X_n <- P %*% J_n %*% P_inv %*% X0
  
  # Arrondir les résultats à 10^-2
  X_n_rounded <- round(Re(X_n), 2)
  
  return(X_n_rounded)
}

# Initialisation X0 (a0, b0, c0)
X0 <- matrix(c(1, 2, -3), nrow=3)

# Demander à l'utilisateur de saisir n
n_input <- as.integer(readline(prompt = "Entrez un entier naturel non nul n : "))

# Calculer (a_n, b_n, c_n)
abc_n <- calculate_abc_n(n_input, P, J, X0)

cat("Pour n =", n_input, ":\n")
cat("a_n =", abc_n[1], "\n")
cat("b_n =", abc_n[2], "\n")
cat("c_n =", abc_n[3], "\n")

# Interprétation
cat("\nObservation : Pour n grand, les valeurs de (a_n, b_n, c_n) croissent rapidement,\n")
cat("ce qui est cohérent avec la valeur propre dominante λ=2 et la structure du bloc de Jordan\n")


