#Exercice 04---------------------------------------------------------------------------------------------------
is_palindrome <- function(mot) {
  # mettre en minuscule
  mot <- tolower(mot)
  
  # enlever les espaces
  mot_sans_espaces <- ""
  for (i in 1:nchar(mot)) {
    lettre <- substr(mot, i, i)
    if (lettre != " ") {
      mot_sans_espaces <- paste0(mot_sans_espaces, lettre)
    }
  }
  
  # longueur de la chaîne nettoyée
  n <- nchar(mot_sans_espaces)
  
  # vérifier caractère par caractère
  est_palindrome <- TRUE
  for (i in 1:floor(n/2)) {
    if (substr(mot_sans_espaces, i, i) != substr(mot_sans_espaces, n - i + 1, n - i + 1)) {
      est_palindrome <- FALSE
      break
    }
  }
  
  # message de sortie
  if (est_palindrome) {
    return(paste0(mot, " est un palindrome"))
  } else {
    return(paste0(mot, " n'est pas un palindrome"))
  }
}

mots <- c("radar", "bonne année", "sept", "kayak", "la mariee ira mal", "statistiques", "engage le jeu que je le gagne", "esope reste ici et se repose")

for (mot in mots) {
  print(is_palindrome(mot))
}


# Fonction générant un mot aléatoire
generate_random_word <- function(length_word) {
  lettres <- letters  # alphabet a-z
  mot <- ""
  for (i in 1:length_word) {
    mot <- paste0(mot, sample(lettres, 1))
  }
  return(mot)
}

# Générer dictionnaire
dictionnaire <- c()
for (len in 3:8) {
  for (i in 1:1000) {
    dictionnaire <- c(dictionnaire, generate_random_word(len))
  }
}

# Fonction pour trouver les palindromes dans un vecteur de mots (sans fonctions prédéfinies)
find_palindromes <- function(words) {
  resultats <- c()
  for (mot in words) {
    if (is_palindrome(mot) == paste0(mot, " est un palindrome")) {
      resultats <- c(resultats, mot)
    }
  }
  return(resultats)
}

# Appliquer la fonction sur le dictionnaire
palindromes_trouves <- find_palindromes(dictionnaire)
cat("Nombre de palindromes trouvés dans le dictionnaire :", length(palindromes_trouves), "\n")


