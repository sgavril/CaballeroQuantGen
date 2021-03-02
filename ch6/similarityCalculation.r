# Caballero QG Chpt 6 question 6.4

weights <- c(16.6, 22.1, 18, 12.4)
mean_weight <- mean(weights)
var_weight <- var(weights)

# Initialize empty vector
similarities <- c()

# Calculate pairwise similarities for phenotypic values
for (i in 1:length(weights)){
  for (j in 1:length(weights)) {
    if (j <= (length(weights))) {
      print(paste0("i is: ", i, "j is: ", j))
      value <- ((weights[i] - mean_weight)*(weights[j] - mean_weight))/var_weight
      print(value)
      similarities <- c(similarities, value)
    }
    else {}
  }
}
similarities
fMij <- c(1,0.25,0.5,0.5,0.25,1,0.75,0.5,0.5,0.75,1.25,0.375,0.5,0.5,0.375,1)

cov(similarities, fMij)
mean(fMij)
var(fMij)

# Compute heritability
cov(similarities,  fMij)/(2*var(fMij))
# Using rounded values
0.1109/(2*0.08958)