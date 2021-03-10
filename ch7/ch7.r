# Recursive function to calculate additive variance
# Equation from Armando Caballero's quantitative genetics text (4.26)

additiveVariance <- function(t, Ne, VM) {
  if (t == 0) { return(0) }
  else {
    V_tm1 <- additiveVariance(t-1, Ne, VM)
    V_t <- V_tm1*(1 - 1/(2*Ne)) + VM
    print(paste0("V_t at generation ", t, " is: ", V_t))
    return (V_t)
  }
}

additiveVariance(100, 20, 0.0005)


# Question 7.5
P <- c(0.78, 0.75, 0.81, 0.79, 0.83, 0.85)
S <- c(0.63, 0.61, 0.70, 0.69, 0.72, 0.74)
T = 4*S - 2*P
T

mean(P) ; mean(S) ; var(T)
var(P) ; var(S) ; var(T)
cov(mean(P),mean(T))

# Calculate U from W_P = 1- 2U, rearrange to get U = (1_W_P)/2
W_P = mean(P)
U = (1 - W_P)/2
# Calculate h_bar from W_S = 1-U[1+0.5(1/h_bar)]
W_S = mean(S)
h_bar <- 1/(2*((1-W_S)/U - 1))
V_WP = var(P)
s_bar = V_WP / (2*U*h_bar)
covWP_T <- 2*U*s_bar
covWP_T
