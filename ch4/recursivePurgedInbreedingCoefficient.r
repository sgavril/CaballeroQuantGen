# Recursive function to calculate purged inbreeding coefficient
# Equation from Armando Caballero's quantitative genetics text (4.26)

purged <- function(t, N, Ft, d) {
  # At generation 0, inbreeding is 0
  if (t == 0) {
    return (0)
  }
  
  else {
    # Calculate inbreeding for generation t-1
    inbreeding <- (1 - (1 - (1/(2 * N)))^(t-1))
    # Recursively calculate purged coefficient for for generation t-1
    g_tm1 <- purged(t-1, N, inbreeding, d)
    # Calculate value for 
    g_t <- ( (1/(2*N)) + (1 - (1/(2*N)))*g_tm1)*(1 - (2*d*inbreeding))
    print(paste0("g_t at generation ", t, " is: ", g_t))
    return (g_t)
  }
}

ft20 <- (1 - (1 - (1/(2 * 20)))^20)
purged(t = 20, N = 20, Ft = ft20, d = 0.2)