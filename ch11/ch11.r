q1 <- data.frame("1" = c("T", "T", "C", "C", "C"),
           "2" = c("C", "C", "T", "T", "T"), 
           "3" = c("T", "C", "C", "C", "C"), 
           "4" = c("A", "T", "C", "C", "C"), 
           "5" = c("C", "A", "C", "C", "C"), 
           "6" = c("C", "C", "C", "C", "T"),
           "7" = c("T", "C", "C", "C", "C"),
           "8" = c("C", "T", "T", "T", "T"), 
           "9" = c("C", "C", "C", "T", "T"),
           "10" = c("T", "C", "T", "C", "T"),
           "11" = c("C", "T", "T", "T", "T"),
           "12" = c("G", "G", "T", "G", "G"), 
           "13" = c("G", "G", "G", "A", "G"),
           "14" = c("T", "T", "C", "C", "C"),
           "15" = c("T", "T", "T", "T", "C"),
           "16" = c("A", "T", "A", "T", "A"))

# Compute theta_pi: the average number of nucleotide differences
# between two given sequences
num_comparisons = 0
num_differences = 0
for (i in 1:(nrow(q1) - 1)) {
  for (j in (i+1):nrow(q1)) {
    #print(paste0("i is: ", print(i), ", j is: ", print(j)))
    num_comparisons = num_comparisons + 1
    num_differences = num_differences + length(which(!q1[i, ] == q1[j, ]))
  }
}
# theta_pi:
num_differences/num_comparisons
# Diversity per position
(num_differences/num_comparisons)/500


# Theta w: 
16/sum(1/(1:4))
# 7.68 is sufficiently close to theta_pi suggesting neutrality

# Q2
q2.obs <- matrix(data = c(71, 106, 23, 13), nrow = 2, ncol = 2)
q2.exp <- matrix(data = c(78.11, 98.99, 15.89, 20.11))
chisq.test(x = q2.obs, p = q2.exp, correct = F)