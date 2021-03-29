# Caballero ch 9 problem 1
eggs <- c(11,12,13,14,15,16,17,18,19,20,21,22,23)
females <- c(2,3,6,4,8,9,12,13,11,9,5,2,1)

0.2*85 # 17 top females, corresponds to all of 20, 21, 22, 23
selected_eggs <- c(20,21,22,23)
selected_females <- c(9,5,2,1)
# Selection differential: mean of selected - mean of population
S =  sum(selected_eggs*selected_females)/17 - sum(eggs*females)/85
S

# Expected response = heritability * Selection differential (breeders eq)
R = 0.2 * S
R

# Expected mean
sum(eggs*females)/85 + R
