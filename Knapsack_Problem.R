# Gewicht und Wert von jedem Gegenstand im Rucksack
##weights <- c(2, 3, 4, 5)
##values <- c(3, 4, 5, 6)
weights <-
  c(3.5, 2.5, 2.0, 3.0, 1.0, 1.75, 0.75, 3, 2.5, 2.25)
values <- c(375, 300, 100, 225, 50, 125, 75, 275, 150, 50)
max_weight <- 15

# Maximales Gewicht, das der Rucksack tragen kann
max_weight <- 8

# Fitnessfunktion: Bewertet die Eignung eines Rucksacks
fitness <- function(x) {
  total_weight <- sum(x * weights)
  if (total_weight > max_weight) {
    return(0)
  } else {
    return(sum(x * values))
  }
}

# Anzahl der Gene im Rucksack
n_genes <- length(weights)

# Größe der Population
population_size <- 8

# Anzahl der Generationen
num_generations <- 2

generation1 <- matrix(c(1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1, 1, 1, 
                        1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 1, 1, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 1, 1, 1, 1, 0, 0, 1, 0, 
                        1, 0, 1, 0, 1, 1, 1, 0, 0, 0), nrow = 8, ncol = 8)
# Generiere eine zufällige Population von Rucksäcken
#population <- replicate(population_size, sample(c(0,1), n_genes, replace = TRUE))

# Schleife für die Anzahl der Generationen
for (i in 1:num_generations) {
  
  # Berechne die Fitness jedes Rucksacks in der Population
  # apply (Matrixinput, 1= Zeile und 2 = Spalte , Funktion die ausgeführt werden soll)
  fitness_scores <- apply(generation1, 2, fitness)
  
  # Wähle die besten 2 Rucksäcke aus der Population aus
  best_rucksacks <- generation1[, order(fitness_scores, decreasing = TRUE)][1:2, ]
  
  # Kreuzen der besten 2 Rucksäcke, um eine neue Population zu generieren
  new_population <- matrix(NA, nrow = n_genes, ncol = population_size)
  for (j in 1:population_size) {
    parent1 <- best_rucksacks[, sample(2, 1)]
    parent2 <- best_rucksacks[, sample(2, 1)]
    child <- parent1
    child[which(parent2 != parent1)] <- parent2[which(parent2 != parent1)]
    new_population[, j] <- child
  }
  
  # Aktualisiere die Population
  generation1 <- new_population
}

# Wähle den besten Rucksack aus der finalen Population
best_rucksack <- generation1[, which.max(apply(generation1, 2, fitness))]

# Zeige den besten Rucksack an
print(best_rucksack)
