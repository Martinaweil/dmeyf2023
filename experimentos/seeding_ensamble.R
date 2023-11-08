
#limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection
library(data.table)

initial_seed <- 100019
num_seeds <- 30
seeds_to_ensemble <- seq(initial_seed, initial_seed + num_seeds - 1)

# Read the first prediction file and create the starting point for the ensemble_results
seed <- seeds_to_ensemble[1]  # Choose the first seed
predicted_prob_file <- paste0("prediccion_", seed, ".txt")
ensemble_results <- fread(predicted_prob_file, sep = "\t", header = TRUE)

# Rename the 'prob' column to include the seed number
setnames(ensemble_results, "prob", paste0("prob_", seed))

# Loop over the remaining seeds and add their probabilities to ensemble_results
for (i in 2:length(seeds_to_ensemble)) {  # Start from the second seed
  seed <- seeds_to_ensemble[i]
  predicted_prob_file <- paste0("prediccion_", seed, ".txt")
  predicted_prob <- fread(predicted_prob_file, sep = "\t", header = TRUE)
  
  # Rename the 'prob' column in predicted_prob to avoid naming conflicts
  setnames(predicted_prob, "prob", paste0("prob_", seed))
  
  # Merge the predicted probabilities with the ensemble_results based on numero_de_cliente and foto_mes
  ensemble_results <- merge(ensemble_results, predicted_prob, by = c("numero_de_cliente", "foto_mes"), all = TRUE)
  
  # Calculate the average probability for each client and month
  ensemble_results[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prob_", names(ensemble_results))]
  
  # Remove individual seed probability columns
  ensemble_results[, grep("prob_", names(ensemble_results)) := NULL]
}

# Save the ensembled results
fwrite(ensemble_results, file = "ensemble_results.txt", sep = "\t")
setorder(ensemble_results, -ensemble_prob)
cortes <- seq(8000, 15000, by = 500)
for (envios in cortes) {
  ensemble_results[, Predicted := 0L]
  ensemble_results[1:envios, Predicted := 1L]
  
  fwrite(ensemble_results[, list(numero_de_cliente, Predicted)],
         file = paste0("entrega_", envios, ".csv"),
         sep = ","
  )
}

cat("\n\nLa generacion de los archivos para Kaggle ha terminado\n")



