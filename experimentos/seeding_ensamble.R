# Limpieza inicial
rm(list = ls())
gc()
library(data.table)

initial_seed <- 100019
num_seeds <- 30
seeds_to_ensemble <- seq(initial_seed, initial_seed + num_seeds - 1)
# Función para realizar un ensamble con un conjunto de semillas específico
realizar_ensamble <- function(seed_indices) {
  # Leer las predicciones que hiciste
  seed <- seeds_to_ensemble[seed_indices[1]]  # Choose the first seed
  predicted_prob_file <- paste0("prediccion_", seed, ".txt")
  ensemble_results <- fread(predicted_prob_file, sep = "\t", header = TRUE)
  setnames(ensemble_results, "prob", paste0("prob_", seed))
  
  # Loop sobre las semillas
  for (i in 2:length(seed_indices)) {
    setwd("~/buckets/b1/exp/exp_1")
    seed <- seeds_to_ensemble[seed_indices[i]]
    predicted_prob_file <- paste0("prediccion_", seed, ".txt")
    predicted_prob <- fread(predicted_prob_file, sep = "\t", header = TRUE)
    
    setnames(predicted_prob, "prob", paste0("prob_", seed))
    
    # Merge en base al numero_de_cliente y foto_mes
    ensemble_results <- merge(ensemble_results, predicted_prob, by = c("numero_de_cliente", "foto_mes"), all = TRUE)
    
    # Promedio las probabilidades
    ensemble_results[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prob_", names(ensemble_results))]
    
    ensemble_results[, grep("prob_", names(ensemble_results)) := NULL]
  }
  
  # Guardo el resultado
  fwrite(ensemble_results, file = "ensemble_results.txt", sep = "\t")
  setorder(ensemble_results, -ensemble_prob)
  
  ensemble_results[, Predicted := 0L]
  ensemble_results[1:11000, Predicted := 1L]
  
  # Cálculo de ganancia. Envío estímulos a los primeros 11,000 en el ranking
  setwd("~/buckets/b1")
  datos <- fread("./datasets/competencia_03.csv.gz", sep = ",", header = TRUE)
  datos <- datos[foto_mes == '202107']
  
  setwd("~/buckets/b1/exp/exp_1")
  merged_data <- merge(ensemble_results, datos, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
  merged_data <- merged_data[, c("foto_mes", "numero_de_cliente", "clase_ternaria", "Predicted"), with = FALSE]
  
  merged_data[, ganancia := ifelse(Predicted == 1 & clase_ternaria == "BAJA+2", 273000,
                                   ifelse(Predicted == 1 & clase_ternaria != "BAJA+2", -7000, 0))]
  
  total_gain <- sum(merged_data$ganancia)
  return(total_gain)
}

setwd("~/buckets/b1/exp/exp_1")
# Número de ensambles
num_ensambles <- 10
ensamble_results <- numeric(num_ensambles)

# Realiza 10 ensambles con submuestras de 20 semillas
for (ensamble_idx in 1:num_ensambles) {
  # Submuestreo aleatorio de 20 semillas
  sampled_seeds <- sample(1:num_seeds, 20)
  
  # Realiza el ensamble y almacena la ganancia
  ensamble_results[ensamble_idx] <- realizar_ensamble(sampled_seeds)
}

# Crear un gráfico de líneas para mostrar la evolución de las ganancias a lo largo de los ensambles
plot(1:num_ensambles, ensamble_results, type = "o", xlab = "Ensamble", ylab = "Ganancia Total")
lines(1:num_ensambles, ensamble_results, col = "blue")









