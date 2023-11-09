
#limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection
library(data.table)

initial_seed <- 100019
num_seeds <- 30
seeds_to_ensemble <- seq(initial_seed, initial_seed + num_seeds - 1)

# leemos las predicciones que hicimos
seed <- seeds_to_ensemble[1]  # Choose the first seed
predicted_prob_file <- paste0("prediccion_", seed, ".txt")
ensemble_results <- fread(predicted_prob_file, sep = "\t", header = TRUE)


setnames(ensemble_results, "prob", paste0("prob_", seed))

# Loop sobre las semillas
for (i in 2:length(seeds_to_ensemble)) {  
  seed <- seeds_to_ensemble[i]
  predicted_prob_file <- paste0("prediccion_", seed, ".txt")
  predicted_prob <- fread(predicted_prob_file, sep = "\t", header = TRUE)
  
 
  setnames(predicted_prob, "prob", paste0("prob_", seed))
  
  # mergeo en base al  numero_de_cliente y foto_mes
  ensemble_results <- merge(ensemble_results, predicted_prob, by = c("numero_de_cliente", "foto_mes"), all = TRUE)
  
  # promedio las prob
  ensemble_results[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prob_", names(ensemble_results))]
  
  
  ensemble_results[, grep("prob_", names(ensemble_results)) := NULL]
}

# guardo el resultado
fwrite(ensemble_results, file = "ensemble_results.txt", sep = "\t")
setorder(ensemble_results, -ensemble_prob)


ensemble_results[, Predicted := 0L]
ensemble_results[1:11000, Predicted := 1L]

#calculo de ganancia. Envio estimulos a los primeros 11mil en el ranking

datos<-fread("./datasets/competencia_03.csv.gz", sep = ",", header = TRUE) 
datos <- datos[foto_mes == '202107']


merged_data <- merge(ensemble_results, datos, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
merged_data <- merged_data[, c("foto_mes", "numero_de_cliente", "clase_ternaria","Predicted"), with = FALSE]




merged_data[, ganancia := ifelse(Predicted == 1 & clase_ternaria == "BAJA+2", 273000,
                                 ifelse(Predicted == 1 & clase_ternaria != "BAJA+2", -7000, 0))]


total_gain <- sum(merged_data$ganancia)









