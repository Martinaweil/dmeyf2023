# Limpieza inicial
rm(list = ls())
gc()
library(data.table)
setwd("~/buckets/b1")
datos <- fread("./datasets/competencia_03.csv.gz", sep = ",", header = TRUE)
datos <- datos[foto_mes == '202107']

experimento<-"exp_2" #reemplazar con el nombre del experimento 
setwd(paste0("~/buckets/b1/exp/",experimento))
initial_seed <- 100019
num_seeds <- 30
seeds_to_ensemble <- seq(initial_seed, initial_seed + num_seeds - 1)
# Función para realizar un ensamble con un conjunto de semillas específico
realizar_ensamble <- function(seed_indices) {
  # Leer las predicciones 
  setwd(paste0("~/buckets/b1/exp/",experimento))
  seed <- seeds_to_ensemble[seed_indices[1]]  # Choose the first seed
  predicted_prob_file <- paste0("prediccion_", seed, ".txt")
  ensemble_results <- fread(predicted_prob_file, sep = "\t", header = TRUE)
  setnames(ensemble_results, "prob", paste0("prob_", seed))
  
  
    # Loop sobre las semillas
    for (i in 2:length(seed_indices)) {
      setwd(paste0("~/buckets/b1/exp/",experimento))
      seed <- seeds_to_ensemble[seed_indices[i]]
      predicted_prob_file <- paste0("prediccion_", seed, ".txt")
      predicted_prob <- fread(predicted_prob_file, sep = "\t", header = TRUE)
      
      setnames(predicted_prob, "prob", paste0("prob_", seed))
      
      # Merge en base al numero_de_cliente y foto_mes
      ensemble_results <- merge(ensemble_results, predicted_prob, by = c("numero_de_cliente", "foto_mes"), all = TRUE)
      
      
      #ensemble_results[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prob_", names(ensemble_results))]
      
      #ensemble_results[, grep("prob_", names(ensemble_results)) := NULL]
       }
    
    # Guardo el resultado
    #fwrite(ensemble_results, file = "ensemble_results.txt", sep = "\t")
    ensemble_results[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prob_", names(ensemble_results))]
    setorder(ensemble_results, -ensemble_prob)
    
    ensemble_results[, Predicted := 0L]
    ensemble_results[1:11000, Predicted := 1L]
    
    # Cálculo de ganancia. Envío estímulos a los primeros 11,000 en el ranking

    
    setwd(paste0("~/buckets/b1/exp/",experimento))
    merged_data <- merge(ensemble_results, datos, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
    merged_data <- merged_data[, c("foto_mes", "numero_de_cliente", "clase_ternaria", "Predicted"), with = FALSE]
    
    merged_data[, ganancia := ifelse(Predicted == 1 & clase_ternaria == "BAJA+2", 273000,
                                     ifelse(Predicted == 1 & clase_ternaria != "BAJA+2", -7000, 0))]
    
    total_gain <- sum(merged_data$ganancia)
    return(total_gain)
  }
  
  realizar_baseline <- function(seed) {
      setwd(paste0("~/buckets/b1/exp/",experimento))
      predicted_prob_file <- paste0("prediccion_", seed, ".txt")
      predicted_prob <- fread(predicted_prob_file, sep = "\t", header = TRUE)
      setorder(predicted_prob, -prob)
      predicted_prob[, Predicted := 0L]
      predicted_prob[1:11000, Predicted := 1L]
      
      
      setwd(paste0("~/buckets/b1/exp/",experimento))
      merged_data <- merge(predicted_prob, datos, by = c("foto_mes", "numero_de_cliente"), all.x = TRUE)
      merged_data <- merged_data[, c("foto_mes", "numero_de_cliente", "clase_ternaria", "Predicted"), with = FALSE]
      
      merged_data[, ganancia := ifelse(Predicted == 1 & clase_ternaria == "BAJA+2", 273000,
                                       ifelse(Predicted == 1 & clase_ternaria != "BAJA+2", -7000, 0))]
      
      total_gain <- sum(merged_data$ganancia)
      return(total_gain)
    }
  

  
 

setwd(paste0("~/buckets/b1/exp/",experimento))
# Número de ensambles
num_ensambles <- 10
ensambles_results <- numeric(num_ensambles)

# Realiza 10 ensambles con submuestras de 20 semillas
for (ensamble_idx in 1:num_ensambles) {
  # Submuestreo aleatorio de 20 semillas
  sampled_seeds <- sample(1:num_seeds, 20)
  
  # Realiza el ensamble y almacena la ganancia
  ensambles_results[ensamble_idx] <- realizar_ensamble(sampled_seeds)
}
baseline_results <- numeric(num_seeds)
for (seed in 1:num_seeds) {
  
   
  #Realiza el ensamble y almacena la ganancia
  baseline_results[seed] <- realizar_baseline(seeds_to_ensemble[seed])
  print(realizar_baseline(seeds_to_ensemble[seed]))
}

# Crear un gráfico de líneas para mostrar la evolución de las ganancias a lo largo de los ensambles
plot(1:num_ensambles, ensambles_results, type = "o", xlab = "Ensamble", ylab = "Ganancia Total")
lines(1:num_ensambles, ensambles_results, col = "blue")


# Crear un gráfico de líneas para mostrar la evolución de las ganancias a lo largo de los ensambles
plot(1:num_seeds, baseline_results, type = "o", xlab = "Ensamble", ylab = "Ganancia Total")
lines(1:num_seeds, baseline_results, col = "blue")



# Set up a multi-panel plot
par(mfrow = c(1, 2), mar = c(5, 4, 4, 2) + 0.1)

# Plot for ensemble results
plot(1:num_ensambles, ensambles_results, type = "o", xlab = "Ensemble", ylab = "Total Gain", col = "red")
title(main = "Ensemble Results")

# Plot for baseline results
plot(1:num_seeds, baseline_results, type = "o", xlab = "Baseline", ylab = "Total Gain", col = "blue")
title(main = "Baseline Results")

# Reset the plot parameters to default
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)

##


# Create a scatter plot to show the gains for ensembles
plot(1:num_ensambles, ensambles_results, type = "o", xlab = "Model", ylab = "Total Gain", col = "red", ylim = range(c(ensambles_results, baseline_results)))

# Add points for baseline results on the same plot
points(rep(1:num_ensambles, each = 3) + 0.2, baseline_results, col = "blue")

# Add legend to differentiate between ensemble and baseline
legend("topright", legend = c("Ensemble", "Baseline"), col = c("red", "blue"), pch = 1)

# Customize the x-axis labels
axis(1, at = c(1), labels = c("Ensemble"))




