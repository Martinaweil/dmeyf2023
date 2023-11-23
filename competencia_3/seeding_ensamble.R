# Limpieza inicial
rm(list = ls())
gc()
library(data.table)
setwd("~/buckets/b1")

experimento<-"comp3_1" #reemplazar con el nombre del experimento 
setwd(paste0("~/buckets/b1/exp/",experimento))
initial_seed <- 100019
num_seeds <- 30
seeds_to_ensemble <- seq(initial_seed, initial_seed + num_seeds - 1)

  setwd(paste0("~/buckets/b1/exp/",experimento))
  seed <- seeds_to_ensemble[1]  # Choose the first seed
  predicted_prob_file <- paste0("prediccion_", seed, ".txt")
  ensemble_results <- fread(predicted_prob_file, sep = "\t", header = TRUE)
  setnames(ensemble_results, "prob", paste0("prob_", seed))
  
  
    # Loop sobre las semillas
    for (i in 2:length(num_seeds)) {
      setwd(paste0("~/buckets/b1/exp/",experimento))
      seed <- seeds_to_ensemble[i]
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
    fwrite(ensemble_results[, list(numero_de_cliente, Predicted)],
           file = paste0(experimento, "_11000", ".csv"),
           sep = ",")
  

    

  
  