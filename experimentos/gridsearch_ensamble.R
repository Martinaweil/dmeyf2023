
require("data.table")
require("rlist")

require("lightgbm")
library(lightgbm)

# Definir los hiperparámetros y sus posibles valores
param_grid <- list(
  learning_rate = c(0.01, 0.05, 0.1,0.5,1),
  num_leaves = c(15, 31, 63,40,80),
  feature_fraction = c(0.8, 0.6,0.4,0.2),
  min_data_in_leaf = c(3000,5000,6000,7000,8000)
  
)

# Datos de ejemplo (reemplaza con tus propios datos)
PARAM <- list()
dataset <- fread("./datasets/competencia_03.csv.gz")
PARAM$input$testing <- c(202106)
PARAM$input$validation <- c(202105)
PARAM$input$training <- c(202011,202012,202101,202102,202103,202104)

# Inicializar variables para almacenar los resultados
best_params <- NULL
best_auc <- -Inf

initial_seed <- 100019
num_seeds <- 30
seeds <- seq(initial_seed, initial_seed + num_seeds - 1)


# Feature Engineering Historico  ----------------------------------------------
#   aqui deben calcularse los  lags y  lag_delta
#   Sin lags no hay paraiso ! corta la bocha
#   https://rdrr.io/cran/data.table/man/shift.html
col_original <- setdiff(colnames(dataset), c("numero_de_cliente","foto_mes","clase_ternaria"))
# LAGS
lags <- c(1, 3, 6)
for (i in lags){
  dataset[, paste0("lag_", i, "_", col_original) := lapply(.SD, function(x) shift(x, type = "lag", n = i)), 
          by = numero_de_cliente, .SDcols = col_original]
}


# los campos que se van a utilizar
campos_buenos <- setdiff(
  colnames(dataset),
  c("clase_ternaria", "clase01", "azar", "training")
)

# defino los datos que forma parte del training
# aqui se hace el undersampling de los CONTINUA
set.seed(PARAM$trainingstrategy$semilla_azar)
dataset[, azar := runif(nrow(dataset))]
dataset[, training := 0L]
dataset[
  foto_mes %in% PARAM$input$training &
    (azar <= PARAM$trainingstrategy$undersampling | clase_ternaria %in% c("BAJA+1", "BAJA+2")),
  training := 1L
]

# dejo los datos en el formato que necesita LightGBM
dtrain <- lgb.Dataset(
  data = data.matrix(dataset[training == 1L, campos_buenos, with = FALSE]),
  label = dataset[training == 1L, clase01],
  weight = dataset[training == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos que forman parte de validation
#  no hay undersampling
dataset[, validation := 0L]
dataset[ foto_mes %in% PARAM$input$validation,  validation := 1L]

dvalidate <- lgb.Dataset(
  data = data.matrix(dataset[validation == 1L, campos_buenos, with = FALSE]),
  label = dataset[validation == 1L, clase01],
  weight = dataset[validation == 1L, 
                   ifelse(clase_ternaria == "BAJA+2", 1.0000001, 
                          ifelse(clase_ternaria == "BAJA+1", 1.0, 1.0))],
  free_raw_data = FALSE
)


# defino los datos de testing
dataset[, testing := 0L]
dataset[ foto_mes %in% PARAM$input$testing,  testing := 1L]


dataset_test <- dataset[testing == 1, ]

# libero espacio
rm(dataset)
gc()

entrenar_modelo<-function(){
  for (seed in seeds){
    set.seed(seed)
    param_completo <- c(params,
                        seed = seed) 
    model <- lgb.train(
      params = param_completo,
      data = dtrain,
      valids = list(valid = dvalidate),
      nrounds = 100,  # Número de iteraciones
      verbose = -1
    )
    dapply <- dataset[foto_mes == PARAM$input$future]
    
    prediccion <- predict(
      model,
      data.matrix(dataset_test[, campos_buenos, with = FALSE])
    )
    
  }
}

# Bucle sobre todas las combinaciones de hiperparámetros
for (lr in param_grid$learning_rate) {
  for (nl in param_grid$num_leaves) {
    for (md in param_grid$max_depth) {
      for (mc in param_grid$min_child_samples) {
        
        # Configurar hiperparámetros
        params <- list(
          objective = "binary",
          metric = "binary_logloss",
          learning_rate = lr,
          num_leaves = nl,
          max_depth = md,
          min_child_samples = mc
        )
        
        # Entrenar el modelo con los hiperparámetros actuales
        model <- lgb.train(
          params = params,
          data = train_data,
          nrounds = 100,  # Número de iteraciones
          verbose = -1
        )
        
        # Evaluar el modelo en algún conjunto de validación o realizar validación cruzada
        
        # Aquí asumimos que estás utilizando una métrica como el AUC
        auc <- some_evaluation_metric(model)
        
        # Actualizar los mejores resultados si es necesario
        if (auc > best_auc) {
          best_auc <- auc
          best_params <- params
        }
      }
    }
  }
}

# Imprimir los mejores resultados
cat("Mejores hiperparámetros:", best_params, "\n")
cat("Mejor AUC:", best_auc, "\n")
