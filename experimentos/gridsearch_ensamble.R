# para correr el Google Cloud
#   8 vCPU
#  64 GB memoria RAM


# limpio la memoria
rm(list = ls()) # remove all objects
gc() # garbage collection

require("data.table")
require("lightgbm")


# defino los parametros de la corrida, en una lista, la variable global  PARAM
#  muy pronto esto se leera desde un archivo formato .yaml
PARAM <- list()
PARAM$experimento <- "gridsearch_1"

PARAM$input$dataset <- "./datasets/competencia_03.csv.gz"

# meses donde se entrena el modelo
PARAM$input$training <- c(202101,202102,202103,202104,202105)
PARAM$input$future <- c(202106) # meses donde se testea
initial_seed <- 100019
num_seeds <- 30
seeds <- seq(initial_seed, initial_seed + num_seeds - 1)
PARAM$finalmodel$semilla <- seeds




# hiperparametros que vamos a probar
PARAM$finalmodel$optim$num_iterations <- c(20)
PARAM$finalmodel$optim$learning_rate <- c(1)
PARAM$finalmodel$optim$feature_fraction <- c(0.8, 0.6,0.4,0.2)
PARAM$finalmodel$optim$min_data_in_leaf <- c(3000,5000,6000,7000,8000)
PARAM$finalmodel$optim$num_leaves <- c(15, 31, 63,40,80)


# Hiperparametros FIJOS de  lightgbm
PARAM$finalmodel$lgb_basicos <- list(
  boosting = "gbdt", # puede ir  dart  , ni pruebe random_forest
  objective = "binary",
  metric = "custom",
  first_metric_only = TRUE,
  boost_from_average = TRUE,
  feature_pre_filter = FALSE,
  force_row_wise = TRUE, # para reducir warnings
  verbosity = -100,
  max_depth = -1L, # -1 significa no limitar,  por ahora lo dejo fijo
  min_gain_to_split = 0.0, # min_gain_to_split >= 0.0
  min_sum_hessian_in_leaf = 0.001, #  min_sum_hessian_in_leaf >= 0.0
  lambda_l1 = 0.0, # lambda_l1 >= 0.0
  lambda_l2 = 0.0, # lambda_l2 >= 0.0
  max_bin = 31L, # lo debo dejar fijo, no participa de la BO
  
  bagging_fraction = 1.0, # 0.0 < bagging_fraction <= 1.0
  pos_bagging_fraction = 1.0, # 0.0 < pos_bagging_fraction <= 1.0
  neg_bagging_fraction = 1.0, # 0.0 < neg_bagging_fraction <= 1.0
  is_unbalance = FALSE, #
  scale_pos_weight = 1.0, # scale_pos_weight > 0.0
  
  drop_rate = 0.1, # 0.0 < neg_bagging_fraction <= 1.0
  max_drop = 50, # <=0 means no limit
  skip_drop = 0.5, # 0.0 <= skip_drop <= 1.0
  
  extra_trees = TRUE # Magic Sauce
  
  #,seed = PARAM$finalmodel$semilla
)


#------------------------------------------------------------------------------
#------------------------------------------------------------------------------
# Aqui empieza el programa
setwd("~/buckets/b1")

# cargo el dataset donde voy a entrenar
dataset <- fread(PARAM$input$dataset, stringsAsFactors = TRUE)


# Catastrophe Analysis  -------------------------------------------------------
# deben ir cosas de este estilo
#   dataset[foto_mes == 202006, active_quarter := NA]

# Data Drifting
# por ahora, no hago nada


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



#--------------------------------------

# paso la clase a binaria que tome valores {0,1}  enteros
# set trabaja con la clase  POS = { BAJA+1, BAJA+2 }
# esta estrategia es MUY importante
dataset[, clase01 := ifelse(clase_ternaria %in% c("BAJA+2", "BAJA+1"), 1L, 0L)]

#--------------------------------------

# los campos que se van a utilizar
campos_buenos <- setdiff(colnames(dataset), c("clase_ternaria", "clase01"))

#--------------------------------------


# establezco donde entreno
dataset[, train := 0L]
dataset[foto_mes %in% PARAM$input$training, train := 1L]

#--------------------------------------
# creo las carpetas donde van los resultados
# creo la carpeta donde va el experimento
dir.create("./exp/", showWarnings = FALSE)
dir.create(paste0("./exp/", PARAM$experimento, "/"), showWarnings = FALSE)


                                                             
#preparo el dataset para testear
dapply <- dataset[foto_mes == PARAM$input$future]
dataset_test <- data.matrix(dapply[, campos_buenos, with = FALSE])

# Loop over each seed and perform training
for (ni in PARAM$finalmodel$optim$num_iterations) {
  for (ff in PARAM$finalmodel$optim$feature_fraction) {
    for (md in PARAM$finalmodel$optim$min_data_in_leaf) {
      for (nl in PARAM$finalmodel$optim$num_leaves) {
        for (lr in PARAM$finalmodel$optim$learning_rate) {
          setwd("~/buckets/b1")
          dir.create(paste0("./exp/", PARAM$experimento, "/",lr,"_",nl,"_",md,"_",ff,"_",ni, "/"), showWarnings = TRUE)
        
          
          # Establezco el Working Directory DEL EXPERIMENTO
          setwd(paste0("./exp/gridsearch_1/", lr,"_",nl,"_",md,"_",ff,"_",ni, "/"))
          # genero la tabla de entrega
          tb_entrega <- dapply[, list(numero_de_cliente, foto_mes,clase_ternaria)]
for (seed in PARAM$finalmodel$semilla) {
  # Set the seed for this iteration
  set.seed(seed)
  
  # dejo los datos en the format that LightGBM needs
  dtrain <- lgb.Dataset(
    data = data.matrix(dataset[train == 1L, campos_buenos, with = FALSE]),
    label = dataset[train == 1L, clase01]
  )
  
  # generate the model
  param_completo <- c(PARAM$finalmodel$lgb_basicos,
                      learning_rate=lr,
                      num_iterations=ni,
                      min_data_in_leaf=md,
                      feature_fraction=ff,
                      num_leaves=nl,
                      seed = seed)  # Update the seed for each model
  modelo <- lgb.train(
    data = dtrain,
    param = param_completo
  )
  
  
  
  # aplico el modelo a los datos nuevos
  prediccion <- predict(
    modelo,
    dataset_test)
  
  
  #agrego el resultado de esta seed
  tb_entrega[, paste0("prediccion_", seed) := prediccion]
  
  
  
}
          #hago un promedio de las prob de cada semilla
          tb_entrega[, ensemble_prob := rowMeans(.SD, na.rm = TRUE), .SDcols = grep("prediccion_", names(tb_entrega))]
          setorder(tb_entrega, -ensemble_prob)
          #envio 11mil estimulos
          tb_entrega[, Predicted := 0L]
          tb_entrega[1:11000, Predicted := 1L]
          #calculo ganacia
          tb_entrega[, ganancia := ifelse(Predicted == 1 & clase_ternaria == "BAJA+2", 273000,
                                           ifelse(Predicted == 1 & clase_ternaria != "BAJA+2", -7000, 0))]
          
          total_gain <- sum(tb_entrega$ganancia)
          return(total_gain)
          # grabo las probabilidad del modelo
          fwrite(tb_entrega,
                 file = paste0("prediccion", seed, ".txt"),
                 sep = "\t"
          )
      }
    }
  }
  }
}
