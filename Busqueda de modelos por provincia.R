# Codigo por provincias 

# Comenzamos con el estudio de las series por comunidades
datos_p = read_excel("C:/Users/david/Desktop/Universidad/Cuarto/TFG/data.xlsx", sheet = "data_p")
datos_p$fecha=as.Date(datos_p$fecha,format='%m/%d/%Y')
datos_p = as.data.frame(datos_p)
# Dividimos los datos en train y test
datos_p_train = subset(datos_p, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2009 a 2017 (ambos incluidos)
datos_p_test = subset(datos_p, fecha>as.Date('12/01/2017',format='%m/%d/%Y')) # desde 2018 a 2019 (ambos incluidos)

# Oobtener todos los datos como series temporales
datos_p_st = lapply(datos_p, function(col) ts(col, start=c(2009,1), frequency = 12))
datos_train_st = lapply(datos_c_train, function(col) ts(col, start=c(2009,1), frequency = 12))
datos_test_st = lapply(datos_c_test, function(col) ts(col, start=c(2018,1), frequency = 12))

plot(ts(datos_p$`49 Zamora`, start = c(2009,1), frequency = 12))

modelos_provincias = list()
for(provincia in names(datos_p)[-1]){
  print(provincia)
  # Creamos el data frame
  datos_aux = datos_p[, c("fecha",provincia), drop = FALSE]
  modelos_provincias[[provincia]] = mejor_modelo(datos_aux, explicativas =1,  frecuencia = 12)
}

# Autoarima para cada provincia

lista_resultados_provincia_autoarima = list()
for(provincia in names(datos_p)[-1]){
  # Creamos el data frame
  datos_aux = datos_p[, c("fecha",provincia), drop = FALSE]
  colnames(datos_aux) =  c('fecha', 'Target')
  datos_aux_train = subset(datos_aux, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))
  datos_aux_test = subset(datos_aux, fecha>as.Date('12/01/2017',format='%m/%d/%Y'))
  
  # Comprobamos la aplicacion de alguna transformacion 
  
  # Comprobamos posibles transformaciones
  box_cox = boxcox(Target ~ fecha,
                   data = datos_aux_train,
                   lambda = c(0, 0.5, 1),plotit=FALSE)
  lambda = box_cox$x[which.max(box_cox$y)] # 0 = log , 0.5 = sqrt y 1 = sin tranformada
  
  # Transformamos los datos 
  if(lambda == 0){
    datos_aux_trans = log(datos_aux$Target)
    datos_trans = log(datos_aux_train$Target)
  }
  if(lambda == 0.5){
    datos_aux_trans = sqrt(datos_aux$Target)
    datos_trans = sqrt(datos_aux_train$Target)
  }
  if(lambda == 1){
    datos_aux_trans = (datos_aux$Target)
    datos_trans = datos_aux_train$Target
  }
  
  comunidad = encontrar_comunidad(provincia)
  explicativasCalendarioTrain = calculoExplicativasCalendario(datos_aux_train$fecha,comunidad = comunidad, domingoYFestivosJuntos=1)
  explicativas = as.matrix(explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto")])
  datos_trans_ts = ts(datos_trans, start = c(2009,1), frequency = 12)
  modelo_autoarima = auto.arima(datos_trans_ts, 
                                xreg = explicativas,
                                seasonal = TRUE,
                                allowdrift=F)
  # Calculamos las predicciones con las explicativas para todo el conjunto 
  explicativasCalendario = calculoExplicativasCalendario(datos_aux$fecha, comunidad = comunidad, domingoYFestivosJuntos=1)
  explicativas_pred = as.matrix(explicativasCalendario[,c("semanaSanta", "dt", "bisiesto")])
  
  # Con el modelo autoarima predecimos
  lista_predicciones_autoarima = pred(modelo_autoarima, datos_aux_trans, explicativas_pred, transformacion = lambda , num_predicciones = 24)
  # Extraemos los datos
  predicciones = lista_predicciones_autoarima$predicciones
  intervalo_lower = lista_predicciones_autoarima$lower
  intervalo_upper = lista_predicciones_autoarima$upper
  
  predicciones_reales_autoarima = data.frame( Tiempo = datos_aux_test$fecha,
                                              Prediccion = predicciones,
                                              L = intervalo_lower,  # Límite inferior del intervalo de confianza
                                              U = intervalo_upper,  # Límite superior del intervalo de confianza
                                              error_mape = lista_predicciones_autoarima$error_mape
  )
  
  # Error del modelo propuesto 
  predicciones_train_trans = modelo_autoarima$fitted
  # Destransformamos
  if(lambda == 0){
    predicciones_train = exp(predicciones_train_trans)
  }
  if(lambda == 0.5){
    predicciones_train = (predicciones_train_trans)^2
  }
  if(lambda == 1){
    predicciones_train = predicciones_train_trans
  }
  
  #Calculamos el MAPE punto a punto 
  error_mape_modelo = numeric()
  for(i in 1:length(predicciones_train)){
    dato_real = datos_aux_train$Target[i]
    error_mape_modelo[i] = abs(dato_real - predicciones_train[i])/ dato_real *100
  }
  
  
  lista_resultados_provincia_autoarima[[provincia]] = list(predicciones = predicciones_reales_autoarima,
                                                           modelo = modelo_autoarima,
                                                           error_mape_modelo = mean(error_mape_modelo))
  
}






#########################################################
# Creamos una tabla con los errores del modelo para cada uno MAPE ,AIC y autoarima
vector_errores_provincia_mape = c()
vector_errores_provincia_aic = c()
vector_errores_provincia_autoarima = c()
for(provincia in names(datos_p)[-1]){
  vector_errores_provincia_aic = c(vector_errores_provincia_aic, mean(modelos_provincias[[provincia]]$modelo_AIC$error_mape_modelo))
  vector_errores_provincia_mape = c(vector_errores_provincia_mape, mean(modelos_provincias[[provincia]]$modelo_MAPE$error_mape_modelo))
  vector_errores_provincia_autoarima = c(vector_errores_provincia_autoarima, mean(lista_resultados_provincia_autoarima[[provincia]]$error_mape_modelo))
}

vector_medias_provincia = c(mean(vector_errores_comunidad_aic), mean(vector_errores_comunidad_mape) , mean(vector_errores_comunidad_autoarima))

tabla_errores_modelos_provincia = data.frame(Provincia = names(datos_p)[-1],
                                             Error_modelo_AIC = vector_errores_provincia_aic,
                                             Error_modelo_MAPE = vector_errores_provincia_mape,
                                             Error_modelo_Auto.arima = vector_errores_provincia_autoarima)


###########################################
# Error de prediccion para cada comunidad 

vector_error_pred_provincia = c()
for(provincia in names(datos_p)[-1]){
  vector_error_pred_provincia = c(vector_error_pred_provincia, mean(modelos_provincias[[provincia]]$modelo_MAPE$predicciones$error_mape))
}
mean(vector_error_pred_provincia)
