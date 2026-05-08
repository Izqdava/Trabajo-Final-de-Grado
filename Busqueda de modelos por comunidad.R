#Comenzamos con el estudio de las series por comunidades
datos_c = read_excel("C:/Users/david/Desktop/Universidad/Cuarto/TFG/data.xlsx", sheet = "data_c")
datos_c$fecha=as.Date(datos_c$fecha,format='%m/%d/%Y')
datos_c = as.data.frame(datos_c)
# Dividimos los datos en train y test
datos_c_train = subset(datos_c, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2009 a 2017 (ambos incluidos)
datos_c_test = subset(datos_c, fecha>as.Date('12/01/2017',format='%m/%d/%Y')) # desde 2018 a 2019 (ambos incluidos)

# Oobtener todos los datos como series temporales
datos_c_st = lapply(datos_c, function(col) ts(col, start=c(2009,1), frequency = 12))
datos_train_st = lapply(datos_c_train, function(col) ts(col, start=c(2009,1), frequency = 12))
datos_test_st = lapply(datos_c_test, function(col) ts(col, start=c(2018,1), frequency = 12))


modelos = list()
for(comunidad in names(datos_c)[-1]){
  print(comunidad)
  # Creamos el data frame
  datos_aux = datos_c[, c("fecha",comunidad), drop = FALSE]
  modelos[[comunidad]] = mejor_modelo(datos_aux, explicativas =1,  frecuencia = 12)
  #lista_resultados[[comunidad]] = modelo_mejor_aic(datos_aux, explicativas =1,  frecuencia = 12)
}

###############################
# Autoarima para cada comunidad

lista_resultados_comunidad_autoarima = list()
for(comunidad in names(datos_c)[-1]){
  # Creamos el data frame
  datos_aux = datos_c[, c("fecha",comunidad), drop = FALSE]
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
  
  
  lista_resultados_comunidad_autoarima[[comunidad]] = list(predicciones = predicciones_reales_autoarima,
                                                           modelo = modelo_autoarima,
                                                           error_mape_modelo = mean(error_mape_modelo))
  
}


#########################################################
# Creamos una tabla con los errores del modelo para cada uno MAPE ,AIC y autoarima
vector_errores_comunidad_mape = c()
vector_errores_comunidad_aic = c()
vector_errores_comunidad_autoarima = c()
for(comunidad in names(datos_c)[-1]){
  vector_errores_comunidad_aic = c(vector_errores_comunidad_aic, mean(modelos[[comunidad]]$modelo_AIC$error_mape_modelo))
  vector_errores_comunidad_mape = c(vector_errores_comunidad_mape, mean(modelos[[comunidad]]$modelo_MAPE$error_mape_modelo))
  vector_errores_comunidad_autoarima = c(vector_errores_comunidad_autoarima, mean(lista_resultados_comunidad_autoarima[[comunidad]]$error_mape_modelo))
}

vector_medias = c(mean(vector_errores_comunidad_aic), mean(vector_errores_comunidad_mape) , mean(vector_errores_comunidad_autoarima))

tabla_errores_modelos_comunidad = data.frame(comunidad = names(datos_c)[-1],
                                             Error_modelo_AIC = vector_errores_comunidad_aic,
                                             Error_modelo_MAPE = vector_errores_comunidad_mape,
                                             Error_modelo_Auto.arima = vector_errores_comunidad_autoarima)


###########################################
# Error de prediccion para cada comunidad 

vector_error_pred_comunidad = c()
for(comunidad in names(datos_c)[-1]){
  vector_error_pred_comunidad = c(vector_error_pred_comunidad, mean(modelos[[comunidad]]$modelo_MAPE$predicciones$error_mape))
}
mean(vector_error_pred_comunidad)

\end{Verbatim}

\subsection{Estrategia Bottom-up por comunidades}
\begin{Verbatim}
# Estategia Bottom-up por comunidades
suma_predicciones_aic = rep(0,24)
suma_L_aic = rep(0,24)
suma_U_aic = rep(0,24)
suma_predicciones_mape = rep(0,24)
suma_L_mape = rep(0,24)
suma_U_mape = rep(0,24)
suma_predicciones_autoarima = rep(0,24)
suma_L_autoarima = rep(0,24)
suma_U_autoarima = rep(0,24)

#Sumamos todas las predicciones
for(comunidad in names(modelos)){
  lista_predicciones_aic = modelos[[comunidad]]$modelo_AIC$predicciones
  lista_predicciones_mape = modelos[[comunidad]]$modelo_MAPE$predicciones
  lista_predicciones_autoarima = lista_resultados_comunidad_autoarima[[comunidad]]$predicciones
  
  # Extraemos las predicciones y los intervalos de confianza
  pred_comunidad_aic = lista_predicciones_aic$Prediccion
  pred_comunidad_mape = lista_predicciones_mape$Prediccion
  pred_comunidad_autoarima = lista_predicciones_autoarima$Prediccion
  
  L_comunidad_aic = lista_predicciones_aic$L
  L_comunidad_mape = lista_predicciones_mape$L
  L_comunidad_autoarima = lista_predicciones_autoarima$L
  
  U_comunidad_aic = lista_predicciones_aic$U
  U_comunidad_mape = lista_predicciones_mape$U
  U_comunidad_autoarima = lista_predicciones_autoarima$U
  
  # Sumamos las predicciones e intervalos de cada comunidad
  suma_predicciones_aic = suma_predicciones_aic + pred_comunidad_aic
  suma_predicciones_mape = suma_predicciones_mape + pred_comunidad_mape
  suma_predicciones_autoarima = suma_predicciones_autoarima + pred_comunidad_autoarima
  
  suma_L_aic = suma_L_aic + L_comunidad_aic
  suma_L_mape = suma_L_mape + L_comunidad_mape
  suma_L_autoarima = suma_L_autoarima + L_comunidad_autoarima
  
  suma_U_aic = suma_U_aic + U_comunidad_aic
  suma_U_mape = suma_U_mape + U_comunidad_mape
  suma_U_autoarima = suma_U_autoarima +U_comunidad_autoarima
  
}


# Calculamos el error mape de las predicciones
error_mape_predicciones_aic = numeric()
error_mape_predicciones_mape = numeric()
error_mape_predicciones_autoarima = numeric()

for(i in 1:length(suma_predicciones_aic)){
  dato_real = datos_n_test$Nacional[i]
  error_mape_predicciones_aic[i] = abs(dato_real - suma_predicciones_aic[i])/ dato_real *100
  error_mape_predicciones_mape[i] = abs(dato_real - suma_predicciones_mape[i])/ dato_real *100
  error_mape_predicciones_autoarima[i] = abs(dato_real - suma_predicciones_autoarima[i])/ dato_real *100
}
error_mape_comunidad_aic = mean(error_mape_predicciones_aic)
error_mape_comunidad_mape = mean(error_mape_predicciones_mape)
error_mape_comunidad_autoarima = mean(error_mape_predicciones_autoarima)

error_mape_comunidad_aic
error_mape_comunidad_mape
error_mape_comunidad_autoarima





# Configuración de límites con un margen visual
lim_i = min(datos_n_test_ts, pred_modelo_5_explicativas_outliers$lower, suma_L_aic, suma_L_mape, suma_L_autoarima)
lim_s = max(datos_n_test_ts, pred_modelo_5_explicativas_outliers$upper, suma_U_aic, suma_U_mape, suma_U_autoarima)
margen = 0.05 * (lim_s - lim_i)
# Gráfico 2: Modelos por AIC
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "#4D4D4D",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Estrategia Bottom-up modelos AIC por comunidad",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) AIC
polygon(c(time(suma_L_aic), rev(time(suma_L_aic))),
        c(suma_L_aic, rev(suma_U_aic)),
        col = rgb(0.2, 0.7, 0.2, 0.3), border = NA)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción AIC
lines(suma_predicciones_aic, col = "#2ca02c", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción AIC", "Intervalo confianza AIC"),
  col = c("#1f77b4", "#2ca02c", rgb(0.2, 0.7, 0.2, 0.3)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)

# Gráfico 3: Modelos por MAPE
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "#4D4D4D",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Estrategia Bottom-up modelos MAPE por comunidades",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) MAPE
polygon(c(time(suma_L_mape), rev(time(suma_L_mape))),
        c(suma_L_mape, rev(suma_U_mape)),
        col = rgb(1, 0.5, 0, 0.3), border = NA)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción MAPE
lines(suma_predicciones_mape, col = "#ff7f0e", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción MAPE", "Intervalo confianza MAPE"),
  col = c("#1f77b4", "#ff7f0e", rgb(1, 0.5, 0, 0.3)),
  lwd = c(2, 2,NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)

# Gráfico 4: Modelos por Autoarima
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "#4D4D4D",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Estrategia Bottom-up modelos Autoarima por comunidad",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) Autoarima
polygon(c(time(suma_L_autoarima), rev(time(suma_L_autoarima))),
        c(suma_L_autoarima, rev(suma_U_autoarima)),
        col = rgb(0, 0.7, 0.7, 0.3), border = NA)
# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción Autoarima
lines(suma_predicciones_autoarima, col = "#17becf", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción Autoarima", "Intervalo confianza Autoarima"),
  col = c("#1f77b4", "#17becf", rgb(0, 0.7, 0.7, 0.3)),
  lwd = c(2, 2,NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)
