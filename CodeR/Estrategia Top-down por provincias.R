# Estrategia Top-down

#Calculo de porcentaje

num_predicciones = 24 # numero de predicciones que realizamos
# inicializamos el vector que almacenará los porcentajes para cada comunidad
porcentaje_provincia = c()
# inicializamos la lista de los resultados
lista_porcentajes_provincia = list()
for(provincia in names(datos_p)[-1]){
  for(j in 1:num_predicciones){
    dato_total = suma_predicciones_provincia_mape[j]
    dato_provincia = modelos_provincias[[provincia]]$modelo_MAPE$predicciones$Prediccion[j]
    porcentaje_provincia[j] = dato_provincia/dato_total
  }
  lista_porcentajes_provincia[[provincia]] = porcentaje_provincia
}
lista_porcentajes_provincia
# La suma de todos los porcentajes ha de ser 1.
Reduce("+", lista_porcentajes_provincia)

# Calculamos las estimaciones para cada comunidad
predicciones_ajuste_manual = pred_modelo_5_explicativas_outliers$predicciones
estimaciones_top_down_provincia = list()
prediccion_provincia = c()
for(provincia in names(datos_p)[-1]){
  for (j in 1:num_predicciones) {
    prediccion_provincia[j] = predicciones_ajuste_manual[j]*lista_porcentajes_provincia[[provincia]][j]
  }
  estimaciones_top_down_provincia[[provincia]] = ts(prediccion_provincia, start = c(2018,1), frequency = 12)
}
estimaciones_top_down_provincia

error_mape_top_down_provincia = list()
error_mape_provincia = c()
for(provincia in names(datos_p)[-1]){
  for(i in 1:num_predicciones){
    dato_real = datos_p_test[[provincia]][i]
    dato_top_down_provincia = estimaciones_top_down_provincia[[provincia]][i]
    error_mape_provincia[i] = abs(dato_real - dato_top_down_provincia)/ dato_real *100
  }
  error_mape_top_down_provincia[[provincia]] = error_mape_provincia
}
errores_medios_top_down_provincia = sapply(error_mape_top_down_provincia, mean)
tabla_error_top_down_provincia = data.frame(Provincia = names(estimaciones_top_down_provincia),
                                            Error_MAPE_predicciones = errores_medios_top_down_provincia)

mean(tabla_error_top_down_provincia$Error_MAPE_predicciones)
\end{Verbatim}

\subsection{Estrategia Middle-out para las provincias}

\begin{Verbatim}
# Estrategia Middle-out

#Hay que calcular el peso de cada provincia dentro de su comunidad
pesos_provincias = list()
pesos_provincia = c()

for(provincia in names(datos_p)[-1]){
  comunidad = encontrar_comunidad(provincia)
  predicciones_comunidad = modelos[[comunidad]]$modelo_MAPE$predicciones$Prediccion
  # Calculamos el peso
  for(j in 1:num_predicciones){
    predicciones_comunidad_j = modelos[[comunidad]]$modelo_MAPE$predicciones$Prediccion[j]
    dato_provincia = modelos_provincias[[provincia]]$modelo_MAPE$predicciones$Prediccion[j]
    pesos_provincia[j] = dato_provincia/predicciones_comunidad
  }
  pesos_provincias[[provincia]] =pesos_provincia
  # ajustamos la prediccion para cada provincia desde las comunidades
  for (j in 1:num_predicciones) {
    prediccion_diddle.out[j] = predicciones_comunidad[j]*pesos_provincias[[provincia]][j]
  }
  
  estimaciones_top_down_provincia[[provincia]] = ts(prediccion_diddle.out, start = c(2018,1), frequency = 12)
  
}