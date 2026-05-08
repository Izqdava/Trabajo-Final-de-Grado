# Estrategia Top-down

#Calculo de porcentaje

num_predicciones = 24 # numero de predicciones que realizamos
# inicializamos el vector que almacenará los porcentajes para cada comunidad
porcentaje_comunidad = c()
# inicializamos la lista de los resultados
lista_porcentajes = list()
for(comunidad in names(datos_c)[-1]){
  for(j in 1:num_predicciones){
    dato_total = suma_predicciones_mape[j]
    dato_comunidad = modelos[[comunidad]]$modelo_MAPE$predicciones$Prediccion[j]
    porcentaje_comunidad[j] = dato_comunidad/dato_total
  }
  lista_porcentajes[[comunidad]] = porcentaje_comunidad
}
lista_porcentajes
# La suma de todos los porcentajes ha de ser 1.
Reduce("+", lista_porcentajes)

# Calculamos las estimaciones para cada comunidad
predicciones_ajuste_manual = pred_modelo_5_explicativas_outliers$predicciones
estimaciones_top_down = list()
prediccion_comunidad =c()
for(comunidad in names(datos_c)[-1]){
  for (j in 1:num_predicciones) {
    prediccion_comunidad[j] = predicciones_ajuste_manual[j]*lista_porcentajes[[comunidad]][j]
  }
  estimaciones_top_down[[comunidad]] = ts(prediccion_comunidad, start = c(2018,1), frequency = 12)
}
estimaciones_top_down

error_mape_top_down = list()
error_mape_comnuidad = c()
for(comunidad in names(datos_c)[-1]){
  for(i in 1:num_predicciones){
    dato_real = datos_c_test[[comunidad]][i]
    dato_top_down = estimaciones_top_down[[comunidad]][i]
    error_mape_comnuidad[i] = abs(dato_real - dato_top_down)/ dato_real *100
  }
  error_mape_top_down[[comunidad]] = error_mape_comnuidad
}
errores_medios_top_down = sapply(error_mape_top_down, mean)
tabla_error_top_down = data.frame(Comunidad = names(errores_medios_top_down),
                                  Error_MAPE_predicciones = errores_medios_top_down)

mean(tabla_error_top_down$Error_MAPE_predicciones)