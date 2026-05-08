#Estrategia Bottom-up por provincias
suma_predicciones_provincia_aic = rep(0,24)
suma_L_provincia_aic = rep(0,24)
suma_U_provincia_aic = rep(0,24)
suma_predicciones_provincia_mape = rep(0,24)
suma_L_provincia_mape = rep(0,24)
suma_U_provincia_mape = rep(0,24)
suma_predicciones_provincia_autoarima = rep(0,24)
suma_L_provincia_autoarima = rep(0,24)
suma_U_provincia_autoarima = rep(0,24)

for(provincia in names(modelos_provincias)){
  lista_predicciones_provincia_aic = modelos_provincias[[provincia]]$modelo_AIC$predicciones
  lista_predicciones_provincia_mape = modelos_provincias[[provincia]]$modelo_MAPE$predicciones
  lista_predicciones_provincia_autoarima = lista_resultados_provincia_autoarima[[provincia]]$predicciones
  
  # Extraemos las predicciones y los intervalos de confianza
  pred_provincia_aic = lista_predicciones_provincia_aic$Prediccion
  pred_provincia_mape = lista_predicciones_provincia_mape$Prediccion
  pred_provincia_autoarima = lista_predicciones_provincia_autoarima$Prediccion
  
  L_provincia_aic = lista_predicciones_provincia_aic$L
  L_provincia_mape = lista_predicciones_provincia_mape$L
  L_provincia_autoarima = lista_predicciones_provincia_autoarima$L
  
  U_provincia_aic = lista_predicciones_provincia_aic$U
  U_provincia_mape = lista_predicciones_provincia_mape$U
  U_provincia_autoarima = lista_predicciones_provincia_autoarima$U
  
  # Sumamos las predicciones e intervalos de cada comunidad
  suma_predicciones_provincia_aic = suma_predicciones_provincia_aic + pred_provincia_aic
  suma_predicciones_provincia_mape = suma_predicciones_provincia_mape + pred_provincia_mape
  suma_predicciones_provincia_autoarima = suma_predicciones_provincia_autoarima + pred_provincia_autoarima
  
  suma_L_provincia_aic = suma_L_provincia_aic + L_provincia_aic
  suma_L_provincia_mape = suma_L_provincia_mape + L_provincia_mape
  suma_L_provincia_autoarima = suma_L_provincia_autoarima + L_provincia_autoarima
  
  suma_U_provincia_aic = suma_U_provincia_aic + U_provincia_aic
  suma_U_provincia_mape = suma_U_provincia_mape + U_provincia_mape
  suma_U_provincia_autoarima = suma_U_provincia_autoarima +U_provincia_autoarima
  
}

# Calculamos el error mape de las predicciones
error_mape_predicciones_provincia_aic = numeric()
error_mape_predicciones_provincia_mape = numeric()
error_mape_predicciones_provincia_autoarima = numeric()

for(i in 1:length(suma_predicciones_provincia_aic)){
  dato_real = datos_n_test$Nacional[i]
  error_mape_predicciones_provincia_aic[i] = abs(dato_real - suma_predicciones_provincia_aic[i])/ dato_real *100
  error_mape_predicciones_provincia_mape[i] = abs(dato_real - suma_predicciones_provincia_mape[i])/ dato_real *100
  error_mape_predicciones_provincia_autoarima[i] = abs(dato_real - suma_predicciones_provincia_autoarima[i])/ dato_real *100
}
error_mape_provincia_aic = mean(error_mape_predicciones_provincia_aic)
error_mape_provincia_mape = mean(error_mape_predicciones_provincia_mape)
error_mape_provincia_autoarima = mean(error_mape_predicciones_provincia_autoarima)

error_mape_provincia_aic
error_mape_provincia_mape
error_mape_provincia_autoarima

# Configuración de límites con un margen visual
lim_i = min(datos_n_test_ts, pred_modelo_5_explicativas_outliers$lower, suma_L_provincia_aic, suma_L_provincia_mape, suma_L_provincia_autoarima)
lim_s = max(datos_n_test_ts, pred_modelo_5_explicativas_outliers$upper, suma_U_provincia_aic, suma_U_provincia_mape, suma_U_provincia_autoarima)
margen = 0.05 * (lim_s - lim_i)

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
  main = "Estrategia Bottom-up modelos MAPE por provincias",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) MAPE
polygon(c(time(suma_L_provincia_mape), rev(time(suma_L_provincia_mape))),
        c(suma_L_provincia_mape, rev(suma_U_provincia_mape)),
        col = rgb(0, 0.7, 0.7, 0.3), border = NA)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción MAPE
lines(suma_predicciones_provincia_mape, col = "#17becf", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción MAPE", "Intervalo confianza MAPE"),
  col = c("#1f77b4", "#17becf", rgb(0, 0.7, 0.7, 0.3)),
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
  main = "Estrategia Bottom-up modelos Autoarima por provincia",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) Autoarima
polygon(c(time(suma_L_provincia_autoarima), rev(time(suma_L_provincia_autoarima))),
        c(suma_L_provincia_autoarima, rev(suma_U_provincia_autoarima)),
        col = rgb(1, 0.5, 0, 0.3), border = NA)
# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción Autoarima
lines(suma_predicciones_provincia_autoarima, col = "#ff7f0e", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción Autoarima", "Intervalo confianza Autoarima"),
  col = c("#1f77b4", "#ff7f0e", rgb(1, 0.5, 0, 0.3)),
  lwd = c(2, 2,NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)
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
  main = "Estrategia Bottom-up modelos AIC por provincia",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Intervalo de confianza (sombra) AIC
polygon(c(time(suma_L_provincia_aic), rev(time(suma_L_provincia_aic))),
        c(suma_L_provincia_aic, rev(suma_U_provincia_aic)),
        col = rgb(0.2, 0.7, 0.2, 0.3), border = NA)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
# Líneas de predicción AIC
lines(suma_predicciones_provincia_aic, col = "#2ca02c", lwd = 2)

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción AIC", "Intervalo confianza AIC"),
  col = c("#1f77b4", "#2ca02c", rgb(0.2, 0.7, 0.2, 0.3)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)
