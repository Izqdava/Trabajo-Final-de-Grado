# Ajuste serie nacional 
# Datos desde 2009 hasta 2019 de compra venta de viviendas 

# Ajuste de la serie siguiendo: calibración del modelo arima con variables explicativas(periodos vacacionales)
# Cargamos librerias necesarias
library(readxl)
library(haven)
library(datasets)
library(timsac)
library(forecast)
library(lmtest)
library(descomponer)
library(tsoutliers)
library(readxl)
library(tseries)
library(nortest)
library(expsmooth)
library(fma)
library(caschrono)
library(MASS)
# Cargamos los datos
library(readxl)
datos_n = read_excel("C:/ruta_de_acceso/data.xlsx", sheet = "data_n")
head(datos_n)
summary(datos_n) # vamos a transformar la variable temporal en formato fecha 
datos_n$fecha=as.Date(datos_n$fecha,format='%m/%d/%Y')

# Dividimos en datos train y datos test
# 80% para train (8 años) 20% para test (2 años)
datos_n_train = subset(datos_n, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2009 a 2017 (ambos incluidos)
datos_n_test = subset(datos_n, fecha>as.Date('12/01/2017',format='%m/%d/%Y')) # desde 2018 a 2019 (ambos incluidos)

# Convertimos en serie temporal
datos_n_train_ts = ts(datos_n_train$Nacional, start = c(2009,1), frequency = 12)
datos_n_test_ts = ts(datos_n_test$Nacional, start = c(2018,1), frequency = 12)
datos_n_ts = ts(datos_n$Nacional, start=c(2009,1), frequency = 12)

plot(datos_n_train_ts, xlim = c(2009,2020), ylim = c(min(datos_n_train_ts),max(datos_n_test_ts)))
lines(datos_n_test_ts, col ="red")
legend("topleft", legend = c("Train","Test"), 
       col = c("black","red"), lty = 1, cex = 0.8, bty = "n") 

# Vamos primero a intentar ajustar la serie sin diferenciaciones
library(MASS)
# Probamos 3 transformaciones 0 = log, 0,5 = sqrt y 1 = sin transformación
box_cox = boxcox(Nacional ~ fecha,
                 data = datos_n_train,
                 lambda = c(0, 0.5, 1),plotit=FALSE)
lambda = box_cox$x[which.max(box_cox$y)]
lambda # Transformación de logaritmo
# Aplicamos transformación
datos_n_train_ts_trans = log(datos_n_train_ts)
datos_n_test_ts_trans = log(datos_n_test_ts)
datos_n_ts_trans = log(datos_n_ts)

# Graficamos
plot(datos_n_train_ts_trans,main = "Serie transformada",
     xlim = c(2009,2020), ylim = c(min(datos_n_train_ts_trans),max(datos_n_test_ts_trans)))
lines(datos_n_test_ts_trans, col ="red")
legend("topleft", legend = c("Train","Test"), 
       col = c("black","red"), lty = 1, cex = 0.8, bty = "n")

# install.packages("urca")
library(urca)

# Función para obtener p-valores de ADF

adf_all_tests <- function(y, max_lags = "AIC") {
  types <- c("none", "drift", "trend")  # Tipos de prueba
  names_types <- c("Sin Intercepto", "Con Intercepto", "Con Intercepto y Tendencia")
  
  # Lista para guardar los p-valores
  p_values <- numeric(3)
  
  for (i in 1:3) {
    # Ejecutar el test ADF con el tipo correspondiente
    test <- ur.df(y, type = types[i], selectlags = max_lags)
    
    # Obtener el resumen del test
    summary_test <- summary(test)
    
    # Inspeccionar el nombre exacto del coeficiente y p-valor
    coef_table <- summary_test@testreg$coefficients
    print(coef_table)  # Para ver cómo están organizados los coeficientes
    
    # Acceder al p-valor asociado al coeficiente 'y.lag.1'
    p_values[i] <- coef_table[1, "Pr(>|t|)"]  # Ajusta el índice si es necesario
  }
  
  # Crear un data.frame con los resultados
  data.frame(Test = names_types, P_value = p_values)
}

adf_p_values <- adf_all_tests(datos_n_train_ts_trans)
print(adf_p_values)


#############################
# Comenzamos con el ajuste 
#############################
# Graficamos las coorelaciones simples y parciales 
# Calculamos la FAS
acf(datos_n_train_ts_trans,lag.max = 48, main = "FAS datos transformados", ylab ='')
# Calculamos la FAP
pacf(datos_n_train_ts_trans,lag.max = 48, main = "FAP datos transformados", ylab ='')
# Ajustamos un modelo MA(inf) = AR(p)  y con la FAP observamos en principio un AR(1)

# Calculamos las variables explicativas
explicativasCalendarioTrain = calculoExplicativasCalendario(datos_n_train$fecha,domingoYFestivosJuntos=1)
calendarioTrain = as.matrix(explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto")])
head(calendarioTrain)
nrow(calendarioTrain)
# Modificamos las variables explicativas
calendarioTrain_sin_bisiesto = as.matrix(explicativasCalendarioTrain[,c("semanaSanta", "dt")])
head(calendarioTrain_sin_bisiesto)

modelo_1 = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                 seasonal = list(order = c(0,0,0), period = 12),
                 method="ML")
coeftest(modelo_1)

summary(modelo_1) # Parametro del AR no cercano a 1 por lo que no parece necesitar diferenciacio de momento

residuos = modelo_1$residuals
acf(residuos, lag.max = 48, main="FAS residuos ARMA(1,0)")
pacf(residuos, lag.max = 48, main="FAP residuos ARMA(1,0)")

modelo_2 = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                 seasonal = list(order = c(1,0,1), period = 12),
                 method="ML")
coeftest(modelo_2)

modelo_3 = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                 seasonal = list(order = c(1,0,0), period = 12),
                 method="ML")
coeftest(modelo_3)
cor.arma(modelo_3)
summary(modelo_3) # Parametro del AR no cercano a 1 por lo que no parece necesitar diferenciacio de momento

residuos = modelo_3$residuals
acf(residuos, lag.max = 48, main="FAS residuos SARIMA(1,0,0)x(1,0,0)[12]")
pacf(residuos, lag.max = 48, main="FAP residuos SARIMA(1,0,0)x(1,0,0)[12]")

modelo_4_explicativas = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                              seasonal = list(order = c(1,0,0), period = 12),
                              xreg = calendarioTrain,
                              method="ML")
coeftest(modelo_4_explicativas)# Bisiesto no coherente 

modelo_5_explicativas = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                              seasonal = list(order = c(1,0,0), period = 12),
                              xreg = calendarioTrain_sin_bisiesto,
                              method="ML")
coeftest(modelo_5_explicativas)
cor.arma(modelo_5_explicativas)
summary(modelo_5_explicativas) # Parametro del AR no cercano a 1 por lo que no parece necesitar diferenciacio de momento

# Independencia: Ljung-Box
Box.test.2(residuals(modelo_5_explicativas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")
# No podemos rechazar la hipótesis nula(Todos los p-valores son mayores de 0.05), 
#por tanto, los datos son independientes

# Homocedasticidad
residuos = residuals(modelo_5_explicativas)
n = length(residuos)
regresor = 1:n
lmtest::bptest(residuos~regresor) #Si es homocedastico

# Normalidad con test de Lillie
library(nortest)
lillie.test(residuos) # Si es normal 

# Vamos entonces a tratar valores outliers
listaOutliersTrain =locate.outliers(modelo_5_explicativas$residuals,
                                    pars = coefs2poly(modelo_5_explicativas),
                                    types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# Cruzamos con la tabla original para obtener la fecha

datos_n_train$ind = as.numeric(rownames(datos_n_train))
listaOutliersTrainFecha = merge(listaOutliersTrain, datos_n_train[,c("ind", "fecha")], by = "ind")

listaOutliersTrainFecha

# install.packages("dplyr")
library(dplyr)

arrange(listaOutliersTrainFecha,desc(listaOutliersTrainFecha$abststat))

outliersTrain <- outliers(c("LS"), c(63))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(modelo_5_explicativas$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain_sin_bisiesto,outliersVariablesTrain))

modelo_5_explicativas_outliers = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                                       seasonal = list(order = c(1,0,0), period = 12),
                                       xreg = calendarioMasOutliersTrain,
                                       method="ML")
coeftest(modelo_5_explicativas_outliers)

summary(modelo_5_explicativas_outliers)

# Vamos entonces a tratar valores outliers
listaOutliersTrain =locate.outliers(modelo_5_explicativas_outliers$residuals,
                                    pars = coefs2poly(modelo_5_explicativas_outliers),
                                    types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# Cruzamos con la tabla original para obtener la fecha

datos_n_train$ind = as.numeric(rownames(datos_n_train))
listaOutliersTrainFecha = merge(listaOutliersTrain, datos_n_train[,c("ind", "fecha")], by = "ind")

listaOutliersTrainFecha

# install.packages("dplyr")
library(dplyr)

arrange(listaOutliersTrainFecha,desc(listaOutliersTrainFecha$abststat))

outliersTrain <- outliers(c("LS", "LS"), c(63, 27))
outliersVariablesTrain <- outliers.effects(outliersTrain, length(modelo_5_explicativas_outliers$residuals))
calendarioMasOutliersTrain <- as.matrix(cbind(calendarioTrain_sin_bisiesto,outliersVariablesTrain))

modelo_5_explicativas_outliers = Arima(datos_n_train_ts_trans, order=c(1,0,0), 
                                       seasonal = list(order = c(1,0,0), period = 12),
                                       xreg = calendarioMasOutliersTrain,
                                       method="ML")
coeftest(modelo_5_explicativas_outliers)
cor.arma(modelo_5_explicativas_outliers)
summary(modelo_5_explicativas_outliers)


# Vamos entonces a tratar valores outliers
listaOutliersTrain =locate.outliers(modelo_5_explicativas_outliers$residuals,
                                    pars = coefs2poly(modelo_5_explicativas_outliers),
                                    types = c("AO", "LS", "TC"),cval=3)

listaOutliersTrain$abststat=abs(listaOutliersTrain$tstat)

# Cruzamos con la tabla original para obtener la fecha

datos_n_train$ind = as.numeric(rownames(datos_n_train))
listaOutliersTrainFecha = merge(listaOutliersTrain, datos_n_train[,c("ind", "fecha")], by = "ind")

listaOutliersTrainFecha




# Independencia: Ljung-Box
Box.test.2(residuals(modelo_5_explicativas_outliers),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")
# No podemos rechazar la hipótesis nula(Todos los p-valores son mayores de 0.05), 
#por tanto, los datos son independientes

# Homocedasticidad
residuos = residuals(modelo_5_explicativas_outliers)
n = length(residuos)
regresor = 1:n
lmtest::bptest(residuos~regresor) #No es homocedastico

# Normalidad con test de Lillie
library(nortest)
lillie.test(residuos) # Si es normal 


# Error del modelo propuesto 
predicciones_train_trans = modelo_5_explicativas_outliers$fitted
# Destransformamos 
predicciones_train = exp(predicciones_train_trans)
#Calculamos el MAPE punto a punto 
error_mape_modelo_5_explicativas_outliers = numeric()
for(i in 1:length(predicciones_train)){
  error_mape_modelo_5_explicativas_outliers[i] = abs(datos_n$Nacional[i] - predicciones_train[i])/ datos_n$Nacional[i] *100
}
mean(error_mape_modelo_5_explicativas_outliers)


# Lineas aereas

lineas_aereas = Arima(datos_n_train_ts_trans, order=c(0,1,1), 
                      seasonal = list(order = c(0,1,1), period = 12),
                      xreg = calendarioMasOutliersTrain,
                      method="ML")
# Independencia: Ljung-Box
Box.test.2(residuals(lineas_aereas),
           nlag = c(6,12,18,24,30,36,42,48),
           type="Ljung-Box")
# No podemos rechazar la hipótesis nula(Todos los p-valores son mayores de 0.05), 
#por tanto, los datos son independientes

# Homocedasticidad
residuos = residuals(lineas_aereas)
n = length(residuos)
regresor = 1:n
lmtest::bptest(residuos~regresor) #No es homocedastico

# Normalidad con test de Lillie
library(nortest)
lillie.test(residuos) # Si es normal 


# Error del modelo propuesto 
predicciones_train_trans = lineas_aereas$fitted
# Destransformamos 
predicciones_train = exp(predicciones_train_trans)
#Calculamos el MAPE punto a punto 
error_mape_modelo_lineas_aereas = numeric()
for(i in 1:length(predicciones_train)){
  error_mape_modelo_lineas_aereas[i] = abs(datos_n$Nacional[i] - predicciones_train[i])/ datos_n$Nacional[i] *100
}
mean(error_mape_modelo_lineas_aereas)


# Autoarima
modelo_autoarima = auto.arima(datos_n_train_ts_trans, 
                              xreg = calendarioMasOutliersTrain,
                              seasonal = TRUE,
                              allowdrift=F)
modelo_autoarima


# Error del modelo propuesto 
predicciones_train_trans = modelo_autoarima$fitted
# Destransformamos 
predicciones_train = exp(predicciones_train_trans)
#Calculamos el MAPE punto a punto 
error_mape_modelo_autoarima = numeric()
for(i in 1:length(predicciones_train)){
  error_mape_modelo_autoarima[i] = abs(datos_n$Nacional[i] - predicciones_train[i])/ datos_n$Nacional[i] *100
}
mean(error_mape_modelo_autoarima)


explicativasCalendarioTest <- calculoExplicativasCalendario(datos_n_test$fecha,domingoYFestivosJuntos=0)
calendarioTest <- as.matrix(explicativasCalendarioTest[,c("semanaSanta", "dt")])
outliersVariablesTest <- tail(outliersVariablesTrain,24)
calendarioMasOutliersTest <- as.matrix(cbind(calendarioTest[,c(1:2)],
                                             outliersVariablesTest))


calendarioMasOutliers = as.matrix(rbind(calendarioMasOutliersTrain,calendarioMasOutliersTest))
mejores_modelos = mejor_modelo_nacional(datos_n, explicativas = calendarioMasOutliersTrain, explicativas_pred = calendarioMasOutliers,  frecuencia = 12)

mejores_modelos$modelo_MAPE$modelo
mejores_modelos$modelo_AIC$modelo

mean(mejores_modelos$modelo_MAPE$predicciones$error_mape)
mean(mejores_modelos$modelo_AIC$predicciones$error_mape)

mean(mejores_modelos$modelo_MAPE$error_mape_modelo)
mean(mejores_modelos$modelo_AIC$error_mape_modelo)

pred_lineas_aereas = pred(lineas_aereas, datos_n_ts_trans, calendarioMasOutliers, transformacion = 0 , num_predicciones = 24)


pred_modelo_5_explicativas_outliers = pred(modelo_5_explicativas_outliers, datos_n_ts_trans, calendarioMasOutliers, transformacion = 0 , num_predicciones = 24)


mean(pred_lineas_aereas$error_mape)

mean(pred_modelo_5_explicativas_outliers$error_mape)

# Calculamos los límites del eje y con un margen visual (incluyendo la banda)
lim_i = min(datos_n_test_ts, mejores_modelos$modelo_MAPE$predicciones$L)
lim_s = max(datos_n_test_ts, mejores_modelos$modelo_MAPE$predicciones$U)
margen = 0.5 * (lim_s - lim_i)

# Creamos el gráfico base sin eje X automático
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "gray40",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Predicciones vs Serie Real (Modelo MAPE/Autoarima)",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

# Eje X con años enteros
axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Extraemos las predicciones
pred = mejores_modelos$modelo_MAPE$predicciones$Prediccion
lwr = mejores_modelos$modelo_MAPE$predicciones$L
upr = mejores_modelos$modelo_MAPE$predicciones$U
time_index = time(pred)  # Extraemos el índice temporal de las predicciones

# Dibujamos banda de confianza
polygon(
  c(time_index, rev(time_index)),
  c(lwr, rev(upr)),
  col = rgb(1, 0.5, 0, 0.2),  # Naranja suave semitransparente
  border = NA
)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)

# Predicción
lines(pred, col = "#ff7f0e", lwd = 2)

# Leyenda
legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción modelo MAPE", "Intervalo de confianza"),
  col = c("#1f77b4", "#ff7f0e", rgb(1, 0.5, 0, 0.5)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)

# Graficos mejor modelo AIC
# Calculamos los límites del eje y con un margen visual (incluyendo la banda)
lim_i = min(datos_n_test_ts, mejores_modelos$modelo_AIC$predicciones$L)
lim_s = max(datos_n_test_ts, mejores_modelos$modelo_AIC$predicciones$U)
margen = 0.5 * (lim_s - lim_i)

# Creamos el gráfico base sin eje X automático
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "gray40",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Predicciones vs Serie Real (Modelo AIC)",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

# Eje X con años enteros
axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Extraemos las predicciones del modelo AIC
pred = mejores_modelos$modelo_AIC$predicciones$Prediccion
lwr = mejores_modelos$modelo_AIC$predicciones$L
upr = mejores_modelos$modelo_AIC$predicciones$U
time_index = time(pred)

# Dibujamos la banda de confianza
polygon(
  c(time_index, rev(time_index)),
  c(lwr, rev(upr)),
  col = rgb(0.2, 0.6, 0.2, 0.2),  # Verde suave semitransparente
  border = NA
)

# Serie real (test)
lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)

# Predicción del modelo AIC
lines(pred, col = "#2ca02c", lwd = 2)  # Verde fuerte

# Leyenda
legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción modelo AIC", "Intervalo de confianza"),
  col = c("#1f77b4", "#2ca02c", rgb(0.2, 0.6, 0.2, 0.5)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)

### Lineas aereas 
# Calculamos los límites del eje y con un margen visual
lim_i = min(datos_n_test_ts, pred_lineas_aereas$lower)
lim_s = max(datos_n_test_ts, pred_lineas_aereas$upper)
margen = 0.5 * (lim_s - lim_i)

# Gráfico base
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "#4D4D4D",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Predicciones vs Serie Real (Modelo Líneas Aéreas)",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Predicciones y banda
pred = pred_lineas_aereas$predicciones
lwr = pred_lineas_aereas$lower
upr = pred_lineas_aereas$upper
time_index = time(pred)

polygon(
  c(time_index, rev(time_index)),
  c(lwr, rev(upr)),
  col = rgb(0.1, 0.75, 0.85, 0.2),  # Azul verdoso semitransparente
  border = NA
)

lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
lines(pred, col = "#17becf", lwd = 2)  # Azul verdoso

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción modelo Líneas Aéreas", "Intervalo de confianza"),
  col = c("#1f77b4", "#17becf", rgb(0.1, 0.75, 0.85, 0.5)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)



# Modelo ajuste manual
# Calculamos los límites del eje y con un margen visual
lim_i = min(datos_n_test_ts, pred_modelo_5_explicativas_outliers$lower)
lim_s = max(datos_n_test_ts, pred_modelo_5_explicativas_outliers$upper)
margen = 0.5 * (lim_s - lim_i)

# Gráfico base
plot(
  datos_n_ts,
  xlim = c(2017, 2020),
  ylim = c(lim_i - margen, lim_s + margen),
  type = "l",
  col = "#4D4D4D",
  lwd = 2,
  xlab = "Año",
  ylab = "Valor",
  main = "Predicciones vs Serie Real (Ajuste manual)",
  cex.main = 1.8,
  cex.lab = 1.1,
  xaxt = "n"
)

axis(side = 1, at = 2017:2020, labels = 2017:2020)

# Predicciones y banda
pred = pred_modelo_5_explicativas_outliers$predicciones
lwr = pred_modelo_5_explicativas_outliers$lower
upr = pred_modelo_5_explicativas_outliers$upper
time_index = time(pred)

polygon(
  c(time_index, rev(time_index)),
  c(lwr, rev(upr)),
  col = rgb(0.6, 0.4, 0.8, 0.2),  # Morado suave
  border = NA
)

lines(datos_n_test_ts, col = "#1f77b4", lwd = 2)
lines(pred, col = "#9467bd", lwd = 2)  # Morado intenso

legend(
  "topleft",
  legend = c("Serie real (test)", "Predicción ajuste manual", "Intervalo de confianza"),
  col = c("#1f77b4", "#9467bd", rgb(0.6, 0.4, 0.8, 0.5)),
  lwd = c(2, 2, NA),
  pch = c(NA, NA, 15),
  pt.cex = 2,
  bty = "n"
)

