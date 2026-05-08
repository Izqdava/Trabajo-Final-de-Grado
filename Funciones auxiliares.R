# Funcion para prediccion
# Modelo ARIMA 
# datos, datos target completos de la serie transformar 
# transformacion (0,0.5 y 1) para transformacion logaritmica, raiz cuadrada y sin tranformacion
# explicativas, variables explicativas para los datos totales

pred =function(modelo, datos, explicativas = NULL, transformacion = 1,num_predicciones = 12, alpha = 0.05, frecuencia = 12, fecha_y = 2009, fecha_m=1){
  #Extraemos los ordenes del modelo 
  ordenes = modelo$arma
  p = ordenes[1]
  q = ordenes[2]
  P = ordenes[3]
  Q = ordenes[4]
  d = ordenes[6]
  D = ordenes[7]
  
  # Inicializamos las variables necesarias
  vector_mape = numeric()
  vector_prediccion = numeric()
  # Intervalos de confianza para las predicciones
  vector_lower = numeric()
  vector_upper = numeric()
  
  # Extraemos los datos train y transformamos en serie temporal
  datos_aux = datos[1:108]
  datos_aux = ts(datos_aux, start = c(fecha_y, fecha_m), frequency = frecuencia)
  
  for(i in 1:num_predicciones){
    # Rehacemos el modelo arima con los ordenes y los datos de la iteracion
    
    # dividimos el modelo entre con explicativas y sin 
    if(is.null(explicativas)){
      modelo = Arima(datos_aux,
                     order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia), 
                     method="CSS")
      # Calculamos predicciones y guardamos
      prediccion = forecast(modelo, h=1 , level=c(1-alpha))
      
    }
    else{
      modelo = Arima(datos_aux,
                     order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia),
                     xreg = explicativas[1:length(datos_aux),, drop =FALSE],
                     method="CSS")
      # Calculamos predicciones y guardamos
      prediccion = forecast(modelo, h=1 ,xreg = explicativas[length(datos_aux)+1,, drop = FALSE], level=c(1-alpha))
    }
    
    
    #Destransformamos todos los calculos 
    if(transformacion == 0){
      prediccion_real = exp(prediccion$mean)
      lower_real = exp(prediccion$lower[,1])
      upper_real = exp(prediccion$upper[,1])
      datos_reales = exp(datos)
    }
    if(transformacion == 0.5){
      prediccion_real = (prediccion$mean)^2
      lower_real = (prediccion$lower[,1])^2
      upper_real = (prediccion$upper[,1])^2
      datos_reales = (datos)^2
    }
    if(transformacion == 1){
      prediccion_real = (prediccion$mean)
      lower_real = (prediccion$lower[,1])
      upper_real = (prediccion$upper[,1])
      datos_reales = (datos)
    }
    
    # Guardamos los calculos destransformados
    vector_prediccion[i] = prediccion_real
    # Calculamos el intervalo de confianza
    vector_lower[i] = lower_real
    vector_upper[i] = upper_real
    
    # Calculamos mape
    vector_mape[i] = abs(datos_reales[length(datos_aux)+i] - prediccion_real)/datos_reales[length(datos_aux)+i] *100
    # Recalculamos los datos_aux con un nuevo valor
    datos_aux = datos[1:length(datos_aux)+i]
    datos_aux = ts(datos_aux, start = c(fecha_y, fecha_m), frequency = frecuencia)
  }
  # Convertimos las predicciones en serie temporal
  pred_ts = ts(vector_prediccion, start = c(2018,1), frequency = 12)
  pred_lower = ts(vector_lower, start = c(2018,1), frequency = 12)
  pred_upper = ts(vector_upper, start = c(2018,1), frequency = 12)
  return(list(predicciones = pred_ts,
              lower = pred_lower,
              upper = pred_upper,
              error_mape = vector_mape))
}


# Función para obtener la comunidad autónoma
encontrar_comunidad = function(provincia_o_comunidad) {
  comunidades <- list(
    "01 Andalucía" = c("04 Almería", "11 Cádiz", "14 Córdoba", "18 Granada", "21 Huelva", "23 Jaén", "29 Málaga", "41 Sevilla"),
    "02 Aragón" = c("22 Huesca", "44 Teruel", "50 Zaragoza"),
    "03 Asturias, Principado de" = c("33 Asturias"),
    "04 Balears, Illes" = c("07 Balears, Illes"),
    "05 Canarias" = c("35 Palmas, Las", "38 Santa Cruz de Tenerife"),
    "06 Cantabria" = c("39 Cantabria"),
    "07 Castilla y León" = c("05 Ávila", "09 Burgos", "24 León", "34 Palencia", "37 Salamanca", "40 Segovia", "42 Soria", "47 Valladolid", "49 Zamora"),
    "08 Castilla - La Mancha" = c("02 Albacete", "13 Ciudad Real", "16 Cuenca", "19 Guadalajara", "45 Toledo"),
    "09 Cataluña" = c("08 Barcelona", "17 Girona", "25 Lleida", "43 Tarragona"),
    "10 Comunitat Valenciana" = c("03 Alicante/Alacant", "12 Castellón/Castelló", "46 Valencia/València"),
    "11 Extremadura" = c("06 Badajoz", "10 Cáceres"),
    "12 Galicia" = c("15 Coruña, A", "27 Lugo", "32 Ourense", "36 Pontevedra"),
    "13 Madrid, Comunidad de" = c("28 Madrid"),
    "14 Murcia, Región de" = c("30 Murcia"),
    "15 Navarra, Comunidad Foral de" = c("31 Navarra"),
    "16 País Vasco" = c("01 Araba/Álava", "48 Bizkaia", "20 Gipuzkoa"),
    "17 Rioja, La" = c("26 Rioja, La"),
    "18 Ceuta" = c("51 Ceuta"),
    "19 Melilla" = c("52 Melilla")
  )
  if (provincia_o_comunidad %in% names(comunidades)) {
    return(provincia_o_comunidad)
  }
  for (comunidad in names(comunidades)) {
    if (provincia_o_comunidad %in% comunidades[[comunidad]]) {
      return(comunidad)
    }
  }
  return("Nombre no encontrado")
}

# Variables explicativas
calculoExplicativasCalendario = function(variableFecha,comunidad="", domingoYFestivosJuntos){
  comunidad = encontrar_comunidad(comunidad)
  
  #######################################
  #     Creacion de todas las fechas    #
  #######################################
  
  # Se crean las fechas a nivel diario entre una primera y una ultima fecha dada.
  # Para llegar al ultimo dia del mes de la ?ltima fecha, se suman dias para llegar
  # al 28/29, 30 o 31 segun proceda.
  
  library(lubridate)
  
  if (month(max(variableFecha)) %in% c(1,3,5,7,8,10,12)) {
    diasHastaFinMes <- 30
  } else if (month(max(variableFecha)) %in% c(4,6,9,11)) {
    diasHastaFinMes <- 29
  } else if (year(max(variableFecha))%%4==0) {
    diasHastaFinMes <- 28
  } else {diasHastaFinMes <- 27}
  
  todasLasFechas <- data.frame(fechas=seq(min(variableFecha),
                                          max(variableFecha)+diasHastaFinMes,
                                          by="days"))
  
  #######################################
  #     C?lculo de la Semana Santa      #
  #######################################
  
  # Funci?n Easter de timeDate
  
  # install.packages("timeDate")
  library(timeDate)
  
  domingoResurrecion <- as.Date(Easter(year(min(variableFecha)):year(max(variableFecha))))
  lunesPascua <- domingoResurrecion+1
  sabadoSanto <- domingoResurrecion-1
  viernesSanto <- domingoResurrecion-2
  juevesSanto <- domingoResurrecion-3
  
  # Se unen y ordenan todos los d?as que forman la Semana Santa
  semanaSanta <- sort(c(juevesSanto, viernesSanto, sabadoSanto, domingoResurrecion, lunesPascua))
  
  # Se pone en formato data.frame y se a?ade un indicador
  semanaSanta <- data.frame(fechas=semanaSanta, semanaSanta=rep(1,length(semanaSanta)))
  
  # Se a?aden a la tabla maestra de fechas
  todasLasFechas_2 <- merge(x = todasLasFechas, y = semanaSanta, by = "fechas", all.x = TRUE)
  
  # Se reemplazan los NAs por 0, terminando de definir as? el indicador de SemanaSanta
  todasLasFechas_2$semanaSanta[is.na(todasLasFechas_2$semanaSanta)] <- 0
  
  
  ######################################
  #     C?lculo de la variable dt      #
  ######################################
  
  # 1. Definici?n de festivos:
  ############################
  
  calendario <- todasLasFechas
  
  calendario$diaSemana <- as.factor(wday(calendario$fecha))
  calendario$diaMes <- as.factor(day(calendario$fecha))
  calendario$mes <- as.factor(month(calendario$fecha))
  calendario$anyo <- as.factor(year(calendario$fecha))
  
  # Festivos comunes a todo el conjunto nacional
  calendario$p_01ene <- ifelse(calendario$diaMes==1 & calendario$mes==1, 1, 0)
  calendario$p_06ene <- ifelse(calendario$diaMes==6 & calendario$mes==1, 1, 0)
  
  if(comunidad=="01 Andalucía"){
    calendario$p_28feb <- ifelse(calendario$diaMes==28 & calendario$mes==2, 1, 0)}
  
  if(comunidad=="04 Balears, Illes"){
    calendario$p_1mar <- ifelse(calendario$diaMes==1 & calendario$mes==3, 1, 0)}
  
  calendario$p_19mar <- ifelse(calendario$diaMes==19 & calendario$mes==3, 1, 0)
  
  if(comunidad=="02 Aragón" || comunidad == "07 Castilla y León"){
    calendario$p_23abr <- ifelse(calendario$diaMes==23 & calendario$mes==4, 1, 0)}
  
  
  calendario$p_01may <- ifelse(calendario$diaMes==1 & calendario$mes==5, 1, 0)
  
  if(comunidad=="13 Madrid, Comunidad de"){
    calendario$p_02may <- ifelse(calendario$diaMes==2 & calendario$mes==5, 1, 0)
  }
  
  if(comunidad=="05 Canarias"){
    calendario$p_30may <- ifelse(calendario$diaMes==30 & calendario$mes==5, 1, 0)}
  
  if(comunidad=="08 Castilla - La Mancha" ){
    calendario$p_31may <- ifelse(calendario$diaMes==31 & calendario$mes==5, 1, 0)}
  
  if(comunidad=="14 Murcia, Región de" || comunidad=="17 Rioja, La"){
    calendario$p_9jun <- ifelse(calendario$diaMes==9 & calendario$mes==6, 1, 0)}
  
  if(comunidad=="12 Galicia"){
    calendario$p_25jul <- ifelse(calendario$diaMes==25 & calendario$mes==7, 1, 0)}
  
  if(comunidad=="06 Cantabria"){
    calendario$p_28jul <- ifelse(calendario$diaMes==28 & calendario$mes==7, 1, 0)}
  
  calendario$p_15ago <- ifelse(calendario$diaMes==15 & calendario$mes==8, 1, 0)
  
  if(comunidad=="18 Ceuta"){
    calendario$p_2sep <- ifelse(calendario$diaMes==2 & calendario$mes==9, 1, 0)}
  
  if(comunidad=="03 Asturias, Principado de" || comunidad=="11 Extremadura"){
    calendario$p_8sep <- ifelse(calendario$diaMes==8 & calendario$mes==9, 1, 0)}
  
  if(comunidad=="09 Cataluña"){
    calendario$p_11sep <- ifelse(calendario$diaMes==11 & calendario$mes==9, 1, 0)}
  
  if(comunidad=="19 Melilla"){
    calendario$p_17sep <- ifelse(calendario$diaMes==17 & calendario$mes==9, 1, 0)}
  
  if(comunidad=="10 Comunitat Valenciana"){
    calendario$p_9oct <- ifelse(calendario$diaMes==9 & calendario$mes==10, 1, 0)}
  
  calendario$p_12oct <- ifelse(calendario$diaMes==12 & calendario$mes==10,1, 0)
  
  if(comunidad=="16 País Vasco"){
    calendario$p_25oct <- ifelse(calendario$diaMes==25 & calendario$mes==10, 1, 0)}
  
  calendario$p_01nov <- ifelse(calendario$diaMes==1 & calendario$mes==11, 1 ,0)
  
  if(comunidad=="15 Navarra, Comunidad Foral de"){
    calendario$p_3dic <- ifelse(calendario$diaMes==3 & calendario$mes==12, 1, 0)}
  
  calendario$p_06dic <- ifelse(calendario$diaMes==6 & calendario$mes==12, 1 ,0)
  calendario$p_08dic <- ifelse(calendario$diaMes==8 & calendario$mes==12, 1 ,0)
  calendario$p_25dic <- ifelse(calendario$diaMes==25 & calendario$mes==12, 1 ,0)
  
  calendario$festivo <- rowSums(subset(calendario, select=p_01ene:p_25dic))
  
  # La definicion de la variable dt varia segun la opcion domingoYFestivosJuntos.
  
  if (domingoYFestivosJuntos==0){
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    
    # D?as laborables: todos menos s?bados y domingos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo
    
  } else {
    
    calendario$sabado <- ifelse(calendario$diaSemana==7, 1 ,0)
    calendario$domingo <- ifelse(calendario$diaSemana==1, 1 ,0)
    # Domingo=1 si domingo=1 o festivo=1
    calendario$domingo <- ifelse(calendario$domingo==1 | calendario$festivo==1, 1 ,0)
    
    # D?as laborables: todos menos s?bados y domingos/festivos
    calendario$laborable <- 1-calendario$sabado-calendario$domingo    
  }
  
  
  # 2. Definici?n de variable dt:
  ###############################
  
  # Se filtran las columnas de inter?s y se a?ade la Semana Santa
  
  calendario_2 <- calendario[, c("fechas", "mes", "anyo", "sabado", "domingo", "laborable", "festivo")]
  
  todasLasFechasFinal <- merge(x = todasLasFechas_2, y = calendario_2,
                               by = "fechas", all.x = TRUE)
  
  # Agregamos la serie a nivel a?o-mes
  
  calendarioAnyoMes <- aggregate(todasLasFechasFinal[,c("sabado","domingo",
                                                        "laborable", "semanaSanta", "festivo")],
                                 by=list(mes=todasLasFechasFinal$mes,
                                         anyo=todasLasFechasFinal$anyo),
                                 "sum")
  
  # Se calcula la variable dt:
  
  calendarioAnyoMes$dt <- calendarioAnyoMes$laborable-(5/2)*(calendarioAnyoMes$sabado+calendarioAnyoMes$domingo)
  
  ######################################
  #     C?lculo de a?os bisiestos      #
  ######################################
  
  calendarioAnyoMes$anyoNum <- as.numeric(levels(calendarioAnyoMes$anyo))[calendarioAnyoMes$anyo]
  
  calendarioAnyoMes$bisiesto <- ifelse(calendarioAnyoMes$mes==2 &(calendarioAnyoMes$anyoNum %% 4)==0, 1 ,0)
  
  #######################################################
  #     Tabla final con explicativas de calendario      #
  #######################################################
  
  if (domingoYFestivosJuntos==0){
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto", "festivo")])
  } else {
    explicativasCalendario <- cbind(fecha=variableFecha, calendarioAnyoMes[, c("semanaSanta", "dt", "bisiesto")])
  }
  
  return(explicativasCalendario)
  
}

mejor_modelo = function(datos_completos, explicativas_bin = 0,  frecuencia = 12, alpha = 0.05, p_sup = 2, d_sup = 1, q_sup = 2, P_sup = 2, D_sup = 1, Q_sup = 2){
  # Rangos de cada variable
  p_rango = 0:p_sup
  d_rango = 0:d_sup
  q_rango = 0:q_sup
  P_rango = 0:P_sup
  D_rango = 0:D_sup
  Q_rango = 0:Q_sup
  
  # Creamos todas las combinaciones
  combinaciones = expand.grid(p = p_rango, d = d_rango, q = q_rango, P = P_rango, D = D_rango, Q = Q_rango)
  
  # Inicializamos las condiciones inciales para el mejor modelo
  mejor_mape = Inf 
  mejor_aic = Inf
  mejor_modelo_mape = NULL
  mejor_modelo_aic = NULL
  
  # Comenzamos con el modelo
  # Obtenemos los datos de train
  comunidad_datos = names(datos_completos)[2]
  datos_completos$fecha=as.Date(datos_completos$fecha,format='%m/%d/%Y')
  colnames(datos_completos) = c('fecha', 'Target')
  datos = subset(datos_completos, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2009 a 2017 (ambos incluidos)
  datos_test = subset(datos_completos, fecha>as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2018 a 2019 (ambos incluidos)
  
  # Calculamos las variables explicativas si procede
  if(explicativas_bin==1){
    explicativasCalendarioTrain = calculoExplicativasCalendario(datos_completos$fecha,comunidad = comunidad_datos, domingoYFestivosJuntos=1)
    explicativas = as.matrix(explicativasCalendarioTrain[,c("semanaSanta", "dt", "bisiesto")])
    explicativas_train = explicativas[1:108,]
  }
  
  
  # Comprobamos posibles transformaciones
  box_cox = boxcox(Target ~ fecha,
                   data = datos,
                   lambda = c(0, 0.5, 1),plotit=FALSE)
  lambda = box_cox$x[which.max(box_cox$y)] # 0 = log , 0.5 = sqrt y 1 = sin tranformada
  
  # Transformamos los datos 
  if(lambda == 0){
    datos_completos_trans = log(datos_completos$Target)
    datos_trans = log(datos$Target)
  }
  if(lambda == 0.5){
    datos_completos_trans = sqrt(datos_completos$Target)
    datos_trans = sqrt(datos$Target)
  }
  if(lambda == 1){
    datos_completos_trans = (datos_completos$Target)
    datos_trans = datos$Target
  }
  # Convertimos en serie temporal
  datos_ts_trans = ts(datos_trans, start = c(2009,1), frequency = 12)
  
  
  # Comenzamos el bucle de busqueda para todas las combinaciones
  for(i in 1:nrow(combinaciones)){
    # Extraemos cada valor de la combinacion actual
    p = combinaciones$p[i]
    d = combinaciones$d[i]
    q = combinaciones$q[i]
    P = combinaciones$P[i]
    D = combinaciones$D[i]
    Q = combinaciones$Q[i]
    
    # Eliminamos combinaciones erroneas
    if((p+q+P+Q) == 0){next}
    
    # Comenzamos el proceso de calculo del modelo arima y las hipotesis que han de validarse
    
    # Tenemos la opcion de añadir variables explicativas 
    if(explicativas_bin == 0){
      modelo_propuesto = tryCatch({
        Arima(datos_ts_trans, order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia), method="ML")},
        error = function(e){NULL})# Si ocurre un error, devuelve NULL
    }
    else{
      modelo_propuesto = tryCatch({
        Arima(datos_ts_trans, 
              order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia),
              xreg = explicativas_train,
              method="ML",
              include.drift = TRUE)}, error = function(e){NULL})# Si ocurre un error, devuelve NULL
      
      if(is.null(modelo_propuesto)){next} # Saltamos a otra iteración 
      
      # Comprobamos que sean significativos 
      p_val_explicativas = tail(coeftest(modelo_propuesto)[,4],3)
      # En primer lugar elegimos las varaibales explicativas que añadir
      coef_explicativas = tail(modelo_propuesto$coef,3)
      # Queremos que las variables tengan signo <, > y >
      coef_explicativas[1] = -coef_explicativas[1]
      nuevas_explicativas = as.matrix(explicativas_train[, which(coef_explicativas>0 & p_val_explicativas<0.05)])
      
      if(ncol(nuevas_explicativas) == 0){
        modelo_propuesto = tryCatch({
          Arima(datos_ts_trans, 
                order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia),
                method="ML")}, error = function(e){NULL})# Si ocurre un error, devuelve NULL
      }else if(ncol(nuevas_explicativas) == 1 || ncol(nuevas_explicativas) == 2){
        modelo_propuesto = tryCatch({
          Arima(datos_ts_trans, 
                order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia),
                xreg = nuevas_explicativas,
                method="ML")}, error = function(e){NULL})# Si ocurre un error, devuelve NULL
      }
      
    }
    # Vamos a ir comprobando diferentes errores y comprobando las hipotesis
    
    if(is.null(modelo_propuesto)){next} # Saltamos a otra iteración 
    
    # Calculamos los p-valores de los coeficientes 
    p_val_coef = as.numeric(head(coeftest(modelo_propuesto),p+q+P+Q)[,4]) # Calculamos los p_valores de los coeficientes
    # Comprobamos si los p_valores son valores o son NA, NaN
    if(any(is.na(p_val_coef))){next}
    # Comprobamos si los coeficientes son significativos
    if(any(!(p_val_coef < alpha))){next} # Saltamos a otra iteración 
    
    # Validamos las hipótesis del modelo
    # Independencia 
    indep = checkresiduals(modelo_propuesto, plot = FALSE, test = F)
    residuos=modelo_propuesto$residuals
    # Verificamos que los residuos no contengan NA
    if(any(is.na(residuos))){next} # Si los residuos contienen NA, saltamos a la siguiente iteración
    # Homocedasticidad
    n = length(residuos)
    regresor = 1:n
    homocedas = lmtest::bptest(residuos~regresor)
    # Normalidad con test de Lillie
    normalidad = lillie.test(residuos)
    
    # p_valores de los test
    p_val_test = c(indep$p.value, homocedas$p.value, normalidad$p.value)
    if(any((p_val_test < alpha))){next}
    
    # Una vez todas nuestras hipótesis son validadas comprobamos el error del modelo
    error_mape = accuracy(modelo_propuesto)[1,"MAPE"]
    
    if(error_mape < mejor_mape){
      mejor_mape = error_mape
      mejor_modelo_mape = modelo_propuesto
    }
    
    # Una vez todas nuestras hipótesis son validadas comprobamos el error del modelo
    error_aic = modelo_propuesto$aic
    
    if(error_aic < mejor_aic){
      mejor_aic = error_aic
      mejor_modelo_aic = modelo_propuesto
    }
  }
  
  if(is.null(mejor_modelo_mape) || is.null(mejor_modelo_aic)){
    return(NULL)
  }
  else{
    # Una vez tenemos el mejor modelo vamos a predecir 
    lista_predicciones_mape = pred(mejor_modelo_mape, datos_completos_trans, explicativas, transformacion = lambda , num_predicciones = 24)
    lista_predicciones_aic = pred(mejor_modelo_aic, datos_completos_trans, explicativas, transformacion = lambda , num_predicciones = 24)
    
    # Extraemos los datos
    predicciones_mape = lista_predicciones_mape$predicciones
    intervalo_lower_mape = lista_predicciones_mape$lower
    intervalo_upper_mape = lista_predicciones_mape$upper
    
    predicciones_reales_mape = data.frame( Tiempo = datos_test$fecha,
                                           Prediccion = predicciones_mape,
                                           L = intervalo_lower_mape,  # Límite inferior del intervalo de confianza
                                           U = intervalo_upper_mape,  # Límite superior del intervalo de confianza
                                           error_mape = lista_predicciones_mape$error_mape
    )
    
    # Extraemos los datos
    predicciones_aic = lista_predicciones_aic$predicciones
    intervalo_lower_aic = lista_predicciones_aic$lower
    intervalo_upper_aic = lista_predicciones_aic$upper
    
    predicciones_reales_aic = data.frame( Tiempo = datos_test$fecha,
                                          Prediccion = predicciones_aic,
                                          L = intervalo_lower_aic,  # Límite inferior del intervalo de confianza
                                          U = intervalo_upper_aic,  # Límite superior del intervalo de confianza
                                          error_mape = lista_predicciones_aic$error_mape
    )
    
    
    
    # Error del modelo propuesto 
    predicciones_train_trans_mape = mejor_modelo_mape$fitted
    predicciones_train_trans_aic = mejor_modelo_aic$fitted
    # Destransformamos
    if(lambda == 0){
      predicciones_train_mape = exp(predicciones_train_trans_mape)
      predicciones_train_aic = exp(predicciones_train_trans_aic)
    }
    if(lambda == 0.5){
      predicciones_train_mape = (predicciones_train_trans_mape)^2
      predicciones_train_aic = (predicciones_train_trans_aic)^2
    }
    if(lambda == 1){
      predicciones_train_mape = predicciones_train_trans_mape
      predicciones_train_aic = predicciones_train_trans_aic
    }
    
    #Calculamos el MAPE punto a punto 
    error_mape_modelo_mape = numeric()
    error_mape_modelo_aic = numeric()
    for(i in 1:length(predicciones_train_mape)){
      dato_real = datos_completos$Target[i]
      error_mape_modelo_mape[i] = abs(dato_real - predicciones_train_mape[i])/ dato_real *100
      error_mape_modelo_aic[i] = abs(dato_real - predicciones_train_aic[i])/ dato_real *100
    }
    
    MAPE = list(modelo = mejor_modelo_mape, error_mape_modelo = error_mape_modelo_mape,
                tranformacion = lambda,
                predicciones = predicciones_reales_mape)
    
    AIC = list(modelo = mejor_modelo_aic, error_mape_modelo = error_mape_modelo_aic,
               tranformacion = lambda,
               predicciones = predicciones_reales_aic)
    
    return(list(modelo_MAPE = MAPE,
                modelo_AIC = AIC))
  }
  
}



mejor_modelo_nacional= function(datos_completos, explicativas, explicativas_pred, frecuencia = 12, alpha = 0.05, p_sup = 2, d_sup = 1, q_sup = 2, P_sup = 2, D_sup = 1, Q_sup = 2){
  # Rangos de cada variable
  p_rango = 0:p_sup
  d_rango = 0:d_sup
  q_rango = 0:q_sup
  P_rango = 0:P_sup
  D_rango = 0:D_sup
  Q_rango = 0:Q_sup
  
  # Creamos todas las combinaciones
  combinaciones = expand.grid(p = p_rango, d = d_rango, q = q_rango, P = P_rango, D = D_rango, Q = Q_rango)
  
  # Inicializamos las condiciones inciales para el mejor modelo
  mejor_mape = Inf 
  mejor_aic = Inf
  mejor_modelo_mape = NULL
  mejor_modelo_aic = NULL
  
  # Comenzamos con el modelo
  # Obtenemos los datos de train
  comunidad_datos = names(datos_completos)[2]
  datos_completos$fecha=as.Date(datos_completos$fecha,format='%m/%d/%Y')
  colnames(datos_completos) = c('fecha', 'Target')
  datos = subset(datos_completos, fecha<=as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2009 a 2017 (ambos incluidos)
  datos_test = subset(datos_completos, fecha>as.Date('12/01/2017',format='%m/%d/%Y'))# desde 2018 a 2019 (ambos incluidos)
  
  # Comprobamos posibles transformaciones
  box_cox = boxcox(Target ~ fecha,
                   data = datos,
                   lambda = c(0, 0.5, 1),plotit=FALSE)
  lambda = box_cox$x[which.max(box_cox$y)] # 0 = log , 0.5 = sqrt y 1 = sin tranformada
  
  # Transformamos los datos 
  if(lambda == 0){
    datos_completos_trans = log(datos_completos$Target)
    datos_trans = log(datos$Target)
  }
  if(lambda == 0.5){
    datos_completos_trans = sqrt(datos_completos$Target)
    datos_trans = sqrt(datos$Target)
  }
  if(lambda == 1){
    datos_completos_trans = (datos_completos$Target)
    datos_trans = datos$Target
  }
  # Convertimos en serie temporal
  datos_ts_trans = ts(datos_trans, start = c(2009,1), frequency = 12)
  
  
  # Comenzamos el bucle de busqueda para todas las combinaciones
  for(i in 1:nrow(combinaciones)){
    # Extraemos cada valor de la combinacion actual
    p = combinaciones$p[i]
    d = combinaciones$d[i]
    q = combinaciones$q[i]
    P = combinaciones$P[i]
    D = combinaciones$D[i]
    Q = combinaciones$Q[i]
    
    # Eliminamos combinaciones erroneas
    if((p+q+P+Q) == 0){next}
    
    # Comenzamos el proceso de calculo del modelo arima y las hipotesis que han de validarse
    
    # Tenemos la opcion de añadir variables explicativas 
    modelo_propuesto = tryCatch({
      Arima(datos_ts_trans, 
            order=c(p,d,q), seasonal = list(order = c(P,D,Q), period = frecuencia),
            xreg = explicativas,
            method="ML",
            include.drift = TRUE)}, error = function(e){NULL})# Si ocurre un error, devuelve NULL
    
    if(is.null(modelo_propuesto)){next} # Saltamos a otra iteración 
    
    # Vamos a ir comprobando diferentes errores y comprobando las hipotesis
    
    if(is.null(modelo_propuesto)){next} # Saltamos a otra iteración 
    
    # Calculamos los p-valores de los coeficientes 
    p_val_coef = as.numeric(head(coeftest(modelo_propuesto),p+q+P+Q)[,4]) # Calculamos los p_valores de los coeficientes
    # Comprobamos si los p_valores son valores o son NA, NaN
    if(any(is.na(p_val_coef))){next}
    # Comprobamos si los coeficientes son significativos
    if(any(!(p_val_coef < alpha))){next} # Saltamos a otra iteración 
    
    # Validamos las hipótesis del modelo
    # Independencia 
    indep = checkresiduals(modelo_propuesto, plot = FALSE, test = F)
    residuos=modelo_propuesto$residuals
    # Verificamos que los residuos no contengan NA
    if(any(is.na(residuos))){next} # Si los residuos contienen NA, saltamos a la siguiente iteración
    # Homocedasticidad
    n = length(residuos)
    regresor = 1:n
    homocedas = lmtest::bptest(residuos~regresor)
    # Normalidad con test de Lillie
    normalidad = lillie.test(residuos)
    
    # p_valores de los test
    p_val_test = c(indep$p.value, homocedas$p.value, normalidad$p.value)
    if(any((p_val_test < alpha))){next}
    
    # Una vez todas nuestras hipótesis son validadas comprobamos el error del modelo
    error_mape = accuracy(modelo_propuesto)[1,"MAPE"]
    
    if(error_mape < mejor_mape){
      mejor_mape = error_mape
      mejor_modelo_mape = modelo_propuesto
    }
    
    # Una vez todas nuestras hipótesis son validadas comprobamos el error del modelo
    error_aic = modelo_propuesto$aic
    
    if(error_aic < mejor_aic){
      mejor_aic = error_aic
      mejor_modelo_aic = modelo_propuesto
    }
  }
  
  if(is.null(mejor_modelo_mape) || is.null(mejor_modelo_aic)){
    return(NULL)
  }
  else{
    # Una vez tenemos el mejor modelo vamos a predecir 
    
    lista_predicciones_mape = pred(mejor_modelo_mape, datos_completos_trans, explicativas_pred, transformacion = lambda , num_predicciones = 24)
    lista_predicciones_aic = pred(mejor_modelo_aic, datos_completos_trans, explicativas_pred, transformacion = lambda , num_predicciones = 24)
    
    # Extraemos los datos
    predicciones_mape = lista_predicciones_mape$predicciones
    intervalo_lower_mape = lista_predicciones_mape$lower
    intervalo_upper_mape = lista_predicciones_mape$upper
    
    predicciones_reales_mape = data.frame( Tiempo = datos_test$fecha,
                                           Prediccion = predicciones_mape,
                                           L = intervalo_lower_mape,  # Límite inferior del intervalo de confianza
                                           U = intervalo_upper_mape,  # Límite superior del intervalo de confianza
                                           error_mape = lista_predicciones_mape$error_mape
    )
    
    # Extraemos los datos
    predicciones_aic = lista_predicciones_aic$predicciones
    intervalo_lower_aic = lista_predicciones_aic$lower
    intervalo_upper_aic = lista_predicciones_aic$upper
    
    predicciones_reales_aic = data.frame( Tiempo = datos_test$fecha,
                                          Prediccion = predicciones_aic,
                                          L = intervalo_lower_aic,  # Límite inferior del intervalo de confianza
                                          U = intervalo_upper_aic,  # Límite superior del intervalo de confianza
                                          error_mape = lista_predicciones_aic$error_mape
    )
    
    
    
    # Error del modelo propuesto 
    predicciones_train_trans_mape = mejor_modelo_mape$fitted
    predicciones_train_trans_aic = mejor_modelo_aic$fitted
    # Destransformamos
    if(lambda == 0){
      predicciones_train_mape = exp(predicciones_train_trans_mape)
      predicciones_train_aic = exp(predicciones_train_trans_aic)
    }
    if(lambda == 0.5){
      predicciones_train_mape = (predicciones_train_trans_mape)^2
      predicciones_train_aic = (predicciones_train_trans_aic)^2
    }
    if(lambda == 1){
      predicciones_train_mape = predicciones_train_trans_mape
      predicciones_train_aic = predicciones_train_trans_aic
    }
    
    #Calculamos el MAPE punto a punto 
    error_mape_modelo_mape = numeric()
    error_mape_modelo_aic = numeric()
    for(i in 1:length(predicciones_train_mape)){
      dato_real = datos_completos$Target[i]
      error_mape_modelo_mape[i] = abs(dato_real - predicciones_train_mape[i])/ dato_real *100
      error_mape_modelo_aic[i] = abs(dato_real - predicciones_train_aic[i])/ dato_real *100
    }
    
    MAPE = list(modelo = mejor_modelo_mape, error_mape_modelo = error_mape_modelo_mape,
                tranformacion = lambda,
                predicciones = predicciones_reales_mape)
    
    AIC = list(modelo = mejor_modelo_aic, error_mape_modelo = error_mape_modelo_aic,
               tranformacion = lambda,
               predicciones = predicciones_reales_aic)
    
    return(list(modelo_MAPE = MAPE,
                modelo_AIC = AIC))
  }
  
}
