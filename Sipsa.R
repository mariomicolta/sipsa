# Date: 19 enero de 2021
# Authors: Jose Luis Gamarra Palacios - John Mario Micolta Garzon

# Attrs
# * artiId: Id del articulo
# * artiNombre: Nombre del articulo
# * fechaIni: Fecha de captura del dato
# * fuenId: Unknown
# * fuenNombre: Unknown
# * futiId: Unknown
# * maximoKg: Precio maximo por kg
# * minimoKg: Precio minimo por kg
# * promedioKg: Precio promedio por kg


###########################################


# Load libraries

#Para el formato de fechas en español Jan -> Ene
Sys.setlocale(locale = "Spanish")
set.seed(1234)


library(tidyverse)
library(lubridate)
library(xts)
library(forecast)
library(tseries)
library(rlist)
library(ForecastComb)



# Load Data
loadData <- function(pathFile = 'datamensual.csv'){
  # Capturar con soap
  # Funcion para cargar los datos
  data <- read_csv2(pathFile, locale = locale(encoding = "Latin1"))
  return(data)
}


# Filter Data
filterData <- function(productName){
  # Filtramos por nombre.
  data <- data %>% filter(Producto == productName)
  return(data)
}

formatDate <- function(fecha){
  f <- lubridate::dmy(paste0("01-", fecha))
  return(f)
}


# Prepare Data
prepareData <- function(){
  #CAMBIAR
  data <- data %>% mutate(
    Fecha = formatDate(Fecha),
    Grupo = as.character(Grupo),
    Producto = as.character(Producto),
    Mercado = as.character(Mercado),
    precio = as.double(`Precio  por kilogramo`)
  )
  
  data <- data[,-c(5)]
  
  #Sort 
  data <- data %>% arrange(Fecha)
  return(data)
}

createTimeSeries <- function(target, start, frequency){
  #target: variable objeto de estudio
  data.ts <- ts(data[,target], start = c(year(start), month(start)), frequency = frequency)
  return(data.ts)
}


splitData <- function(data, percentage = .3){
  #Funcion para cortar los datos
  
  #data: datos en formato ts o xts 
  #percentage: porcentaje de datos a utilizar como test
  
  h <- round(length(data) * percentage)
  th <- length(data.ts) - h
  
  #train <- data.ts[1:th,]
  #test <- data.ts[(th+1):(th+h),]
  train <- subset(data.ts, start = 1, end = th)
  test <- subset(data.ts, start = th + 1, end = th + h)
  
  return(list(train = train, test = test, h = h, th = th))
}


trainArima <- function(ic = 'aic', data){
  #ic = ('aic', 'bic', 'aicc')
  significance_level <- 0.05
  
  arima <- auto.arima(y = data, max.p = 30, max.q = 30, ic = ic, stepwise = FALSE)
  orders <- data.frame(as.list(arimaorder(arima)))
  
  model <- Arima(train, order = c(orders$p, orders$d, orders$q))
  
  # *** No autocorrelación
  
  # Prueba de rachas
  runstest <- runs.test(factor(residuals(model) > 0))
  
  if(runstest$p.value < significance_level){
    stop('Se rechaza la no autocorrelación')
  }
  
  # Box pierce
  boxPierceTable <- tableBoxPierceOrLjungBox(residuals(model))
  minBoxPierce <- min(boxPierceTable['p-valor'])
  maxBoxPierce <- max(boxPierceTable['p-valor'])
  
  if(minBoxPierce < significance_level){
    stop('Se rechaza la no autocorrelación')
  }
  
  # *** Homocedasticidad
  
  #Ljung-Box
  ljungBoxTable <- tableBoxPierceOrLjungBox(residuals(model)^2, type = "Ljung-Box")
  minLjungBox <- min(ljungBoxTable['p-valor'])
  maxLjungBox <- max(ljungBoxTable['p-valor'])
  
  if(minLjungBox < significance_level){
    stop('Se rechaza la homocedasticidad')
  }
  
  # Normalidad
  
  testNormJarqueBera <- jarque.bera.test(residuals(model))
  testNormShapiro <- shapiro.test(residuals(model))
  
  if(testNormJarqueBera$p.value < significance_level | testNormShapiro$p.value < significance_level){
    stop('Se rechaza la normalidad de los residuos')
  }
  
  return(list(flag = TRUE, p = orders$p, d = orders$d, q = orders$q))

}


# Da error
trainPolynomial <- function(data, degree = 2){

  significance_level <- 0.05
  
  #ERROR
  # No reconoce los parámetros dentro de la función poly. Es decir, al parecer, se debe escribir directamente
  #el grado del polinomio y no poner degree
  model <- tslm(data ~ poly(trend, degree) + season, data = data)
  
  
  # *** No autocorrelación
  
  # Prueba de rachas
  runstest <- runs.test(factor(residuals(model) > 0))
  
  if(runstest$p.value < significance_level){
    stop('Se rechaza la no autocorrelación')
  }
  
  # Box pierce
  boxPierceTable <- tableBoxPierceOrLjungBox(residuals(model))
  minBoxPierce <- min(boxPierceTable['p-valor'])
  maxBoxPierce <- max(boxPierceTable['p-valor'])
  
  if(minBoxPierce < significance_level){
    stop('Se rechaza la no autocorrelación')
  }
  
  # *** Homocedasticidad
  
  #Ljung-Box
  ljungBoxTable <- tableBoxPierceOrLjungBox(residuals(model)^2, type = "Ljung-Box")
  minLjungBox <- min(ljungBoxTable['p-valor'])
  maxLjungBox <- max(ljungBoxTable['p-valor'])
  
  if(minLjungBox < significance_level){
    stop('Se rechaza la homocedasticidad')
  }
  
  # Normalidad
  
  testNormJarqueBera <- jarque.bera.test(residuals(model))
  testNormShapiro <- shapiro.test(residuals(model))
  
  if(testNormJarqueBera$p.value < significance_level | testNormShapiro$p.value < significance_level){
    stop('Se rechaza la normalidad de los residuos')
  }
  
  return(list(flag = TRUE))
  
}


trainingModelsMoving <- function(h, th, data, train){
  ptm <- proc.time()
  #Funcion para entrenar los modelos usando una ventana movil
  
  #h: tamaño del horizonte (cuantos periodos usamos como test)
  #th: periodos en los datos de train
  #data: todos los datos
  
  
  ordersArimaAIC <- trainArima(ic = 'aic', data = train)#organizar este train luego
  ordersArimaBIC <- trainArima(ic = 'bic', data = train)
  ordersArimaAICC <- trainArima(ic = 'aicc', data = train)
  
  polynomial2 <- trainPolynomial(degree = 2, data = train)
  
  arimaAIC <- NULL
  arimaBIC <- NULL
  arimaAICC <- NULL
  
  mediaMovil3 <- NULL
  mediaMovil4 <- NULL
  mediaMovil5 <- NULL
  mediaMovil6 <- NULL
  mediaMovil7 <- NULL
  mediaMovil8 <- NULL
  mediaMovil9 <- NULL

  suavizacionExponencialSimple <- NULL
  suavizacionExponencialLineal <- NULL
  holtWinterAditivo <- NULL
  holtWinterMultiplicativo <- NULL
  
  for(i in 1:h){
    ventana <- subset(data, start = 1, end = th - 1 + i)
    
    if(ordersArimaAIC$flag){
      arimaAIC[i] <- forecast(Arima(ventana, order = c(ordersArimaAIC$p, ordersArimaAIC$d, ordersArimaAIC$q)), h = 1)$mean
    }
    
    if(ordersArimaBIC$flag){
      arimaBIC[i] <- forecast(Arima(ventana, order = c(ordersArimaBIC$p, ordersArimaBIC$d, ordersArimaBIC$q)), h = 1)$mean
    }
    
    if(ordersArimaAICC$flag){
      arimaAICC[i] <- forecast(Arima(ventana, order = c(ordersArimaAICC$p, ordersArimaAICC$d, ordersArimaAICC$q)), h = 1)$mean
    }
    
    
    mediaMovil3[i] <- tail(forecast(ma(ventana , order = 3), h = 2)$mean, 1)
    mediaMovil4[i] <- tail(forecast(ma(ventana , order = 4), h = 3)$mean, 1)
    mediaMovil5[i] <- tail(forecast(ma(ventana , order = 5), h = 3)$mean, 1)
    mediaMovil6[i] <- tail(forecast(ma(ventana , order = 6), h = 4)$mean, 1)
    mediaMovil7[i] <- tail(forecast(ma(ventana , order = 7), h = 4)$mean, 1)
    mediaMovil8[i] <- tail(forecast(ma(ventana , order = 8), h = 5)$mean, 1)
    mediaMovil9[i] <- tail(forecast(ma(ventana , order = 9), h = 5)$mean, 1)
    
    suavizacionExponencialSimple[i] <- ses(ventana, h = 1)$mean
    suavizacionExponencialLineal[i] <- holt(ventana, h = 1)$mean
    
    holtWinterAditivo[i] <- hw(ventana, h = 1, seasonal = "additive")$mean
    holtWinterMultiplicativo[i] <- hw(ventana, h = 1, seasonal = "multiplicative")$mean
  }

  proc.time () - ptm
  
  # Estos modelos no requieren validacion, por lo tanto, entran sin compromiso
  lista <- list(
    'mediaMovil3' = list('name' = 'mediaMovil3', 'forecast' = mediaMovil3),
    'mediaMovil4' = list('name' = 'mediaMovil4', 'forecast' = mediaMovil4),
    'mediaMovil5' = list('name' = 'mediaMovil5', 'forecast' = mediaMovil5),
    'mediaMovil6' = list('name' = 'mediaMovil6', 'forecast' = mediaMovil6),
    'mediaMovil7' = list('name' = 'mediaMovil7', 'forecast' = mediaMovil7),
    'mediaMovil8' = list('name' = 'mediaMovil8', 'forecast' = mediaMovil8),
    'mediaMovil9' = list('name' = 'mediaMovil9', 'forecast' = mediaMovil9),
    'suavizacionExponencialSimple' = list('name' = 'suavizacionExponencialSimple', 'forecast' = suavizacionExponencialSimple),
    'suavizacionExponencialLineal' = list('name' = 'suavizacionExponencialLineal', 'forecast' = suavizacionExponencialLineal),
    'holtWinterAditivo' = list('name' = 'holtWinterAditivo', 'forecast' = holtWinterAditivo),
    'holtWinterMultiplicativo' = list('name' = 'holtWinterMultiplicativo', 'forecast' = holtWinterMultiplicativo))
    
  if(ordersArimaAIC$flag){
    lista <- list.append(lista, 'arimaAIC' = list('name' = 'arimaAIC', 'forecast' = arimaAIC))
  }
  
  if(ordersArimaBIC$flag){
    lista <- list.append(lista, 'arimaBIC' = list('name' = 'arimaBIC', 'forecast' = arimaBIC))
  }
  
  if(ordersArimaAICC$flag){
    lista <- list.append(lista, 'arimaAICC' = list('name' = 'arimaAICC', 'forecast' = arimaAICC))
  }
  
  
  return(lista)
}


evaluateModels <- function(models, test){
  # Funcion para evaluar el rendimiento de los modelos frente a los datos de test
  
  #models: lista de pronosticos de los modelos evaluados anteriormente
  #test: datos de test
  
  metricsLabels <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  metrics <- NULL
  names <- NULL
  
  for (model in models){
    acc <- accuracy(model$forecast, test)["Test set", metricsLabels]
    metrics <- rbind(metrics, acc)
    names <- rbind(names, model$name)
  }
  
  metrics <- data.frame(metrics)
  row.names(metrics) <- names
  metrics <- cbind(metrics, names)
  
  return(metrics)
}




compareModels <- function(metrics){
  #PARAMETRIZAR LA ELECCION DE LA METRICA
  #Funcion para comparar los modelos previamente evaluados. Esta retorna el mejor modelo y su indice en el arr
  
  #metrics: dataframe que contiene todas las metricas de evaluacion de cada modelo
  
  #indexMetrics = list("ME" = 1, "RMSE" = 2, "MAE" = 3, "MPE" = 4, "MAPE" = 5)
  indexMinValue <- which.min(metrics$RMSE)
  bestModel <- metrics[indexMinValue,]
  #row.names(metrics[indexMinValue,])
  
  #return(list(bestModel = bestModel, indexMinValue = indexMinValue))
  return(bestModel)
}
metrics



tableBoxPierceOrLjungBox <- function(residuo, maxLag = 20, type = "Box-Pierce"){
  # se crean objetos para guardar los resultados
  estadistico <- matrix(0, maxLag, 1)
  pvalue <- matrix(0, maxLag, 1)
  # se calcula la prueba para los diferentes rezagos
  for (i in 1:maxLag) {
    test <- Box.test(residuo, lag = i, type = type)
    estadistico[i] <- test$statistic
    pvalue[i] <- round(test$p.value, 5)
  }
  labels <- c("Rezagos", type, "p-valor")
  tableBody <- cbind(matrix(1:maxLag, maxLag, 1), estadistico, pvalue)
  #TABLABP <- data.frame(tableBody)
  table <- data.frame(tableBody)
  names(table) <- labels
  return(table)
}    





# 



#Start
#globalParams <- setParams(productName = 'Queso costeño', 
#                          frequencyTs = 365.25/7, 
#                          startTs = decimal_date(ymd("2020-02-01")))


start <- function(){
  set.seed(1234)
  ptm <- proc.time()
  
  # meter todo aki
  
  proc.time() - ptm
}



data <- loadData(pathFile = 'datamensual.csv')

productName = 'Queso costeño'
#frequencyTs = 365.25/7 #Semanal
frequencyTs = 12


data <- filterData(productName = productName)
#startTs = decimal_date(ymd("2020-02-01"))
startTs <- formatDate(min(data$Fecha))

startTs

data <- prepareData()
  
data.ts <- createTimeSeries('precio', start = startTs, frequency = frequencyTs)

splitDataResult <- splitData(data = data.ts, percentage = .3)

train <- splitDataResult$train
test <- splitDataResult$test
h <- splitDataResult$h
th <- splitDataResult$th # se puede sustituir con length(train)

models <- trainingModelsMoving(h, th, data.ts, train)


metrics <- evaluateModels(models, test)


bestModel <- compareModels(metrics)

bestModel
# Pasar este best model a una función que con varios if o casewhen
#trainModelA()
#Al final el restulado lo ploteamos y devolvemos toda la data de inter[es]





# Ensambles
trainEnsambles <- function(data, train){
  ptm <- proc.time()
  
  
  arimaAIC <- NULL
  arimaBIC <- NULL
  arimaAICC <- NULL
  
  mediaMovil3 <- NULL
  mediaMovil4 <- NULL
  mediaMovil5 <- NULL
  mediaMovil6 <- NULL
  mediaMovil7 <- NULL
  mediaMovil8 <- NULL
  mediaMovil9 <- NULL
  
  suavizacionExponencialSimple <- NULL
  suavizacionExponencialLineal <- NULL
  holtWinterAditivo <- NULL
  holtWinterMultiplicativo <- NULL
  
  
  mediaMovil3 <- ma(data, order = 3)
  mediaMovil3[length(mediaMovil3)] <- forecast(ma(train, order = 3), h=2)$mean
  
  suavizacionExponencialSimple <- ses(data)$fitted
  suavizacionExponencialLineal <- holt(data)$fitted
  holtWinterAditivo <- hw(data, seasonal = 'additive')$fitted
  holtWinterMultiplicativo <- hw(data, seasonal = 'multiplicative')$fitted
  
  arima <- Arima(data.ts, order = c(2,2,3))$fitted
  
  inSample <- cbind(mediaMovil3, 
                    suavizacionExponencialSimple, 
                    suavizacionExponencialLineal, 
                    holtWinterAditivo, 
                    holtWinterMultiplicativo,
                    arima)
  inSample
  
  dataComb <- foreccomb(observed_vector = data, prediction_matrix = inSample, newobs = data,
                        newpreds = inSample)

  
  # *** MÉTODOS SIMPLES
  comb_SA <- rolling_combine(dataComb, comb_method ="comb_SA")
  comb_MED <- rolling_combine(dataComb, comb_method ="comb_MED")
  comb_TA <- rolling_combine(dataComb, comb_method ="comb_TA")
  comb_WA <- rolling_combine(dataComb, comb_method ="comb_WA")

  # MÉTODOS BASADOS EN REGRESIÓN
  comb_OLS <- rolling_combine(dataComb, comb_method ="comb_OLS")
  comb_LAD <- rolling_combine(dataComb, comb_method ="comb_LAD")
  comb_CLS <- rolling_combine(dataComb, comb_method ="comb_CLS")
  
  proc.time () - ptm
  
  
  # Test
  
  metricsLabels <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  names <- c('comb_SA', 'comb_MED', 'comb_TA', 'comb_WA', 'comb_OLS', 'comb_LAD', 'comb_CLS')
  metrics <- rbind(comb_SA$Accuracy_Test["Test set", metricsLabels],
                   comb_MED$Accuracy_Test["Test set", metricsLabels],
                   comb_TA$Accuracy_Test["Test set", metricsLabels],
                   comb_WA$Accuracy_Test["Test set", metricsLabels],
                   comb_OLS$Accuracy_Test["Test set", metricsLabels],
                   comb_LAD$Accuracy_Test["Test set", metricsLabels],
                   comb_CLS$Accuracy_Test["Test set", metricsLabels])
  
  
  metrics <- data.frame(metrics)
  row.names(metrics) <- names
  metrics <- cbind(metrics, names)
  
  return(metrics)
  
}

dataComb <- trainandTestEnsambles(data.ts, train)
dataComb


# *** MÉTODOS SIMPLES
comb_SA <- rolling_combine(dataComb, comb_method ="comb_SA")
comb_MED <- rolling_combine(dataComb, comb_method ="comb_MED")
comb_TA <- rolling_combine(dataComb, comb_method ="comb_TA")
comb_WA <- rolling_combine(dataComb, comb_method ="comb_WA")
#comb_BG <- rolling_combine(dataComb, comb_method ="comb_BG") #longitud de la serie-temporal/vector incompatible
#comb_NG <- rolling_combine(dataComb, comb_method ="comb_NG") #longitud de la serie-temporal/vector incompatible
#comb_InvW <- rolling_combine(dataComb, comb_method ="comb_InvW") #longitud de la serie-temporal/vector incompatible

# MÉTODOS BASADOS EN REGRESIÓN
comb_OLS <- rolling_combine(dataComb, comb_method ="comb_OLS")
comb_LAD <- rolling_combine(dataComb, comb_method ="comb_LAD")
comb_CLS <- rolling_combine(dataComb, comb_method ="comb_CLS")
#comb_CSR <- rolling_combine(dataComb, comb_method ="comb_CSR") #Error: Rolling Forecast Combination method is not available for Complete Subset Regression

# MÉTODOS BASADOS EN VECTORES PROPIOS
#comb_EIG1 <- rolling_combine(dataComb, comb_method ="comb_EIG1") #Error in `-.default`(observed_vector, prediction_matrix) : longitud de la serie-temporal/vector incompatible
#comb_EIG2 <- rolling_combine(dataComb, comb_method ="comb_EIG2")
#comb_EIG3 <- rolling_combine(dataComb, comb_method ="comb_EIG3")
#comb_EIG4 <- rolling_combine(dataComb, comb_method ="comb_EIG4")

comb_SA$Accuracy_Test["Test set", ]
comb_MED$Accuracy_Test["Test set", "RMSE"]
comb_TA$Accuracy_Test["Test set", "RMSE"]
comb_WA$Accuracy_Test["Test set", "RMSE"]

comb_OLS$Accuracy_Test["Test set", "RMSE"]
comb_LAD$Accuracy_Test["Test set", "RMSE"]
comb_CLS$Accuracy_Test["Test set", "RMSE"]



