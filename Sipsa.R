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


trainingModelsMoving <- function(h, th, data, train){
  ptm <- proc.time()
  #Funcion para entrenar los modelos usando una ventana movil
  
  #h: tamaño del horizonte (cuantos periodos usamos como test)
  #th: periodos en los datos de train
  #data: todos los datos
  
  
  ordersArimaAIC <- trainArima(ic = 'aic', data = train)#organizar este train luego
  ordersArimaBIC <- trainArima(ic = 'bic', data = train)
  ordersArimaAICC <- trainArima(ic = 'aicc', data = train)
  
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
    'mediaMovil3' = mediaMovil3,
    'mediaMovil4' = mediaMovil4,
    'mediaMovil5' = mediaMovil5,
    'mediaMovil6' = mediaMovil6,
    'mediaMovil7' = mediaMovil7,
    'mediaMovil8' = mediaMovil8,
    'mediaMovil9' = mediaMovil9,
    'suavizacionExponencialSimple' = suavizacionExponencialSimple,
    'suavizacionExponencialLineal' = suavizacionExponencialLineal,
    'holtWinterAditivo' = holtWinterAditivo,
    'holtWinterMultiplicativo' = holtWinterMultiplicativo)
    
  if(ordersArimaAIC$flag){
    lista <- list.append(lista, 'arimaAIC' = arimaAIC)
  }
  
  if(ordersArimaBIC$flag){
    lista <- list.append(lista, 'arimaBIC' = arimaBIC)
  }
  
  if(ordersArimaAICC$flag){
    lista <- list.append(lista, 'arimaAICC' = arimaAICC)
  }
  
  
  return(lista)
}


evaluateModels <- function(models, test){
  # Funcion para evaluar el rendimiento de los modelos frente a los datos de test
  
  #models: lista de pronosticos de los modelos evaluados anteriormente
  #test: datos de test
  
  metricsLabels <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  metrics <- rbind(accuracy(models$arimaAIC, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil3, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil4, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil5, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil6, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil7, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil8, test)["Test set", metricsLabels],
             accuracy(models$mediaMovil9, test)["Test set", metricsLabels],
             accuracy(models$suavizacionExponencialSimple, test)["Test set", metricsLabels],
             accuracy(models$suavizacionExponencialLineal, test)["Test set", metricsLabels],
             accuracy(models$holtWinterAditivo, test)["Test set", metricsLabels],
             accuracy(models$holtWinterMultiplicativo, test)["Test set", metricsLabels])
  
  metrics <- data.frame(metrics)
  row.names(metrics) <- c("ARIMA-AIC","M3", "M4", "M5", "M6", "M7", "M8", "M9", "SES", "HOLT", "HOLT-AD", "HOLT-MT")
  return(metrics)
}

evaluateModels2 <- function(models, test){
  # Funcion para evaluar el rendimiento de los modelos frente a los datos de test
  
  #models: lista de pronosticos de los modelos evaluados anteriormente
  #test: datos de test
  
  metricsLabels <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  for (model in models){
    print(accuracy(model, test)["Test set", metricsLabels])
    #print(model)
  }
  
  
  metrics <- rbind(accuracy(models$arimaAIC, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil3, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil4, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil5, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil6, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil7, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil8, test)["Test set", metricsLabels],
                   accuracy(models$mediaMovil9, test)["Test set", metricsLabels],
                   accuracy(models$suavizacionExponencialSimple, test)["Test set", metricsLabels],
                   accuracy(models$suavizacionExponencialLineal, test)["Test set", metricsLabels],
                   accuracy(models$holtWinterAditivo, test)["Test set", metricsLabels],
                   accuracy(models$holtWinterMultiplicativo, test)["Test set", metricsLabels])
  
  metrics <- data.frame(metrics)
  row.names(metrics) <- c("ARIMA-AIC","M3", "M4", "M5", "M6", "M7", "M8", "M9", "SES", "HOLT", "HOLT-AD", "HOLT-MT")
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

models

models$mediaMovil3





for (model in models){
  print(row.names(model))
  #print(accuracy(model, test)["Test set", metricsLabels])
  #print(model)
}

#


metrics <- evaluateModels(models, test)

bestModel <- compareModels(metrics)

bestModel
# Pasar este best model a una función que con varios if o casewhen
#trainModelA()
#Al final el restulado lo ploteamos y devolvemos toda la data de inter[es]







#Automatizacion arima


ar <- trainArima(ic = 'aic', data = train)
ar

arima <- auto.arima(train, max.p = 30, max.q = 30, ic = 'aic', stepwise = FALSE)
#(0,1,3)
arima


orders <- data.frame(as.list(arimaorder(arimaAic)))
orders

model <- Arima(train, order = c(orders$p, orders$d, orders$q))
model

#no autocorrelación
#prueba de rachas
runstest <- runs.test(factor(residuals(model) > 0))
runstest$p.value

#box pierce
boxPierceTable <- tableBoxPierceOrLjungBox(residuals(model))

minBoxPierce <- min(boxPierceTable['p-valor'])
maxBoxPierce <- max(boxPierceTable['p-valor'])

#Homocedasticidad

ljungBoxTable <- tableBoxPierceOrLjungBox(residuals(model)^2, type = "Ljung-Box")

minLjungBox <- min(ljungBoxTable['p-valor'])
maxLjungBox <- max(ljungBoxTable['p-valor'])


#Normalidad

testNormJarqueBera <- jarque.bera.test(residuals(model))$p.value

testNormShapiro <- shapiro.test(residuals(model))$p.value






  
  #percentage <- .3
  #h <- round(length(data.ts) * percentage)
  #th <- length(data.ts) - h
  


#REVISAR ESTO. DA PROBLEMA 
#Da problema al mirar la periodicidad
#Error in try.xts(x, error = "'x' needs to be timeBased or xtsible") : 
#'x' needs to be timeBased or xtsible
#data.ts <- createTimeSeries(target = 'promedioKg', globalParamsP = globalParams)
data.ts <- xts(data$promedioKg, order.by = data$fechaIni)

#periodicity(data.ts)
#plot(data.xts)
#decompose(data.ts)

# split

percentage <- .3

h <- round(length(data.ts) * percentage)
th <- length(data.ts) - h

#train <- subset(x=data.ts, start = 1, end = th)
#periodicity(train)
#test <- subset(x=data.ts, start = th + 1, end = th + h)
#periodicity(test)



train <- data.ts[1:th,]
periodicity(train)

test <- data.ts[(th+1):(th+h),]
periodicity(test)









#Entrenar modelos con ventana movil


results <- trainingModelsMoving(h, th, data.ts)
results




resultMetrics <- evaluateModels(results, test)

which.min(resultMetrics$RMSE)
row.names(resultMetrics[1,])


compareModels <- fucntion(metrics){
  #indexMetrics = list("ME" = 1, "RMSE" = 2, "MAE" = 3, "MPE" = 4, "MAPE" = 5)
  indexMinValue <- which.min(metrics$RMSE)
  bestModel <- metrics[indexMinValue,]
  row.names(resultMetrics[indexMinValue,])
  
  return(list(bestModel = bestModel, indexMinValue = indexMinValue))
}

min(results[1:3])



compareModels(results)

results$suavizacionExponencialSimple[indexMetrics$RMSE]

results[1]





res <- auto.arima(train, max.p = 30, max.q = 30, ic = 'aic', stepwise = FALSE)





arima.aic <- Arima(train, order = c(0,0,4))











auto.arima(train, max.p = 30, max.q = 30, ic = 'bic', stepwise = FALSE)

auto.arima(train, max.p = 30, max.q = 30, ic = 'aicc', stepwise = FALSE)





mediaMovil <- function(window, orderP, hP){
  fcst <- tail(forecast(ma(window, order = orderP), h = hP)$mean, 1)
  return(fcst)
}

aa<-mediaMovil(train, 3,2)

aa



paste0("01-", data$Fecha)

parse_date("31 DICIEMBRE 2011","%d %B %Y",locale=locale("es"))






# Error format fecha
#https://stackoverflow.com/questions/53380650/b-y-date-conversion-gives-na


# EXPERIMENT

#https://robjhyndman.com/hyndsight/seasonal-periods/

prueba <- msts(data.ts, seasonal.periods=c(365,25))
               
decompose(prueba)
