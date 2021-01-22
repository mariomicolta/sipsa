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


###########################################     H DEBE SER UN PARAMS GLOBAL


# Load libraries

library(tidyverse)
library(lubridate)
library(xts)
library(forecast)

# Global Variables (Params)


setParams <- function(productName = NA, frequencyTs = NA, startTs = NA){
  # Funcion para establecer los parametros globales
  
  # productName: nombre del producto
  # frequencyTs: periodicidad de la serie de tiempo
  # startTs: fecha de inicio de la serie de tiempo
  product <- productName
  frequency <- frequencyTs
  start <- startTs
  
  return(list(product = product, frequency = frequency, start = start))
}



init <- function(globalParams){
  # Funcion para validar que hayan llegado todos los parametros
  
  #globalParams: vector que contiene los parametros globales obligatorios
  #1. product, 2. frequency, 3. start
  
  flag = FALSE
  if(is.na(globalParams$product) || is.na(globalParams$frequency) || is.na(globalParams$start)){
    print('PARAR LA EJECUCION')
    flag = FALSE
  }else{
    print('Everything is ok! Has fun!')
    flag = TRUE
  }
  return(flag)
}


# Load Data
loadData <- function(){
  # Capturar con soap
  # Funcion para cargar los datos
  data <- read_csv2('datasemanal.csv', locale = locale(encoding = "Latin1"))
  data
}


# Filter Data
filterData <- function(productName){
  # Filtramos por nombre.
  data <- data %>% filter(artiNombre == productName)
  data
}


# Prepare Data

prepareData <- function(){
  data <- data %>% mutate(
    artiId = as.character(artiId),
    artiNombre = as.character(artiNombre),
    fechaIni = lubridate::as_date(fechaIni),
    maximoKg = as.double(maximoKg),
    minimoKg = as.double(minimoKg),
    promedioKg = as.double(promedioKg)
  )
  
  #Sort 
  data <- data %>% arrange(fechaIni)
  data
}

createTimeSeries <- function(target, globalParamsP){
  #target: variable objeto de estudio
  data.ts <- ts(data[,target], start = globalParamsP$start, frequency = globalParamsP$frequency)
  data.ts
}


splitData <- function(data, percentage = .3){
  #Funcion para cortar los datos
  
  #data: datos en formato ts o xts 
  #percentage: porcentaje de datos a utilizar como test
  
  h <- round(length(data) * percentage)
  th <- length(data.ts) - h
  
  train <- data.ts[1:th,]
  test <- data.ts[(th+1):(th+h),]
  return(list(train = train, test = test, h = h, th = th))
}

# 
trainingModelsMoving <- function(h, th, data){
  #Funcion para entrenar los modelos usando una ventana movil
  
  #h: tamaño del horizonte (cuantos periodos usamos como test)
  #th: periodos en los datos de train
  #data: todos los datos
  suavizacionExponencialSimple <- NULL
  suavizacionExponencialLineal <- NULL
  holtWinterAditivo <- NULL
  holtWinterMultiplicativo <- NULL
  
  for(i in 1:h){
    ventana <- subset(data, start = 1, end = th - 1 + i)
    
    suavizacionExponencialSimple[i] <- ses(ventana, h = 1)$mean
    suavizacionExponencialLineal[i] <- holt(ventana, h = 1)$mean
    #holtWinterAditivo[i] <- hw(ventana, h = 1, seasonal = "additive")$mean
    #holtWinterMultiplicativo[i] <- hw(ventana, h = 1, seasonal = "multiplicative")$mean
  }
  
  return(list(
    'suavizacionExponencialSimple' = suavizacionExponencialSimple,
    'suavizacionExponencialLineal' = suavizacionExponencialLineal
    #'holtWinterAditivo' = holtWinterAditivo,
    #'holtWinterMultiplicativo' = holtWinterMultiplicativo,
  ))
}


evaluateModels <- function(models, test){
  # Funcion para evaluar el rendimiento de los modelos frente a los datos de test
  
  #models: lista de pronosticos de los modelos evaluados anteriormente
  #test: datos de test
  
  metrics <- c("ME", "RMSE", "MAE", "MPE", "MAPE")
  
  r <- rbind(accuracy(models$suavizacionExponencialSimple, test)["Test set", metrics],
             accuracy(models$suavizacionExponencialLineal, test)["Test set", metrics])
  
  metricas <- data.frame(r)
  row.names(metricas) <- c("SES", "HOLT")
  return(metricas)
}

compareModels <- fucntion(metrics){
  #PARAMETRIZAR LA ELECCION DE LA METRICA
  #Funcion para comparar los modelos previamente evaluados. Esta retorna el mejor modelo y su indice en el arr
  
  #metrics: dataframe que contiene todas las metricas de evaluacion de cada modelo
  
  #indexMetrics = list("ME" = 1, "RMSE" = 2, "MAE" = 3, "MPE" = 4, "MAPE" = 5)
  indexMinValue <- which.min(metrics$RMSE)
  bestModel <- metrics[indexMinValue,]
  row.names(resultMetrics[indexMinValue,])
  
  return(list(bestModel = bestModel, indexMinValue = indexMinValue))
}




#Start
globalParams <- setParams(productName = 'Queso costeño', frequencyTs = 365.25/7, startTs = decimal_date(ymd("2020-02-01")))

if(init(globalParams = globalParams)){
  data <- loadData()
  data <- filterData(globalParams$product)
  data <- prepareData()
  
  
  # Mientras tanto
  #lo de abajo reemplazarlo con createdataframe una vez se descargue full data y evitar el error
  #data.ts <- xts(data$promedioKg, order.by = data$fechaIni)
  data.ts <- xts(data$promedioKg, order.by = data$fechaIni)
  splitDataResult <- splitData(data = data.ts, percentage = .3)
  train <- splitDataResult$train
  test <- splitDataResult$test
  h <- splitDataResult$h
  th <- splitDataResult$th
  
  results <- trainingModelsMoving(h, th, data.ts)
  
  resultMetrics <- evaluateModels(results, test)
  
  bestModelo <- compareModels(results)
  
  #percentage <- .3
  #h <- round(length(data.ts) * percentage)
  #th <- length(data.ts) - h
  
  
  
}else{
  print('Some parameter is missing')
  #api.response(200, 'data is missing')
  return(FALSE)
}

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




# EXPERIMENT

#https://robjhyndman.com/hyndsight/seasonal-periods/

prueba <- msts(data.ts, seasonal.periods=c(365,25))
               
decompose(prueba)
