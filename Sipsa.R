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

# Load Data
loadData <- function(pathFile = 'datamensual.csv'){
  # Capturar con soap
  # Funcion para cargar los datos
  data <- read_csv2(pathFile, locale = locale(encoding = "Latin1"))
  data
}


# Filter Data
filterData <- function(productName){
  # Filtramos por nombre.
  data <- data %>% filter(Producto == productName)
  data
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
  data
}

createTimeSeries <- function(target, start, frequency){
  #target: variable objeto de estudio
  data.ts <- ts(data[,target], start = c(year(start), month(start)), frequency = frequency)
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
  
  return(list(
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
    'holtWinterMultiplicativo' = holtWinterMultiplicativo
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

compareModels <- function(metrics){
  #PARAMETRIZAR LA ELECCION DE LA METRICA
  #Funcion para comparar los modelos previamente evaluados. Esta retorna el mejor modelo y su indice en el arr
  
  #metrics: dataframe que contiene todas las metricas de evaluacion de cada modelo
  
  #indexMetrics = list("ME" = 1, "RMSE" = 2, "MAE" = 3, "MPE" = 4, "MAPE" = 5)
  indexMinValue <- which.min(metrics$RMSE)
  bestModel <- metrics[indexMinValue,]
  row.names(resultMetrics[indexMinValue,])
  
  return(list(bestModel = bestModel, indexMinValue = indexMinValue))
}


formatDate <- function(fecha){
  f <- lubridate::dmy(paste0("01-", fecha))
  return(f)
}

# 



#Start
#globalParams <- setParams(productName = 'Queso costeño', 
#                          frequencyTs = 365.25/7, 
#                          startTs = decimal_date(ymd("2020-02-01")))


start <- function(){
  
  # meter todo aki
}

data <- loadData(pathFile = 'datamensual.csv')

productName = 'Queso costeño'
#frequencyTs = 365.25/7 #Semanal
frequencyTs = 12


data <- filterData(productName = productName)
#startTs = decimal_date(ymd("2020-02-01"))
startTs <- formatDate(min(data$Fecha))
data <- prepareData()
  
data.ts <- createTimeSeries('precio', start = startTs, frequencyTs)

#data.ts <- xts(data$promedioKg, order.by = data$fechaIni)

splitDataResult <- splitData(data = data.ts, percentage = .3)

train <- splitDataResult$train
test <- splitDataResult$test
h <- splitDataResult$h
th <- splitDataResult$th # se puede sustituir con length(train)

ptm <- proc.time()
results <- trainingModelsMoving(h, th, data.ts)
proc.time () - ptm

  
  resultMetrics <- evaluateModels(results, test)
  
  bestModel <- compareModels(results)
  
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
