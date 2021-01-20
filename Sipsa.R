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



# Load libraries

library(tidyverse)
library(lubridate)
library(xts)
library(forecast)

# Global Variables (Params)

setParams <- function(productName = NA, frequencyTs = NA, startTs = NA){
  #https://stackoverflow.com/questions/22188660/r-time-series-modeling-on-weekly-data-using-ts-object
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



#Start
globalParams <- setParams(productName = 'Queso costeño', frequencyTs = 365.25/7, startTs = decimal_date(ymd("2020-02-01")))

if(init(globalParams = globalParams)){
  data <- loadData()
  data <- filterData(globalParams$product)
  data <- prepareData()
}else{
  print('Some parameter is missing')
}


#REVISAR ESTO. DA PROBLEMA 
#Da problema al mirar la periodicidad
#Error in try.xts(x, error = "'x' needs to be timeBased or xtsible") : 
#'x' needs to be timeBased or xtsible
data.ts <- createTimeSeries(target = 'promedioKg', globalParamsP = globalParams)

data.xts <- xts(data$promedioKg, order.by = data$fechaIni)

periodicity(data.ts)
plot(data.xts)
decompose(data.ts)

# split

percentage <- .3

h <- round(length(data.ts) * percentage)
th <- length(data.ts) - h

#train <- subset(x=data.ts, start = 1, end = th)
#periodicity(train)
#test <- subset(x=data.ts, start = th + 1, end = th + h)
#periodicity(test)

train <- data.xts[1:th,]
periodicity(train)

test <- data.xts[(th+1):(th+h),]
periodicity(test)


mediaMovil <- function(window, orderP, hP){
  fcst <- tail(forecast(ma(window, order = orderP), h = hP)$mean, 1)
  return(fcst)
}

aa<-mediaMovil(train, 3,2)

aa







res <- auto.arima(train, max.p = 30, max.q = 30, ic = 'aic', stepwise = FALSE)





arima.aic <- Arima(train, order = c(0,0,4))











auto.arima(train, max.p = 30, max.q = 30, ic = 'bic', stepwise = FALSE)

auto.arima(train, max.p = 30, max.q = 30, ic = 'aicc', stepwise = FALSE)






