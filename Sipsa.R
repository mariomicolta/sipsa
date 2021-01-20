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

# Global Variables (Params)

setParams <- function(productName = NA, frequencyTs = NA, startTs = NA){
  # Buscar como volver esto un diccionario clave valor
  # Asignamos los valores
  
  # Funcion para establecer los parametros globales
  
  # --- PARAMETROS
  # productName: nombre del producto
  # frequencyTs: periodicidad de la serie de tiempo
  # startTs: fecha de inicio de la serie de tiempo
  product <- productName
  frequency <- frequencyTs
  start <- startTs
  
  return(list(product = product, frequency = frequency, start = start))
}

#Fuction
globalParams <- setParams(productName = 'Curuba', frequencyTs = 52, startTs = c(2021, 1, 1))

init <- function(globalParams){
  # Validamos que hayan llegado todos los parametros obligatorios
  # Funcion para validar que hayan llegado todos los parametros
  
  # --- PARAMETROS
  #globalParams: vector que contiene los parametros globales obligatorios
  #1. product, 2. frequency, 3. start
  if(is.na(globalParams[1]) || is.na(globalParams[2]) || is.na(globalParams[3])){
    print('PARAR LA EJECUCION')
    FALSE
  }else{
    print('Everything is ok! Has fun!')
  }
}
#Function
init(globalParams = globalParams)


# Load Data
loadData <- function(){
  # Capturar con soap
  # Funcion para cargar los datos
  data <- read_csv2('datasemanal.csv')
  #ordenarlos
  data
}

#Fuction
data <- loadData();

# Filter Data
filterData <- function(productName){
  # Filtramos por nombre. Posteriormente cambiarlo por id, dado que el nombre puede cambiar levemente
  data <- data %>% filter(artiNombre == productName)
  data
}

#Function
data <- filterData(globalParams[1])


# Prepare Data

prepareData <- function(){
  data <- data %>% mutate(
    artiId = as.character(artiId),
    artiNombre = as.character(artiNombre),
    fechaIni = lubridate::as_datetime(fechaIni),
    maximoKg = as.double(maximoKg),
    minimoKg = as.double(minimoKg),
    promedioKg = as.double(promedioKg)
  )
  data
}

data <- prepareData()

createTimeSeries <- function(target, globalParamsP){
  #target: variable objeto de estudio
  data.ts <- ts(data[,target], start = c(2020,2,1), frequency = as.integer(globalParamsP[2]))
  data.ts
}

#Function
data.ts <- createTimeSeries(target = 'promedioKg', globalParamsP = globalParams)

periodicity(data.ts)

# split

percentage <- .3

h <- round(length(data.ts) * percentage)
th <- length(data.ts) - h

train <- subset(x=data.ts, start = 1, end = th)
#periodicity(train)

test <- subset(x=data.ts, start = th + 1, end = th + h)
#periodicity(test)






