

#------------------------------------------------------#
## Carga Precios de Oferta #############################
#------------------------------------------------------#


library(xlsx)    

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/PreciosOferta")

file.list <- list.files(recursive = T, pattern = '*xlsx') # Obtener la lista de archivo en el folder

for (i in 1:length(file.list)){                                           
  wb <- loadWorkbook(file.list[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(file.list[i], sheetIndex=j, colIndex= c(1:6),
                      sheetName=NULL, startRow=5, endRow=NULL,
                      as.data.frame=TRUE, header=F)   
    tmp <- tmp[1:(nrow(tmp)-2),]
    if (i==1&j==1) DatosInicialesPO<-tmp else DatosInicialesPO<-rbind(DatosInicialesPO,tmp)   #happend to previous
    
  }
}


for (i in 1:2){                                           
  wb <- loadWorkbook(file.list[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(file.list[i], sheetIndex=j, colIndex= c(1:6),
      sheetName=NULL, startRow=5, endRow=NULL,
      as.data.frame=TRUE, header=F)   
    tmp <- tmp[1:(nrow(tmp)-2),]
    if (i==1&j==1) DatosInicialesPO<-tmp else DatosInicialesPO<-rbind(DatosInicialesPO,tmp)   #happend to previous
    
  }
}


# Al tener otra extencion de archivos se genera un nuevo proceso. Más adelante tendremos
# los datos con un solo proceso

file.list2 <- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 16:length(file.list2)){                                           
  wb <- loadWorkbook(file.list2[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(file.list2[i], sheetIndex=j, colIndex= c(1:6),
      sheetName=NULL, startRow=5, endRow=NULL,
      as.data.frame=TRUE, header=F)   
    tmp <- tmp[1:(nrow(tmp)-2),]
    if (i==16 & j==1) DatosInicialesPO2<-tmp else DatosInicialesPO2<-rbind(DatosInicialesPO2,tmp)   #happend to previous
    
  }
}

# Se enlanzan los dos dataset en uno solo por filas

PreciosOferta <- rbind(DatosInicialesPO,DatosInicialesPO2)


# Nombramos las colomnas 

NombresColumnas <- c("Fecha", "Recurso", "CodigoAgente", "PrecioOfertaIdeal$/kwh",
  "PrecioOfertaDespacho$/kwh", "PrecioOfertaDeclarado$/kwh")

colnames(PreciosOferta) <- NombresColumnas

 
###

PreciosOferta$Fecha <- as.Date(PreciosOferta$Fecha ,  format = "%Y-%m-%d")


PreciosOferta$dia <- c("Domingo", "Lunes", "Martes", "Miercoles", "Jueves", 
  "Viernes", "Sabado")[as.POSIXlt(PreciosOferta$Fecha)$wday+1]

# Vector con el nombre de lso días de la semana

DiasSemana <- c("Lunes", "Martes", "Miercoles", "Jueves", "Viernes" )

FinesSemana <-   c("Domingo", "Sabado")


PreciosOferta$TipoDia <- ifelse(PreciosOferta$dia %in% c("Domingo", "Sabado"),
  'FindeSemana', 'DiadeSemana')


# creamso un objeto de R para almacenar dichos datos


save(PreciosOferta, file = "PreciosOferta.RData")


write.csv(PreciosOferta,row.names = F, file = "PreciosOferta.csv")


#------------------------------------------------------#
## Carga  Precios Mensuales ############################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Precios")

filelistPrecios <- list.files(recursive = T, pattern = '*xlsx')  #get files list from folder

for (i in 1:length(filelistPrecios)){                                           
  wb <- loadWorkbook(filelistPrecios[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(filelistPrecios[i], sheetIndex=j, colIndex= c(1:10),
      sheetName=NULL, startRow=5, endRow=16,
      as.data.frame=TRUE, header=F)   
    if (i==1&j==1) PreciosMensuales<-tmp else PreciosMensuales<-rbind(PreciosMensuales,tmp)   #happend to previous
    
  }
}

tmp <- read.xlsx('Precios_Mensuales_($kWh)_2016.xls', sheetIndex=j, colIndex= c(1:10),
  sheetName=NULL, startRow=5, endRow=16,
  as.data.frame=TRUE, header=F)   

PreciosMensuales <- rbind(PreciosMensuales,tmp)


NombresColumnasMensuales <- c("Año", "Mes", "PrecioEscasez$/kwh", "MC$/kwh",
  "CERE$/kwh", "CEE$/kwh", "FAZNI$/kwh", "PrecioPromedioContrato", 
  "PrecioPromedioContratoRegulados", "PrecioPromedioContratoNoRegulados")

colnames(PreciosMensuales) <- NombresColumnasMensuales

PreciosMensuales$Fecha <- seq(as.Date("2000-01-01"), by = "month", length = nrow(PreciosMensuales)) 


save(PreciosMensuales, file = "PreciosMensuales.RData")


#------------------------------------------------------#
## Carga  Servicios AGC ################################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/ServiciosAGC")

fileServiciosAGC <- list.files(recursive = T, pattern = '*xlsx')  #get files list from folder

for (i in 1:length(fileServiciosAGC)){                                           
  wb <- loadWorkbook(fileServiciosAGC[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(fileServiciosAGC[i], sheetIndex=j,colIndex= c(1:28),
      sheetName=NULL, startRow=5, endRow=NULL,
      as.data.frame=TRUE, header=F)   
    if (i==1&j==1) ServiciosAGC<-tmp else ServiciosAGC<-rbind(ServiciosAGC,tmp)   #happend to previous
    
  }
}




tmp <- read.xlsx('Servicio_AGC_(kwh)_2016.xls', sheetIndex=j,
  sheetName=NULL, startRow=5, endRow=NULL,
  as.data.frame=TRUE, header=F)   

ServiciosAGC <- rbind(ServiciosAGC,tmp)


NombresColumnasServiciosAGC<- c("Fecha", "Recurso", "CodigoAgente", "0", 
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" ,"15",
  "16", "17", "18", "19", "20", "21", "22", "23", "Version")

colnames(ServiciosAGC) <- NombresColumnasServiciosAGC

ServiciosAGC$Fecha <- seq(as.Date("2000-01-01"), by = "month", length = nrow(ServiciosAGC)) 


save(ServiciosAGC, file = "ServiciosAGC.RData")



#------------------------------------------------------#
## Costo Marginal Despacho programado ##################
#------------------------------------------------------#


setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Costo Marginal Despacho Programado")

fileMarginalDespachoProgramado<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileMarginalDespachoProgramado)){                                           
  wb <- loadWorkbook(fileMarginalDespachoProgramado[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(fileMarginalDespachoProgramado[i], sheetIndex=j,colIndex= c(1:28),
      sheetName=NULL, startRow=4, endRow=NULL,
      as.data.frame=TRUE, header=F)   
    if (i==1&j==1) MarginalDespProg<-tmp else MarginalDespProg<-rbind(MarginalDespProg,tmp)   #happend to previous
    
  }
}


NombresMarginalDespachoProgramado<- c("Fecha","0", 
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" ,"15",
  "16", "17", "18", "19", "20", "21", "22", "23")

colnames(MarginalDespProg) <- NombresMarginalDespachoProgramado

MarginalDespProg$Fecha <- seq(as.Date("2000-01-01"), by = "month", length = nrow(MarginalDespProg)) 


save(MarginalDespProg, file = "MarginalDespProg.RData")


#------------------------------------------------------#
## Precio Naconal de Bolsa ##################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Precio de Bolsa Nacional")

filePrecioNacionalBolsa<- list.files(recursive = T, pattern = '*xls')  #get files list from folder



for (i in 1:length(filePrecioNacionalBolsa)){                                           
  wb <- loadWorkbook(filePrecioNacionalBolsa[i])           #select a file & load workbook
  sheet <- getSheets(wb)                      #get sheet list
  
  for (j in 1:length(sheet)){ 
    tmp <- read.xlsx(filePrecioNacionalBolsa[i], sheetIndex=j,colIndex= c(1:26),
      sheetName=NULL, startRow=4, endRow=NULL,
      as.data.frame=TRUE, header=F)   
    if (i==1&j==1) PreciosBolsa<-tmp else PreciosBolsa<-rbind(PreciosBolsa,tmp)   #happend to previous
    
  }
}

NombresPrecioNacionalBolsa<- c("Fecha","0", 
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" ,"15",
  "16", "17", "18", "19", "20", "21", "22", "23", "Version")

colnames(PreciosBolsa) <- NombresPrecioNacionalBolsa



PreciosBolsa$Fecha <- as.Date(PreciosBolsa$Fecha ,  format = "%Y-%m-%d")


## Precios Bolsa

PreciosBolsa$PromDia <-  rowMeans(PreciosBolsa[,2:25] , na.rm=T)

save(PreciosBolsa, file = "PreciosBolsa.RData")

#------------------------------------------------------#
## Hidrologia - Aportes Diarios ########################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Hidrologia/Aportes/Aportes Diarios")

fileAportesDiarios<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileAportesDiarios)){    
  
  tmp <- read_excel(fileAportesDiarios[i], sheet=1, skip=4,  col_names =F)  
  
  if (i==1) HAportesDiarios<-tmp else HAportesDiarios<-rbind(HAportesDiarios,tmp)   #happend to previous
  
}

NombresHAportesDiarios<- c("Fecha","RegionHidrologica", "NombreRio", "AportesCaudal(m3/s)",
      "AportesEnergiaKwh", "Aportes%")

colnames(HAportesDiarios) <- NombresHAportesDiarios

HAportesDiarios <- HAportesDiarios[rowSums(is.na(HAportesDiarios))!=ncol(HAportesDiarios), ]

save(HAportesDiarios, file = "HAportesDiarios.RData")



#------------------------------------------------------#
## Hidrologia - Reservas Diarias #######################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Hidrologia/Reservas/Reservas Diarias")

fileReservasDiarias<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileReservasDiarias)){    
  
  tmp <- read_excel(fileReservasDiarias[i], sheet=1, skip=4,  col_names =F)  
  
  if (i==1) HReservasDiarias<-tmp else HReservasDiarias<-rbind(HReservasDiarias,tmp)   #happend to previous
  
}

NombresHReservasDiarias<- c("Fecha","RegionHidrologica", "NombreEmbalse", "VolumenUtilDiario(Mm3)",
  "VolumenUtilDiarioEnergia(kwh)", "VolumenUtilDiario%", "Volumen(Mm3)", "VolumenEnergia(kwh)",
  "Volumen%")

colnames(HReservasDiarias) <- NombresHReservasDiarias

HReservasDiarias <- HReservasDiarias[rowSums(is.na(HReservasDiarias))!=ncol(HReservasDiarias), ]

save(HReservasDiarias, file = "HReservasDiarias.RData")

#------------------------------------------------------#
## Hidrologia - Vertimientos Diarios ###################
#------------------------------------------------------#

setwd("C:/Users/admin/Desktop/Tesis Maestria/Datos/Hidrologia/Vertimientos")

fileVertimientosDiarios<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileVertimientosDiarios)){    
  
  tmp <- read_excel(fileVertimientosDiarios[i], sheet=1, skip=4,  col_names =F)  
  
  if (i==1) HVertimientosDiarios<-tmp else HVertimientosDiarios<-rbind(HVertimientosDiarios,tmp)   #happend to previous
  
}

NombresHVertimientosDiarios<- c("Fecha","RegionHidrologica", "NombreEmbalse", 
            "VerimientosVolumenMiles(m3)", "VerimientosEnergia(kwh)")

colnames(HVertimientosDiarios) <- NombresHVertimientosDiarios

HVertimientosDiarios <- HVertimientosDiarios[rowSums(is.na(HVertimientosDiarios))!=ncol(HVertimientosDiarios), ]

save(HVertimientosDiarios, file = "HVertimientosDiarios.RData")

#------------------------------------------------------#
## Capacidad Efectiva Neta ###################
#------------------------------------------------------#

library(readxl)

setwd("C:/Users/admin/Desktop/Tesis Maestria/Fuentes Datos/CapacidadEfectivaNeta")

fileCapacidadEfectivaNeta<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileCapacidadEfectivaNeta)){    
  
  tmp <- read_excel(fileCapacidadEfectivaNeta[i], sheet=1, skip=3,  col_names =F)  
  
  if (i==1) CapacidadEfectivaNeta <-tmp else CapacidadEfectivaNeta <- rbind(CapacidadEfectivaNeta,tmp)   #happend to previous
  
}

NombresCapacidadEfectivaNeta<- c("Fecha","Recurso", "CodigoAgente", 
  "TipoGeneracion", "CombustiblePorDefecto", "TipoDespacho", "CapacidadEfectivaNeta")

colnames(CapacidadEfectivaNeta) <- NombresCapacidadEfectivaNeta

CapacidadEfectivaNeta <- CapacidadEfectivaNeta[rowSums(is.na(CapacidadEfectivaNeta))!=ncol(CapacidadEfectivaNeta), ]

setwd("C:/Users/admin/Desktop/Tesis Maestria")


save(CapacidadEfectivaNeta, file = "Datos Preparados/CapacidadEfectivaNeta.RData")


#------------------------------------------------------#
## Carga Generación ###################
#------------------------------------------------------#

library(readxl)

setwd("C:/Users/admin/Desktop/Tesis Maestria/Fuentes Datos/Generacion")

fileGeneracion<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 6:length(fileGeneracion)){    
  
  tmp <- read_excel(fileGeneracion[i], sheet=1, skip=3,  col_names =F)  
  
  if (i==6){ 
    Generacion <- tmp 
  } else {
    
    names(tmp)=names(Generacion) 
    
    Generacion <- rbind(Generacion,tmp)   #happend to previous
    
    }
      
}



NombresGeneracion<- c("Fecha","Recurso","TipoGeneracion","Combustible","CodigoAgente", 
  "TipoDespacho","EsMenor","EsAutoGenerador","0","1", "2", "3", "4", "5", "6", "7",
  "8", "9", "10", "11", "12", "13", "14" ,"15","16", "17", "18", "19", "20", "21",
  "22", "23")

colnames(Generacion) <- NombresGeneracion

Generacion <- Generacion[rowSums(is.na(Generacion))!=ncol(Generacion), ]

setwd("C:/Users/admin/Desktop/Tesis Maestria")


save(Generacion, file = "Datos Preparados/Generacion.RData")




#------------------------------------------------------#
## Disponibilidad Declarada #############################
#------------------------------------------------------#

library(readxl)

setwd("C:/Users/admin/Desktop/Tesis Maestria/Fuentes Datos/DisponibilidadDeclarada")

fileDisponibilidadDeclarada<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileDisponibilidadDeclarada)){    
  
  tmp <- read_excel(fileDisponibilidadDeclarada[i], sheet=1, skip=3,  col_names =F)  
  
  if (i==1) DisponibilidadDeclarada <-tmp 
  
  else  names(tmp)=names(DisponibilidadDeclarada) 
        DisponibilidadDeclarada <- rbind(DisponibilidadDeclarada,tmp)   #happend to previous
  
}



NombresDisponibilidadDeclarada<- c("Fecha","Recurso","CodigoAgente" ,"0", 
  "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14" ,"15",
  "16", "17", "18", "19", "20", "21", "22", "23")

colnames(DisponibilidadDeclarada) <- NombresDisponibilidadDeclarada

DisponibilidadDeclarada <- DisponibilidadDeclarada[rowSums(is.na(DisponibilidadDeclarada))!=ncol(DisponibilidadDeclarada), ]

setwd("C:/Users/admin/Desktop/Tesis Maestria")


save(DisponibilidadDeclarada, file = "Datos Preparados/DisponibilidadDeclarada.RData")


#------------------------------------------------------#
## Demanda Energia SIN #############################
#------------------------------------------------------#

library(readxl)

setwd("C:/Users/admin/Desktop/Tesis Maestria/Fuentes Datos/DemandaEnergiaSIN")

fileDemandaEnergiaSIN<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileDemandaEnergiaSIN)){    
  
  tmp <- read_excel(fileDemandaEnergiaSIN[i], sheet=1, skip=4,  col_names =F)  
  
  if (i==1){ 
    DemandaEnergiaSIN <- tmp 
  } else {
    
    names(tmp)=names(DemandaEnergiaSIN) 
    
    DemandaEnergiaSIN <- rbind(DemandaEnergiaSIN,tmp)   #happend to previous
    
  }  #happend to previous
  
}



NombresDemandaEnergiaSIN<- c("Fecha","DemandaEnergiaSIN[kWh]","Generacion[kWh]"
  ,"DemandaNoAtendida[kWh]","Exportaciones[kWh]","Importaciones[kWh]")

colnames(DemandaEnergiaSIN) <- NombresDemandaEnergiaSIN

DemandaEnergiaSIN <- DemandaEnergiaSIN[rowSums(is.na(DemandaEnergiaSIN))!=ncol(DemandaEnergiaSIN), ]

setwd("C:/Users/admin/Desktop/Tesis Maestria")


save(DemandaEnergiaSIN, file = "Datos Preparados/DemandaEnergiaSIN.RData")

#------------------------------------------------------#
## Contratos  #############################
#------------------------------------------------------#

library(readxl)

setwd("C:/Users/admin/Desktop/Tesis Maestria/Fuentes Datos/Contratos")

fileContratos<- list.files(recursive = T, pattern = '*xls')  #get files list from folder

for (i in 1:length(fileContratos)){    
  
  tmp <- read_excel(fileContratos[i], sheet=1, skip=4,  col_names =F)  
  
  if (i==1){ 
    Contratos <- tmp 
  } else {
    
    names(tmp)=names(Contratos) 
    
    Contratos <- rbind(Contratos,tmp)   #happend to previous
    
  }  #happend to previous
  
}



NombresContratos<- c("Fecha","Contratos[kWh]","Generacion[kWh]"
  ,"DemandaNoAtendida[kWh]","Exportaciones[kWh]","Importaciones[kWh]")

colnames(Contratos) <- NombresContratos

Contratos <- Contratos[rowSums(is.na(Contratos))!=ncol(Contratos), ]

setwd("C:/Users/admin/Desktop/Tesis Maestria")


save(Contratos, file = "Datos Preparados/Contratos.RData")



