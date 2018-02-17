
load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOferta.RData")


library(tidyr)
library(dplyr)


AgentesQueGeneraron <- PreciosOferta %>% group_by(Fecha) %>%
                        filter(`PrecioOfertaIdeal$/kwh` <= which(ColocoPrecio=='Si')) 


AgentesQueGeneraron <- data.frame()

for (i in unique(PreciosOferta$Fecha)){
  
  temp <- subset(PreciosOferta, PreciosOferta$Fecha == i)
  
  tryCatch(temp2 <-temp %>% filter(`PrecioOfertaIdeal$/kwh`<= temp[which(ColocoPrecio=='Si'),4]))
  
  AgentesQueGeneraron <- rbind(AgentesQueGeneraron, temp2)
  
}
  
  for ( j in unique(DatosModelamiento$Recurso)){
    
    temp2 <- subset(temp, temp$Recurso == j)  
    out <-  boxplot.stats(temp2$`PrecioOfertaIdeal$/kwh`)$out
    ind <- which(temp2$`PrecioOfertaIdeal$/kwh` %in% out)
    outlier <- rbind(outlier, temp2[ind,])
    
  }
}


temp <- subset(PreciosOferta, PreciosOferta$Fecha == '2010-11-30')

try(temp %>% filter(`PrecioOfertaIdeal$/kwh` <= temp[which(ColocoPrecio=='Si'),4]))

if (missing(temp %>% filter(`PrecioOfertaIdeal$/kwh` <= temp[which(ColocoPrecio=='Si'),4])))
temp2 <-temp %>% filter(`PrecioOfertaIdeal$/kwh` <= temp[which(ColocoPrecio=='Si'),4])


PreciosOferta[which(ColocoPrecio=='Si'),4]


if ( is.null(temp %>% filter(`PrecioOfertaIdeal$/kwh` <= temp[which(ColocoPrecio=='Si'),4]))) { 
  print("is null")
  
  }


###### Generacion ########

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/Generacion.RData")


Generacion[is.na(Generacion)] <- 0


Generacion$OfertaDia <-  rowSums( Generacion[,9:32] )


OfertaResidual <- data.frame()


for (i in unique(Generacion$Fecha)){
  
  temp <- subset(Generacion, Generacion$Fecha == i)
  
  temp$OfertaResidual <- colSums(temp[, 33]) - temp$OfertaDia  
  

  OfertaResidual <- rbind(OfertaResidual, temp)
  
}

save(OfertaResidual, file = "Datos Preparados/OfertaResidual.RData")


load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/DemandaEnergiaSIN.RData")

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/OfertaResidual.RData")


library(dplyr)


IOR <- left_join(OfertaResidual, DemandaEnergiaSIN[ , 1:2], by = c("Fecha" = "Fecha"),copy=F)



IOR$IOR <- IOR$OfertaResidual/IOR$`DemandaEnergiaSIN[kWh]`

IOR$SharePlanta <- (1-IOR$IOR)*100

IOR$SharePlanta2 <- IOR$SharePlanta^2

HHIPlantas <- aggregate(IOR$SharePlanta^2, by=list(Fecha=IOR$Fecha), FUN=sum)


HHIAgentes <-IOR %>% group_by(Fecha, CodigoAgente) %>% summarise(ShareAgente=sum(SharePlanta, na.rm = TRUE))

HHIAgentes$ShareAgente2 <- HHIAgentes$ShareAgente^2


HHIAgentesxFecha  <- HHIAgentes %>% group_by(Fecha)  %>% summarise(HHIAgentes=sum(ShareAgente2, na.rm = TRUE))

### Congregar Variables

VariablesEstrategicas <- left_join(IOR, HHIAgentesxFecha, by = c("Fecha" = "Fecha"),copy=F)

VariablesEstrategicas <- left_join(VariablesEstrategicas, HHIPlantas, by = c("Fecha" = "Fecha"),copy=F)

names(VariablesEstrategicas)[40] <- 'HHIPlantas'

VariablesEstrategicas <- left_join(VariablesEstrategicas, HHIAgentes[,c(1:3)],
                          by = c("Fecha" = "Fecha","CodigoAgente" = "CodigoAgente"))


VariablesEstrategicas <- VariablesEstrategicas[, -c(9:32)]


save(VariablesEstrategicas, file = "Datos Preparados/VariablesEstrategicas.RData")
