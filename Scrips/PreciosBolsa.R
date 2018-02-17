

#------------------------------------------------------#
## Carga Precios de Oferta #############################
#------------------------------------------------------#

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOferta.RData")


## Precios Bolsa

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados//PreciosBolsa.RData")



vlookup <- function(ref, #the value or values that you want to look for
  table, #the table where you want to look for it; will look in first column
  column, #the column that you want the return data to come from,
  range=FALSE, #if there is not an exact match, return the closest?
  larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}


### intentar por SQL


###

PreciosOferta$PromDia <- vlookup(PreciosOferta$Fecha,table = PreciosBolsa, 27)

PreciosOferta$DesvPrecio <- round(abs(PreciosOferta$`PrecioOfertaIdeal$/kwh` - PreciosOferta$PromDia), 2)

PreciosOferta$Salio <- ifelse(PreciosOferta$DesvPrecio < 10 , 'Si', 'No')



library(dplyr)


AgentesColocadoresPrecio <- PreciosOferta %>% group_by(Fecha) %>% 
      filter(abs(`PrecioOfertaIdeal$/kwh` - PromDia) == 
      min(abs(`PrecioOfertaIdeal$/kwh` - PromDia)))

AgentesColocadoresPrecio$ColocoPrecio <- 'Si'


PreciosOferta <-left_join(PreciosOferta, AgentesColocadoresPrecio, by = c("Fecha" = "Fecha", "Recurso" = "Recurso"))    

PreciosOferta$ColocoPrecio <- ifelse(is.na(PreciosOferta$ColocoPrecio) == TRUE, 'No', PreciosOferta$ColocoPrecio )


PreciosOferta <- PreciosOferta[, -(12:20)]

NombresColumnas <- c("Fecha", "Recurso", " CodigoAgente", "PrecioOfertaIdeal$/kwh",
  "PrecioOfertaDespacho$/kwh", "PrecioOfertaDeclarado$/kwh", "Dia", "TipoDia", "PromDia", 
  "DesvPrecio", "CercanoPrecioMarginal", "ColocoPrecio")

colnames(PreciosOferta) <- NombresColumnas


#------------------------------------------------------#
## Analisis Precios Bolsa ###############################
#------------------------------------------------------#



library(tidyr)
library(lubridate)
library(ggplot2)

PreciosBolsaLong <- PreciosBolsa %>% select(-PromDia ,-Version) %>%
                            gather( "Hora", "Precios($/kwh)", 2:25)

PreciosBolsaLong$Hora <- as.numeric(PreciosBolsaLong$Hora)

PreciosBolsaLong <- PreciosBolsaLong[order(PreciosBolsaLong[,'Fecha'], PreciosBolsaLong[, 'Hora']),]


PreciosBolsaLong2016 <- subset(PreciosBolsaLong, year(Fecha)==2016)

PreciosBolsaLong2016$dia <- c("Domingo", "Lunes", "Martes", "Miercoles", "Jueves", 
  "Viernes", "Sabado")[as.POSIXlt(PreciosBolsaLong2016$Fecha)$wday+1]


PreciosBolsaLong2016$TipoDia <- ifelse(PreciosBolsaLong2016$dia %in% c("Domingo", "Sabado"),
  'FindeSemana', 'DiadeSemana')

hist(PreciosBolsaLong2016$`Precios($/kwh)`)

library(ggvis)

PreciosBolsaLong2016 %>% ggvis(~`Precios($/kwh)`, fill := "#881128") %>%
  layer_histograms(width = 50)

ggplot( PreciosBolsaLong2016[PreciosBolsaLong2016$TipoDia=='FindeSemana',], aes(x=`Precios($/kwh)`)) + geom_density(alpha=.25, fill="lightblue")
ggplot( PreciosBolsaLong2016[PreciosBolsaLong2016$TipoDia=='DiadeSemana',], aes(x=`Precios($/kwh)`)) + geom_density(alpha=.25, fill="lightblue")

p <- ggplot(data = PreciosBolsaLong2016, aes(x=`Precios($/kwh)`,fill=TipoDia)) 
p <- p + geom_density(alpha=.45)
p <- p + scale_fill_brewer(palette="Paired")
p <- p + facet_wrap( ~ TipoDia, ncol=1)
p

###  Analsis por día

p <- ggplot(data = PreciosBolsaLong2016, aes(x=`Precios($/kwh)`,fill=dia)) 
p <- p + geom_density(alpha=.65)
p <- p + scale_fill_brewer(palette="Paired")
p <- p + facet_wrap( ~ dia, ncol=1)
p


### Analisi de serie de tiempo

library(xts)


TSPreciosBolsa2016 <- xts(PreciosBolsaLong2016[, 3], order.by=as.POSIXct(PreciosBolsaLong2016$Fecha,start = min(PreciosBolsaLong2016[, 3]), 
  end = max(PreciosBolsaLong2016[, 3]), frequency = 24))

plot(TSPreciosBolsa2016)

colnames(TSPreciosBolsa2016) <- 'Precios'

TSPreciosBolsa2016$Dif <- diff(TSPreciosBolsa2016, lag = 1)


plot(TSPreciosBolsa2016$Dif )

#acf(TSPreciosBolsa2016)

hist(TSPreciosBolsa2016$Dif)

#------------------------------------------------------#
## Analisis Precos Bolsa  y Precios Oferta #################
#------------------------------------------------------#


library(Hmisc) # Lag


PreciosOfertaAgentes2016Corwide$PreciosBolsa <- subset(PreciosBolsa, year(Fecha)==2016)$PromDia

PreciosOfertaAgentes2016Corwide$PreciosBolsalag1 <- Lag(PreciosOfertaAgentes2016Corwide$PreciosBolsa, shift = 1)

PreciosOfertaAgentes2016Corwide$PreciosBolsalag2 <- Lag(PreciosOfertaAgentes2016Corwide$PreciosBolsa, shift = 2)


library(corrplot) 
library(RColorBrewer)

CorPreciosBolsaAgentes <- cor(na.omit(as.matrix(PreciosOfertaAgentes2016Corwide[, 2:23])))


corrplot.mixed(CorPreciosBolsaAgentes, upper="ellipse", tl.pos = "lt", diag = "u",
  col=brewer.pal(n=8, name="RdYlBu"))


CorPreciosBolsaAgentes[,20]

CorPreciosBolsaAgentes[,21]


write.csv(CorPreciosBolsaAgentes[,20], file = "Graficos,Tablas/CorPreciosBolsaAgentes.csv", quote = FALSE, row.names = T)

write.csv(CorPreciosBolsaAgentes[,21], file = "Graficos,Tablas/CorPreciosBolsaAgenteslag1.csv", quote = FALSE, row.names = T)


library(Hmisc)

rcorr(as.matrix(CorPreciosBolsaAgentes), type="pearson")


### Hidro


PreciosAgenes2016Hidrowide$PreciosBolsa <- subset(PreciosBolsa, year(Fecha)==2016)$PromDia

PreciosAgenes2016Hidrowide$PreciosBolsalag1 <- Lag(PreciosAgenes2016Hidrowide$PreciosBolsa, shift = 1)

PreciosAgenes2016Hidrowide$PreciosBolsalag2 <- Lag(PreciosAgenes2016Hidrowide$PreciosBolsa, shift = 2)

library(corrplot) 
library(RColorBrewer)

CorPreciosBolsaAgentesHidro <- cor(na.omit(as.matrix(PreciosAgenes2016Hidrowide[, 3:10])))


corrplot.mixed(CorPreciosBolsaAgentesHidro, upper="ellipse", tl.pos = "lt", diag = "u",
  col=brewer.pal(n=8, name="RdYlBu"))


CorPreciosBolsaAgentesHidro[,6]

CorPreciosBolsaAgentesHidro[,7]

write.csv(CorPreciosBolsaAgentesHidro[,6], file = "Graficos,Tablas/CorPreciosBolsaAgentesHidro.csv", quote = FALSE, row.names = T)

write.csv(CorPreciosBolsaAgentesHidro[,7], file = "Graficos,Tablas/CorPreciosBolsaAgentesHidrolag1.csv", quote = FALSE, row.names = T)



### Termo


PreciosOfertaAgentes2016CorTermwide$PreciosBolsa <- subset(PreciosBolsa, year(Fecha)==2016)$PromDia

PreciosOfertaAgentes2016CorTermwide$PreciosBolsalag1 <- Lag(PreciosOfertaAgentes2016CorTermwide$PreciosBolsa, shift = 1)

PreciosOfertaAgentes2016CorTermwide$PreciosBolsalag2 <- Lag(PreciosOfertaAgentes2016CorTermwide$PreciosBolsa, shift = 2)



library(corrplot) 
library(RColorBrewer)

CorPreciosBolsaAgentesTerm <- cor(na.omit(as.matrix(PreciosOfertaAgentes2016CorTermwide[, 3:20])))


corrplot.mixed(CorPreciosBolsaAgentesTerm, upper="ellipse", tl.pos = "lt", diag = "u",
  col=brewer.pal(n=8, name="RdYlBu"))

CorPreciosBolsaAgentesTerm[,16]

CorPreciosBolsaAgentesTerm[,17]

write.csv(CorPreciosBolsaAgentesTerm[,16], file = "Graficos,Tablas/CorPreciosBolsaAgentesTerm.csv", quote = FALSE, row.names = T)

write.csv(CorPreciosBolsaAgentesTerm[,17], file = "Graficos,Tablas/CorPreciosBolsaAgentesTermlag1.csv", quote = FALSE, row.names = T)


#------------------------------------------------------#
## Analisis Desvio Precios Bolsa #######################
#------------------------------------------------------#

hist(PreciosOferta$DesvPrecio, breaks = 50)

library(fitdistrplus)


AjusteDesviolnorm <- fitdist(as.numeric(na.omit(PreciosOferta$DesvPrecio[PreciosOferta$DesvPrecio > 0])), distr = 'lnorm')

plot(AjusteDesviolnorm)

summary(AjusteDesviolnorm)



AjusteDesvioexp<- fitdist(as.numeric(na.omit(PreciosOferta$DesvPrecio[PreciosOferta$DesvPrecio > 0])), distr = 'exp')

plot(AjusteDesvioexp)

summary(AjusteDesvioexp)


# PreciosOferta$ColocoPrecio <- ifelse(PreciosOferta$ColocoPrecio=='Si', 1, 0)
# 
# sum(is.na(summary(PreciosOferta$ColocoPrecio)))
# 
# hist(PreciosOferta$ColocoPrecio)


table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio)

prop.table(table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio), margin = 1)

table(PreciosOferta$Recurso, PreciosOferta$ColocoPrecio)

prop.table(table(PreciosOferta$Recurso, PreciosOferta$ColocoPrecio), margin = 1)

###
hist(table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio)[,2], breaks = 15)

quantile(table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio)[,2])

###

hist(prop.table(table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio), margin = 1)[,2], breaks = 15)

quantile(prop.table(table(PreciosOferta$` CodigoAgente`, PreciosOferta$ColocoPrecio), margin = 1)[,2])



#------------------------------------------------------#
## Analisis Colocacion de precio por hora #######################
#------------------------------------------------------#

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOferta.RData")


## Precios Bolsa

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosBolsa.RData")



library(tidyr)
library(dplyr)

PreciosBolsaLong <- PreciosBolsa %>% select(-PromDia ,-Version) %>%
  gather( "Hora", "Precios($/kwh)", 2:25)

PreciosBolsaLong$Hora <- as.numeric(PreciosBolsaLong$Hora)

PreciosBolsaLong <- PreciosBolsaLong[order(PreciosBolsaLong[,'Fecha'], PreciosBolsaLong[, 'Hora']),]


temp <- left_join(PreciosOferta, PreciosBolsaLong, by = c("Fecha" = "Fecha"))

temp$DesvHora <- temp$`PrecioOfertaIdeal$/kwh` -temp$`Precios($/kwh)`

AgentesColocadoresPrecioHora <- temp %>% group_by_(.dots=c("Fecha","Hora")) %>% 
  filter(abs(DesvHora) == min(abs(DesvHora)))


AgentesColocadoresPrecioHora$ColocoPrecio <- 'Si'



NroRecursoColocadorePrecio <- as.data.frame(table(AgentesColocadoresPrecioHora$Recurso, AgentesColocadoresPrecioHora$ColocoPrecio))

NroRecursoColocadorePrecio$Porcentaje <- round(prop.table(NroRecursoColocadorePrecio$Freq), 4)

NroRecursoColocadorePrecio <- NroRecursoColocadorePrecio[order(NroRecursoColocadorePrecio$Freq, decreasing = T),]

write.csv(NroRecursoColocadorePrecio, file = "Graficos,Tablas/NroRecursoColocadorePrecio.csv", quote = FALSE, row.names = F)



NroAgenteColocadorePrecio <- as.data.frame(table(AgentesColocadoresPrecioHora$CodigoAgente, AgentesColocadoresPrecioHora$ColocoPrecio))

NroAgenteColocadorePrecio$Porcentaje <- round(prop.table(NroAgenteColocadorePrecio$Freq), 4)

NroAgenteColocadorePrecio <- NroAgenteColocadorePrecio[order(NroAgenteColocadorePrecio$Freq, decreasing = T),]

write.csv(NroAgenteColocadorePrecio, file = "Graficos,Tablas/NroAgenteColocadorePrecio.csv", quote = FALSE, row.names = F)




temp2 <- AgentesColocadoresPrecioHora %>% group_by_(.dots=c("Fecha","Recurso")) %>% 
  do(data.frame(nrow=nrow(.)))



PreciosOfertaColocacionHora <-left_join(PreciosOferta, temp2, by = c("Fecha" = "Fecha", "Recurso" = "Recurso"))    

names(PreciosOfertaColocacionHora)[names(PreciosOfertaColocacionHora)=="nrow"] <- "NroColocaciones"

PreciosOfertaColocacionHora$NroColocaciones[is.na(PreciosOfertaColocacionHora$NroColocaciones)] <- 0

 ### Analsis 

hist(AgentesColocadoresPrecioHora$DesvHora)


# AgentesColocadoresPrecioHora %>% ggvis(~DesvHora, fill := "#881128") %>%
#   layer_histograms(width = 50)
