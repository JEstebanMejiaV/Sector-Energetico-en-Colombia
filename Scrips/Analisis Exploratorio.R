

#------------------------------------------------------#
## Carga Precios de Oferta #############################
#------------------------------------------------------#


setwd("C:/Users/admin/Desktop/Tesis Maestria")


load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOferta.RData")


####### Agregar más Info de los Agentes ######

library(xlsx)    

AgentesInfo <- read.xlsx2('InfoAgentes.xlsx',sheetName='Datos', endRow=NULL,as.data.frame=TRUE, header=T,encoding = "UTF-8")


PreciosOfertaAgentes <- merge(PreciosOferta, AgentesInfo[ , c(1:2, 7,9)], 
  by.x = c('Recurso', 'CodigoAgente'), 
  by.y = c('Recurso', 'CodigoAgente'), all.x = T)

PreciosOfertaAgentes <- PreciosOfertaAgentes[order(PreciosOfertaAgentes$Fecha),]

save(PreciosOfertaAgentes, file = "Datos Preparados/PreciosOfertaAgentes.RData")

#------------------------------------------------------#
##  Dispercion de precios de oferta por año ############
#------------------------------------------------------#

#### Dispercion de precios de oferta por año

library(ggplot2)
library(lubridate)

ggplot(PreciosOfertaAgentes, aes(x=as.factor(year(Fecha)), y=`PrecioOfertaIdeal$/kwh`,
  fill=as.factor(year(Fecha))) ) + geom_boxplot()

## Dispercion de precios de oferta por año por tipo de genración

ggplot(PreciosOfertaAgentes, aes(x=as.factor(year(Fecha)), y=`PrecioOfertaIdeal$/kwh`,
  fill=as.factor(year(Fecha))) ) + geom_boxplot() + facet_wrap( ~ TipoGeneracion , ncol=1) +
  labs(x = "Fecha", y ="Precio Oferta Ideal [$/kwh]",colour = " ",
  title = " a) Precios de Oferta Con Outliers") +
  theme(legend.position="none",axis.text=element_text(size=11),axis.title=element_text(size=12,face="bold"))

##### Dispercion de precios de oferta por año Removiendo los outliers

RemoverOutliers<- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(0, .95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}

RemoverOutliersTodos <- function(df){
  # We only want the numeric columns
  df[,sapply(df, is.numeric)] <- lapply(df[,sapply(df, is.numeric)], RemoverOutliers)
  df
}

PreciosOfertaSinOutliers <- RemoverOutliersTodos(PreciosOfertaAgentes)

## Grafica de dispercion de precios de ofertan removiendo outliers


ggplot(PreciosOfertaSinOutliers, aes(x=as.factor(year(Fecha)), y=`PrecioOfertaIdeal$/kwh`,
  fill=as.factor(year(Fecha))) ) + geom_boxplot() 

# Por tipo de Generación

ggplot(PreciosOfertaSinOutliers, aes(x=as.factor(year(Fecha)), y=`PrecioOfertaIdeal$/kwh`,
  fill=as.factor(year(Fecha))) ) + geom_boxplot() + facet_wrap( ~ TipoGeneracion , ncol=1)+
  labs(x = "Fecha", y ="Precio Oferta Ideal [$/kwh]",colour = " ",
       title = "Precios de Oferta sin Outliers")+
  theme(legend.position="none",axis.text=element_text(size=12),
        axis.title=element_text(size=13,face="bold"))
  

library(ggvis)

PreciosOfertaAgentes %>% ggvis(~`PrecioOfertaIdeal$/kwh`, fill := "#881128") %>%
  layer_histograms(width = 50) %>% add_axis("x", title = "Precio de Oferta Ideal [$/kwh]")%>% 
  add_axis("y", title = "Frecuencia") %>%   
  add_axis("x", orient = "top", ticks = 0, title = "Histograma de Precios de Oferta: 2000-2016",
    properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0)))


PreciosOfertaAgentes$LogPrecios  <- log(PreciosOfertaAgentes$`PrecioOfertaIdeal$/kwh`)


PreciosOfertaAgentes %>% ggvis(~ LogPrecios, fill := "#881128") %>%
  layer_histograms(width = 0.5) %>% add_axis("x", title = "Precio de Oferta Ideal [$/kwh]")%>% 
  add_axis("y", title = "Frecuencia") %>%   
  add_axis("x", orient = "top", ticks = 0, title = "Histograma de Logaritmo Precios de Oferta: 2000-2016",
    properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0)))

#------------------------------------------------------#
##  Analisisde precios de ofera para  2016 #############
#------------------------------------------------------#


PreciosOferta2016 <- subset(PreciosOferta, year(PreciosOferta$Fecha)==2016)

save(PreciosOferta2016, file = "Datos Preparados/PreciosOferta2016.RData")

plot(PreciosOferta2016$`PrecioOfertaIdeal$/kwh`)

PreciosOferta2016 %>% ggvis(~`PrecioOfertaIdeal$/kwh`, fill := "#881128") %>%
  layer_histograms(width = 50) %>% add_axis("x", title = "Precio de Oferta Ideal [$/kwh]")%>% 
  add_axis("y", title = "Frecuencia") %>%   
  add_axis("x", orient = "top", ticks = 0, title = "Histograma de Precios de Oferta 2016",
    properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0)))

PreciosOferta2016 %>% ggvis(~log(`PrecioOfertaIdeal$/kwh`), fill := "#881128") %>%
  layer_histograms(width = 0.3) %>% add_axis("x", title = "Precio de Oferta Ideal [$/kwh]")%>% 
  add_axis("y", title = "Frecuencia") %>%   
  add_axis("x", orient = "top", ticks = 0, title = "Histograma de Precios de Oferta 2016",
    properties = axis_props(
      axis = list(stroke = "white"),
      labels = list(fontSize = 0)))


PreciosOferta2016SanCarlos <- subset(PreciosOferta2016, PreciosOferta2016$Recurso=='SAN CARLOS')

library(xts)

TSPreciosOferta2016SanCarlos <- xts(PreciosOferta2016SanCarlos[, 4], order.by=as.POSIXct(PreciosOferta2016SanCarlos$Fecha,start = min(PreciosOferta2016SanCarlos[, 4]), 
  end = max(PreciosOferta2016SanCarlos[, 4]), frequency = 24))

plot(TSPreciosOferta2016SanCarlos)

sd(TSPreciosOferta2016SanCarlos)
median(TSPreciosOferta2016SanCarlos)

hist(PreciosOferta2016$`PrecioOfertaIdeal$/kwh`)

hist(PreciosOferta2016$`PrecioOfertaDespacho$/kwh`)

hist(PreciosOferta2016$`PrecioOfertaDeclarado$/kwh`)


library(reshape2)


PreciosOferta2016long <-PreciosOferta2016[, c(1,4:8)]

PreciosOferta2016long <- melt(PreciosOferta2016long, id.vars = c("Fecha", "Dia", 'TipoDia'))


## Comparación de las distribuciones de los precios de oferta ideal, despachado y declarado

ggplot(PreciosOferta2016long, aes(x=value, fill=variable)) + geom_density(alpha=.25)

## Analsis cuntitativo de dicha comparación

round(tapply(PreciosOferta2016long$value, PreciosOferta2016long$variable, mean, na.rm=T), 2)
round(tapply(PreciosOferta2016long$value, PreciosOferta2016long$variable, sd, na.rm=T), 2)
a <- tapply(PreciosOferta2016long$value, PreciosOferta2016long$variable, quantile, na.rm=T)[2]


## Analisis por día

ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Lunes',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Martes',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Miercoles',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Jueves',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Viernes',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Sabado',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$Dia=='Domingo',], aes(x=value, fill=variable)) + geom_density(alpha=.25)

p <- ggplot(data = PreciosOferta2016, aes(x=`PrecioOfertaIdeal$/kwh`,fill=Dia)) 
p <- p + geom_density(alpha=.25)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Dia)
p


p <- ggplot(data = PreciosOferta2016, aes(x=`PrecioOfertaIdeal$/kwh`,fill=Dia)) 
p <- p + geom_density(alpha=.25)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Dia, ncol=1)
p

## Analisis cuntitativo por día

tapply(PreciosOferta2016long$value, PreciosOferta2016long$Dia, mean, na.rm=T)
tapply(PreciosOferta2016long$value, PreciosOferta2016long$Dia, sd, na.rm=T)
tapply(PreciosOferta2016long$value, PreciosOferta2016long$Dia, quantile, na.rm=T)

ggplot(PreciosOferta2016long, aes(x=Dia, y=value, fill=Dia) ) + geom_boxplot()+
  labs(x = "Día", y ="Precio Oferta Ideal [$/kwh]",colour = " ",
    title = "Box Plots de Precios de Oferta X Día") +
  theme(legend.position="none",axis.text=element_text(size=11),axis.title=element_text(size=12,face="bold"))

  

## Analisis en semana y los fines de semana

ggplot(PreciosOferta2016long[PreciosOferta2016long$TipoDia=='FindeSemana',], aes(x=value, fill=variable)) + geom_density(alpha=.25)
ggplot(PreciosOferta2016long[PreciosOferta2016long$TipoDia=='DiadeSemana',], aes(x=value, fill=variable)) + geom_density(alpha=.25)

tapply(PreciosOferta2016long$value, PreciosOferta2016long$TipoDia, mean, na.rm=T)
tapply(PreciosOferta2016long$value, PreciosOferta2016long$TipoDia, sd, na.rm=T)
tapply(PreciosOferta2016long$value, PreciosOferta2016long$TipoDia, quantile, na.rm=T)

p <- ggplot(data = PreciosOferta2016, aes(x=`PrecioOfertaIdeal$/kwh`,fill=TipoDia)) 
p <- p + geom_density(alpha=.8)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ TipoDia, ncol=1)
p


ggplot(PreciosOferta2016long, aes(x=TipoDia, y=value, fill=TipoDia) ) + geom_boxplot()


########

PreciosOfertaAgentes2016 <- subset(PreciosOfertaAgentes, year(Fecha) ==2016 )

save(PreciosOfertaAgentes2016, file = "Datos Preparados/PreciosOfertaAgentes2016.RData")

####


require(ggplot2)    
p <- ggplot(data = PreciosOfertaAgentes2016, aes(x=`PrecioOfertaIdeal$/kwh`,fill=Dia)) 
p <- p + geom_density(alpha=.25)
p <- p + scale_fill_brewer(palette="Set3")
p <- p + facet_wrap( ~ Dia, ncol=1)
p

### Analsis del 2016

PreciosOferta2016longAgentes <- PreciosOfertaAgentes2016[, c(3,4,7,8,10)]

PreciosOferta2016longAgentes <- melt(PreciosOferta2016longAgentes,
  id.vars = c("Fecha", "Dia", 'TipoDia','TipoGeneracion'),measure.vars='PrecioOfertaIdeal$/kwh')


# Grafica de precio ideal de ofeta por tecnologia y tipodía

p <- ggplot(PreciosOferta2016longAgentes, aes(x=value, fill=variable)) + geom_density(alpha=.25)

p <- p + facet_wrap( TipoGeneracion~ TipoDia,scales="free")

p

##### Analsis de periodo disponible

PreciosOfertaAgentes <- PreciosOfertaAgentes[, c(3,4,7,8,10)]

PreciosOfertaAgentes <- melt(PreciosOfertaAgentes,
  id.vars = c("Fecha", "Dia", 'TipoDia','TipoGeneracion'),measure.vars='PrecioOfertaIdeal$/kwh')


# Grafica de precio ideal de ofeta por tecnologia y tipodía

p <- ggplot(PreciosOfertaAgentes, aes(x=value, fill=variable)) + geom_density(alpha=.25)

p <- p + facet_wrap( TipoGeneracion~ TipoDia,scales="free")

p


###### Pruebas varias ############

library(xts)

load('C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosMensuales.RData')



PreciosMensualesTS <- xts(PreciosMensuales[, -11], order.by=as.POSIXct(PreciosMensuales$Fecha))



plot(PreciosMensualesTS$PrecioPromedioContrato)
lines(PreciosMensualesTS$PrecioPromedioContrato, col='navy', lwd=2)
lines(PreciosMensualesTS$PrecioPromedioContratoRegulado, col='orange', lwd=2)
abline(h=mean(as.numeric(PreciosMensualesTS$PrecioPromedioContrato), na.rm=T), col='red')

ind <- which(PreciosMensualesTS[,1]==2016)

PreciosMensualesTS2016 <- PreciosMensualesTS[ ind , ] 

plot(PreciosMensualesTS2016[,8]  )
lines(PreciosMensualesTS2016[,8], col='navy', lwd=2)


library(PerformanceAnalytics)

chart.TimeSeries(PreciosMensualesTS$PrecioPromedioContrato, 
  period.areas = c('2003-01/2005-01'), period.color = 'lightgrey')


library(TTR)

PreciosMensualesTS$Dif  <- ROC(as.numeric(PreciosMensualesTS$PrecioPromedioContrato))
plot(PreciosMensualesTS$Dif )

acf(na.omit(PreciosMensualesTS$Dif))

hist(PreciosMensualesTS$Dif)





