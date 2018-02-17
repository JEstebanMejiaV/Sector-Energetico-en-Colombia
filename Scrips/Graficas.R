
##### Indice de Ocilación del Sur ####

library(readxl)
library(xts)
library(zoo)

SOI <- read_excel('Indice Ocilacion sur.xlsx')

SOI$SOI3MonAve <- rollmean(SOI$SOI, 3, na.pad=TRUE, align="right")


SOI <- subset(SOI, Year >= 2000 & Year <= 2016)

SOI$Fecha <- as.Date(SOI$Fecha ,  format = "%Y-%m-%d")


SOI$SOI3MonAve <- rollmedian(SOI$SOI, 3, na.pad=TRUE, align="right")

TSSOI <- xts(SOI[, -c(1, 2, 4, 6)], order.by=as.POSIXct(SOI$Fecha))

plot(TSSOI$)

library(ggplot2)
library(scales)

SOI$Fenomeno <- ifelse(SOI$SOI3MonAve >= 5, 'Niña', ifelse(SOI$SOI3MonAve <= -5 , 'Niño', 'Regular'))

SOI$Fenomeno <- as.factor(SOI$Fenomeno)

SOI <- na.omit(SOI)

ggplot(SOI,aes(Fecha,SOI,fill=Fenomeno))  + 
  geom_bar(stat = "identity", position=position_dodge())+  
  ggtitle("Evolución del Índice de Ocilación del Sur (SOI), 2000-2016" ) + 
  theme_bw(base_size=13) +
  labs(x = "Fecha", y ="The Southern Oscillation Index (SOI)",colour = " ")+
  scale_fill_manual(values=c("#496d96", "#fa9040", "#e5dfc6"), " ")+
  scale_x_date(labels = date_format("%Y"),date_breaks = "1 year")
  

source("Scrips/ggplot2multiplot.R")


SOIFre <-ggplot(SOI, aes(x=SOI, fill=Fenomeno, color=Fenomeno)) +
          geom_histogram(position="identity", alpha=1)+
          theme_bw(base_size=13)+
          scale_fill_manual(values=c("#496d96", "#fa9040", "#e5dfc6"), " ")+
          labs(x = "The Southern Oscillation Index (SOI)", y ="Frecuencia",colour = " ")+
          theme(legend.justification=c(0,0), legend.position=c(0.1,0.7))
SOIFre

SOICicl<-ggplot( data=SOI, aes(x=Fecha, y=SOI) )            +
          geom_line(col=rgb(0, 0,.9, alpha=.4))    +
          stat_smooth(span=1/12, col='#496d96', se=FALSE)  +       # El Niño
          stat_smooth(col=rgb(.7, 0, .7))+
          theme_bw(base_size=13)+
          labs(y = "The Southern Oscillation Index (SOI)", x ="Año",colour = " ")
SOICicl
        
multiplot(SOIFre,SOICicl, cols = 2)

### IOR

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/VariablesEstrategicas.RData")

library(lubridate)

VE2016 <- subset(VariablesEstrategicas, year(Fecha)==2016)

Plantas <- c("SAN CARLOS","GUAVIO", "CHIVOR","GUATRON", "ALVAN", "GUATAPE")
Plantas <- c("SAN CARLOS","GUAVIO", "CHIVOR")

Plantas2016 <- subset(VE2016[, c(1,2,12)], Recurso %in% Plantas)

library(tidyverse)

Plantas2016Wide <- Plantas2016 %>% spread(Recurso, IOR)

TSPlantas2016Wide <- xts(Plantas2016Wide[,2:4], order.by=as.POSIXct(Plantas2016Wide$Fecha))

ts.plot(TSPlantas2016Wide)

plot(TSPlantas2016Wide$`SAN CARLOS`, main = "IOR",  ylim=c(0.7, 1) )

lines(TSPlantas2016Wide$`SAN CARLOS`, col = "#880000", lwd = 2)

lines(TSPlantas2016Wide$GUAVIO, col = "#375372", lwd = 2)

lines(TSPlantas2016Wide$CHIVOR, col = "#328a2e", lwd = 2)


library(PerformanceAnalytics)

par(mfrow=c(3,1))

chart.TimeSeries(TSPlantas2016Wide$`SAN CARLOS`, ylab='IOR', col="#880000",ylim=c(0.8, 1.03),
  cex.axis=1.5,grid.color="black",cex.main=1.5,cex.lab=1.3)


chart.TimeSeries(TSPlantas2016Wide$GUAVIO, ylab="IOR", col="#375372",ylim=c(0.8, 1.03),
  cex.axis=1.5, grid.color="black",cex.main=1.5)

chart.TimeSeries(TSPlantas2016Wide$CHIVOR, ylab="IOR", col="#328a2e",ylim=c(0.8, 1.03),
  cex.axis=1.5,grid.color="black",cex.main=1.5)



par(mfrow=c(1,1))



## Herfindal

VE2016 <- subset(VariablesEstrategicas, year(Fecha)==2016)

HHI <- VE2016 %>% group_by(Fecha) %>% select(HHIAgentes, HHIPlantas)

HHI <- unique(HHI)

TSHHI<- xts(HHI, order.by=as.POSIXct(HHI$Fecha))

ts.plot(TSHHI)

par(mfrow=c(2,1))

chart.TimeSeries(TSHHI$HHIAgentes, ylab="HHI", col="#d60041",ylim=c(0, 55000),
  cex.axis=1.2,grid.color="black",cex.main=1.5, lwd=3)

chart.TimeSeries(TSHHI$HHIPlantas, ylab="HHI", col="#36384c",ylim=c(0, 5500),
  cex.axis=1.2,grid.color="black",cex.main=1.5, lwd=3)


## Pruebas

table.Stats(TSPlantas2016Wide[, 1:3])

chart.Boxplot(TSPlantas2016Wide[, 1:3])

charts.RollingPerformance(TSPlantas2016Wide[, 1:3]
  , Rf=.03/12, colorset = c("red", "orange","green"), lwd = 2)

chart.RollingCorrelation(TSPlantas2016Wide[, 1:2], TSPlantas2016Wide[,2:3],colorset=rich8equal, legend.loc="bottomright",
  width=6, main = "Rolling 12-Month Correlation")

chart.RelativePerformance(TSPlantas2016Wide[, 1:3],TSPlantas2016Wide[, 1:3], 
  colorset = rainbow8equal, lwd = 2, legend.loc ="topleft")

