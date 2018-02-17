
library(lubridate)
library(tidyverse)




load('C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOfertaAgentes2016.RData')


PreciosOfertaAgentes2016Cor <- PreciosOfertaAgentes2016[ , names(PreciosOfertaAgentes2016) %in% 
                                        c('CodigoAgente', 'Fecha','PrecioOfertaIdeal$/kwh')]


PreciosOfertaAgentes2016Cor$id <- 1:nrow(PreciosOfertaAgentes2016Cor)

PreciosOfertaAgentes2016Corwide <- spread(PreciosOfertaAgentes2016Cor, key ='CodigoAgente', 
                                  'PrecioOfertaIdeal$/kwh')

PreciosOfertaAgentes2016Corwide <- PreciosOfertaAgentes2016Corwide[, -2]  # Quitar el id

PreciosOfertaAgentes2016Corwide[is.na(PreciosOfertaAgentes2016Corwide)] <- 0

PreciosOfertaAgentes2016Corwide <- aggregate(PreciosOfertaAgentes2016Corwide[, 2:20],FUN = sum,by = list(Group.date = PreciosOfertaAgentes2016Corwide$Fecha))


#  Sacar a TMVG, PRIG, TMFG, TCIG ,TEMG;  8 9  12 16 17

Cor <- cor(as.matrix(PreciosOfertaAgentes2016Corwide[, 2:20][, c(-7,-9,-11, -15,-16)]))

cor(PreciosOfertaAgentes2016Corwide$EPMG, PreciosOfertaAgentes2016Corwide$ISGG)

## Matriz Sentilla  de correlaciones de precios de todos los agentes con el portafolio completo

library(Hmisc)

rcorr(as.matrix(PreciosOfertaAgentes2016Corwide[, 2:20]), type="pearson")


# Matriz esteticamente más elaborada

library(corrplot) 
library(RColorBrewer)


corrplot.mixed(Cor, upper="ellipse", tl.pos = "lt", diag = "u",col=brewer.pal(n=8, name="RdYlBu"))


cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}


p.mat <- cor.mtest(PreciosOfertaAgentes2016Corwide[, 2:20][, c(-7,-9,-11, -15,-16)])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
  ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(Cor, method="color", col=col(200),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.mat, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)

# Precio promedio de oferta ideal por agente

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOfertaAgentes.RData")

tapply(PreciosOfertaAgentes$`PrecioOfertaIdeal$/kwh`, PreciosOfertaAgentes$`CodigoAgente`, mean)


#####

mean(PreciosOfertaAgentes$`PrecioOfertaIdeal$/kwh`, na.rm = T)

PreciosOfertaAgentes2016Cor <- subset(PreciosOfertaAgentes,year(Fecha) ==2016)

PreciosOfertaAgentes2016Cor2 <- PreciosOfertaAgentes2016[, c(2:4,10)]

PreciosOfertaAgentes2016Cor <- PreciosOfertaAgentes2016[, names(PreciosOfertaAgentes2016) %in%
                                c('CodigoAgente', 'Fecha','PrecioOfertaIdeal$/kwh','TipoGeneracion')]


PreciosOfertaAgentes2016CorHidro <- subset(PreciosOfertaAgentes2016Cor,TipoGeneracion =='HIDRAULICA')

PreciosOfertaAgentes2016CorTerm <- subset(PreciosOfertaAgentes2016Cor,TipoGeneracion =='TERMICA')

#### HIdro


PreciosOfertaAgentes2016CorHidro$id <- 1:nrow(PreciosOfertaAgentes2016CorHidro)

PreciosAgenes2016Hidrowide <- spread(PreciosOfertaAgentes2016CorHidro, key ='CodigoAgente', 
  'PrecioOfertaIdeal$/kwh')

PreciosAgenes2016Hidrowide <- PreciosAgenes2016Hidrowide[, -2]  # Quitar el id

PreciosAgenes2016Hidrowide[is.na(PreciosAgenes2016Hidrowide)] <- 0

PreciosAgenes2016Hidrowide <- aggregate(PreciosAgenes2016Hidrowide[, 2:7],FUN = sum,by = list(Group.date = PreciosAgenes2016Hidrowide$Fecha))


CorHidro <- cor(as.matrix(PreciosAgenes2016Hidrowide[, 3:7]))

CorHidro(PreciosAgenes2016Hidrowide$EPMG, PreciosAgenes2016Hidrowide$ISGG)

library(Hmisc)

rcorr(as.matrix(PreciosAgenes2016Hidrowide[, 3:7]), type="pearson")

library('corrplot') 
library(RColorBrewer)


corrplot.mixed(CorHidro, upper="ellipse", tl.pos = "lt", diag = "u")


p.mat <- cor.mtest(PreciosAgenes2016Hidrowide[, 3:7])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
  ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(CorHidro, method="color", col=col(200),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.mat, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)




#### TERMO


PreciosOfertaAgentes2016CorTerm$id <- 1:nrow(PreciosOfertaAgentes2016CorTerm)

PreciosOfertaAgentes2016CorTermwide <- spread(PreciosOfertaAgentes2016CorTerm, key ='CodigoAgente', 
  'PrecioOfertaIdeal$/kwh')

PreciosOfertaAgentes2016CorTermwide <- PreciosOfertaAgentes2016CorTermwide[, -2]  # Quitar el id

PreciosOfertaAgentes2016CorTermwide[is.na(PreciosOfertaAgentes2016CorTermwide)] <- 0

PreciosOfertaAgentes2016CorTermwide <- aggregate(PreciosOfertaAgentes2016CorTermwide[, 2:17],FUN = sum,by = list(Group.date = PreciosOfertaAgentes2016CorTermwide$Fecha))


Cor <- cor(as.matrix(PreciosOfertaAgentes2016CorTermwide[, 3:17]))

cor(PreciosOfertaAgentes2016CorTermwide$EPMG, PreciosOfertaAgentes2016CorTermwide$ISGG)

library(Hmisc)

rcorr(as.matrix(PreciosOfertaAgentes2016CorTermwide[, 3:17]), type="pearson")

library('corrplot') 
library(RColorBrewer)


corrplot.mixed(Cor, upper="ellipse", tl.pos = "lt", diag = "u")


p.mat <- cor.mtest(PreciosOfertaAgentes2016CorTermwide[, 3:17])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
  ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(Cor, method="color", col=col(200),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.mat, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)


 #  Sacar a TMVG, PRIG, TMFG, CIVG,-6,-10, -13, -14

CorTerm <- cor(as.matrix(PreciosOfertaAgentes2016CorTermwide[, 3:17][,c( -4,-8, -11, -12)]))
CorTerm

p.matTerm <- cor.mtest(PreciosOfertaAgentes2016CorTermwide[, 3:17][,c( -4,-8, -11, -12)])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
                         ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(CorTerm, method="color", col=col(200),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.matTerm, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)


par(mfrow=c(1,2))


p.matHidro <- cor.mtest(PreciosAgenes2016Hidrowide[, 3:7])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
  ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(CorHidro, method="color", col=col(300),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.matHidro, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)

p.matTerm <- cor.mtest(PreciosOfertaAgentes2016CorTermwide[, 3:17][,c( -4,-8, -11, -12)])


col <- colorRampPalette(c("#a50026", "#d73027", "#f46d43", "#fdae61", "#fee090", "#ffffbf"
  ,"#e0f3f8", "#abd9e9", "#74add1", "#4575b4", "#313695"))

corrplot(CorTerm, method="color", col=col(200),  
  type="upper", order="hclust", 
  addCoef.col = "black", # Add coefficient of correlation
  tl.col="black", tl.srt=45, #Text label color and rotation
  # Combine with significance
  p.mat = p.matTerm, sig.level = 0.01, 
  # hide correlation coefficient on the principal diagonal
  diag=FALSE 
)

par(mfrow=c(1,1))

