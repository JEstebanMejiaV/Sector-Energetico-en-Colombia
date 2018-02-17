
#------------------------------------------------------#
## Pre-Procesamiento ###################################
#------------------------------------------------------#

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosOferta.RData")


## Precios Bolsa

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/PreciosBolsa.RData")


PreciosBolsa$Promdia <-  rowMeans(PreciosBolsa[,2:25] , na.rm=T)

PreciosBolsa$Fecha <- as.Date(PreciosBolsa$Fecha ,  format = "%Y-%m-%d")



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

PreciosOferta$Promdia <- vlookup(PreciosOferta$Fecha,table = PreciosBolsa, 27)

PreciosOferta$DesvPrecio <- round(abs(PreciosOferta$`PrecioOfertaIdeal$/kwh` - PreciosOferta$Promdia), 2)

PreciosOferta$Salio <- ifelse(PreciosOferta$DesvPrecio < 10 , 'Si', 'No')



library(dplyr)


AgentesColocadoresPrecio <- PreciosOferta %>% group_by(Fecha) %>% 
  filter(abs(`PrecioOfertaIdeal$/kwh` - Promdia) == 
      min(abs(`PrecioOfertaIdeal$/kwh` - Promdia)))

AgentesColocadoresPrecio$ColocoPrecio <- 'Si'


PreciosOferta <-left_join(PreciosOferta, AgentesColocadoresPrecio, by = c("Fecha" = "Fecha", "Recurso" = "Recurso"))    

PreciosOferta$ColocoPrecio <- ifelse(is.na(PreciosOferta$ColocoPrecio) == TRUE, 'No', PreciosOferta$ColocoPrecio )


PreciosOferta <- PreciosOferta[, -(12:20)]

NombresColumnas <- c("Fecha", "Recurso", " CodigoAgente", "PrecioOfertaIdeal$/kwh",
  "PrecioOfertaDespacho$/kwh", "PrecioOfertaDeclarado$/kwh", "Dia", "TipoDia", "PromDia", 
  "DesvPrecio", "CercanoPrecioMarginal", "ColocoPrecio")

colnames(PreciosOferta) <- NombresColumnas

## Caractiristicas de los agentes
library(xlsx)
library(lubridate)

AgentesInfo <- read.xlsx2('InfoAgentes.xlsx',sheetName='Datos', endRow=NULL,as.data.frame=TRUE, header=T,encoding = "UTF-8"
                           ,colClasses = c("character","character","character","character","character","character"
                                            ,"numeric","character","character","character","character","character","character"
                                            ,"character","numeric","numeric","character")   )


CaracteristicasAgentes <- c('CodigoAgente','Recurso','CapacidadInstalada.MW.','TipoDespacho','TipoGeneracion',
                            'Departamento','Ciudad','TipoCompania','EmbalsePPAl','RioPPAl','VolumenMaximoTecnico.GWh.',
                            'VolumenMaximoUtil.GWh.')


DatosModelamiento <- merge(PreciosOferta, AgentesInfo[ , names(AgentesInfo) %in% CaracteristicasAgentes], 
  by.x = c('Recurso', ' CodigoAgente'), 
  by.y = c('Recurso', 'CodigoAgente'), all.x = T)



DatosModelamiento <- DatosModelamiento[order(DatosModelamiento$Fecha),]

outlier <- data.frame()

for (i in unique(year(DatosModelamiento$Fecha))){
  
  temp <- subset(DatosModelamiento, year(DatosModelamiento$Fecha) == i)
  
  for ( j in unique(DatosModelamiento$Recurso)){

  temp2 <- subset(temp, temp$Recurso == j)  
  out <-  boxplot.stats(temp2$`PrecioOfertaIdeal$/kwh`)$out
  ind <- which(temp2$`PrecioOfertaIdeal$/kwh` %in% out)
  outlier <- rbind(outlier, temp2[ind,])
  
  }
}

outlier$Anomalo <- 'Si'

outlier <- outlier[order(outlier$Fecha),]


library(dplyr)


DatosModelamiento2 <- left_join(DatosModelamiento, outlier[,c(1,3,23)], by = c("Fecha" = "Fecha", "Recurso"="Recurso"))

DatosModelamiento2 <- DatosModelamiento2[order(DatosModelamiento2$Fecha),]

DatosModelamiento2$Anomalo[is.na(DatosModelamiento2$Anomalo)] <- 0

## Otras Variables

DatosModelamiento2$yrm <- format(as.Date(DatosModelamiento2$Fecha), "%Y-%m")

DatosModelamiento2$yrm <- as.factor(DatosModelamiento2$yrm)

OcilacionSur <- read.xlsx2('Indice Ocilacion sur.xlsx',sheetName='Datos', endRow=NULL,as.data.frame=TRUE, header=T,encoding = "UTF-8",
                            colClasses =c("character", "character","numeric","numeric","character","character") )

OcilacionSur$Fecha <- as.Date(OcilacionSur$Fecha ,  format = "%Y-%m-%d")


OcilacionSur$Fecha <- as.factor(OcilacionSur$Fecha)

OcilacionSur$yrm <- format(as.Date(OcilacionSur$yrm), "%Y-%m")

DatosModelamiento2 <- left_join(DatosModelamiento2, OcilacionSur[ ,c(3,6)], by = c("yrm" = "yrm"))


DatosModelamiento2 <- DatosModelamiento2[order(DatosModelamiento2$Fecha),]

### Variables Estrategicas

load("C:/Users/admin/Desktop/Tesis Maestria/Datos Preparados/VariablesEstrategicas.RData")

VariablesEstrategicas$Fecha <- as.Date(VariablesEstrategicas$Fecha ,  format = "%Y-%m-%d")


DatosModelamiento2 <- left_join(DatosModelamiento2, VariablesEstrategicas[ ,c(1:2,9:17)], by = c("Fecha" = "Fecha","Recurso" = "Recurso"))

DatosModelamiento2 <- subset(DatosModelamiento2, DatosModelamiento2$CapacidadInstalada.MW. >20)

### Modelameinto


VariablesCluster <- c('ColocoPrecio','TipoGeneracion','Departamento','TipoCompania',
  'Anomalo', 'Dia', 'TipoDia','SOI','CapacidadInstalada.MW.',"PrecioOfertaIdeal$/kwh")

ModA <- DatosModelamiento2[, names(DatosModelamiento2) %in% VariablesCluster]

library(rpart); library(partykit)

ModA$Anomalo = as.factor(ModA$Anomalo)
ModA$ColocoPrecio = as.factor(ModA$ColocoPrecio)
ModA$Dia = as.factor(ModA$Dia)
ModA$TipoDia = as.factor(ModA$TipoDia)

homeless.rpart = rpart(Anomalo ~ ., method="class", data=ModA)
plot(homeless.rpart)
text(homeless.rpart)

printcp(homeless.rpart)

plot(as.party(homeless.rpart), type="simple")

library(party)

homeless.ctree = ctree(Anomalo ~ TipoGeneracion+CapacidadInstalada.MW.+SOI, data=ModA ,controls = ctree_control(maxdepth=4))

homeless.ctree

plot(homeless.ctree)

##########

findsplit <- function(response, data, weights, alpha = 0.01) {
  
     ## extract response values from data
       y <- factor(rep(data[[response]], weights))
    
       ## perform chi-squared test of y vs. x
      mychisqtest <- function(x) {
         x <- factor(x)
         if(length(levels(x)) < 2) return(NA)
         ct <- suppressWarnings(chisq.test(table(y, x), correct = FALSE))
         pchisq(ct$statistic, ct$parameter, log = TRUE, lower.tail = FALSE)
        }
       xselect <- which(names(data) != response)
       logp <- sapply(xselect, function(i) mychisqtest(rep(data[[i]], weights)))
       names(logp) <- names(data)[xselect]
      
         ## Bonferroni-adjusted p-value small enough?
         if(all(is.na(logp))) return(NULL)
       minp <- exp(min(logp, na.rm = TRUE))
       minp <- 1 - (1 - minp)^sum(!is.na(logp))
       if(minp > alpha) return(NULL)
      
         ## for selected variable, search for split minimizing p-value
         xselect <- xselect[which.min(logp)]
       x <- rep(data[[xselect]], weights)
      
         ## set up all possible splits in two kid nodes
         lev <- levels(x[drop = TRUE])
       if(length(lev) == 2) {
         splitpoint <- lev[1]
         } else {
           comb <- do.call("c", lapply(1:(length(lev) - 2),
             function(x) combn(lev, x, simplify = FALSE)))
           xlogp <- sapply(comb, function(q) mychisqtest(x %in% q))
           splitpoint <- comb[[which.min(xlogp)]]
        }
      
         ## split into two groups (setting groups that do not occur to NA)
         splitindex <- !(levels(data[[xselect]]) %in% splitpoint)
       splitindex[!(levels(data[[xselect]]) %in% lev)] <- NA_integer_
       splitindex <- splitindex - min(splitindex, na.rm = TRUE) + 1L
      
         ## return split as partysplit object
         return(partysplit(varid = as.integer(xselect),
           index = splitindex,
           info = list(p.value = 1 - (1 - exp(logp))^sum(!is.na(logp)))))
     }

growtree <- function(id = 1L, response, data, weights, minbucket = 30) {
  
    ## for less than 30 observations stop here
     if (sum(weights) < minbucket) return(partynode(id = id))
  
     ## find best split
     sp <- findsplit(response, data, weights)
     ## no split found, stop here
       if (is.null(sp)) return(partynode(id = id))
    
     ## actually split the data
       kidids <- kidids_split(sp, data = data)
      
         ## set up all daugther nodes
         kids <- vector(mode = "list", length = max(kidids, na.rm = TRUE))
         for (kidid in 1:length(kids)) {
           ## select observations for current node
             w <- weights
             w[kidids != kidid] <- 0
             ## get next node id
               if (kidid > 1) {
                 myid <- max(nodeids(kids[[kidid - 1]]))
                 } else {
                   myid <- id
                   }
             ## start recursion on this daugther node
               kids[[kidid]] <- growtree(id = as.integer(myid + 1), response, data, w)
               }
        
           ## return nodes
           return(partynode(id = as.integer(id), split = sp, kids = kids,
             info = list(p.value = min(info_split(sp)$p.value, na.rm = TRUE))))
         }

mytree <- function(formula, data, weights = NULL) {
  
     ## name of the response variable
     response <- all.vars(formula)[1]
     ## data without missing values, response comes last
       data <- data[complete.cases(data), c(all.vars(formula)[-1], response)]
       ## data is factors only
         stopifnot(all(sapply(data, is.factor)))
      
        if (is.null(weights)) weights <- rep(1L, nrow(data))
         ## weights are case weights, i.e., integers
             stopifnot(length(weights) == nrow(data) &
                 max(abs(weights - floor(weights))) < .Machine$double.eps)
        
             ## grow tree
             nodes <- growtree(id = 1L, response, data, weights)
            
               ## compute terminal node number for each observation
               fitted <- fitted_node(nodes, data = data)
               ## return rich constparty object
                 ret <- party(nodes, data = data,
                   fitted = data.frame("(fitted)" = fitted,
                     "(response)" = data[[response]],
                     "(weights)" = weights,
                     check.names = FALSE),
                   terms = terms(formula))
                 as.constparty(ret)
                 }


homeless.ctree = mytree(Anomalo ~ TipoGeneracion, data=ModA )

plot(homeless.ctree)
## Podar

nid <- nodeids(homeless.ctree)

iid <- nid[!(nid %in% nodeids(homeless.ctree, terminal = TRUE))]

(pval <- unlist(nodeapply(homeless.ctree, ids = iid,
    FUN = function(n) info_node(n)$p.value)))


myttnc2 <- nodeprune(homeless.ctree)

#####
churn.cp = homeless.rpart$cptable[which.min(homeless.rpart$cptable[,"xerror"]),"CP"]

prune.tree = prune(homeless.rpart, cp= churn.cp)

plot(prune.tree, margin= 0.1)
text(prune.tree, all=TRUE , use.n=TRUE)

#------------------------------------------------------#
## Clusters ###################################
#------------------------------------------------------#


rownames(DatosModelamiento) <- DatosModelamiento$` CodigoAgente`

DatosModelamiento2016 <- subset(DatosModelamiento, year(Fecha)==2016)

VariablesCluster <- c('PrecioOfertaIdeal$/kwh','ColocoPrecio','TipoGeneracion','Departamento',
                      'TipoCompania')

Cluster <- DatosModelamiento2016[, names(DatosModelamiento2016) %in% VariablesCluster]

Cluster$ColocoPrecio <- as.factor(Cluster$ColocoPrecio)

library("cluster")


diss<-suppressWarnings(daisy(Cluster, metric = "gower"))





