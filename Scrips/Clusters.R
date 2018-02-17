library(dplyr)

PromVarEstrAgente <- aggregate(VariablesEstrategicas[, 12:17], list(VariablesEstrategicas$CodigoAgente), median,na.rm=T)

PromVarEstrPlanta <- aggregate(VariablesEstrategicas[, 12:17], list(VariablesEstrategicas$Recurso), median,na.rm=T)


Clusters <- left_join(AgentesInfo, PromVarEstrAgente[ ,c(1:2,5, 7)], by = c("CodigoAgente" = "Group.1"))


Clusters <- left_join(Clusters, PromVarEstrPlanta[ ,c(1,3,6)], by = c("Recurso" = "Group.1"))

Clusters <- subset(Clusters, Estado =='OPERACION' & CapacidadInstalada.MW.> 20)



ClusterPlantas <- Clusters[, c(2, 7, 9,10, 18, 22)]

ClusterPlantas <- ClusterPlantas[!duplicated(ClusterPlantas$Recurso),]

library("cluster")
library(amap)

ClusterPlantas.names <- ClusterPlantas[, 1]

ClusterPlantas <- data.frame(row.names = ClusterPlantas.names, ClusterPlantas[2:6])

diss<-suppressWarnings(daisy(ClusterPlantas, metric = "gower"))

h.factor <- hclusterpar(na.omit(diss,method="complete",nbproc=3))


plot(h.factor, hang = -1,main="Cluster Dendrogram")
















