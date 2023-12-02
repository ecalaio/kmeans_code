library(readxl)
setwd("C:/Eli compu/Portfolio")
excel<- read_excel("Datos_curados.xlsx")
h <- as.data.frame(excel)

Cliente_ID <- h$Cliente_ID
h$Cliente_ID <- NULL
Id <- h$Id
h$Id <- NULL
territorio <- h$TERRITORIO
h$TERRITORIO <- NULL
conca <- h$conca
h$conca <- NULL

#normalizamos
renormaliza<- function(v){(v-mean(v))/sd(v)}
for(i in 1:ncol(h))
{h[,i]<- renormaliza(h[,i])}

ag.kmeans <- kmeans(h,3)

plot(excel$total_has,excel$margen_2022 ,col=ag.kmeans$cluster)

# el grafico se ve muy apelotonado asi que voy a ver de acotar los ejes x 

which.max(h$total_has)
hr<-h[1:3755,]
hr<-rbind(hr,h[3757:nrow(h),])

#vamos a hacer lo mismo con eje y
which.max(h$margen_2022)
hr2<-hr[1:4425,]
hr2<-rbind(hr2,hr[4427:nrow(hr),])

#al cluster tambien le tenemos que sacar las 2 filas que sacamos la 3756 y la 4426
colores<-ag.kmeans$cluster
colores<-c(colores[1:3755],colores[3757:4426],colores[4428:length(colores)])

plot(hr2$total_has,hr2$margen_2022 ,col=colores)

#otra forma de verlo aumentado, mas simple es cortar el grafico
plot(hr2$total_has,hr2$margen_2022 ,col=colores,xlim = c(0,6),ylim = c(0,3))

#para hacer grafico sobre datos reales, no normalizados

plot(excel$total_has,excel$margen_2022 ,col=ag.kmeans$cluster,main="Clientes segun margen-has", 
     xlab="has",ylab="margen",xlim=c(0,5000),ylim=c(0,100000))
plot(excel$total_has,excel$total_inversion ,col=ag.kmeans$cluster,main="Clientes segun has-inversion", 
     xlab="has",ylab="inversión",xlim=c(0,5000),ylim=c(0,1500000))
plot(excel$total_inversion,excel$Total ,col=ag.kmeans$cluster,main="Clientes segun inversión-Facturación", 
     xlab="Inversión",ylab="Facturación",xlim=c(0,600000),ylim=c(0,600000))
plot(excel$total_has,excel$Total ,col=ag.kmeans$cluster,main="Clientes segun has-Facturación", 
     xlab="has",ylab="Facturación",xlim=c(300,3000),ylim=c(28000,100000))


hist(excel$total_has[which(ag.kmeans$cluster==1)], main = "Q de clientes segun has", 
     xlab= "Has",ylab="Q clientes")
summary(excel$total_has[which(ag.kmeans$cluster==1)])

#quiero saber cuantos clientes tienen cero has
length(which(excel$total_has[which(ag.kmeans$cluster==1)]==0))

#para ver caracteristicas de cada segmento
boxplot(excel$Antigüedad_Cliente~ag.kmeans$cluster)
boxplot(excel$`%_de_recurrencia_compra`~ag.kmeans$cluster)
boxplot(excel$Frecuencia_Compra_Ano~ag.kmeans$cluster,ylim=c(2,15))
boxplot(excel$Dias_ultima_compra~ag.kmeans$cluster,ylim=c(70,500))
boxplot(excel$Categoria_campana~ag.kmeans$cluster)
boxplot(excel$Facturacion_2022~ag.kmeans$cluster,ylim=c(20000,100000))


# es para conocer la paleta de colores
barplot(1:8,col=1:8)

#esto es para exportarlo a csv, para saber la ruta pongo en la consola getwd()
Resultado <- cbind(conca ,ag.kmeans$cluster)
Resultado <- as.data.frame(Resultado)
write.csv(Resultado,"Resultado.csv")

#esto es para ver caracteristicas de los clusters
View(h)

View(excel)
# esto es para ver la tabla de resultados de los clusters-> table(Resultado$V2)

boxplot(excel$total_inversion ~ ag.kmeans$cluster )
excel2 <- excel[which(ag.kmeans$cluster==2),]
excel1 <- excel[which(ag.kmeans$cluster==1),]
excel3 <- excel[which(ag.kmeans$cluster==3),]
summary(excel3$margen_2022)

 summary(excel2$total_inversion)
 summary(excel1$total_inversion)
 summary(excel3$total_inversion)
 summary(excel3$total_has)
 summary(excel1$Facturacion_2022)
 summary(excel2$Facturacion_2022)
 summary(excel3$Facturacion_2022)
 boxplot(excel$Facturacion_2022 ~ ag.kmeans$cluster )

boxplot(excel$margen_2022 ~ excel$Categoria_campana)
boxplot(excel$Facturacion_2022 ~ excel$Qaños_recurrencia_compra)
fe <- excel$Facturacion_2022[which(excel$Qaños_recurrencia_compra==max(excel$Qaños_recurrencia_compra))]
hist(fe)
fp <- h$Facturacion_2022[which(h$persona_fisica==max(h$persona_fisica))]
hist(fp)
# para la proxima estos histogramas pero separados por ag.kmeans$cluster y ver cuando las 2 vbles son discretas
boxplot(excel$`%_de_recurrencia_compra` ~ ag.kmeans$cluster)
boxplot(excel$total_has ~ ag.kmeans$cluster,ylim=c(300,3000))
boxplot(excel$Total ~ ag.kmeans$cluster,ylim=c(20000,70000))
boxplot(excel$Facturacion_2022 ~ ag.kmeans$cluster,ylim=c(20000,50000))
boxplot(excel$cross ~ ag.kmeans$cluster)
boxplot(excel$Antigüedad_Cliente ~ ag.kmeans$cluster)
boxplot(excel$Categoria_campana ~ ag.kmeans$cluster)
boxplot(excel$Frecuencia_Compra_Ano ~ ag.kmeans$cluster,ylim=c(2,8))
boxplot(excel$Dias_ultima_compra ~ ag.kmeans$cluster,ylim=c(0,300))
boxplot(excel$Maiz_temprano + excel$Maiz_tardio + excel$Trigo ~ ag.kmeans$cluster)
boxplot(excel$Qaños_recurrencia_compra  ~ ag.kmeans$cluster)
table(ag.kmeans$cluster)

#esto es para exportarlo a csv, para saber la ruta pongo en la consola getwd()
Resultado <- cbind(conca,ag.kmeans$cluster)
Resultado <- as.data.frame(Resultado)
write.csv(Resultado,"Resultado.csv")

#para buscar la cantidad de centroide optimo

library(fpc)
#no la ejecute porque tarda mucho
t1 <- Sys.time()
ag.pamk <- pamk(h)
t2 <- Sys.time()
t2-t1
ag.pamk$nc
boxplot(excel$Facturacion_2022 ~ ag.pamk$pamobject$clustering )
boxplot(excel$margen_2022 ~ ag.pamk$pamobject$clustering )
boxplot(excel$Antigüedad_Cliente ~ ag.pamk$pamobject$clustering )
boxplot(excel$Frecuencia_Compra_Ano ~ ag.pamk$pamobject$clustering,ylim=c(2,16) )
boxplot(excel$Categoria_campana ~ ag.pamk$pamobject$clustering )
boxplot(excel$total_inversion ~ ag.pamk$pamobject$clustering )
boxplot(excel$total_has ~ ag.pamk$pamobject$clustering,ylim=c(300,3000) )
boxplot(excel$Maiz_temprano + excel$Maiz_tardio ~ ag.pamk$pamobject$clustering )
boxplot(excel$Trigo ~ ag.pamk$pamobject$clustering )
boxplot(excel$Soja_1era + excel$Soja_2da ~ ag.pamk$pamobject$clustering )
ht <- cbind(h$Facturacion_2022,h$Cebada+h$Trigo)
cor(ht)

boxplot(h$cross ~ ag.pamk$pamobject$clustering )
boxplot(h$Trigo + h$Maiz_temprano + h$Maiz_tardio ~ ag.pamk$pamobject$clustering )

hh <- cbind(h$Facturacion_2022, h$Soja_1era, h$Soja_2da,h$Trigo,h$Maiz_temprano, h$Maiz_tardio,
            h$Cebada, h$Girasol)
cor(hh)
pairs(hh)

ag.kmeans <- kmeans(h,3)
plot(excel$total_has,excel$margen_2022 ,col=ag.kmeans$cluster)
boxplot(excel$Maiz_temprano+excel$Maiz_tardio ~ excel$TERRITORIO)
fe <- h$Facturacion_2022[which(h$Categoria_campana)]
hist(fe)

