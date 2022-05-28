#AUTOR John Erik Ibarra Guerrón

install.packages("MASS")
install.packages("reshape2")
install.packages("reshape")
install.packages("factoextra")
install.packages('useful')
install.packages('dendextend')

#Loading the libraries
library(MASS)
library(reshape2)
library(reshape)
library(factoextra)
library(dplyr)
library(useful)
library(dendextend)

#num <- readline("Introduce tamaño máximo del clúster > ")
#num <- as.integer(num)


#Fijamos el número máximo de clusters
num <- 4
tam_df <- num+1

#Lectura del archivo CSV de peatones
df_media_peatones_agosto_20h <-  function(){
  peatones<-read.csv2("./PEATONES_2020.csv")
  library(dplyr)
  library(ggplot2)
  library(dplyr)
  library(useful)
  
  #Obtenemos los campos peatones, dia, mes, año y filtramos a fecha agosto y 20h 
  peatones.class<-peatones[, c(7,4,1,2)]
  peatones.class$dia <- as.numeric(format(as.Date(peatones.class$ï..FECHA, '%d/%m/%Y'),'%d'))
  peatones.class$mes <- as.numeric(format(as.Date(peatones.class$ï..FECHA, '%d/%m/%Y'),'%m'))
  peatones.class$anio <- as.numeric(format(as.Date(peatones.class$ï..FECHA, '%d/%m/%Y'),'%Y'))
  peatones_agosto_20h<-filter(peatones.class, HORA=="20:00", mes==8)
  
  #Creacion del datframes con los campos necesarios
  df <- data.frame(calle = peatones_agosto_20h$NOMBRE_VIAL,
                   peatones = peatones_agosto_20h$PEATONES,
                   dia = peatones_agosto_20h$dia,
                   mes = peatones_agosto_20h$mes,
                   año = peatones_agosto_20h$anio,
                   hora = peatones_agosto_20h$HORA)
  
  #Obtenemos la media 
  media_peatones_agosto_20h <- aggregate(df$peatones, by=list(df$calle, df$dia), FUN=mean)
  
  df_melt <- melt(media_peatones_agosto_20h)
  cols_dias<- reshape(media_peatones_agosto_20h, direction = "wide", idvar = "Group.1", timevar = "Group.2")
  
  #Dataframe final con las columnas nombre de vía y las respectivas columnas de los dias de la semana con la media de peatones
  dataset_final <- data.frame(calle = cols_dias$Group.1, 
                              lunes = cols_dias$x.3+cols_dias$x.10+cols_dias$x.17+cols_dias$x.24+cols_dias$x.31,
                              martes = cols_dias$x.4+cols_dias$x.11+cols_dias$x.18+cols_dias$x.25,
                              miercoles = cols_dias$x.5+cols_dias$x.12+cols_dias$x.19+cols_dias$x.26, 
                              jueves = cols_dias$x.6+cols_dias$x.13+cols_dias$x.20+cols_dias$x.27,
                              viernes = cols_dias$x.7+cols_dias$x.14+cols_dias$x.21+cols_dias$x.28, 
                              sabado = cols_dias$x.1+cols_dias$x.8+cols_dias$x.15+cols_dias$x.22+cols_dias$x.29, 
                              domingo = cols_dias$x.2+cols_dias$x.9+cols_dias$x.16+cols_dias$x.23+cols_dias$x.30)
  
  return(dataset_final)
}

#Completar->JOHN
buscarPrimos <- function(cluster_obs1, cluster_obs2){
  lista_primos <- filter(df,df$cluster_hoja==cluster_obs1 | df$cluster_hoja==cluster_obs2)
  df$agrupado <<- ifelse(df$cluster_hoja == cluster_obs1 | df$cluster_hoja == cluster_obs2, TRUE, df$agrupado)
  return(df)
}

#Completar->JOHN
generaRuta <- function(n, i, lista, ruta_extraida_dendograma) {
  if(!is.numeric(n)) {
    lista[i] <- 1
    ruta_extraida_dendograma <- generaRuta(n[[1]], (i+1), lista, ruta_extraida_dendograma)
    lista[i] <- 2
    ruta_extraida_dendograma <- generaRuta(n[[2]], (i+1), lista, ruta_extraida_dendograma)
    return(ruta_extraida_dendograma)
  } else {
    ruta_extraida_dendograma[[n[1]]] <- lista
    return(ruta_extraida_dendograma)
  }
}

#Completar->JOHN
Obs_in_cluster <- function(l) {
  cont <- 1
  obs <- list()
  cluster <- list()
  for (i in 1:length(l$withinss)) {
    for (j in 1:length(l$cluster)) {
      if (l$cluster[j] == i) {
        obs[cont] <- j
        cont <- cont + 1
      }
    }
    cluster[[i]] <- obs
  }
  return(cluster)
}


"
  elementos_agrupados:  recoge todas las filas que han sido agrupadas en la busqueda de hermanos/primos
  grupos_tamanios:      contiene los clusters y el número de ocurrencias de cada uno
  grupos_grandes:       contiene los clusters que superan el limite de ocurrencias que define el usuario
  mayor:                dataframe que contiene todas las ocurrencias de los grupos que superan los limites
  grupos_divididos:     separamos los grupos que superan los limites
  maximo:               valor maximo de la columna cluster_nuevo
  contador:             cuenta el número de ocurrencias de cada cluster
"
comprobarTamanio <- function(){
  elementos_agrupados <- filter(df, df$agrupado == TRUE)
  grupos_tamanios <- aggregate(elementos_agrupados$cluster_nuevo, by=list(elementos_agrupados$cluster_nuevo), FUN=length)
  grupos_grandes <- filter(grupos_tamanios, grupos_tamanios$x > tam_df-1)$Group.1
  mayor<-df[df$cluster_nuevo %in% grupos_grandes,]
  grupos_divididos <- split(mayor, mayor$cluster_nuevo)
  maximo <- max(df$cluster_nuevo)
  
  
  if(length(grupos_divididos) > 0) {
    for(i in 1:length(grupos_divididos)){
      contador <- 1
      for(j in 1:length(grupos_divididos[[i]]$cluster_nuevo)){
        if(contador%%tam_df==0) {
          maximo <- max(df$cluster_nuevo)+1
          df[df$observacion == grupos_divididos[[i]]$observacion[j],]$cluster_nuevo <<- maximo
          contador <- 1
        } else{
          df[df$observacion == grupos_divididos[[i]]$observacion[j],]$cluster_nuevo <<- maximo
        }
        contador <- contador + 1
      }
      maximo <- max(df$cluster_nuevo) + 1
    }
  }
}

"
Función encargada de buscar aquellas observaciones que sean hermanas
Obs1: observación a comparar
ruta1: ruta de la obs1 extraida del dendograma

obs2: segunda observacion a comparar
ruta2: ruta de la obs2 extraida del dendograma

Funcionamientos:
1 Comprobamos si se ha agrupado la observacion
  Sí -> pasamos a la siguiente observación
  No -> comparamos con el resto de observaciones

2 Comprobamos si se ha agrupado la observación 2
  Sí -> pasamos a la siguiente observación
  No -> pasamos a comprobar las 2 rutas
  

Variables 
tam: Extraemos el tamaño minimo de las dos observaciones, esto se hace para comparar hasta n-1

posicionesDiferentes: Comparamos las dos rutas con ruta1 desde 1 hasta min(ruta1, ruta2) para comparar con ruta2

ultimoValor: Obtenemos el último valor de posicionesDiferentes


Comprobamos si son hermanos y de ser así buscamos primos
"
buscar_hermanos <- function(conjunto) {
  inicio <- as.numeric(conjunto[1, ]$posicion)
  fin <- as.numeric(conjunto[as.numeric(count(conjunto)), ]$posicion)
  
  if(length(fin) <= 0) {
    return()
  }
  
  for(i in inicio:(fin-1)){
    obs1 <- filter(df, df$posicion == i)
    ruta1 <- as.list(strsplit(obs1$ruta, ","))[[1]]
    if(obs1$agrupado == FALSE){
      for(j in (inicio+1):fin){
        obs2 <- filter(df, df$posicion == j)
        ruta2 <- as.list(strsplit(obs2$ruta, ","))[[1]]
        if(obs2$agrupado != TRUE){
          tam <- min(obs1$tamanio, obs2$tamanio)
          if(length(ruta1) >= length(ruta2)){
            posicionesDiferentes <- compare.list(ruta1[1:tam], ruta2)
          } else{
            posicionesDiferentes <- compare.list(ruta2[1:tam], ruta1)
          }
          ultimoValor <- posicionesDiferentes[tam]
          if(length(which(posicionesDiferentes == FALSE)) == 1 && ultimoValor == FALSE) {
            df <<- buscarPrimos(obs1$cluster_hoja, obs2$cluster_hoja)
          }
        }
      }
    }
  }
  df$cluster_nuevo <<- ifelse(df$agrupado == TRUE, df$cluster_nuevo+1, df$cluster_nuevo)
}


lista <- list()
ruta_extraida_dendograma <- c()
resultados <- list()
posicion <- c(1:18)
ruta <- c()
posiciones <- c()
tamanio <- c()
cluster_ordenado <- c()
visitado <- c()
accuracy <-c()

peatones_agos_20horas_2020 <- df_media_peatones_agosto_20h()

#calculamos la matriz de distancias para 7 variables
peatones_agos_20horas_2020.dist<-dist(as.matrix(peatones_agos_20horas_2020[,2:8], method = "euclidean"))

#Inicio de aplicación de KMEANS
#Datos control Contiene los datos de los días de la semana
datos_control<-as.matrix(peatones_agos_20horas_2020[,2:8])

#Aplicamos Kmeans y se crea dos listas una con los resultados de KMEANS llamada clasifica y otra con
#los valores de precisión, luego se crea un dataframe con la calle y el clúster al que pertenece
K_value<-4:12
for(cont in K_value) {
  clasifica<-kmeans(datos_control, centers=cont)
  accuracy[cont]<-clasifica$betweenss/clasifica$tot.withinss
  resultado<-kmeans(as.matrix(peatones_agos_20horas_2020[,2:8]), centers=cont)
  print(resultado)
  #resultados<-data.frame(peatones_agos_20horas_2020$calle,clasifica_final$cluster)
}

#Seleccionamos el primer máximo encontrado
i<- 4
while (i < length(accuracy)-1 && accuracy[i] < accuracy[i+1]) {
  i <- i+1
}
maximo <- i

#Aplicamos KMEANS con el número máximo de centers obtenido del accuracy
clasifica_final<-kmeans(as.matrix(peatones_agos_20horas_2020[,2:8]), centers=maximo)
resultados_final<-data.frame(peatones_agos_20horas_2020$calle,clasifica_final$cluster)

#Pintamos el plot
plot(K_value, accuracy[4:12], type="l")


#HCLUST
# Cálculo de hierarchical clustering
hc_average <- hclust(peatones_agos_20horas_2020.dist, method = "average")

#Obtenemos la ruta de listas
ruta_dendograma <- as.dendrogram(hc_average) 
par(mar=c(9,7,1,1))

ruta_dendograma %>% 
  # Custom branches
  set("branches_col", "black") %>% set("branches_lwd", 1) %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  set("nodes_col", "red") %>%
  plot(main="Peatones clustering")

hc_average$labels <- peatones_agos_20horas_2020$calle
ruta_dendograma <- as.dendrogram(hc_average) 
par(mar=c(9,7,1,1))

ruta_dendograma %>% 
  # Custom branches
  set("branches_col", "black") %>% set("branches_lwd", 1) %>%
  set("labels_col", value = c("skyblue", "orange", "grey"), k=3) %>%
  set("branches_k_color", value = c("skyblue", "orange", "grey"), k = 3) %>%
  set("leaves_pch", 19)  %>% 
  set("nodes_cex", 0.7) %>% 
  set("nodes_col", "red") %>%
  plot(main="Peatones clustering")

#Extraemos la ruta de cada observacion
ruta_extraida_dendograma <- generaRuta(ruta_dendograma, 1, lista, ruta_extraida_dendograma)

cluster <- resultados_final$clasifica_final.cluster

for (i in 1:18) {
  posiciones[i] <- length(unlist(ruta_extraida_dendograma[i]))
}

#ordenamos las observaciones en funcion de 
observacion <- order(-posiciones)


for (i in 1:18) {
  ruta[i] <- toString(unlist(ruta_extraida_dendograma[observacion[i]]))
  tamanio[i] <- length(unlist(ruta_extraida_dendograma[observacion[i]]))
  cluster_ordenado[i] <- cluster[observacion[i]]
}


#cluster_hoja <- c(6,6,3,3,6,6,7,3,7,2,2,7,3,1,4,5,8,4)

cluster_nuevo <- replicate(18, 0)
agrupado <- replicate(18, FALSE)
encolado <- replicate(18, FALSE)

cluster_hoja <- cluster

#cluster_hoja <- c(1,7,1,7,5,1,10,4,4,8,12,5,4,4,9,3,1,1,1,9,8,9,6,12,10,8,2,11,8,8)

#CREAMOS EL DATAFRAME CON EL QUE VAMOS A TRABAJAR
df <- data.frame(posicion,
                 observacion,
                 ruta,
                 tamanio,
                 cluster_hoja,
                 cluster_nuevo,
                 agrupado)
caminos <- strsplit(ruta, split = ', ')

#Seleccionamos aquellos elementos que no han sido agrupados o cumplen la condición de tener el mismo
#o tamanio o tamanio-1, posteriomente si el conjunto tiene más de 1 elemento se busca hermanos
#de dicho conjunto sino

for (i in unique(df$tamanio)) {
  conjunto <- filter(df, df$agrupado == FALSE & (df$tamanio == i | df$tamanio == i-1))
  if(count(conjunto) > 1) {
    buscar_hermanos(conjunto)
  }
  else if(count(conjunto) == 1) {
    df$cluster_nuevo <- df$cluster_nuevo+1
    df$agrupado <- ifelse(df$posicion == conjunto$posicion, TRUE, df$agrupado)
  }
}

print(df)

#comprobamos si cumple el criterio de tamanios del cluster y subdividimos en caso contrario
comprobarTamanio()
print(df)
