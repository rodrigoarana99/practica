setwd ("C:/Users/gonza/OneDrive/Documentos/Escritorio/MASTER_UBA/Reduccion_visualizacion_datos")

# importamos el dataset
paises=read.csv("paises_mundo.csv")

# recortamos la primer columna del dataset para
# quedarnos con las columnas numericas
paises_num=paises[,2:6]


# usamos la libreria psych para hacer scatterplots de pares
#method:Este argumento especifica que se debe calcular la correlación de Pearson entre las variables numéricas para mostrarla en la matriz de gráficos. La correlación de Pearson mide la relación lineal entre dos variables.
#density = TRUE: Este argumento indica que se deben mostrar gráficos de densidad superpuestos a los histogrmas en la diagonal.

library(psych)

pairs.panels(paises_num, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)



# generamos la matriz X de datos
par(mfrow=c(1,1))
X=as.matrix(paises_num)
sigma_muestral=cov(X)
eig=eigen(sigma_muestral)
pc=eig$vectors
pc
scores_dim2=X%*%pc[,1:2]
scores_dim2
colnames(scores_dim2) <- c("E1", "E2")
#X=as.matrix(paises_num): Convierte el conjunto de datos paises_num en una matriz llamada X. Esto es necesario para realizar cálculos matriciales en el PCA.
#sigma_muestral=cov(X): Calcula la matriz de covarianza muestral (sigma_muestral) de los datos en X. Esta matriz se utiliza en el PCA para calcular las direcciones principales de la variación en los datos.
#eig=eigen(sigma_muestral): Calcula los valores propios y vectores propios de la matriz de covarianza muestral. Estos valores propios representan la varianza explicada por cada componente principal, y los vectores propios representan las direcciones de los componentes principales
#pc=eig$vectors: Almacena los vectores propios (componentes principales) en la variable pc. Estos vectores son las direcciones principales en las que los datos muestran la mayor variación.
#scores_dim2=X%*%pc[,1:2]: Calcula las puntuaciones (scores) de las dos primeras dimensiones principales para cada observación en los datos originales. Esto reduce la dimensionalidad de los datos originales a dos dimensiones principales.



biplot(main="Biplot datos no estandarizados PCA",x =cbind(scores_dim2[,1],scores_dim2[,2]),y=pc[,1:2],xlabs=paises$Nombre,ylabs = colnames(paises_num)
       ,cex = 0.6,xlab="PC1",ylab="PC2"
       )

# propocion de la varianza explicada
sum(eig$values[1:2])/sum(eig$values)
#Este fragmento de código calcula la proporción de la varianza explicada por las dos primeras dimensiones principales obtenidas mediante el análisis de componentes principales (PCA).
library(ComplexHeatmap)
matriz_total = cbind(X,scores_dim2)
cor_mat=cor(matriz_total)
Heatmap(matrix = cor_mat,show_row_dend = FALSE,show_column_dend =  FALSE,name = "X,E1,E2")
#Este fragmento de código  genera un mapa de calor (heatmap) para visualizar las correlaciones entre las variables originales y los nuevos componentes obtenidos mediante el análisis de componentes principales (PCA).
#El resultado es un mapa de calor que representa gráficamente las correlaciones entre las variables originales y los nuevos componentes en las dos primeras dimensiones principales. Esto puede ayudar a visualizar cómo las variables originales se relacionan con los componentes principales y entre sí.


#----------------- DATOS ESTANDARIZADOS Y CON LOGARITMO -------------------
#se transforman cada una de las columnas numericas a logaritmo
paises_num_log=paises_num
paises_num_log$mortinf=log(paises_num_log$mortinf)
paises_num_log$PNB =log(paises_num_log$PNB)
paises_num_log$prod_elec=log(paises_num_log$prod_elec)
paises_num_log$cons_energia=log(paises_num_log$cons_energia)
paises_num_log$co2 =log(paises_num_log$co2)



# A partir de los datos de de logaritmo, genero la matriz de datos centrada y estandarizada:
Zl=scale(as.matrix(paises_num_log))

# anaslisis exploratorio luego de la transformacion
pairs.panels(Zl, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = FALSE # show correlation ellipses
)

# volvemos a buscar los componentes principales, peor ahora de los datos estandarizados
sigma_muestral_z=cov(Zl)
eig_z=eigen(sigma_muestral_z)
pcz=eig_z$vectors
pcz
scores_z_dim2=Zl%*%pcz[,1:2]
scores_z_dim2
colnames(scores_z_dim2) <- c("E1", "E2")


biplot(main="Biplot datos Transformados PCA",x =cbind(scores_z_dim2[,1],scores_z_dim2[,2]),y=pcz[,1:2],xlabs=paises$Nombre,ylabs = colnames(paises_num)
       ,cex = 0.6,xlab="PC1",ylab="PC2"
)


# propocion de la varianza explicada
sum(eig_z$values[1:2])/sum(eig_z$values)


# heatmap de correlaciones:
matriz_total_z = cbind(Zl,scores_z_dim2)
cor_matz=cor(matriz_total_z)
Heatmap(matrix = cor_matz,show_row_dend = FALSE,show_column_dend =  FALSE,name = "Z,E1,E2")

