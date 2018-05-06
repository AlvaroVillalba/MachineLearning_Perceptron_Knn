### "K vecinos mpas cercanos" sobre los datos artificiales de la clase para
### estimar Sexo a partir de Peso y Estatura


### Debe comentar este código indicando los pasos más importantes
### e identificando todas las líneas que corresponden efectivamente
### al algoritmo de k-nn.

# Preámbulo
rm(list=ls())
graphics.off()

X <- readRDS("X.rds")

### ¿Qué pasa si no se hace esta normalización?
X[,c(2,3)] <- X[,c(2,3)]/100

X$G <- substr(X$Sexo, 1, 1)
X$g <- as.numeric(X$G=="M")


l <- nrow(X)

# Visualización
x11()
plot(X$Estatura, X$Peso, type = "n", xlab="Estatura", ylab="Peso",asp=1,
xlim=c(1,2.5),ylim=c(0,1.5))
cols <- rep("blue",nrow(X))
cols[which(X$G=="M")] <- "black"
#text(X$Estatura, X$Peso, X$G, col=cols)
points(X$Estatura, X$Peso, pch=16, col=cols)


# Límites de simulación, basados en {estatura, peso}/100
lims <- c(1, 2.5,#Estatura, eje X
          0, 1.5) #Peso, eje Y


nuevopunto <- function(lims=lims){
  a <- runif(1,lims[1],lims[2])
  b <- runif(1,lims[3],lims[4])
  return(c(a, b))
}


### Parámetro k del algoritmo.
### Evalúe el comportamiento para k={1, 3}
### ¿Qué problema hay con k=2? ¿Cómo se puede resolver?
k <- 2


for(iter in 1:20000){
  x <- nuevopunto(lims)
  diss <- dist(21]
  knn <- head(order(diss),k)
  etiqueta <- names(sort(table(X$G[knn]),decreasing = TRUE))[1]
  clase <- as.numeric(etiqueta=="M")
  for(inn in 1:k){
    if(X$G[knn[inn]]==etiqueta){
      ### Ensaye ambos tipos de visualización
      #lines(c(x[1],X$Estatura[knn[inn]]),c(x[2],X$Peso[knn[inn]]),
      #      col=c("blue","black")[clase+1])
      points(x[1],x[2],col=c("blue","black")[clase+1],pch=20)
    }
  }
}

cols2 <- rep("red",nrow(X))
cols2[which(X$G=="M")] <- "green"
points(X$Estatura, X$Peso, pch=16, col=cols2)


