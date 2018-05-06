plotcd <- function(v){
  x <- matrix(v,64,64)
  image(1:65,1:65,t(apply(x,2,rev)),asp=1,xaxt="n",yaxt="n",
        col=grey((0:255)/255),ann=FALSE,bty="n")
}


### Ejemplo de uso.

load("gatosperros.Rdata")
graphics.off()
x11()
plotcd(dm[sample(1:198,1),])
dim(dm)

PCA <- prcomp(dm, scale = TRUE)
plot(PCA)
biplot(prcomp(dm, scale = TRUE))

plot(PCA$sdev)


plot(cumsum(PCA$sdev^2)/sum(PCA$sdev^2))
abline(h=0.5, col="blue")
abline(h=0.8, col="red")

#model.knn <- knn(train=X[trainIndex,], test=X[trainIndex,], cl=responseY[trainIndex], k=k, prob=F)
#library(class)
#knn(PCA[1:2], k = 3)

library("FactoMineR")
library("Factoshiny")
library("foreign")
library("PerdormanceAnalytics")
library("corrplot")


res.PCA<-PCA(dm,quali.sup=NULL,quanti.sup=NULL,ind.sup=NULL,scale.unit=TRUE,graph=FALSE,ncp=5)
plot.PCA(res.PCA,axes=c(1,2),choix='var',select=NULL,cex=1,cex.main=1,cex.axis=1,title='Variables factor map (PCA)',unselect=0,col.quanti.sup='blue',col.var='#000000')
plot.PCA(res.PCA,axes=c(1,2),choix='ind',select='contrib  24',habillage='none',title='Plano departamentos',cex=0.8,cex.main=0.8,cex.axis=0.8,col.ind='#1E1ED1',col.ind.sup='blue',col.quali='magenta')
res.PCA<-PCA(dm,quali.sup=NULL,quanti.sup=NULL,ind.sup=NULL,scale.unit=TRUE,graph=FALSE,ncp=5)
plot.PCA(res.PCA,axes=c(1,2),choix='var',select=NULL,cex=1,cex.main=1,cex.axis=1,title='Variables factor map (PCA)',unselect=0,col.quanti.sup='blue',col.var='#000000')
plot.PCA(res.PCA,axes=c(1,2),choix='ind',select='contrib  24',habillage='none',title='Plano departamentos',cex=0.8,cex.main=0.8,cex.axis=0.8,col.ind='#1E1ED1',col.ind.sup='blue',col.quali='magenta')

PCA$x

dim(dm)

dos_dim <- data.frame("Comp1" = PCA$x[,1], "Comp2" = PCA$x[,2])
dos_dim$animal <- c(rep(1,99), rep(0, 99))
dos_dim$animal2 <- ifelse(dos_dim$animal==1, "Gato", "Perro")

library(ggplot2)
ggplot(dos_dim, aes(Comp1, Comp2)) + geom_point(aes(colour=animal2))

# Límites de simulación, basados en {estatura, peso}/100
lims <- c(-4000, 2000,#Estatura, eje X
          -4000, 4000) #Peso, eje Y


nuevopunto <- function(lims=lims){
  a <- runif(1,lims[1],lims[2])
  b <- runif(1,lims[3],lims[4])
  return(c(a, b))
}


### Parámetro k del algoritmo.
### Evalúe el comportamiento para k={1, 3}
### ¿Qué problema hay con k=2? ¿Cómo se puede resolver?
k <- 3


for(iter in 1:2000){
  x <- nuevopunto(lims)
  diss <- dist(rbind(x,X[,c(2,3)]))[1:198]
  knn <- head(order(diss),k)
  etiqueta <- names(sort(table(X$G[knn]),decreasing = TRUE))[1]
  clase <- as.numeric(etiqueta=="M")
  for(inn in 1:k){
    if(X$G[knn[inn]]==etiqueta){
      
      ### Ensaye ambos tipos de visualización
      
      #lines(c(x[1],X$Estatura[knn[inn]]),c(x[2],X$Peso[knn[inn]]),
      #col=c("blue","black")[clase+1])
      points(x[1],x[2],col=c("blue","black")[clase+1],pch=20)
    }
  }
}

cols2 <- rep("red",nrow(X))
cols2[which(X$G=="M")] <- "green"
points(X$Estatura, X$Peso, pch=16, col=cols2)



