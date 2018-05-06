### Perceptrón sobre los datos artificiales de la clase para
### Estimar Sexo a partir de Peso y Estatura

### Debe comentar este código indicando los pasos más importantes
### e identificando todas las líneas que corresponden efectivamente
### al algoritmo del perceptrón.

# Preámbulo
rm(list=ls())
graphics.off()
X <- readRDS("X.rds")

### ¿Qué pasa si no se hace esta normalización?

# No separa hombres de mujeres afectivamente
X[,c(2:3)] <- X[,c(2,3)]/100

###
X$G <- substr(X$Sexo, 1, 1)
X$g <- as.numeric(X$G=="M")
X$Uno <- 1

head(X)

###
### ¿Qué hace esta función?
### ¿Por qué se requiere?
f <- function(x){
  if(x>0) return(1)
  return(0)
}

l <- nrow(X)

# Visualización
x11()
plot(X$Estatura, X$Peso, type = "n", xlab="Estatura", ylab="Peso",asp=1)
cols <- rep("blue",nrow(X))
cols[which(X$G=="M")] <- "black"
text(X$Estatura, X$Peso, X$G, col=cols)


##
w <- c(0,0,0) #Estatura, Peso, Ind


done <- FALSE
iter <- 0
max.iter <- 1000
a <- 1
b <- 1
z <- c(10,10,10,10)
names(z) <- c("iter", "a", "b", "err")
err <- 0

while(!done){
  ind <- sample(1:l,1)
  h <- f(sum(w*X[ind,c(2,3,9)]))
  w <- w + (X$g[ind] - h) * unlist(X[ind,c(2,3,9)])
  a <- -w[3]/w[2]
  b <- -w[1]/w[2]
  rr <- NULL
  for(i in 1:l){
    r <- ifelse((X[i,3] < X[i,2]*b + a & X[i,8] == 0) | (X[i,3] > X[i,2]*b + a & X[i,8] == 1),1,0)
    rr <- rbind(rr, r)
    err <- 1 - sum(rr)/l
  }
  z <- rbind(z, c(iter, a, b, err))
  iter <- iter + 1
  if(iter >= max.iter) done <- TRUE
}

head(X)

abline(a,b,col="black",lwd=2)

abline(z[iter_err_min,2],z[iter_err_min,3],col="black",lwd=2)

iter_err_min <- order(z[,4], decreasing = F)[1]
z[iter_err_min,c(1,2,3,4)]

plot(z[-1,4])
?geom_point

?
### Si los datos no son linealmente separables, el método "perceptrón
### con bolsillo" va llevando un registro de la versión (iteración) con menor
### error sobre los datos de entrenamiento (en este caso todos los
### datos). Modifique el código para que incluir el "bolsillo" y
### empléelo para clasificar si estarura/peso/edad (según indicación
### del profesor) es superior o inferior a la media, a partir de las otras
### dos variables.

# Explicar Peso a partir de estatura y edad

# Preámbulo
rm(list=ls())
graphics.off()
X <- readRDS("X.rds")

### ¿Qué pasa si no se hace esta normalización?

# No se para hombres de mujeres afectivamente
X[,c(2:3)] <- X[,c(2,3)]/100

head(X)

###
#X$G <- substr(X$Sexo, 1, 1)
X$G <- as.numeric(X$Peso>=mean(X$Peso))
X$Uno <- 1
X$Peso_class <- ifelse(X$G==1, "Peso Mayor a la Media", "Peso Menor a la Media")

head(X)

###
### ¿Qué hace esta función?
### ¿Por qué se requiere?
f <- function(x){
  if(x>0) return(1)
  return(0)
}

l <- nrow(X)

# Visualización
x11()
plot(X$Estatura, X$Edad, type = "n", xlab="Estatura", ylab="Edad")
cols <- rep("blue",nrow(X))
cols[which(X$G==1)] <- "black"
text(X$Estatura, X$Edad, X$G, col=cols)

##
w <- c(0,0,0) #Estatura, Edad, Ind

done <- FALSE
iter <- 0
max.iter <- 10000
a <- 0
b <- 0
h <- 0
error <- h
coor_a <- a
coor_b <- b

while(!done){
  ind <- sample(1:l,1)
  h <- f(sum(w*X[ind,c(2,1,3)]))
  error <- c(error, h)
  w <- w + (X$G[ind] - h) * unlist(X[ind,c(2,1,3)])
  a <- -w[3]/w[2]
  coor_a <- c(coor_a, a)
  b <- -w[1]/w[2]
  coor_b <- c(coor_b, b)
  if(iter%%100==0){
    abline(a,b, col="grey")
    #Sys.sleep(0.1)
    print(iter)
  }
  
  iter <- iter + 1
  ###
  ### Aparte de esta, qué otra(s) condición de parada se podría usar?
  if(iter >= max.iter) done <- TRUE
  ###
}

abline(a,b,col="black",lwd=2)

summary(error)

