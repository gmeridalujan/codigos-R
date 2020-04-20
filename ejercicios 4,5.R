##4. Realice una función para el calculo del tamaño de muestra para el muestreo aleatorio simple, considere la media, el margen de error relativo y coeficientes de confianza.
aux<-c(rnorm(1000,10,7.5))
aux
FN<-function(e,x,alfa){
  N<-length(x)
  sig<-var(x)
  n=(((qnorm(alfa))^2)*N*sig)/(((e^2)*(N-1))+(sig*((qnorm(alfa))^2)))
  return(n)
  }
FN(0.9,aux,0.05)
##5. Crear una función que realice la prueba de independencia Chi-cuadrado 

Fch<- function(x,p,i,j){
    n<-length(x[,i])
    z<-(x[i]+x[j])/2
    x1<-(x[i]-z)^2/z
    x2<-(x[j]-z)^2/z
    X<-sum(x1+x2)
    if(X<qchisq(p,n-1)){
      h<-"son independientes"
    }else{
      h<-"no son independientes"
    }
    return(h)
}
#ejemplo con la base de datos de mtcars
Fch(mtcars,0.5,2,3)
