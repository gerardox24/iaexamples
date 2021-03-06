#Ejemplo.- Se elige al azar un n�mero entero positivo del 1 al 19. �Cu�l es la probabilidad de 
#que el n�mero sea m�ltiplo de 3 � de 5?

Prob = function(Vec)
{
  return(sum(Vec$Probs))
}

Union = function(A,B)
{
  return(rbind(A,B))
}

Interseccion = function(A,B)
{
  T1 = length(A$Mat)
  T2 = length(B$Mat)
  R = NULL
  for(i in 1:T1) 
    for(j in 1:T2)
      if(A$Mat[i]==B$Mat[j])
        R = rbind(R,A[i,])
  return(R)
}


S <- expand.grid(Mat=c('M','B','Q','F','L'))
Probs=c(rep(1/5,5))
S = data.frame(S,Probs)
A = subset(S,(Mat=='M'))
B = subset(S,(Mat=='B'))

cat('S \n'); print(S)
cat('A \n'); print(A)
cat('B \n'); print(B)
cat('Prob(A) \n')
print(Prob(A))

cat('Prob(B) \n')
print(Prob(B))

cat('Union(A,B)')
print(Union(A,B))

cat('Interccion(A,B)')
print(Interseccion(A,B))

cat('Respuesta \n')
cat('P(AUB) = P(A) + P(B) -P(A,B) ')
print(Prob(A)+Prob(B)-Prob(Interseccion(A,B)))




#Una pareja de esposos desean tener 3 hijos. Suponiendo que las probabilidades
#de tener un ni�o o una ni�a son iguales, calcular la probabilidad de �xito en tener
#hombre en el primer nacimiento, mujer en el segundo nacimiento y hombre en el tercer nacimiento.
S <- expand.grid(H1=c('H','M'),H2=c('H','M'),H3=c('H','M'))



