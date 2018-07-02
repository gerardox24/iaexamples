library(bnlearn)
Grafo = empty.graph(c('X','Y1'))
Grafo
Matriz = matrix(0,ncol=2,nrow=2,dimname=list(c('X','Y1'),c('X','Y1')))
Matriz['X','Y1']=1
Matriz
amat(Grafo)=Matriz
Grafo
Resp = c('si','no')
MX  =  matrix(c(0.003,0.997),ncol=2, dimnames = list("Probs",Resp))
MY1 = matrix(c(0.992,0.008,0.0006,0.9994),ncol=2,nrow=2, dimnames = list("Y1"=Resp,"X"=Resp))
X
Y1
Modelo = custom.fit(Grafo,dist = list(X=MX,Y1=MY1))
Modelo
cpquery(Modelo,X=='si',TRUE)
cpquery(Modelo,X=='si',Y1=='si')

#T. BAYES
P(A|B)=P(AnB)/P(B)

