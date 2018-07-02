Grafo = empty.graph(c('A','S','T','L','B','E','X','D'))
Grafo
#Arcos = matrix(0,ncol = 8, nrow = 8,dimnames = list(LETTERS[1:8],LETTERS[1:8]))
Matriz = matrix(0,ncol = 8, nrow = 8,
              dimnames = 
              list(c('A','S','T','L','B','E','X','D'),
                   c('A','S','T','L','B','E','X','D')))
Matriz['A','T']=1
Matriz['S','L']=1
Matriz['S','B']=1
Matriz['T','E']=1
Matriz['L','E']=1
Matriz['B','D']=1
Matriz['E','X']=1
Matriz['E','D']=1
Matriz
amat(Grafo) = Matriz
Grafo

Resp <- c("yes", "no")
A <- matrix(c(0.01, 0.99), ncol=2, dimnames=list("Probs", Resp))
A
S <- matrix(c(0.5, 0.5), ncol=2, dimnames=list("Probs", Resp))
S
T <- matrix(c(0.05, 0.95, 0.01, 0.99),ncol=2, dimnames=list("T"=Resp, "A"=Resp))
T
L <- matrix(c(0.1, 0.9, 0.01, 0.99), ncol=2, dimnames=list("L"=Resp, "S"=Resp))
L
B <- matrix(c(0.6, 0.4, 0.3, 0.7), ncol=2, dimnames=list("B"=Resp, "S"=Resp));
B
E <- c(1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0);
dim(E) <- c(2, 2, 2); #2filas x 2 columnas 2 matrices
E
dimnames(E) <- list("E"=Resp, "L"=Resp, "T"=Resp)
E
X <- matrix(c(0.98, 0.02, 0.05, 0.95),ncol=2, dimnames=list("X"=Resp, "E"=Resp))
X
D <- c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9)
dim(D) <- c(2, 2, 2)
dimnames(D) <- list("D"=Resp, "E"=Resp, "B"=Resp)
D
Distribucion <- custom.fit(Grafo, dist=list(A=A, S=S, T=T, L=L,B=B, E=E, X=X, D=D))
Distribucion
#plot(Grafo)

cpquery(Distribucion,event=(A=="si"),evidence = (S=="no"),n=10^5)

cpquery(Distribucion,event=(X=="no"),evidence = (D=="no"),n=10^5)


