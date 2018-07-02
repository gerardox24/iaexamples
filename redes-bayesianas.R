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

#P.C
P(A|B)=P(AnB)/P(B)
#R.P
P(A,B)=P(A|B)P(B)
P(A,B)=P(B|A)P(A)
#R.P.C
P(A,B|C)=P(A|B,C)P(B|C);
P(A,B|C)=P(B|A,C)P(A|C);
#T.B
P(A|B)=P(A)P(B|A)/P(B)
#S.T
+Y1|+X ..
#Y1
+Y1=Z(P(+Y1|+X)P(+X)+P(+Y1|-X)P(-X))
-Y1=Z(P(-Y1|+X)P(+X)+P(-Y1|-X)P(-X))
Y = +Y1+-Y1;

#LF
Tri(x;a,b,c)=max(min(x-a/b-a,c-x/c-b),0);
Tra(x;a,b,c,d)=max(min(x-a/b-a,1,d-x/d-c),0)
Gau(x;o,c)=e^-(x-c/o)^2
Cam(x;a,b,c)=1/1+(x-c/a)^2b
Sin(x;a,c)=1/1+e^-a(x-c)
#BNF
<frase>::= <sujeto><predicado>
<sujeto>::= juan | pedro | maría | salgado
<predicado>::= <verbo transitivo><objeto directo>
<predicado>::= <verbo intransitivo>
<verbo transitivo>::= ama | lava | peina | adora
<objeto directo>::= paula | antonio | sultán
<verbo intransitivo>::= corre | salta | camina

P: S→ ab|aSb

#E
S::abc|aBSc
Ba::aB
Bb::Bb

#E
S::ASB|e
A::aAb|e
B::bBa|ba
#MPBG
S ->NP VP				1,0
NP->DT N 				0,4
   |N 					 0,2
   |NP PP 				 0,4
VP->V NP 				0,5
   |V 					 0,2
   |VP PP 				 0,3
PP->P NP 				0,8
   |P 					 0,2
DT -> el | los					0,50 C.U
N ->hombre|amigos|cafe|leche    0,25 C.U
V -> toma|toman 				0,50 C.U
P -> con | solo 				0,50 C.U


P("lo escencial es invicible ") = p(lo)*p(escencial|lo)*p(es|lo escencial)*p(invicible|lo escencial es);

