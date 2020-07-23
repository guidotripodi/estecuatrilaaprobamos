%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% DEFINICIONES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

mapaEjemplo([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10)]).
      
mapaEjemplo2([
      ruta(valitupu, funafuti, 30),
      ruta(valitupu, savave, 10),
      ruta(savave, valitupu, 20),
      ruta(savave, funafuti, 10),
      ruta(funafuti, valitupu, 30),
      ruta(funafuti, savave, 20)]).
      
mapaEjemplo3([
      ruta(nui, valitupu, 50),
      ruta(nui, savave, 40),
      ruta(valitupu, funafuti, 30),
      ruta(valitupu, savave, 10),
      ruta(savave, valitupu, 20),
      ruta(savave, funafuti, 10),
      ruta(savave, nui, 50),
      ruta(funafuti, valitupu, 30),
      ruta(funafuti, savave, 20)]).
      
noMapa([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(uturoa, tahiti, 20)]).

noMapa2([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10),
      ruta(mururoa,rikitea,20),
      ruta(rikitea,mururoa,20)]).
      
noMapa3([
      ruta(uturoa, tahiti, 50),
      ruta(tahiti, uturoa, 30),
      ruta(tahiti, tahiti, 10),
      ruta(papeete, uturoa, 20),
      ruta(uturoa, papeete, 20),
      ruta(tahiti, papeete, 20),
      ruta(papeete, tahiti, 10)]).
      

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% EJERCICIOS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% EJERCICIO 1

% islas(+M, -Is)
islas([],[]).
islas([ruta(I1,_,_)|RS], [I1|L]):- islas(RS,L), not(member(I1,L)).
islas([ruta(I1,_,_)|RS], L):- islas(RS,L), member(I1,L).

%%% EJERCICIO 2

% islasVecinas(+M, +I, -Is)

islasVecinas([], _, []).
islasVecinas([ruta(I,I1,_)|RS], I, [I1|L]):- islasVecinas(RS,I,L), not(member(I1,L)).
islasVecinas([ruta(I2,_,_)|RS], I, L):- I \=I2, islasVecinas(RS,I,L).


%%% EJERCICIO 3 

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas(M, I1, I2,N):- member(ruta(I1,I2,N),M).
 

%%% EJERCICIO 4

% caminoSimple(+M, +O, +D, -C) 

caminoSimple(M, O,D,C):- caminoSimpleAux(M,O,[D],C).

caminoSimpleAux(_,O,[O|C1],[O|C1]).
caminoSimpleAux(M,O,[Y|C1],C) :-vecina(M,X,Y),not(member(X,[Y|C1])),caminoSimpleAux(M,O,[X,Y|C1],C).

vecina(M,X, Y):- member(ruta(X,Y,_),M).

%Es reversible en O y D ya que toma la "cabeza" de la lista como elemento Origen 
%y el final de la lista como elemento Destino 
%No es reversible en Mapa ya que en la primer clausula tiene infinitas soluciones posibles de M
% y por lo tanto se cuelga

%%% EJERCICIO 5 

% mapa(+M)
mapa(M):- noSoyVecinoDeMiMismo(M),alcanzable(M),rutaNoEstaDosVeces(M),unaIdaYunaVuelta(M,M).

unaIdaYunaVuelta(_,[]).
unaIdaYunaVuelta(M,[ruta(X,Y,_)|MS]):- member(ruta(Y,X,_),M),unaIdaYunaVuelta(M,MS).


alcanzable(M):- todasSonAlcanzables(M,M,L), islas(M,L1), length(L,P), length(L1,P).

todasSonAlcanzables(_,[],[]).
todasSonAlcanzables(M,[ruta(X,_,_)|MS],[X|XS]):-islas(M, L), member(X,L), delete(L,X,L1), forall(member(Y,L1),caminoSimple(M, X, Y, _)),todasSonAlcanzables(M,MS, XS),not(member(X,XS)).
todasSonAlcanzables(M,[ruta(X,_,_)|MS],XS):-todasSonAlcanzables(M, MS, XS),member(X,XS).

noSoyVecinoDeMiMismo([]).
noSoyVecinoDeMiMismo([ruta(X,Y,_)|LS]):- X \= Y, noSoyVecinoDeMiMismo(LS).

rutaNoEstaDosVeces([]).
rutaNoEstaDosVeces([ruta(X,Y, _)|XS]):- not(member(ruta(X,Y, _), XS)), rutaNoEstaDosVeces(XS).

%%% EJERCICIO 6

% caminoHamiltoniano(+M, +O, +D, -C)
caminoHamiltoniano(M, O, D, L):- caminoSimple(M,O,D,L), islas(M,L1), forall(member(Y, L1), member(Y, L)).

%%% EJERCICIO 7

% caminoHamiltoniano(+M, -C)
caminoHamiltoniano(M, C):- islas(M,L), member(X,L), member(Y,L),caminoHamiltoniano(M, X, Y, C).


%%% Ejercicio 8 

% caminoMinimo(+M, +O, +D, -C, -Distancia)
caminoMinimo(M, O, D, C, DIS):-  camino(M,O,D,C, DIS), not(caminoMasCorto(M,O,D,C)).

camino(RS, O, D, C, DIS):- caminoSimple(RS, O, D, C), distanciaTotal(RS, C, DIS).

distanciaTotal(_,X, 0):- length(X,P), P = 1.
distanciaTotal(RS,[X|L], DIS):- length(L, P), P > 0, headL(L, Y),distanciaVecinas(RS,X,Y,D2), distanciaTotal(RS,L,D1), DIS is D1+D2.

headL([X|_], X).

caminoMasCorto(M,O,D,C):- camino(M, O, D, C1, D1), distanciaTotal(M,C,DIS), C1 \= C, DIS >= D1.

%Si en DIS colocamos el valor del camino , se buscara el camino con dicha distancia, no es reversible 
%en M ya que caminoSimple tampoco lo es en M por ende se cuelga ya que encuentra infinitos caminos posibles
%Idem si colocamos el camino, nos devolvera la distancia correspondiente., si colocamos tanto camino
% como distancia intentara unificar y encontrar valido dicho camino, retornando true o false.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(3). 
testIslas(1) :- mapaEjemplo(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [papeete, tahiti, uturoa]).
testIslas(2) :- mapaEjemplo2(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [funafuti, savave,valitupu]).
testIslas(3) :- mapaEjemplo3(Mapa), islas(Mapa, Islas), length(Islas, 4).

cantidadTestVecinas(2).
testsVecinas(1):- mapaEjemplo(Mapa), islasVecinas(Mapa,uturoa, Vecis), length(Vecis, 2).
testsVecinas(2):- mapaEjemplo2(Mapa), islasVecinas(Mapa,valitupu, Vecis), length(Vecis, 2).

cantidadTestsMapa(3). 
testMapa(1) :- noMapa(NM), not(mapa(NM)).
testMapa(2) :- mapaEjemplo(NM), mapa(NM).
testMapa(3) :- mapaEjemplo2(NM), mapa(NM).

cantidadTestsCaminos(5). 
testCaminos(1) :- mapaEjemplo(Mapa), setof(C, caminoSimple(Mapa, uturoa, papeete, C), L), length(L, 2).
testCaminos(2) :- mapaEjemplo(Mapa), setof(C, caminoHamiltoniano(Mapa, uturoa, papeete, C), L), length(L, 1).
testCaminos(3) :- mapaEjemplo3(M),setof(C, caminoHamiltoniano(M, C), L), length(L, 8).
testCaminos(4) :- mapaEjemplo2(M),caminoMinimo(M, valitupu, savave, _, D), D = 10.
testCaminos(5) :- mapaEjemplo3(M),caminoMinimo(M, nui, funafuti, _, D), D = 50.

tests(islas) :- cantidadTestsIslas(M), forall(between(1,M,N), testIslas(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).

tests(todos) :-
  tests(islas),
  tests(mapa),
  tests(caminos).

tests :- tests(todos).
