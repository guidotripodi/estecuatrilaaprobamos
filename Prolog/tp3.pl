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
islasVecinas([ruta(I2,_,_)|RS], I, L):- I \= I2, islasVecinas(RS,I,L).


%%% EJERCICIO 3 

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas(M, I1, I2,N):- member(ruta(I1,I2,N),M).
 

%%% EJERCICIO 4

% caminoSimple(+M, +O, +D, -C) 

caminoSimple(RS, O, D, [O|[D]]):- islasVecinas(RS, O, L), member(D, L).
caminoSimple(RS, O, X, [O|L]):- islasVecinas(RS, O, L1), member(Y,L1),not(member(Y,L)), caminoSimple(RS,Y,X,[Y|L]).

vecina(M,X, Y):- member(ruta(X,Y,_),M).

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
caminoMinimo(M, O, D, C, DIS):-  caminoSimple(RS,O,D,C), distanciaTotal(RS,C,DIS), not(caminoMasCorto(M,O,D,C,_,_,DIS)).

distanciaTotal(RS,[X|[Y]], DIS):-distanciaVecinas(RS,X,Y,DIS).
distanciaTotal(RS,[X,Y|L], DIS):- distanciaVecinas(RS,X,Y,D2), distanciaTotal([Y|L], D1), DIS is D1+D2.

caminoMasCorto(M,O,D,_,C1,D1,DIS):- camino(M, O, D, C1, D1), DIS >= D1.


%camino(RS, O, D, [O|[D]], DIS):- islasVecinas(RS, O, L1), member(D, L1), distanciaVecinas(RS,O,D,DIS).
%camino(RS, O, X, [O, Y|L], DIS):- islasVecinas(RS, O, L1), member(Y, L1),borrar(RS, ruta(O,Y,_), RS1), distanciaVecinas(RS,O,Y, D1), camino(RS1, Y, X, [Y|L], D2),DIS is D1 + D2. 
%camino(RS, O, D, C, DIS):- caminoSimple(RS,O,D,C), distanciaTotal(RS,C,DIS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(3). 
testIslas(1) :- mapaEjemplo(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [papeete, tahiti, uturoa]).
testIslas(2) :- mapaEjemplo2(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [funafuti, savave,valitupu]).
testIslas(3) :- mapaEjemplo3(Mapa), islas(Mapa, Islas), length(Islas, 4).

cantidadTestsMapa(3). 
testMapa(1) :- noMapa(NM), not(mapa(NM)).
testMapa(2) :- mapaEjemplo(NM), mapa(NM).
testMapa(3) :- mapaEjemplo2(NM), mapa(NM).



cantidadTestsCaminos(3). % ¡Actualizar!
testCaminos(1) :- mapaEjemplo(Mapa), setof(C, caminoSimple(Mapa, uturoa, papeete, C), L), length(L, 2).
testCaminos(2) :- mapaEjemplo(Mapa), setof(C, caminoHamiltoniano(Mapa, uturoa, papeete, C), L), length(L, 1).
testCaminos(3) :- mapaEjemplo3(M),setof(C, caminoHamiltoniano(M, C), L), length(L, 8).


tests(islas) :- cantidadTestsIslas(M), forall(between(1,M,N), testIslas(N)).
tests(mapa) :- cantidadTestsMapa(M), forall(between(1,M,N), testMapa(N)).
tests(caminos) :- cantidadTestsCaminos(M), forall(between(1,M,N), testCaminos(N)).

tests(todos) :-
  tests(islas),
  tests(mapa),
  tests(caminos).

tests :- tests(todos).
