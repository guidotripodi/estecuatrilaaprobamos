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
%islas([],[]).
islas([ruta(I1,I2,_)],[I1|[I2]]).
islas([ruta(I1,I2,_)|RS],[I1,I2|L]):- islas(RS,L), not(member(I1, L)), not(member(I2,L)).
islas([ruta(I1,I2,_)|RS],[L]):- islas(RS,L), member(I1, L), member(I2,L).
islas([ruta(I1,I2,_)|RS],[I1|L]):- islas(RS,L), not(member(I1, L)), member(I2,L).
islas([ruta(I1,I2,_)|RS],[I2|L]):- islas(RS,L), member(I1, L), not(member(I2,L)).

%%% EJERCICIO 2

% islasVecinas(+M, +I, -Is)
islasVecinas([], _, []).
islasVecinas([ruta(I1,I2,_)|RS], I, L):- I1 \= I, I \= I2, islasVecinas(RS,I,L).
islasVecinas([ruta(I,I2,_)|RS], I, [I2|L]):- islasVecinas(RS,I,L), not(member(I2, L)).
islasVecinas([ruta(I1,I,_)|RS], I, [I1|L]):- islasVecinas(RS,I,L), not(member(I1, L)).
islasVecinas([ruta(I1,I,_)|RS], I, L):- islasVecinas(RS,I,L), member(I1, L).
islasVecinas([ruta(I,I2,_)|RS], I, L):- islasVecinas(RS,I,L), member(I2, L).

%%% EJERCICIO 3

% distanciaVecinas(+M, +I1, +I2, -N)
distanciaVecinas([ruta(I1,I2, N)|_], I1, I2,N).
distanciaVecinas([ruta(I2,I1, N)|_], I1, I2,N).
distanciaVecinas([ruta(_,_, _)|L], I1, I2,N) :- distanciaVecinas(L, I1, I2, N). 

%%% EJERCICIO 4

% caminoSimple(+M, +O, +D, -C)
%caminoSimple([X], X, Y, []).
caminoSimple(RS, O, D, [O|[D]]):- islasVecinas(RS, O, L1), member(D, L1).
caminoSimple(RS, O, X, [O, Y|L]):- islasVecinas(RS, O, L1), member(Y, L1), not(member(Y,L)), caminoSimple(RS, Y, X, [Y|L]). 



%%% EJERCICIO 5

% mapa(+M)
mapa(M):- noSoyVecinoDeMiMismo(M),alcanzable(M),rutaNoEstaDosVeces(M).

alcanzable(M):- islas(M, L), member(X,L), delete(L,X,L1), forall(member(Y,L1),caminoSimple(M, X, Y, _)).

noSoyVecinoDeMiMismo([]).
noSoyVecinoDeMiMismo([ruta(X,Y,_)|LS]):- X \= Y, noSoyVecinoDeMiMismo(LS).

rutaNoEstaDosVeces([]).
rutaNoEstaDosVeces([ruta(X,Y, _)|XS]):- not(member(ruta(X,Y, _), XS)), rutaNoEstaDosVeces(XS).

%%% EJERCICIO 6

% caminoHamiltoniano(+M, +O, +D, -C)
caminoHamiltoniano(_, _, _, _).

%%% EJERCICIO 7

% caminoHamiltoniano(+M, -C)
caminoHamiltoniano(_, _).

%%% Ejercicio 8

% caminoMinimo(+M, +O, +D, -C, -Distancia)
caminoMinimo(_, _, _, _, _).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% TESTS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

cantidadTestsIslas(1). % ¡Actualizar!
testIslas(1) :- mapaEjemplo(Mapa), islas(Mapa, Islas), length(Islas, 3), sort(Islas, [papeete, tahiti, uturoa]).

cantidadTestsMapa(1). % ¡Actualizar!
testMapa(1) :- noMapa(NM), not(mapa(NM)).

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
