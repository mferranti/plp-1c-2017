%Grupo Lalo Lambda
% Integrantes: 
% Leandro Iannotti 827/06
% Marcelo Ferranti 629/06
% Natasha Martinelli 617/08

% ####################################
% Mochileando con herramientas
% ####################################

%%% Herramientas basicas
herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).



%%% Ejercicio 1
% Definimos jerarquica y su composicion
% composicion(+Composicion, ?Potencial, ?Costo)


%binaria(X,Y):- herramienta(X,_), herramienta(Y,_). 
%jerarquica(X,Y):- herramienta(X,_), herramienta(Y,_).
%----------------------------------------anterior
%composicion(binaria(X,Y),P,5):-herramienta(X, PX), herramienta(Y,PY), P is 2*PX + PY.
%composicion(jerarquica(X,Y),P,4):-herramienta(X, PX), herramienta(Y,PY), P is PX * PY.
%composicion(jerarquica(X,Y),P,C):-herramienta(X, PX),composicion(Y,PY,CY),P is PX * PY,C is 2 * (1 + CY).
%composicion(jerarquica(X,Y),P,C):-composicion(X, PX, CX),herramienta(Y,PY),P is PX * PY,C is 2 * (1 + CX).
%composicion(jerarquica(X,Y),P,C):-composicion(X,P1,C1),composicion(Y,P2,C2),P is P1*P2,C is 2 * (C1 + C2).
%----------------------------------------anterior

%composicion(jerarquia(jerarquia(binaria(rayo,volatilizador), rayo), rayo), X,Y).

composicion(M, P, C) :- not(herramienta(M, _)), calcular(M, P, C).

calcular(X, P, 1) :- herramienta(X, P).
calcular(binaria(X, Y), P, 5) :- calcular(X, PX, _), calcular(Y, PY, _), P is 2 * PX + PY.
calcular(jerarquica(X, Y), P, C) :- calcular(X, PX, CX), calcular(Y, PY, CY), P is PX * PY, C is  2 * (CX + CY).

%%% Ejercicio 2
% Definimos configuracion, utilizamos herramientasDeConf para obtener todas las relaciones posibles de configuracion
% con los elementos dados. Usamos permutations para obtener todas las posibles permutaciones de una lista de herramientas. 
% configuracion(+M, ?Conf, ?P, ?C)

configuracion(M, Conf, P, C):-permutations(M,NPERM),member(N, NPERM),herramientasDeConf(Conf, N),composicion(Conf, P, C).

%falta corregirlo
herramientasDeConf(binaria(X,Y), [X,Y]):-composicion(binaria(X,Y),_,_).
herramientasDeConf(jerarquica(X,Y), [X,Y]):-herramienta(X,_),herramienta(Y,_).
herramientasDeConf(jerarquica(X,Y), [X|HS]):-herramienta(X,_),herramientasDeConf(Y,HS).
herramientasDeConf(jerarquica(X,Y), [Y|HS]):-herramienta(Y,_),herramientasDeConf(X,HS).

permutations(X,L):-setof(A, permutation(X, A), L).


%%% Ejercicio 3
% Definimos masPoderosa, que dadas dos mochilas nos retorna true si existe un potencial en M1 tal que sea mayor a todos
% los potenciales de todas las configuraciones de M2
%masPoderosa(+M1,+M2).

masPoderosa(M1,M2):-maxPotencial(M1,P1),maxPotencial(M2,P2),P1 > P2.

maxPotencial(M, P):-findall(PConf, configuracion(M,_,PConf, _), L),max_member(P,L).


%%% Ejercicio 4
% Definimos mejor, que dadas dos mochilas nos retorna true si existe un potencial en M1 tal que sea mayor o igual a alguna
%  configuraciones y tenga menor costo de M2
%mejor(M1,M2)

%----------------------------------------anterior
%mejor(M1,M2):- not(not(siempreHayMejor(M1,M2))).

%siempreHayMejor(M1,M2):-configuracion(M2,_,PM2, CM2),existeMejor(M1,PM2,CM2).

%existeMejor(M1,PM2,CM2):-configuracion(M1,_,PM1, CM1),PM1 >= PM2,CM1 < CM2,!.
%----------------------------------------anterior

mejor(M1, M2) :- not((configuracion(M2, _, PM2, CM2), not(existeMejor(M1, PM2, CM2)))).

existeMejor(M1, PM2, CM2) :- configuracion(M1, _, PM1, CM1), PM1 >= PM2, CM1 < CM2, !.

%%% Ejercicio 5
% Definimos usar que a partir de una mochila M y una lista Ps de potenciales,
%genera una lista Cs de herramientas compuestas que satisfacen los potenciales solicitados
%usar(+M1,+Ps,?Cs,?M2)

%----------------------------------------anterior
%usar(_,[],[],_).
%usar(M1,[P|PS],[C|CS],M2):-setof(X,sublist(X,M1), L),
%							member(M,L),deleteSublist(M,M1,M1Unused),
%							configuracion(M, C, Pot, _),
%							Pot >= P,usar(M1Unused, PS, CS, M2).

%sublist([], _).
%sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
%sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

%deleteSublist([],L,L).
%deleteSublist([X|S],L,T):- deleteSublist(S,L,T2), selectchk(X,T2,T).
%----------------------------------------anterior

%usar([rayo,rayo,volatilizador,rayo,encendedor],[30,80],C,[encendedor]).

usar(M1, [], [], M1).
usar(M1, [P|PS], [C|CS], M2) :- sublist(X, M1), 
								configuracion(X, C, Pot, _), 
								Pot >= P,
								remover(M1, X, NewM1),
								usar(NewM1, PS, CS, M2).

sublist([], _).
sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

remover(XS, [], XS).
remover([X|XS], [X|YS], M2) :- remover(XS, YS, M2).
remover([X|XS], [Y|YS], [X|M2]) :- X \= Y, remover(XS, [Y|YS], M2).

%preguntar como no retornar repetidos

%%% Ejercicio 6
% Definimos comprar que es verdadero cuando  M tiene como mucho maximo
%C herramientas basicas y permite construir una herramienta con potencial mayor o igual a P 


%comprar(+P,+C,?M)
comprar(P,C,M):-setof(X, solucionesComprar(P,C,X), L),list_to_set(L,S),member(M,S).

solucionesComprar(P,C,M):-generarMochilas(C,L),member(M,L),configuracion(M,_,Pot,_),Pot >= P.

generarMochilas(1,M):- setof(X,generarMochilasDeTamano(1,X),M).
generarMochilas(C,M):-C =\= 1,Csub is C - 1,setof(X,generarMochilasDeTamano(C,X),L1),generarMochilas(Csub,L2),append(L1,L2,M).

generarMochilasDeTamano(1,[X]):-herramienta(X,_).
generarMochilasDeTamano(C,M):-C =\= 1,Csub is C - 1,generarMochilasDeTamano(Csub,M2),herramienta(X,_),select(X,M,M2).