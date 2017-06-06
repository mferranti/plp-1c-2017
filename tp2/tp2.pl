herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).

%binaria(X,Y):- herramienta(X,_), herramienta(Y,_). % Esto es necesario?
%jerarquica(X,Y):- herramienta(X), herramienta(Y). % Esto es necesario?

composicion(binaria(X,Y),P,5):-
  herramienta(X, PX), herramienta(Y,PY), X\= encendedor, P is 2*PX + PY.
composicion(jerarquica(X,Y),P,4):-
  herramienta(X, PX), herramienta(Y,PY), P is PX * PY.
composicion(jerarquica(X,Y),P,C):-
  herramienta(X, PX),
  composicion(Y,PY,CY),
  P is PX * PY,
  C is 2 * (1 + CY).
composicion(jerarquica(X,Y),P,C):-
  composicion(X, PX, CX),
  herramienta(Y,PY),
  P is PX * PY,
  C is 2 * (1 + CX).
composicion(jerarquica(X,Y),P,C):-
  composicion(X,P1,C1),
  composicion(Y,P2,C2),
  P is P1*P2,
  C is 2 * (C1 + C2).

%configuracion(+M, ?Conf, ?P, ?C)
configuracion(M, Conf, P, C):-
  permutations(M,NPERM),
  member(N, NPERM),
  herramientasDeConf(Conf, N),
  composicion(Conf, P, C).


herramientasDeConf(binaria(X,Y), [X,Y]):-
  composicion(binaria(X,Y),_,_).

herramientasDeConf(jerarquica(X,Y), [X,Y]):-
  herramienta(X,_),
  herramienta(Y,_).

herramientasDeConf(jerarquica(X,Y), [X|HS]):-
  herramienta(X,_),
  herramientasDeConf(Y,HS).

herramientasDeConf(jerarquica(X,Y), [Y|HS]):-
  herramienta(Y,_),
  herramientasDeConf(X,HS).

permutations(X,L):-setof(A, permutation(X, A), L).

%masPoderosa(+M1,+M2).
masPoderosa(M1,M2):-
  maxPotencial(M1,P1),
  maxPotencial(M2,P2),
  P1 > P2.

maxPotencial(M, P):-
  findall(PConf, configuracion(M,_,PConf, _), L),
  max_member(P,L).

%mejor(M1,M2)
mejor(M1,M2):- not(not(siempreHayMejor(M1,M2))).

siempreHayMejor(M1,M2):-
  configuracion(M2,_,PM2, CM2),
  existeMejor(M1,PM2,CM2).

existeMejor(M1,PM2,CM2):-
  configuracion(M1,_,PM1, CM1),
  PM1 >= PM2,
  CM1 < CM2,
  !.

%usar(+M1,+Ps,?Cs,?M2)
usar(_,[],[],_).
usar(M1,[P|PS],[C|CS],M2):-
  setof(X,sublist(X,M1), L),
  member(M,L),
  deleteSublist(M,M1,M1Unused),
  configuracion(M, C, Pot, _),
  Pot >= P,
  usar(M1Unused, PS, CS, M2).

sublist([], _).
sublist([X|Xs], [X|Ys]) :- sublist(Xs, Ys).
sublist(Xs, [_|Ys]) :- sublist(Xs, Ys).

deleteSublist([],L,L).
deleteSublist([X|S],L,T):- deleteSublist(S,L,T2), selectchk(X,T2,T).

%comprar(+P,+C,?M)
comprar(P,C,M):-
  setof(X, solucionesComprar(P,C,X), L),
  list_to_set(L,S),
  member(M,S).

solucionesComprar(P,C,M):-
  generarMochilas(C,L),
  member(M,L),
  configuracion(M,_,Pot,_),
  Pot >= P.

generarMochilas(1,M):- setof(X,generarMochilasDeTamano(1,X),M).
generarMochilas(C,M):-
  C =\= 1,
  Csub is C - 1,
  setof(X,generarMochilasDeTamano(C,X),L1),
  generarMochilas(Csub,L2),
  append(L1,L2,M).

generarMochilasDeTamano(1,[X]):-herramienta(X,_).
generarMochilasDeTamano(C,M):-
  C =\= 1,
  Csub is C - 1,
  generarMochilasDeTamano(Csub,M2),
  herramienta(X,_),
  select(X,M,M2).
