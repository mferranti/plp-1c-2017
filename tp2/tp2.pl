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
