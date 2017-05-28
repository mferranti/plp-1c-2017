herramienta(rayo, 10).
herramienta(volatilizador, 40).
herramienta(encendedor, 5).

%binaria(X,Y):- herramienta(X,_), herramienta(Y,_). % Esto es necesario?
%jerarquica(X,Y):- herramienta(X), herramienta(Y). % Esto es necesario?

composicion(binaria(X,Y),P,5):-
  herramienta(X, PX), herramienta(Y,PY), X\= encendedor, P is 2*PX + PY.
composicion(jerarquica(X,Y),P,2):-
  herramienta(X, PX), herramienta(Y,PY), P is PX * PY.
composicion(jerarquica(X,Y),P,C):-
  herramienta(X, PX), composicion(Y,PY,CY)), P is PX * PY, C is 2 * (1 + CX).
composicion(jerarquica(X,Y),P,C):-
  composicion(X, PX, CX), herramienta(Y,PY), P is PX * PY, C is 2 * (1 + CX).
composicion(jerarquica(X,Y),P,C):-
  composicion(X,P1,C1), composicion(Y,P2,C2), P is P1*P2, C is 2 * (C1 + C2).
