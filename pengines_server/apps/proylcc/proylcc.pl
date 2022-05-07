:- module(proylcc, 
	[  
		flick/6
	]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% flick(+Grid, +CN, -NGrid,+PosX,+PosY,-CantTotal)
% Grid grilla de colores pasada por parametro
% CN color nuevo a ser comparado
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Retorna NGrid es el resultado de hacer 'flick' de la grilla Grid con el color CN.
% Retorna CantTotal con la cantidad de celdas capturadas. 

flick(Grid, CN, NGrid, PosX, PosY, CantTotal):-
	obtenerElemGrilla(PosX, PosY, Grid, E),
    CN\=E,
	cambiarC(Grid, E, CN, PosX, PosY, NGrid, CantAdy),
    sum_num(CantAdy,1,CantTotal).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% obtenerElemGrilla(+PosX, +PosY, +Grid, -E)
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla de colores pasada por parametro
% E elemento a retornar
% Retorna el elemento E de la grila Grid en la posicion (PosX,PosY).
obtenerElemGrilla(PosX, PosY, Grid, E):- 
    nth0(PosX, Grid, List), nth0(PosY, List, E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +CN, +PosX, +PosY, -NGrid, -CantTotales)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% NGrid grilla reemplazada con el color nuevo CN
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, modifica el color en la grilla si mi color actual CA 
% es igual al color del elemento E en la posicion (PosX,PosY). 
cambiarC(Grid, CA, CN, PosX, PosY, NGrid, CantTotales):-
   obtenerElemGrilla(PosX, PosY, Grid, E), CA=E,
   cambiarElemG(PosX, PosY, Grid, CN, AuxGrid), 
   adyacentes(AuxGrid, CA, CN, PosX, PosY, NGrid, CantAdy),
   sum_num(CantAdy, 1, CantTotales).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +_CN, +PosX, +PosY, -Grid, -0)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla sin cambios
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, retorna la grilla sin modificar si mi color actual CA 
% es distinto al color del elemento E en la posicion (PosX,PosY). 
cambiarC(Grid, CA, _CN, PosX, PosY, Grid, 0):-
     obtenerElemGrilla(PosX, PosY, Grid, E), CA\=E.  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarC(+Grid, +CA, +_CN, +PosX, +PosY, -Grid, -0)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla sin cambios
% CantTotales cantidad total de celdas capturadas
% El predicado cambiarC/7, verifica que la posicion (PosX,PosY) sea valida. 
cambiarC(Grid, _CA, _CN, PosX, PosY, Grid, 0):-
(PosX<0); (PosY<0); (PosX>13); (PosY>13).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%replace(+Pos, +L, +EN, -ListN)
% Pos posicion a buscar en la lista
% L lista ingresada por parametro
% EN elemento a reemplazar
% ListN lista nueva con el elemento EN reemplazado.
% Dada una posicion en una lista, reemplaza el valor de esa posicion por EN 
%y lo retorna en una nueva lista
replace(Pos, L, EN, ListN) :-
  nth0(Pos, L, _, R),
  nth0(Pos, ListN, EN, R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%cambiarElemG(+PosX, +PosY, +Grid, +CN, -NueGrid)
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% Grid grilla de colores pasada por parametro
% CN color nuevo a comparar
% NuevaGrid grilla con el elemento reemplazado en la posicion (PosX,PosY). 
% Retorna una nueva grilla actualizada con el elemento reemplazado
cambiarElemG(PosX, PosY, Grid, CN, NuevaGrid):-
    nth0(PosX, Grid, L),
    replace(PosY, L, CN, ListN),
    replace(PosX, Grid, ListN, NuevaGrid).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%adyacentes(+Grid, +CA, +CN, +PosX, +PosY, -NGrid, -CantAdy)
% Grid grilla de colores pasada por parametro
% CA color actual del elemento de la posicion(PosX,PosY).
% CN color nuevo a comparar
% PosX es la posicion "X" de la celda pasada por parametro
% PosY es la posicion "Y" de la celda pasada por parametro
% NGrid grilla grilla con los adyacentes 
% CantAdy cantidad total de celdas capturadas
% Retorna una grilla con los adyacentes y la cantidad de adaycentes capturados.
adyacentes(Grid, CA, CN, PosX, PosY, NGrid, CantAdy):-
    AdyArriba is PosX-1, 
    AdyAbajo is PosX+1,
    AdyIzq is PosY-1,
    AdyDer is PosY+1,
    cambiarC(Grid, CA, CN, AdyArriba, PosY, Grid1, R1),
	cambiarC(Grid1, CA, CN, PosX, AdyIzq, Grid2, R2),
	cambiarC(Grid2, CA, CN, AdyAbajo, PosY, Grid3, R3),
	cambiarC(Grid3, CA, CN, PosX, AdyDer, NGrid, R4),
    CantAdy is R1+R2+R3+R4.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%sum_num(+A, +B, -C)
% A primer elemento a sumar
% B segundo elemento a sumar
% C resultado de la suma
% Retorna el resultado de la suma.
sum_num(A, B, C):-
          C is A + B.
    